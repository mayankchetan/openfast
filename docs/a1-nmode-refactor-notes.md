# A1: ElastoDyn tower N-mode refactor — closeout notes

Branch `f/twr-nmodes`, base commit `f5a01662c4fcac2c6ec584efeebb3c4e9e7c3725`
(merge of PR #3348, `dev` tip at branch creation, 2026-06-25). Six commits on
top of base; N=2 (the input file's fixed 2 fore-aft + 2 side-to-side modes)
is bit-for-bit identical to legacy across all seven gate cases.

## What changed, per commit

**`d07a6ff40`** — Adds `SetTowerDOFMap`, a runtime routine computing the tower
DOF indices (`p%DOF_TFA(:)`, `p%DOF_TSS(:)`) that previously lived as
compile-time `PARAMETER`s (`DOF_TFA1/TSS1/TFA2/TSS2`). A temporary assertion
cross-checks the new map against the legacy constants at every call site
during the transition (removed in a later commit once the constants are
gone). No behavior change; values are identical to the legacy layout.

**`211be11f1`** — Sweeps all *non-tower* DOF `PARAMETER`s (yaw, rotor-furl,
generator azimuth, drivetrain, tail-furl, teeter, blade pitch/edge/flap,
`ED_MaxDOFs`, and the `NPx.. / Px..` body-DOF-set constants) from
compile-time constants to the runtime `p%` DOF map. Makes `p%NDOF`
offset-aware: `p%NDOF = {19,24,27} + (p%NTwFAModes + p%NTwSSModes - 4)`,
identical to the legacy literal at the fixed 2+2 count. Two flagged
deviations from the plan's stated two-file scope, both accepted:
- `modules/openfast-library/src/FAST_AeroMap.f90` — the sole external
  (non-ElastoDyn) consumer of the swept `DOF_BF`/`DOF_BE` constants;
  repointed to the already-in-scope `T%ED%p(iED)%DOF_BF/DOF_BE` runtime
  fields.
- `RotorTeeter` MV_AddVar/linearization registration gained an
  `IF (p%NumBl == 2)` guard. Without the legacy artificial cap,
  `p%DOF_Teet` (28) can exceed `p%NDOF` (27) for 3-bladed rotors; this
  fixes a latent out-of-bounds read that the old code masked, not a
  behavior change at N=2/NumBl=2.

**`e0013700e`** — Generalizes the tower-modal init block (`Coeff`,
`Init_DOFparameters`, `SetEnabledDOFIndexArrays`) from hardcoded 2+2 arrays
to N-sized arrays keyed off `p%NTwFAModes`/`p%NTwSSModes`. `TwrFASF`,
`TwrSSSF`, `AxRedTFA`, `AxRedTSS`, `KTFA`, `KTSS`, `CTFA`, `CTSS`, `FreqTFA`,
`FreqTSS` become allocatable and are sized at init. Legacy `TwFAM1Sh`/
`TwFAM2Sh` and `TwSSM1Sh`/`TwSSM2Sh` inputs are repacked into per-mode
`TwFAMSh(:,mode)`/`TwSSMSh(:,mode)` arrays feeding a per-mode `SHP`-fill loop
(seam #1 — see below). Every modal integral (mass, stiffness, gravity
stiffness, axial reduction, stiffness tuners, frequency, damping) converts
from literal `1,2` bounds to `1,NTwFAModes`/`1,NTwSSModes`, with the
previously-shared FA+SS loops split into independent FA and SS loops (safe:
the two accumulations don't interact, so splitting doesn't reorder any sum).

**`40a54ee12`** — Converts the hand-expanded 2-term tower rotation/position
sums in the Kane kinematics to N-mode loops: `ThetaFA`/`ThetaSS`, a new
`TwrAxRedDisp` helper reproducing the legacy 6-term axial-reduction quadratic
form's exact term order (diagonals, then the two off-diagonal cross terms,
FA before SS), and the `rZO`/`rT0T` position sums via `TmpSumFA`/`TmpSumSS`
accumulators. **This is where the FMA-contraction discovery happened**: a
Fortran `DO`-loop reduction over the same terms as a hand-unrolled literal
expression is not guaranteed bit-identical to it under default
`-ffp-contract=fast`, regardless of compile-time-constant trip counts —
verified by direct instrumentation and an 11-variant bisection. The gate
regime was amended (user decision) to build with `-ffp-contract=off` and
the golden references were regenerated under that flag as `baseline-fpoff.*`
(original `golden.*` retained for provenance, no longer the active
reference).

**`9e038582b`** — Converts partial angular/linear velocities (`PAngVelEB`/
`PAngVelEF`, `PLinVelEO`/`PLinVelET`, both 0th/1st derivatives) and the
diagonal AugMat K/C row sums to N-mode loops, using
`p%AxRedTFA/TSS(MIN(I,L),MAX(I,L),node)` for symmetric off-diagonal access.
The velocity accumulations that consume these (`AngVelEB`/`AngPosXB`/
`AngAccEBt`, `AngVelEF`/`AngPosXF`/`AngAccEFt`, `LinVelXO`/`LinAccEOt`,
`LinVelXT`/`LinAccETt`) use an interleaved FA/SS loop that preserves the
legacy term order exactly. Initial conditions (`TTDspFA`/`TTDspSS`) go to
the first-enabled mode.

**`cca38951c`** — Routes the `Q_TFA1/TSS1/TFA2/TSS2` (+`QD_*`/`QD2_*`) output
channels and the `PH`/`PM` angular-velocity-contributor arrays through
`p%DOF_TFA(:)`/`p%DOF_TSS(:)` instead of the legacy constants (seam #2 — see
below). The four diagonal AugMat blocks become two per-mode loops. The four
legacy `TowerFA1/SS1/FA2/SS2` `MV_AddVar` linearization-registration calls
are replaced by an interleaved loop+remainder-split reproducing the
byte-identical `.lin` state names, order, and Perturb schedule. The four
tower DOF `PARAMETER`s and the Task-2 scaffolding assertion are deleted —
the runtime DOF map is now the sole source of truth for tower DOF indices.

## Gate regime

Acceptance is bit-for-bit identity of `.out` (`ES19.11E3`) and `.lin` text,
headers excluded, on the 7 gate cases (`a1-golden/tools/gate.sh`), compiled
with `-ffp-contract=off` (build-config flag only, `CMAKE_Fortran_FLAGS` —
never in committed source or `CMakeLists.txt`). This flag was forced by the
Task 4 (`40a54ee12`) FMA-contraction discovery: default `-ffp-contract=fast`
does not guarantee a loop-reduction is bit-identical to the hand-unrolled
literal it replaces, even at a compile-time-constant trip count. Both the
base-commit golden generation and every branch build use this flag; the
active reference set is `baseline-fpoff.*` (the original `golden.*`, built
with default contraction, is kept only for provenance and is not compared
against). Final sweep for this task: **ALL GATES PASS** on all 7 cases (13
files: 5 `.out` non-linear, 2 `.out` + 1 `.1.lin` linear WP case, 1 `.out` +
4 `.1.*.lin` linear 5MW case) — see `/tmp/t8gate.log`.

## The two A1′ seams

The eigensolve/generalization-past-2+2 phase (A1′) has two concrete places
to pick up:

1. **`TwFAMSh`/`TwSSMSh` repack (introduced `e0013700e`)** — the legacy
   input file still only supplies `TwFAM1Sh`/`TwFAM2Sh` and
   `TwSSM1Sh`/`TwSSM2Sh` (2 fixed mode-shape polynomials each). These are
   repacked into `TwFAMSh(:,mode)`/`TwSSMSh(:,mode)` arrays sized
   `p%NTwFAModes`/`p%NTwSSModes`, but the *source* of that repack is still
   the two legacy scalar inputs — `p%NTwFAModes = 2` and `p%NTwSSModes = 2`
   are hardcoded at input-processing time (`ElastoDyn.f90`, "A1: legacy
   input path is fixed at 2+2 modes"). A1′ needs a new input path (or an
   eigensolve-derived mode set) that populates N>2 mode shapes here.
2. **Channel/PH-PM table (`cca38951c`)** — the `p%PH`/`p%PM` arrays (DOFs
   contributing to hub/blade-element angular velocity) and `p%NPH`/`p%NPM`
   counts (11/12 and 15/16) still hard-list `p%DOF_TFA(1)`, `p%DOF_TSS(1)`,
   `p%DOF_TFA(2)`, `p%DOF_TSS(2)` explicitly rather than looping over
   `p%NTwFAModes`/`p%NTwSSModes`. This was deliberately deferred: at N=2 the
   literal list and a loop are equivalent, and generalizing it prior to
   having a real N>2 eigensolve source would be speculative. A1′ must
   replace these four-element literals with `DO`-loops over the mode counts
   before N can exceed 2.

## Accepted deviations (full list)

- `modules/openfast-library/src/FAST_AeroMap.f90` changed — sole external
  consumer of the swept `DOF_BF`/`DOF_BE` constants (see `211be11f1` above).
- `RotorTeeter` `MV_AddVar` gained an `IF (p%NumBl == 2)` guard — fixes a
  latent out-of-bounds read for 3-bladed rotors, not a behavior change at
  N=2/NumBl=2 (see `211be11f1` above).
- Gate reference regenerated under `-ffp-contract=off` after the FMA-
  contraction discovery in `40a54ee12`; `golden.*` (default contraction)
  retained for provenance only, superseded by `baseline-fpoff.*`.
- `p%PH`/`p%PM` arrays converted to allocatable but **not** generalized past
  2+2 explicit tower-DOF references — deferred to A1′ (seam #2 above).
- `TwrAxRedDisp` (new helper in `40a54ee12`) is a candidate for `PURE`
  attribute; not applied in this branch (no correctness impact, deferred as
  a style/optimization item).

## Grep audit of remaining literal-2 tower assumptions

`grep -nE '\(2,2\)|\(2, 2\)|DO I = 1,2\b' ElastoDyn.f90 | grep -iv blade` was
run and every hit inspected. None are tower-mode literals that escaped
generalization:

| Hit(s) | Context | Classification |
|---|---|---|
| `DOF_BF(2,2)` (Q_B2F2/QD_B2F2/QD2_B2F2, ~L1459-1467) | Blade 2nd-flap-mode output channels, indexed into the (unchanged) blade DOF array `p%DOF_BF` | intentional — blade array, out of this refactor's scope |
| `DO I = 1,2` × 6 (~L4962-5111) | Twisted-shape-function (`Phi`/`Psi`) and blade flap-DOF integration loops, all inside blade-modal-init code (comments read "Loop through Phi and Psi" / "Loop through flap DOFs" / "Loop through all blade DOFs") | intentional-legacy — blade code, not tower; missed the `grep -iv blade` filter only because the line itself doesn't contain the literal word "blade" |
| `A(2,2)`, `TransMat(2,2)`, `Orientation(2,2)` (~L5443-9462) | 2×2/3×3 rotation- and coordinate-transformation-matrix element indices (element `(2,2)` of a fixed-size direction-cosine matrix) | intentional — matrix-element index, unrelated to tower mode count |

Separately, the known-and-accepted legacy-input-path literal-2 sites (not
part of the grep pattern above, listed here per the brief's classification
requirement):
- `TwFADOF1`/`TwFADOF2`, `TwSSDOF1`/`TwSSDOF2` — legacy 2-mode DOF-enable
  input flags, repacked into `TwFADOF(p%NTwFAModes)`/`TwSSDOF(p%NTwSSModes)`
  local arrays via a 2-element array constructor (fine: `NTwFAModes`/
  `NTwSSModes` are hardcoded to 2 at input time, see seam #1).
- `p%DOF_TFA(1)`, `p%DOF_TSS(1)`, `p%DOF_TFA(2)`, `p%DOF_TSS(2)` explicit
  refs in the `p%PH(1:11)`/`p%PM(K,:)` array constructors and `p%NPH=11/12`,
  `p%NPM=15/16` literal counts — see seam #2 above.
- `TwrFAPerturbFact(2)`/`TwrSSPerturbFact(2)` — legacy per-mode
  linearization Perturb-factor schedule (`0.020, 0.002`), size-2 by design
  since it encodes two specific legacy perturbation magnitudes, not a mode
  count.

**No hit represents a tower-mode assumption that would silently break at
N>2** — the "must be empty or reported" list is empty.

## Verification sweep for this task

- Tree: clean, branch `f/twr-nmodes` at `cca38951c`, no uncommitted changes.
- Build config: `CMAKE_Fortran_FLAGS = -ffp-contract=off` (verified via
  `grep fp-contract build/CMakeCache.txt`, not reconfigured).
- `cmake --build build --target openfast -j8` — clean build, exit 0.
- `bash a1-golden/tools/gate.sh` — **ALL GATES PASS**, all 7 cases, all 13
  files (`.out`/`.lin`), bit-identical.
- Full `ctest -L elastodyn`/r-test sweep: **deferred to PR CI.** The r-test
  harness compares against committed platform baselines at numeric
  tolerance (needs `numpy`/`bokeh`), which is a materially different (and
  weaker) acceptance criterion than this branch's bit-for-bit gate. Per the
  task brief, the bit-for-bit `gate.sh` evidence supersedes a tolerance-pass
  ctest sweep for this branch's acceptance; standing up the full r-test
  venv/ctest infrastructure is left to the PR's CI rather than spending
  additional local iteration on it.
