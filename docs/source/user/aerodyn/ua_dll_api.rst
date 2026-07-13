.. _ua_dll_api:

UA User-DLL API Reference (UA_Mod=9)
=====================================

``UA_Mod=9`` lets a user supply their own unsteady-aerodynamics (UA) model as a
shared library (``.so``/``.dylib``/``.dll``) loaded at run time, instead of
using one of AeroDyn's built-in models (``UA_Mod=1..8``, see :numref:`AD_UA`).
The DLL is called through a small, fixed C ABI defined in
``modules/aerodyn/src/ua_dll_api.h``.

.. important::

   **The header is normative.** This page is a human-readable rendering of
   ``modules/aerodyn/src/ua_dll_api.h`` and is maintained by hand; if the two
   disagree, the header is correct. When the header changes, update this page
   in the same commit.

Enabling ``UA_Mod=9`` requires two extra input lines in the AeroDyn primary
file (or the standalone UnsteadyAero driver input) -- ``UADLLFileName`` and
``UADLLParamFile`` -- which must appear as a pair immediately after the
``UA_Mod`` line. See :numref:`ad_ua_inputs` for the full description of
these two lines. Note that ``UA_Mod=9`` is currently rejected at
``AD_Init`` when combined with the free-vortex-wake model
(``WakeMod=3``/OLAF).

.. contents::
   :local:
   :depth: 2


ABI stability and versioning
-----------------------------

The header declares:

.. code-block:: c

   #define UA_DLL_ABI_VERSION 1

Every struct that crosses the ABI boundary carries ``abi_version`` and
``struct_size`` as its first two fields, filled in by the *caller*
(OpenFAST) before the call. The DLL must check both before touching the rest
of the struct:

- ``abi_version`` lets the DLL detect a caller built against an incompatible
  ABI generation and fail cleanly instead of misreading memory.
- ``struct_size`` (``sizeof(...)`` as compiled by the caller) lets a DLL
  built against an *older* header safely ignore fields appended by a newer
  caller, and lets a DLL built against a *newer* header detect that the
  caller can't provide fields it expects. The contract's extension rule is
  append-only: struct layout changes only ever add fields at the end.

The two per-call, per-element structs on the hot path --
``UA_DllElemInput`` and ``UA_DllElemOutput`` -- deliberately do **not**
carry version fields. They are called once per element per time step (and
again per element per output evaluation), so the header keeps them
minimal; their layout is instead pinned by the ``abi_version`` of the
enclosing call.


Struct layouts
---------------

Rendered from ``modules/aerodyn/src/ua_dll_api.h`` (version 1).

``UA_DllInfo`` -- queried once via ``ua_dll_getinfo``, before ``ua_dll_init``:

.. code-block:: c

   typedef struct UA_DllInfo {
       int32_t abi_version;      /* [in] set by caller to UA_DLL_ABI_VERSION; DLL must verify */
       int32_t struct_size;      /* [in] sizeof(UA_DllInfo) as compiled by caller            */
       char    model_name[64];   /* [out] human-readable model name                           */
       int32_t n_states_per_elem;/* [out] informational; -1 if unknown/variable               */
       uint32_t caps;            /* [out] UA_DLL_CAP_* bits                                   */
       double  dt_min;           /* [out] smallest dt the model supports; 0 = no limit        */
       double  dt_max;           /* [out] largest  dt the model supports; 0 = no limit        */
   } UA_DllInfo;

The only capability bit currently defined is ``UA_DLL_CAP_PACK`` (bit 0).
**All 7 entry points, including** ``ua_dll_pack``/``ua_dll_unpack``, **are
required** -- ``UADll_Load`` resolves all 7 symbols and aborts if any is
missing, regardless of this bit. ``UA_DLL_CAP_PACK`` is advisory metadata,
kept for ABI stability: OpenFAST calls ``ua_dll_pack`` after every batched
state update and ``ua_dll_unpack`` on restart unconditionally. A DLL must
implement both so that a ``pack`` followed by an ``unpack`` round-trips its
internal state exactly -- that fidelity, not the capability bit, is what
makes checkpoint/restart exact. Leaving the bit clear only produces a
startup warning; it does not exempt the DLL from implementing pack/unpack
correctly (see `Checkpoint/restart semantics`_).

``UA_DllPolar`` -- one airfoil table (one Re/UserProp interpolation slice),
passed as an array of ``n_polars`` entries inside ``UA_DllInitInput``:

.. code-block:: c

   typedef struct UA_DllPolar {
       int32_t abi_version, struct_size;
       int32_t n_alpha;
       const double *alpha;       /* [n_alpha] rad, ascending, typically -pi..pi              */
       const double *Cl, *Cd, *Cm;/* [n_alpha] each; Cm may be NULL if unavailable            */
       double  alpha0;            /* zero-lift AoA, rad     */
       double  Cl_alpha;          /* lift slope, 1/rad      */
       double  Re;                /* table Reynolds number  */
       double  UserProp;          /* table user property    */
   } UA_DllPolar;

``alpha0`` and ``Cl_alpha`` are passed through verbatim from AeroDyn's own
``AFI_ComputeUACoefs``/``UA_BL`` processing of the airfoil file (the same
values the built-in models use); the DLL does not need to recompute them
from the ``Cl`` table unless it wants to.

Current limitation: ``UA_Mod=9`` marshals only ``Table(1)`` of each
airfoil to the DLL (one ``UA_DllPolar`` per airfoil, not per Re/UserProp
table) -- an airfoil file with more than one table (multi-Reynolds-number
or multi-UserProp polars) raises a fatal error at initialization; the ABI
already supports multiple tables per airfoil via ``polar_id`` (a separate
``polar_id`` per element rather than per airfoil) for a future extension.

``UA_DllInitInput`` -- passed once to ``ua_dll_init``:

.. code-block:: c

   typedef struct UA_DllInitInput {
       int32_t abi_version, struct_size;
       double  dt;                /* glue/UA time step, s                                     */
       double  a_s;               /* speed of sound, m/s                                      */
       double  d_34_to_ac;        /* 3/4-chord to AC distance, chords                         */
       int32_t n_blades;
       int32_t n_nodes_per_blade; /* elements are indexed elem = iB*n_nodes_per_blade + iN    */
       const double *chord;       /* [n_elem] chord per element, m                            */
       const int32_t *polar_id;   /* [n_elem] 0-based index into polars                       */
       int32_t n_polars;
       const UA_DllPolar *polars; /* [n_polars]                                               */
       const char *param_str;     /* UADLLParamFile string from the input file (may be "");
                                     intended use: path to the DLL's own config/weights file  */
       int32_t param_len;         /* strlen(param_str)                                        */
   } UA_DllInitInput;

.. warning::

   Every pointer inside ``UA_DllInitInput`` (``chord``, ``polar_id``,
   ``polars``, and the array pointers inside each ``UA_DllPolar``) is only
   valid for the duration of the ``ua_dll_init`` call. OpenFAST does not
   guarantee the backing memory survives after ``ua_dll_init`` returns. A
   DLL that needs this data later (which is every DLL) **must deep-copy it**
   into its own ``ctx``. The reference implementation
   (``modules/aerodyn/examples/ua_dll_hgm/hgm_dll.c``) does this for every
   array, including derived per-polar tables it builds from them; see
   `Worked example: the reference HGM DLL`_.

``UA_DllElemInput`` -- per-element, per-call-endpoint input (hot path, no
version fields):

.. code-block:: c

   typedef struct UA_DllElemInput {
       double U;        /* relative velocity magnitude at AC, m/s        */
       double alpha34;  /* AoA at 3/4 chord, rad                          */
       double Re;       /* Reynolds number, -                             */
       double UserProp; /* table-interpolation property                   */
       double v_ac_x;   /* AC-relative velocity components, m/s           */
       double v_ac_y;
       double omega;    /* section pitch/twist rate, rad/s                */
   } UA_DllElemInput;

.. _ua_dll_alpha34_caveat:

.. caution::

   **The field is named** ``alpha34`` **but it does not always carry a true
   3/4-chord angle of attack.** OpenFAST populates it from ``u%alpha``, the
   AC-relative flow angle
   (:math:`\alpha_{ac}=\operatorname{atan2}(v_{x,ac},v_{y,ac})`, see
   :numref:`ua_notations`) -- **not** the recomputed 3/4-chord angle
   :math:`\alpha_{34}` that the built-in models derive internally from
   ``v_ac_x``, ``v_ac_y``, ``omega``, and ``d_34_to_ac`` via
   ``Get_Alpha34`` (``UnsteadyAero.f90:2914-2923``). This is a historical
   naming artifact of the struct, not a modeling choice, and it is
   **the single most common integration mistake**: a DLL that reads
   ``alpha34`` and uses it as-is where the underlying physics calls for the
   true 3/4-chord angle will silently get the wrong answer near the tip,
   under pitch-rate, or whenever the aerodynamic center is offset from the
   3/4-chord point.

   The correct pattern -- used throughout the reference DLL -- is to
   recompute the true 3/4-chord angle from the raw kinematics every time it
   is needed:

   .. code-block:: c

      /* UnsteadyAero.f90 Get_Alpha34, lines 2914-2923 */
      static double get_alpha34(double vx, double vy, double omega, double d34_dist) {
          double vx34 = vx + omega * d34_dist;
          return atan2(vx34, vy);
      }
      /* ... */
      double alpha_34 = get_alpha34(fi.vx, fi.vy, omega, d34_frac * chord);

   ``u%alpha`` (i.e. the ABI's ``alpha34`` field) is still used correctly in
   one place in the reference DLL: computing ``CosAlpha``/``SinAlpha`` for
   the :math:`C_n`/:math:`C_c` rotation at output time
   (``UnsteadyAero.f90:3519-3526``), which is defined in terms of the
   AC-relative angle, not the 3/4-chord angle.

``UA_DllElemOutput`` -- per-element output (hot path, no version fields):

.. code-block:: c

   typedef struct UA_DllElemOutput {
       double Cn, Cc, Cl, Cd, Cm;
   } UA_DllElemOutput;


Element indexing
------------------

Elements are flattened across blades and span-wise nodes with a single
0-based index, per the comment on ``n_nodes_per_blade`` in
``UA_DllInitInput``:

.. code-block:: text

   elem = iB * n_nodes_per_blade + iN      (iB, iN both 0-based)

where ``iB`` is the 0-based blade index (``0 .. n_blades-1``) and ``iN`` is
the 0-based span-wise node index (``0 .. n_nodes_per_blade-1``). Every array
sized ``[n_elem]`` in ``UA_DllInitInput`` (``chord``, ``polar_id``), and
every ``n_elem``-length array passed to ``ua_dll_update``/``ua_dll_output``,
uses this ordering. All blades currently share ``n_nodes_per_blade``
(uniform node count across blades); a variable node count per blade is not
representable in this ABI generation.

Note that AeroDyn's ``UAStartRad``/``UAEndRad`` machinery (which disables
unsteady aerodynamics on inner/outer blade elements by radius) is applied
*after* the DLL boundary: the DLL still receives inputs for, and advances
state for, every element in ``0 .. n_elem-1`` regardless of that setting
-- ``ua_dll_update`` has no way to skip or be told about disabled
elements. OpenFAST substitutes its own steady-state airfoil-table
coefficients for a disabled element's output rather than using the DLL's
computed ``ua_dll_output`` result for that element; the DLL still pays
the cost of advancing state for elements whose output it will never see
used, and it cannot distinguish disabled elements from active ones
today.


Entry points
-------------

Seven entry points, resolved by name via ``dlsym``/``GetProcAddress``. Every
call takes a caller-owned ``msg`` buffer of ``msg_len`` bytes that the DLL
may fill with a null-terminated diagnostic on a warning or fatal return (see
`Error convention`_ below); the DLL must never write past ``msg_len`` bytes
and is not required to write anything on success.

.. list-table::
   :header-rows: 1
   :widths: 22 78

   * - Entry point
     - Purpose
   * - ``ua_dll_getinfo``
     - Query model metadata (name, capability bits, state count, dt bounds).
       Called once per rotor instance, immediately before that rotor's
       ``ua_dll_init`` (and again before ``ua_dll_init`` on restart) --
       it has no ``ctx`` yet and cannot depend on per-rotor state.
   * - ``ua_dll_init``
     - Allocate and initialize a DLL-owned opaque context (``ctx``) for one
       rotor instance, from the deep-copyable ``UA_DllInitInput`` (airfoil
       tables, element chords/polar assignments, dt). Called once per rotor
       instance.
   * - ``ua_dll_update``
     - Advance **all** element states for one rotor from ``t`` to ``t+dt``,
       given inputs at both interval endpoints.
   * - ``ua_dll_output``
     - Compute outputs (``Cn``, ``Cc``, ``Cl``, ``Cd``, ``Cm``) for all
       elements at time ``t`` for the *current* states, without mutating
       ``ctx``. May be called many times per step (see `Threading and
       purity rules`_).
   * - ``ua_dll_pack``
     - Serialize ``ctx``'s state into a caller-provided buffer, using the
       two-call size-query protocol (see `Checkpoint/restart semantics`_).
   * - ``ua_dll_unpack``
     - Restore ``ctx``'s state from a previously packed buffer, on restart.
   * - ``ua_dll_end``
     - Free everything owned by ``ctx``. Called once per rotor instance at
       simulation teardown.

Signatures (see the header for the authoritative version):

.. code-block:: c

   int32_t ua_dll_getinfo(UA_DllInfo *info, char *msg, int32_t msg_len);

   int32_t ua_dll_init(const UA_DllInitInput *init, void **ctx,
                       char *msg, int32_t msg_len);

   int32_t ua_dll_update(void *ctx, double t, int64_t step,
                         const UA_DllElemInput *u_t,
                         const UA_DllElemInput *u_tp1,
                         int32_t n_elem, char *msg, int32_t msg_len);

   int32_t ua_dll_output(void *ctx, double t,
                         const UA_DllElemInput *u, int32_t n_elem,
                         UA_DllElemOutput *y, char *msg, int32_t msg_len);

   int32_t ua_dll_pack(void *ctx, unsigned char *buf, int64_t *n_bytes,
                       char *msg, int32_t msg_len);

   int32_t ua_dll_unpack(void *ctx, const unsigned char *buf, int64_t n_bytes,
                         char *msg, int32_t msg_len);

   int32_t ua_dll_end(void *ctx, char *msg, int32_t msg_len);

``ua_dll_update`` receives inputs at **both** interval endpoints
(``u_t``, ``u_tp1``), each an array of ``n_elem`` entries in the flattened
`Element indexing`_ order. This lets a DLL use any quadrature over the step
it wants (the reference DLL uses classical RK4 with linearly-interpolated
mid-point inputs); OpenFAST does not impose an integration scheme.


Lifecycle
----------

.. code-block:: text

   getinfo                                    (once per rotor instance)
      |
      v
   init                                       (once per rotor instance)
      |
      v
   +----------------------------------------+
   | [ update -> output ]  (repeated,        |
   |    output may also be called BEFORE     |
   |    the first update -- see below)       |
   |         |                                |
   |         v  (after a batch of updates,    |
   |       pack     checkpoint boundary)      |
   +----------------------------------------+
      |
      v
   end                                        (once per rotor instance,
                                                simulation teardown)

   --- on restart ---
   getinfo -> init -> unpack(blob from checkpoint) -> [update -> output]* -> end

Key points:

- ``ua_dll_getinfo`` is called once per rotor instance, immediately before
  that rotor's ``ua_dll_init`` (and again on restart, before the restart's
  ``ua_dll_init``/``ua_dll_unpack`` pair) -- it has no ``ctx`` and cannot
  depend on per-rotor state.
- ``ua_dll_init`` is called once per rotor instance (a simulation with
  multiple UA-DLL rotors, e.g. FAST.Farm, calls it once per turbine). The
  ``ctx`` it returns is opaque to OpenFAST and threaded through every
  subsequent call for that rotor.
- Within a rotor's lifetime, ``update`` and ``output`` interleave, but
  **not necessarily 1:1**. In particular, AeroDyn/BEMT's ``CalcOutput`` is
  called repeatedly per time step with varying trial inputs (e.g. during
  BEMT's induction iteration) including *before the first ``update`` has
  ever run* -- see `Threading and purity rules`_ for what a DLL must do
  about this.
- ``ua_dll_pack`` (the UA-DLL entry point, ``UADll_Pack`` on the Fortran
  side) is called after every batched ``update`` to fill ``xd``'s blob with
  the DLL's current state; this is required, not conditioned on
  ``UA_DLL_CAP_PACK``. The generic ``DLLTypePack`` machinery is a separate,
  unrelated mechanism: it only handles the library *handle* (file path,
  reload/re-``dlopen`` on restart), not the DLL's internal state -- it does
  not call ``ua_dll_pack``/``ua_dll_unpack`` itself. Because the blob is
  refreshed after every batched update, it is always current when
  OpenFAST's own checkpoint cadence (a user setting, not tied 1:1 to every
  ``update`` call) decides to write ``xd`` to a restart file -- that
  always-current invariant is what makes checkpoint/restart exact.
- On restart, OpenFAST reloads the DLL automatically (via the same
  DLLTypePack machinery that reloads other user DLLs, e.g. controller
  DLLs), calls ``ua_dll_init`` fresh with the same ``UA_DllInitInput`` as
  the original run, and then calls ``ua_dll_unpack`` with the checkpointed
  blob before any further ``update``/``output`` calls. ``ua_dll_unpack``
  is expected to leave the context in a state equivalent to "already
  initialized" -- the reference DLL, for example, marks every element
  ``initialized = 1`` inside ``ua_dll_unpack`` so its lazy first-touch
  steady-state initialization (see `Worked example: the reference HGM
  DLL`_) does not re-trigger and clobber the restored state.
- ``ua_dll_pack``/``ua_dll_unpack`` are required entry points: OpenFAST
  resolves all 7 symbols at load time and aborts with a fatal error if any
  is missing, so a DLL cannot opt out of implementing them. ``UA_DLL_CAP_PACK``
  left clear in ``ua_dll_getinfo`` only produces a startup warning that the
  DLL's restart fidelity is not guaranteed -- it does not skip the
  ``pack``/``unpack`` calls. A DLL whose ``pack``/``unpack`` do not
  round-trip its state exactly will silently produce an incorrect
  restarted trajectory rather than failing loudly.


Threading and purity rules
----------------------------

- **Single-threaded per rotor instance, today.** OpenFAST does not call
  more than one DLL entry point concurrently for the same ``ctx``. A DLL
  is free to assume no re-entrancy for a given rotor. (Multiple rotors,
  e.g. in FAST.Farm, each get their own ``ctx`` from their own
  ``ua_dll_init`` call; nothing in the ABI currently requires or forbids
  those to be called from different threads, but the reference
  implementation and the built-in bridge both drive everything from a
  single thread.)
- **``ua_dll_output`` must not modify state reachable through ``ctx``.**
  This is a hard contract, not a suggestion: AeroDyn/BEMT calls
  ``CalcOutput`` repeatedly per time step with different trial inputs as
  part of its induction-factor iteration, and expects every call to be a
  pure function of ``(ctx, t, u)`` -- calling it twice with the same
  arguments must return the same result, and calling it does not advance
  or perturb the model's internal state. Only ``ua_dll_update`` may
  mutate state.
- **Uninitialized elements: expect and honor the local-steady-state
  contract.** ``ua_dll_output`` can be called for an element **before**
  that element's first ``ua_dll_update`` has ever run (AeroDyn's
  ``CalcOutput`` is exercised once before the first `UpdateStates`, and
  potentially again during initialization/linearization probing). A DLL
  must not treat this as an error. The documented, built-in-equivalent
  behavior is: compute a *local* steady-state response from the inputs
  passed to that specific ``output`` call, use it to produce ``y``, and
  discard it -- do **not** write it into ``ctx``. Each such call
  recomputes its own steady state from its own inputs; it is not "sticky"
  across calls. The reference DLL implements this exactly (see
  ``ua_dll_output`` in ``hgm_dll.c``, which computes into a stack-local
  array for un-initialized elements and only reads/advances ``ctx``-owned
  state for elements that have already seen an ``update``).


Error convention
------------------

Every entry point returns an ``int32_t``:

.. list-table::
   :header-rows: 1
   :widths: 20 80

   * - Return value
     - Meaning
   * - ``0`` (``UA_DLL_OK``)
     - Success.
   * - ``> 0``
     - Warning. The simulation continues; the ``msg`` buffer should contain
       a human-readable explanation, which OpenFAST surfaces through its
       normal warning-logging path.
   * - ``< 0``
     - Fatal. OpenFAST aborts the run; ``msg`` should explain why.

``msg`` is always a caller-owned buffer of ``msg_len`` bytes. The DLL must
null-terminate anything it writes and must not write ``msg_len`` bytes or
more (i.e. respect the buffer like ``snprintf`` does). Writing to ``msg``
is optional on a plain ``UA_DLL_OK`` return.


Checkpoint/restart semantics
-------------------------------

``ua_dll_pack`` uses a two-call size-query protocol, matching the pattern
OpenFAST uses elsewhere for variable-length serialization:

1. **Size query**: caller passes ``buf == NULL``. The DLL must set
   ``*n_bytes`` to the number of bytes it needs and return without touching
   any other memory.
2. **Write**: caller allocates a buffer of (at least) that many bytes and
   calls again with ``buf`` pointing at it and ``*n_bytes`` set to the
   buffer's capacity. The DLL must check ``*n_bytes >= needed`` (return a
   fatal error if not, since the caller under-allocated), write its state
   into ``buf``, and set ``*n_bytes`` to the number of bytes actually
   written.

``ua_dll_unpack`` is the inverse: given a buffer and its exact length (as
previously reported by ``pack``), restore ``ctx`` to that state. It is
called on a ``ctx`` that ``ua_dll_init`` has already created (with the same
``UA_DllInitInput`` as the run that produced the checkpoint) -- ``unpack``
restores per-element state, not re-derives per-rotor configuration.

The resulting blob is opaque to OpenFAST: it is embedded, verbatim, in the
UA module's discrete-state checkpoint (``xd``) and written out through the
existing DLLTypePack restart machinery, alongside the DLL's own file path
so it can be reloaded and re-``dlopen``'d automatically on restart. A DLL
is free to choose any internal encoding for the blob (the reference DLL
uses a flat ``int32_t`` element count followed by 4 ``double`` states per
element -- see ``ua_dll_pack``/``ua_dll_unpack`` in ``hgm_dll.c``) as long
as ``pack`` followed by ``unpack`` round-trips exactly.

``ua_dll_pack`` and ``ua_dll_unpack`` are always called -- after every
batched ``update`` and on every restart, respectively -- regardless of
``UA_DLL_CAP_PACK``. A DLL that reports the bit unset in
``ua_dll_getinfo`` is only flagging (informationally) that its
implementation of these two entry points may not be trustworthy for exact
restart; OpenFAST still calls them, and a DLL is still required to provide
working (if imperfect) implementations rather than stubs, since
``UADll_Load`` treats all 7 symbols as mandatory.


.. _ua_dll_hgm_walkthrough:

Worked example: the reference HGM DLL
----------------------------------------

``modules/aerodyn/examples/ua_dll_hgm/hgm_dll.c`` is a complete, from-scratch
reference implementation of OpenFAST's built-in ``UA_Mod=4`` model (HGM:
Hansen-Gaunaa-Madsen 4-state Beddoes-Leishman variant, see
:numref:`AD_UA`), ported term-for-term from
``modules/aerodyn/src/UnsteadyAero.f90`` and built as a standalone
``UA_Mod=9`` DLL. It exists to (a) demonstrate a complete, correct ABI
implementation end-to-end, and (b) provide an acceptance target: running
the same case through the built-in ``UA_Mod=4`` and through this DLL under
``UA_Mod=9`` should agree to within double-precision RK4 integration error.
See ``modules/aerodyn/examples/ua_dll_hgm/README.md`` for build and
acceptance-run instructions, and
``.superpowers/sdd/task-5-report.md`` for the full line-by-line
traceability table against the Fortran source.

Reading the file top to bottom:

``getinfo`` (``hgm_dll.c`` around ``ua_dll_getinfo``)
   Reports 4 states per element, ``UA_DLL_CAP_PACK`` set, and no ``dt``
   bounds (the RK4 integration is unconditionally stable for the time
   steps this model is exercised at).

``init``
   Deep-copies every polar's ``alpha``/``Cl``/``Cd``/``Cm`` arrays (per the
   `Struct layouts`_ warning above), then derives two things AeroDyn's own
   ``AirfoilInfo`` module would otherwise compute internally and that the
   ABI has no field for: a default ``Cd0`` (minimum ``Cd`` over
   :math:`|\alpha|\le20^\circ`, mirroring ``AirfoilInfo.f90:929-937,972``)
   and, per polar, a Kirchhoff-inversion separation-function table
   (``f_st``, ``cl_fs``) on the polar's native ``alpha`` grid, mirroring
   ``AirfoilInfo.f90``'s ``ComputeUASeparationFunction_onCl``. Every
   element starts with all 4 states at ``0.0`` and an ``initialized`` flag
   at ``0`` -- the actual steady-state initial condition is computed lazily
   on first touch, because ``ua_dll_init`` receives no element *inputs*
   (only geometry/polar configuration), so there is no angle of attack yet
   to initialize a steady state from.

``update``
   For each element, if this is the first call touching it,
   ``hgm_steady_init`` sets the 4 states to their closed-form steady-state
   values from the ``t``-endpoint input (mirroring
   ``OtherState%FirstPass``/``HGM_Steady`` in the Fortran). Then a classical
   4-stage RK4 step advances the state derivative (``hgm_deriv``, the
   ``UA_HGM`` branch of ``UA_CalcContStateDeriv``) from ``t`` to ``t+dt``,
   using the two ABI-provided endpoint inputs for the RK4 stage inputs
   (linearly interpolated at the mid-point stages) -- see the
   `Element indexing`_ / entry-point description above for why both
   endpoints are provided. Every derivative evaluation recomputes the true
   3/4-chord angle from raw kinematics via ``get_alpha34``, per the
   `alpha34 caveat <ua_dll_alpha34_caveat_>`_ above; it never reads the
   ABI's ``alpha34`` field for this purpose.

``output``
   Pure with respect to ``ctx``, per `Threading and purity rules`_: for an
   already-``initialized`` element it evaluates ``hgm_output`` (the
   ``UA_HGM`` branch of ``UA_CalcOutput``) directly on ``ctx``'s stored
   states; for a not-yet-``initialized`` element it computes a throwaway
   local steady state on the stack, evaluates outputs from *that*, and
   discards it -- ``ctx`` is never touched either way.

``pack``/``unpack``
   Implement the two-call size protocol described above with the simplest
   possible encoding: an ``int32_t`` element count followed by 4
   ``double``\ s per element, in `Element indexing`_ order. ``unpack`` also
   sets every element's ``initialized`` flag to ``1``, since a restarted
   run's states did not come from the lazy first-touch path and must not
   be silently overwritten by it.

``end``
   Frees every allocation made in ``init`` (polars' array copies and
   derived tables, the element array, and the context itself); written to
   be safe to call on a partially-constructed context (every allocating
   path in ``init`` calls a shared ``free_ctx`` on failure, and everything
   is ``calloc``-zeroed so a ``free(NULL)`` on an unreached member is a
   no-op).

Documented fidelity gaps relative to the built-in ``UA_Mod=4`` (all cited
in the source comments and in ``task-5-report.md``): the reference DLL
uses simple clip+linear interpolation over the polar tables where
``AirfoilInfo`` uses cubic-spline interpolation, and the "Ensuring
everything is in harmony" second pass in
``ComputeUASeparationFunction_onCl`` (``AirfoilInfo.f90:1279-1292``) is not
reproduced. These show up as a small, well-characterized absolute floor
(:math:`\approx1.1\times10^{-5}`) on the acceptance comparison's ``Cd``
tolerance; ``Cl`` and ``Cm`` agree to double-precision (bit-identical
modulo RK4 integration error), and the hysteresis loop shape is exact.


.. _ua_dll_ml_packaging:

Shipping an ML model as a UA DLL
-----------------------------------

Nothing in the ABI is specific to closed-form aerodynamic models -- any
function that can be expressed as ``state, inputs -> state', outputs``
inside a shared library qualifies. This section gives packaging guidance
for shipping a machine-learned (ML) UA model through ``UA_Mod=9``.

**Pattern 1: wrap the ONNX Runtime C API.**

If the model was trained in a standard ML framework and exported to ONNX,
the DLL itself can be a thin C (or C++) shim around ONNX Runtime's C API:
``ua_dll_init`` creates an ``Ort::Env``/``Ort::Session`` from the model file
named in ``UADLLParamFile`` (see `Struct layouts`_ -- ``param_str`` is the
intended vehicle for "path to the DLL's own config/weights file"),
``ua_dll_update``/``ua_dll_output`` marshal ``UA_DllElemInput`` fields into
input tensors and run inference. Two operational details matter in
practice:

- **Co-locate ``libonnxruntime`` with the DLL.** Do not rely on the ONNX
  Runtime shared library being on the system's default loader search path
  at the target site -- ship it next to the UA DLL (same directory as
  ``UADLLFileName``) and load it with an explicit ``dlopen``
  (``LoadLibrary`` on Windows) using a path relative to the UA DLL's own
  location (e.g. resolved via ``dladdr``/``GetModuleFileName`` from inside
  the DLL, not assumed from the process's current working directory),
  rather than depending on ``LD_LIBRARY_PATH``/``PATH`` being set correctly
  wherever the simulation happens to run.
- **Batch across elements in one inference call where possible.**
  ``ua_dll_update``/``ua_dll_output`` are already called once per rotor
  with all ``n_elem`` elements passed together (see `Element indexing`_)
  specifically so a DLL can batch; do not make one inference call per
  element if the model supports batched input, since inference
  call overhead dominates for small per-element models.

**Pattern 2: embed weights in pure C.**

For small models (e.g. a compact MLP or a handful of dense layers) it is
often simpler and more portable to skip a runtime dependency entirely and
compile the weights directly into the DLL as ``static const`` arrays, with
hand-written (or generator-emitted) forward-pass code. This avoids the
ONNX Runtime co-location problem above at the cost of needing to
regenerate/recompile the DLL whenever the weights change. For teams
working from a pure-Fortran modeling stack rather than C, the `fiats
<https://go.lbl.gov/fiats>`_ project (pure-Fortran neural-network
inference) is a relevant reference point for the equivalent pattern in
Fortran, which can then be wrapped with ``bind(C)`` interfaces to satisfy
this ABI's ``extern "C"`` entry points.

**State-vector design for recurrent models.**

A recurrent model (RNN/GRU/LSTM-style, or any model whose output depends on
more than the current-step input) maps naturally onto this ABI: the hidden
state lives inside the DLL's ``ctx`` (exactly where the reference HGM DLL
keeps its 4-element state vector, ``Elem.x[4]`` in ``hgm_dll.c``), advanced
once per element in ``ua_dll_update`` and read (never mutated) in
``ua_dll_output``. Two things to get right:

- **Serialize the hidden state in ``pack``, honestly sized.** The hidden
  state is exactly what must round-trip through
  ``ua_dll_pack``/``ua_dll_unpack`` for restart to reproduce the
  pre-checkpoint trajectory (see `Checkpoint/restart semantics`_). Both
  entry points are mandatory regardless of ``UA_DLL_CAP_PACK`` (OpenFAST
  calls them unconditionally), so implement them to round-trip the hidden
  state exactly. Set ``UA_DLL_CAP_PACK`` once that round-trip is verified;
  leave it unset only as an honest signal that the implementation has not
  been validated for exact restart, not as a way to skip implementing it.
- **Size ``n_states_per_elem`` honestly in ``ua_dll_getinfo``.** If the
  hidden-state size is fixed and known at ``getinfo`` time (before
  ``ua_dll_init``, so before the element count is even known), report it.
  If it depends on configuration only available at ``init`` time (e.g. a
  variable hidden-state width read from the ``UADLLParamFile`` weights
  file), or genuinely varies per element, report ``-1``
  ("unknown/variable") rather than guessing -- ``n_states_per_elem`` is
  documented as informational only (see `Struct layouts`_), so nothing in
  the ABI breaks if it is ``-1``, but a wrong non-``-1`` value is
  actively misleading to any tooling or documentation that trusts it.
