# Reference UA DLL: `ua_hgm`

`hgm_dll.c` is a complete, from-scratch reference implementation of the
[`ua_dll_api.h`](../../src/ua_dll_api.h) ABI (`UA_Mod=9`) that reproduces
OpenFAST's built-in `UA_Mod=4` model -- HGM: Hansen-Gaunaa-Madsen 4-state
Beddoes-Leishman variant -- term-for-term, ported from
`modules/aerodyn/src/UnsteadyAero.f90`. It exists for two reasons:

1. To demonstrate a complete, correct `UA_Mod=9` ABI implementation
   end-to-end (every entry point, the two-call `pack` size protocol,
   restart-safe `unpack`, purity of `ua_dll_output`, lazy per-element
   steady-state initialization, ...).
2. To provide an acceptance target: running the same case through the
   built-in `UA_Mod=4` model and through this DLL under `UA_Mod=9` should
   agree to within double-precision RK4 integration error (see
   [Running the acceptance twin case](#running-the-acceptance-twin-case)
   below).

Full narrative walk-through of the source, cross-referenced against the ABI
contract: `docs/source/user/aerodyn/ua_dll_api.rst`
(`Worked example: the reference HGM DLL`). Full line-by-line traceability
against the Fortran source: `.superpowers/sdd/task-5-report.md`.

Plain C11, no dependencies beyond `libm`. `hgm_dll.c` is entirely
self-contained; it does not include or link against any OpenFAST module --
only `ua_dll_api.h`.

## What it implements

All 7 required entry points:

| Entry point       | What this DLL does |
|--------------------|---------------------|
| `ua_dll_getinfo`   | Reports `n_states_per_elem=4`, `UA_DLL_CAP_PACK` set, no `dt` bounds. |
| `ua_dll_init`      | Deep-copies every polar's `alpha`/`Cl`/`Cd`/`Cm` tables; derives a default `Cd0` and Kirchhoff-inversion separation-function tables (`f_st`, `cl_fs`) per polar, since the ABI doesn't carry these. Every element starts uninitialized (states = 0, `initialized=0`). |
| `ua_dll_update`    | Lazily steady-state-initializes an element on first touch, then advances its 4-state HGM ODE from `t` to `t+dt` with classical RK4, using the two ABI-provided endpoint inputs (linearly interpolated at RK4 mid-point stages). |
| `ua_dll_output`    | Pure w.r.t. `ctx`. For an initialized element, evaluates outputs from the stored state; for an uninitialized element, computes a throwaway local steady state on the stack and evaluates from that -- `ctx` is never written by `output`. |
| `ua_dll_pack`      | Two-call size-query protocol; encodes an `int32_t` element count followed by 4 `double`s per element. |
| `ua_dll_unpack`    | Inverse of `pack`; also marks every element `initialized=1` so the restored state isn't immediately overwritten by the lazy first-touch path. |
| `ua_dll_end`       | Frees every allocation from `init` (safe on a partially-constructed context: every failure path in `init` calls a shared `free_ctx`, and everything is `calloc`-zeroed). |

Documented fidelity gaps relative to the built-in `UA_Mod=4` (see the
`Cd`-tolerance discussion below): simple clip+linear interpolation over the
polar tables in place of `AirfoilInfo`'s cubic-spline interpolation, and the
"Ensuring everything is in harmony" second pass of
`ComputeUASeparationFunction_onCl` (`AirfoilInfo.f90:1279-1292`) is not
reproduced.

## Build

The example is wired into the AeroDyn CMake build behind an option, and is
also built automatically under `BUILD_TESTING` so the acceptance regression
test (`ua_dll_hgm`, see below) has the library available:

```cmake
# modules/aerodyn/CMakeLists.txt
option(BUILD_UA_DLL_EXAMPLES "Build reference UA user-DLL examples (modules/aerodyn/examples/ua_dll_*)" OFF)
if(BUILD_TESTING OR BUILD_UA_DLL_EXAMPLES)
  add_subdirectory(examples/ua_dll_hgm)
endif()
```

To build just this library standalone (without the full regression-test
suite), configure OpenFAST with `-DBUILD_UA_DLL_EXAMPLES=ON`:

```bash
cmake -S . -B build -DBUILD_UA_DLL_EXAMPLES=ON
cmake --build build --target ua_hgm
```

This produces `libua_hgm.dylib` (macOS) / `libua_hgm.so` (Linux) /
`ua_hgm.dll` (Windows) under `build/modules/aerodyn/examples/ua_dll_hgm/`.
If OpenFAST's regression-test suite is configured (`-DBUILD_TESTING=ON`,
the default for a normal dev build), `ua_hgm` is built automatically as
part of the `stage_ua_dll_hgm` target, which also copies it into the
staged test-run directory (see below) as `ua_hgm_dll` (suffix-free on
Linux/macOS, `ua_hgm_dll.dll` on Windows, since `LoadLibraryA` appends
the default `.dll` extension whenever the referenced path has none) so
the single committed driver input works unmodified across the
Linux/macOS/Windows build matrix.

## Running the acceptance twin case

The acceptance case lives in `reg_tests/r-test/modules/unsteadyaero/ua_dll_hgm/`
and consists of two standalone UnsteadyAero-driver input decks that are
identical in every physical setting (`DU21_A17.dat` airfoil, chord, `dt`,
prescribed reduced-frequency motion) except for `UAMod`:

- `UA_hgm.dvr` -- `UAMod=4` (built-in HGM)
- `UA_dll.dvr` -- `UAMod=9`, with:
  ```text
  "./ua_hgm_dll"  UADLLFileName  - Path to user UA dynamic library [used only when UAMod=9].
  ""              UADLLParamFile - Parameter string passed to the UA DLL init [used only when UAMod=9]
  ```
  `UADLLFileName` points at the copy of `ua_hgm` that the `stage_ua_dll_hgm`
  CMake target stages into this test directory at configure/build time as
  `ua_hgm_dll` (see [Build](#build) above) -- it is not resolved against the
  source tree. The single committed path works on every platform because
  `dlopen`/`LoadLibraryA` resolve the extension differently but
  consistently: `dlopen` opens the exact path given, and `LoadLibraryA`
  appends `.dll` to an extension-less path, matching the `.dll`-suffixed
  copy staged on Windows.

This pair is registered as a normal OpenFAST regression test
(`ua_regression("ua_dll_hgm" "unsteadyaero")` in `reg_tests/CTestList.cmake`),
so the simplest way to run it is through ctest, from the build directory:

```bash
ctest -R ua_dll_hgm
```

This runs both decks through the standalone `unsteadyaero_driver` and
compares each against its own committed gold-standard output (standard
OpenFAST regression-test mechanism) -- it does **not** by itself compare
the two decks against *each other*.

To directly verify DLL == built-in equivalence (the actual acceptance
claim), run both decks manually and diff their outputs with
`reg_tests/compare_ua_dll.py`:

```bash
# from the staged test directory, e.g. build/reg_tests/modules/unsteadyaero/ua_dll_hgm/
<path-to>/unsteadyaero_driver UA_hgm.dvr   # built-in UA_Mod=4  -> UA_hgm.Coefs.1.out
<path-to>/unsteadyaero_driver UA_dll.dvr   # UA_Mod=9 (this DLL) -> UA_dll.Coefs.1.out

uv run --with numpy python <repo-root>/reg_tests/compare_ua_dll.py \
    UA_hgm.Coefs.1.out UA_dll.Coefs.1.out \
    --rtol 2e-3 --atol 1e-4 --channels Cl,Cd,Cm,Cn,Cc \
    --steps-per-cycle 200 --loop-shape-pct 0.5
```

`compare_ua_dll.py` checks a relative+absolute tolerance envelope
per-channel plus a last-cycle loop-shape check (catches phase bugs a
mean-error metric would hide). Exit code `0` = pass, `1` = mismatch, `2` =
usage/IO error. The acceptance evidence recorded for this DLL: `Cl`/`Cm`
agree to double precision (bit-identical modulo RK4 integration error);
`Cd` requires an absolute floor of `1.1e-5` (from the linear-vs-cubic-spline
table-interpolation difference noted above); loop shape is exact.
