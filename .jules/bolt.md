## 2024-05-24 - OpenMP Offloading in Fortran
**Learning:** When porting Fortran subroutines to OpenMP target regions, intrinsic matrix functions like `matmul` and `transpose` can cause linking or runtime issues on some devices (and with `gfortran` offloading). Explicitly unrolling these operations (e.g., 3x3 matrix-vector multiply) is more robust.
**Action:** Always replace `matmul`/`transpose` with explicit loops in `!$OMP DECLARE TARGET` subroutines.

**Learning:** Accessing module-level `PARAMETER` constants (like `Pi`) inside `!$OMP DECLARE TARGET` subroutines can cause runtime crashes (Exit Code 8) in `gfortran` OpenMP offloading.
**Action:** Define local parameters using literals (e.g., `3.14159..._ReKi`) within the device subroutine instead of using module-level constants or intrinsics like `ACOS(-1.0)`.

## 2026-02-21 - OpenMP Offloading - Parameter Initialization
**Learning:** Initializing `PARAMETER`s with intrinsics (e.g., `epsilon(1.0_ReKi)`) inside `!$OMP DECLARE TARGET` subroutines causes runtime failures (Exit Code 8/SIGFPE) in `gfortran` OpenMP offloading.
**Action:** Use local variables initialized at runtime or pass values as arguments instead of using `parameter` attribute with intrinsics.

**Learning:** Accessing module-level parameters directly in OpenMP device kernels can fail.
**Action:** Assign module parameters to local variables on the host, map them as `firstprivate`, and pass them as arguments to the device subroutine.

**Learning:** Modifying the signature of shared helper functions (e.g., to pass `epsilon`) for device kernels breaks compilation for existing host callers.
**Action:** Create dedicated `_target` versions of helper functions instead of modifying shared ones.
