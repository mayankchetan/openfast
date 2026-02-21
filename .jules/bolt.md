## 2024-05-24 - OpenMP Offloading in Fortran
**Learning:** When porting Fortran subroutines to OpenMP target regions, intrinsic matrix functions like `matmul` and `transpose` can cause linking or runtime issues on some devices (and with `gfortran` offloading). Explicitly unrolling these operations (e.g., 3x3 matrix-vector multiply) is more robust.
**Action:** Always replace `matmul`/`transpose` with explicit loops in `!$OMP DECLARE TARGET` subroutines.

**Learning:** Accessing module-level `PARAMETER` constants (like `Pi`) inside `!$OMP DECLARE TARGET` subroutines can cause runtime crashes (Exit Code 8) in `gfortran` OpenMP offloading.
**Action:** Define local parameters using literals (e.g., `3.14159..._ReKi`) within the device subroutine instead of using module-level constants or intrinsics like `ACOS(-1.0)`.
