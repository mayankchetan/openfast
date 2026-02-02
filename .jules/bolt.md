## 2024-05-23 - Fortran OpenMP Offloading Pitfalls
**Learning:** Mapping zero-sized array sections (e.g., `Array(1:0)`) or unallocated arrays (even if unused in the kernel) to OpenMP target regions causes runtime crashes (Exit Code 8) in `gfortran`. Scalar arguments to device routines must use the `VALUE` attribute to ensure correct pass-by-value semantics on the GPU.
**Action:** Always wrap target regions with `if (N > 0)` checks. Use `!$OMP DECLARE TARGET` for all module constants. Add `VALUE` attribute to all scalar `intent(in)` arguments in device subroutines.
