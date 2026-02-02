## 2024-05-23 - Fortran OpenMP Offloading Pitfalls (Updated)
**Learning:** Mapping zero-sized array sections (e.g., `Array(1:0)`) or unallocated arrays (even if unused in the kernel) to OpenMP target regions causes runtime crashes (Exit Code 8) in `gfortran`. Scalar arguments to device routines must use the `VALUE` attribute to ensure correct pass-by-value semantics on the GPU.
**Action:** Always wrap target regions with `if (N > 0)` checks. Use `!$OMP DECLARE TARGET` for all module constants. Add `VALUE` attribute to all scalar `intent(in)` arguments in device subroutines.
**Learning:** Mapping unallocated arrays is fatal even if the device code branch doesn't access them.
**Action:** Use host-side `IF` blocks to create separate `TARGET` regions with different `MAP` clauses when dealing with potentially unallocated arrays (e.g., `RegParam` when `idRegNone` is active).
**Learning:** Module `PARAMETER`s may fail to map correctly to OpenMP device regions in `gfortran`, leading to zero values and potential floating-point exceptions (e.g., division by zero).
**Action:** Use literals or locally defined constants within device subroutines instead of relying on module-level `PARAMETER`s for critical thresholds (like `MINNORM`).
