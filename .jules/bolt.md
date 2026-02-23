
## 2024-05-23 - Fortran OpenMP Offloading Patterns
**Learning:** When offloading Fortran subroutines that use external module functions (like `EqualRealNos`), reimplement them locally with `!$OMP DECLARE TARGET` to avoid modifying shared libraries. Also, calculate `PARAMETER` constants (like `epsilon`) at runtime in device functions to avoid initialization issues on GPU.
**Action:** Use local `_Target` helper functions and runtime constant calculation for robust OpenMP offloading.
