## 2024-05-22 - [OpenMP Offloading for Biot-Savart N-Body]
**Learning:** Fortran OpenMP offloading requires explicit `!$OMP DECLARE TARGET` for all subroutines called within a target region.
**Action:** Always verify call graphs for offloaded regions and tag dependencies.

**Learning:** Manual inlining in legacy Fortran codes (like `ui_seg`) simplifies GPU offloading by reducing subroutine calls, but requires duplicating directives across `CASE` blocks.
**Action:** Accept code duplication for performance directives when refactoring is too risky or changes the optimization pattern.

**Learning:** Assumed-shape arrays (dimension(:,:)) in Fortran cause Exit Code 8 (Segfault/SIGFPE) when mapped to OpenMP devices using `gfortran` without explicit bounds.
**Action:** Always use explicit bounds in map clauses for assumed-shape arrays, e.g., `map(to: Array(1:3, 1:size(Array,2)))`. This rule applies even if only a subset of functions crashed initially; consistent application prevents regression failures like `MHK_RM1_Floating_MR_Linear`.
