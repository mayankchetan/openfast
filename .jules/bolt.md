## 2024-05-22 - [OpenMP Offloading for Biot-Savart N-Body]
**Learning:** Fortran OpenMP offloading requires explicit `!$OMP DECLARE TARGET` for all subroutines called within a target region.
**Action:** Always verify call graphs for offloaded regions and tag dependencies.

**Learning:** Manual inlining in legacy Fortran codes (like `ui_seg`) simplifies GPU offloading by reducing subroutine calls, but requires duplicating directives across `CASE` blocks.
**Action:** Accept code duplication for performance directives when refactoring is too risky or changes the optimization pattern.
