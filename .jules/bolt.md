## 2026-01-31 - OpenMP Offloading in Fortran N-Body
**Learning:** When offloading Fortran subroutines that call other internal subroutines, scalar arguments must be passed by `VALUE` to avoid issues on the device. Module parameters used on the device must be explicitly declared with `!$OMP DECLARE TARGET`.
**Action:** Always check argument attributes and module scope visibility when porting legacy Fortran to GPU.
## 2026-01-31 - Fortran OpenMP Syntax Strictness
**Learning:** The closing directive for combined OpenMP constructs like `! TARGET TEAMS DISTRIBUTE PARALLEL DO` must be strictly matched, e.g., `! END TARGET TEAMS DISTRIBUTE PARALLEL DO`. Simply using `! END TEAMS...` is insufficient and causes compilation errors in some compilers (e.g. gfortran).
**Action:** Always verify the full string match for OpenMP END directives.
