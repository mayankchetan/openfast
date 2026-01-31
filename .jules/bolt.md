## 2026-01-31 - OpenMP Offloading in Fortran N-Body
**Learning:** When offloading Fortran subroutines that call other internal subroutines, scalar arguments must be passed by `VALUE` to avoid issues on the device. Module parameters used on the device must be explicitly declared with `!$OMP DECLARE TARGET`.
**Action:** Always check argument attributes and module scope visibility when porting legacy Fortran to GPU.
