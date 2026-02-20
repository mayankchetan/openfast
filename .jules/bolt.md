## 2024-05-22 - Fortran OpenMP Offloading Patterns
**Learning:** Standard Fortran intrinsics (`matmul`, `transpose`, `epsilon` in parameters) can cause runtime failures or performance issues in OpenMP `target` regions.
**Action:** Create specialized `_target` kernels with manually unrolled matrix operations and use runtime logic or literals instead of intrinsic-based `parameter` initialization.

## 2024-05-22 - OpenMP Directive Placement in Fortran Modules
**Learning:** In Fortran modules (especially with `gfortran`), `!$OMP DECLARE TARGET` directives for internal procedures (those inside `CONTAINS`) must be placed in the module specification part (before `CONTAINS`) using the list form `(proc_name)`. Placing them inside the `CONTAINS` section immediately before the procedure definition causes compilation errors.
**Action:** Always verify `!$OMP DECLARE TARGET` placement for contained procedures.
