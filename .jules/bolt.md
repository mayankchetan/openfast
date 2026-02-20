## 2024-05-22 - Fortran OpenMP Offloading Patterns
**Learning:** Standard Fortran intrinsics (`matmul`, `transpose`, `epsilon` in parameters) can cause runtime failures or performance issues in OpenMP `target` regions.
**Action:** Create specialized `_target` kernels with manually unrolled matrix operations and use runtime logic or literals instead of intrinsic-based `parameter` initialization.
