## 2024-05-22 - [Optimizing N-Body Calculation with OpenMP Offloading]
**Learning:** Found an opportunity to offload the `ui_seg` N-Body calculation in `FVW_BiotSavart.f90` to GPU.
**Action:** Apply `!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO` directives, carefully handling `RegParam` mapping to avoid runtime errors with unallocated arrays, and using local variables for module parameters to prevent SIGFPE on `gfortran`.
