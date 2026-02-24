## 2024-05-22 - Fortran OpenMP Target Parameter Initialization
**Learning:** In OpenMP `DECLARE TARGET` functions, initializing `PARAMETER`s using intrinsics (e.g., `epsilon(1.0_ReKi)`) can cause runtime Exit Code 8 on GPUs.
**Action:** Use local variables initialized at runtime or pass values as arguments for constants derived from intrinsics in target regions.
