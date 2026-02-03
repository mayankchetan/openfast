## 2026-02-03 - Fortran OpenMP Offloading: Handling Constants and Optional Arrays

**Learning:** When offloading Fortran code to GPUs with `gfortran`, module-level `PARAMETER` constants (like `MINNORM`) declared with `!$OMP DECLARE TARGET` may still fail to propagate correctly to the device kernel, leading to runtime errors (e.g., SIGFPE exit code 8 due to failed singularity checks) or subtle numerical regressions if replaced by literals with different precision. Similarly, mapping arrays that might be unallocated (even if unused in the kernel logic) causes immediate runtime crashes.

**Action:**
1. Pass critical module constants as `VALUE` arguments to the device kernel subroutine. This ensures the exact host value is used, maintaining consistency and avoiding propagation issues.
2. Use host-side `IF/ELSE` branching to explicitly exclude unallocated arrays from `MAP` clauses, even if it requires code duplication. Conditional mapping is safer than relying on runtime NULL pointer handling.
