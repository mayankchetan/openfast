## 2024-05-23 - Fortran OpenMP Offloading Patterns
**Learning:** When porting Fortran code to OpenMP target offloading:
1.  Scalar arguments to device subroutines must use the `VALUE` attribute to ensure pass-by-value semantics on the GPU.
2.  Module-level `PARAMETER`s (constants) should be redefined locally within the device subroutine (or passed as arguments) to avoid implicit mapping issues or runtime SIGFPE errors.
3.  Assumed-shape arrays must be mapped with explicit bounds (e.g., `map(to: A(1:N))`) to avoid runtime failures.
4.  External module procedures (like utility functions) cannot be called from device regions unless they are compiled with `!$OMP DECLARE TARGET`. If modifying the external module is not feasible, re-implement the function locally with the directive.

**Action:** Apply these patterns proactively when identifying further GPU offloading opportunities in `aerodyn` or other modules.
