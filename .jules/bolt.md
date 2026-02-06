## 2024-05-23 - Fortran OpenMP Offloading Patterns
**Learning:** When porting Fortran code to OpenMP target offloading:
1.  Scalar arguments to device subroutines must use the `VALUE` attribute to ensure pass-by-value semantics on the GPU.
2.  Module-level `PARAMETER`s (constants) should be redefined locally within the device subroutine (or passed as arguments) to avoid implicit mapping issues or runtime SIGFPE errors.
3.  Assumed-shape arrays (`dimension(:,:)`) must be avoided in subroutines called from target regions, especially with `gfortran`. Explicit-shape arrays (`dimension(3,N)`) passed with integer bounds prevent descriptor mapping failures (Exit Code 8).
4.  External module procedures cannot be called from device regions unless they are compiled with `!$OMP DECLARE TARGET`. If modifying the external module is not feasible, re-implement the function locally with the directive.

**Action:** Always refactor subroutines to use explicit-shape arrays when enabling OpenMP offloading on existing Fortran codebases.
