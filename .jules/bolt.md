## 2024-05-24 - [Fortran OpenMP Offloading - Intrinsic and External Function Dependencies]
**Learning:** When offloading Fortran code to GPUs using OpenMP `TARGET` directives, accessing host module `PARAMETER`s (even scalars) or calling external functions (like `EqualRealNos` from a library not marked `DECLARE TARGET`) inside the device kernel can cause compilation or runtime errors (Exit Code 8/SIGFPE with `gfortran`). Additionally, intrinsics like `matmul` and `transpose` can be problematic on some device backends.
**Action:**
1. Define local `PARAMETER`s within the device subroutine (e.g., `pi_local`) instead of using module-level constants.
2. Implement local helper functions marked with `!$OMP DECLARE TARGET` (e.g., `EqualRealNos_Target`) instead of calling external library functions.
3. Replace matrix intrinsics with manual loops or explicit element-wise calculations for small matrices.
4. Always verify variable scope in `TARGET` regions: explicitly map arrays with bounds (e.g., `map(to: A(1:N))`) and use `PRIVATE` clauses for thread-local variables.

## 2024-05-24 - [Fortran OpenMP Offloading - Internal Procedures]
**Learning:** In Fortran, when marking an internal subroutine (one inside a `CONTAINS` block) for OpenMP offloading, the `!$OMP DECLARE TARGET` directive must be placed **inside** the subroutine body (e.g., after the `SUBROUTINE` statement) rather than before it. Placing it before the `SUBROUTINE` statement in the `CONTAINS` section causes a compilation error ("Unexpected !$OMP DECLARE TARGET statement in CONTAINS section") with `gfortran`.
**Action:** Always verify the placement of `!$OMP DECLARE TARGET` for internal procedures.
