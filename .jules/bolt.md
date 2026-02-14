## 2024-05-24 - [Fortran OpenMP Offloading - Internal Procedures]
**Learning:** In Fortran, when marking an internal subroutine (one inside a `CONTAINS` block) for OpenMP offloading, the `!$OMP DECLARE TARGET` directive must be placed in the module specification part (e.g., `!$OMP DECLARE TARGET(proc_name)`) rather than inside the subroutine body. Placing it inside (or before) the subroutine in the `CONTAINS` section causes a compilation error ("Unexpected directive") with some `gfortran` versions.
**Action:** Use the list form of `!$OMP DECLARE TARGET` in the module header for module procedures.

## 2024-05-24 - [Fortran Intrinsic Precision vs Manual Loops]
**Learning:** Replacing intrinsic functions like `matmul` and `transpose` with manual loops (to support GPU offloading) can introduce small numerical differences (around 0.5%) in sensitive calculations (like linearization matrices), likely due to differences in instruction ordering or accumulator precision between the intrinsic implementation and standard floating-point arithmetic.
**Action:** Be aware of potential regression failures when refactoring core math kernels for GPU offloading. Ensure consistent implementation across CPU/GPU paths if possible, or update baselines if the new implementation is validated as correct.
