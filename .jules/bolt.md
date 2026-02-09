## 2024-05-22 - OpenMP Offloading in Fortran
**Learning:** When porting Fortran subroutines to GPU using OpenMP `TARGET` constructs, intrinsic matrix functions like `matmul` and `transpose` can cause performance issues or runtime crashes (e.g., stack overflows, linking errors) with compilers like `gfortran`.
**Action:** Replace `matmul` and `transpose` with explicit, manually unrolled loops within `TARGET` regions. This ensures better control over memory and avoids implicit descriptor allocations.

**Learning:** Calling functions from external modules (like `EqualRealNos` from `NWTC_Library`) inside `!$OMP TARGET` regions fails if those modules aren't compiled for the device.
**Action:** Implement local, contained versions of necessary helper functions (e.g., `local_EqualRealNos`) and mark them with `!$OMP DECLARE TARGET` to make the offloaded kernel self-contained.

**Learning:** Module-level `PARAMETER`s (especially those defined via intrinsics like `epsilon`) might not be correctly propagated to the device in `gfortran`.
**Action:** Define critical parameters locally within the device subroutine (e.g., `real(ReKi), parameter :: PRECISION_EPS = epsilon(1.0_ReKi)`) to ensure they are available constant expressions on the GPU.
