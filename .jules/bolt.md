## 2024-10-24 - Offloading ui_quad_src_nn to GPU (Fix 2)
**Learning:** `gfortran` offloading may fail at runtime (Exit Code 8) when using transformational intrinsics like `epsilon` or `tiny` in `PARAMETER` initialization within `DECLARE TARGET` subroutines. Replacing these with literal constants is safer. Additionally, robust guards against division by zero (e.g., checks for small denominators) are critical for stability in GPU kernels.
**Action:** Use literal constants for mathematical and machine parameters in device code. Ensure rigorous checks for small denominators in math kernels.
