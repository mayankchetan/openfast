## 2024-10-24 - Offloading ui_quad_src_nn to GPU (Fix 3)
**Learning:** Even with literal constants, extremely small denominators in GPU kernels can cause numerical instability or exceptions. Increasing the tolerance for singularity checks (e.g., `MinLen`) to a more robust value (like `1.0e-8`) can prevent these issues.
**Action:** Use robust tolerances for singularity checks in device code.
