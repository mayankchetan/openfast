## 2024-10-24 - Offloading ui_quad_src_nn to GPU (Fix 4)
**Learning:** The previous fix using `1.0e-8` for `MinLen` might have been too aggressive, potentially affecting precision. A value like `1.0e-10` is safer for preventing singularities while minimizing impact on results.
**Action:** Tune numerical tolerances carefully when porting to GPU.
