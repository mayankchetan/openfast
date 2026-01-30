# Bolt's Journal

## 2024-05-22 - [Initial Setup]
**Learning:** Starting fresh. No critical learnings yet.
**Action:** Keep eyes open for performance patterns.

## 2024-05-22 - [Vortex Wake Bottleneck]
**Learning:** The Free Vortex Wake (FVW) module in AeroDyn contains explicit N-body calculations ($O(N^2)$) in `WakeInducedVelocities`. This is a classic candidate for GPU acceleration. The code uses `ui_seg` (segments) or `ui_part_nograd` (particles) for these calculations.
**Action:** Target `FVW_BiotSavart.f90` for OpenMP offloading optimization. Start with `ui_part_nograd` as it is cleaner and simpler.
