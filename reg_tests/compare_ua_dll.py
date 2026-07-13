#!/usr/bin/env python3
"""compare_ua_dll.py -- UA DLL acceptance comparison (Task 9).

Compares two UnsteadyAero standalone-driver output files (one built-in UA
model, one running through the UA_Mod=9 user-DLL bridge) channel-by-channel
and asserts they agree within a relative+absolute tolerance envelope, plus a
loop-shape (last-cycle) check that catches phase bugs a mean-error metric
would hide.

Usage:
    uv run --with numpy python reg_tests/compare_ua_dll.py \\
        <builtin>.out <dll>.out \\
        --rtol 2e-3 --atol 1e-4 --channels Cl,Cd,Cm,Cn,Cc \\
        [--steps-per-cycle 200] [--loop-shape-pct 0.5]

Exit code 0 = PASS, 1 = FAIL (mismatch), 2 = usage/IO error.
"""
import argparse
import os
import sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from fast_io import load_output  # noqa: E402


def read_channels(path, channels):
    result = load_output(path)
    data, info = result[0], result[1]
    names = info["attribute_names"]
    idx = {}
    missing = []
    for ch in channels:
        if ch in names:
            idx[ch] = names.index(ch)
        else:
            missing.append(ch)
    if missing:
        raise SystemExit(
            f"[compare_ua_dll] ERROR: channel(s) {missing} not found in {path}. "
            f"Available: {names}"
        )
    if "Time" in names:
        t = data[:, names.index("Time")]
    else:
        t = np.arange(data.shape[0], dtype=float)
    out = {ch: data[:, i] for ch, i in idx.items()}
    return t, out


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("file_a", help="Reference (built-in UA model) .out/.outb file")
    ap.add_argument("file_b", help="Comparison (UA_Mod=9 DLL) .out/.outb file")
    ap.add_argument("--rtol", type=float, default=2e-3, help="Relative tolerance (default 2e-3)")
    ap.add_argument("--atol", type=float, default=1e-4, help="Absolute tolerance (default 1e-4)")
    ap.add_argument("--channels", type=str, default="Cl,Cd,Cm,Cn,Cc", help="Comma-separated channel list")
    ap.add_argument("--steps-per-cycle", type=int, default=None,
                     help="StepsPerCycle used in both driver inputs; required for the loop-shape "
                          "assertion (Step 3). If omitted, the loop-shape check is skipped.")
    ap.add_argument("--loop-shape-pct", type=float, default=0.5,
                     help="Loop-shape assertion threshold, as a percent of the reference channel's "
                          "last-cycle peak-to-peak amplitude (default 0.5%%). Applied to the Cl channel.")
    ap.add_argument("--loop-shape-channel", type=str, default="Cl",
                     help="Channel used for the loop-shape assertion (default Cl)")
    args = ap.parse_args()

    channels = [c.strip() for c in args.channels.split(",") if c.strip()]

    t_a, a = read_channels(args.file_a, channels)
    t_b, b = read_channels(args.file_b, channels)

    if t_a.shape[0] != t_b.shape[0]:
        n = min(t_a.shape[0], t_b.shape[0])
        print(f"[compare_ua_dll] WARNING: length mismatch ({t_a.shape[0]} vs {t_b.shape[0]}); "
              f"truncating both to {n} rows.")
        t_a = t_a[:n]
        t_b = t_b[:n]
        for ch in channels:
            a[ch] = a[ch][:n]
            b[ch] = b[ch][:n]

    dt_mismatch = np.max(np.abs(t_a - t_b)) if len(t_a) else 0.0
    if dt_mismatch > 1e-9:
        print(f"[compare_ua_dll] WARNING: time vectors differ by up to {dt_mismatch:.3e} s")

    print(f"[compare_ua_dll] {args.file_a}")
    print(f"[compare_ua_dll]   vs {args.file_b}")
    print(f"[compare_ua_dll] rtol={args.rtol:g} atol={args.atol:g} channels={channels}")
    print()

    overall_pass = True
    header = f"{'channel':>8s}  {'max_abs_err':>14s}  {'max_rel_err':>14s}  {'ref_amp(pk-pk)':>16s}  {'status':>6s}"
    print(header)
    print("-" * len(header))

    per_channel = {}
    for ch in channels:
        va, vb = a[ch], b[ch]
        abs_err = np.abs(va - vb)
        # Elementwise tolerance envelope: |a-b| <= atol + rtol*|a|  (numpy.allclose convention)
        tol_env = args.atol + args.rtol * np.abs(va)
        ok = np.all(abs_err <= tol_env)
        max_abs = float(np.max(abs_err))
        # Report a representative relative error (excluding atol-dominated near-zero samples)
        denom = np.abs(va)
        with np.errstate(divide="ignore", invalid="ignore"):
            rel_err = np.where(denom > args.atol, abs_err / denom, 0.0)
        max_rel = float(np.max(rel_err))
        amp = float(np.max(va) - np.min(va))
        status = "PASS" if ok else "FAIL"
        overall_pass &= ok
        per_channel[ch] = dict(max_abs=max_abs, max_rel=max_rel, amp=amp, ok=ok)
        print(f"{ch:>8s}  {max_abs:14.6e}  {max_rel:14.6e}  {amp:16.6e}  {status:>6s}")

    print()

    # --- Step 3: loop-shape assertion over the last full forcing cycle ---
    loop_shape_ok = None
    if args.steps_per_cycle:
        spc = args.steps_per_cycle
        n = len(t_a)
        if n < spc:
            print(f"[compare_ua_dll] WARNING: fewer than one cycle of data ({n} rows, "
                  f"{spc} steps/cycle); skipping loop-shape assertion.")
        else:
            ch = args.loop_shape_channel
            va, vb = a[ch][-spc:], b[ch][-spc:]
            d = np.abs(va - vb)
            max_d = float(np.max(d))
            ref_amp = float(np.max(va) - np.min(va))
            thresh = (args.loop_shape_pct / 100.0) * ref_amp
            loop_shape_ok = max_d <= thresh
            status = "PASS" if loop_shape_ok else "FAIL"
            print(f"[compare_ua_dll] Loop-shape check ({ch}, last {spc} steps = last full cycle):")
            print(f"[compare_ua_dll]   max|Delta {ch}| = {max_d:.6e}")
            print(f"[compare_ua_dll]   ref amplitude (pk-pk) = {ref_amp:.6e}")
            print(f"[compare_ua_dll]   threshold ({args.loop_shape_pct}% of amplitude) = {thresh:.6e}")
            print(f"[compare_ua_dll]   -> {status}")
            overall_pass &= loop_shape_ok
    else:
        print("[compare_ua_dll] --steps-per-cycle not given; loop-shape assertion (Step 3) skipped.")

    print()
    print(f"[compare_ua_dll] OVERALL: {'PASS' if overall_pass else 'FAIL'}")

    return 0 if overall_pass else 1


if __name__ == "__main__":
    sys.exit(main())
