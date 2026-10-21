"""U8: estimator noise study and the frozen state configurations (PLAN.md §7 U8) -> results/reports/U08_estimators.md
and the configuration table in specs/H3_regime_gates.yaml (a draft: not registered, not runnable).

    uv run python scripts/u08_estimators.py --vintage 2026-10          (timing on the vintage; no state value read)
    uv run python scripts/u08_estimators.py --draws 50 --out <tmp>     (quick check, synthetic only)

Synthetic only, so no look is recorded: the study reads no market return or P&L. With --vintage the state
variables of the four symbols are computed once to time them and to check that every column has a value on
2016-01-04; only the timing and that NaN count are printed.

Decision rule (§7 U8): an estimator enters H3's headline or variants only at a window where its SD (worst over
H ∈ 0.40 … 0.60 and both generators) is ≤ 0.025; the registered window is the smallest such n ≥ its §6.4 window,
else the estimator is a variant labelled *under-powered* at its §6.4 window."""

import argparse
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from vwap_mr.config import RESULTS_DIR, SPECS_DIR
from vwap_mr.regime import calibrate as cb

REGISTERED_MAX = 1008  # §7 U8's window grid is 252 / 504 / 1,008; 2,016 is reported, never frozen
SECTION_6_4 = [  # label, column, estimator in the noise study, §6.4 window, scale set, favourable set, π
    ("E1", "E1_dfa_std", "DFA-1 standard [10, 126]", 504, "standard [10, 126]", "below trailing-504 median", 0.50),
    ("E2", "E2_dfa_short", "DFA-1 short [4, 32]", 504, "short [4, 32]", "below trailing-504 median", 0.50),
    ("E3", "E3_av_short", "Abry–Veitch short j 1–4", 512, "MODWT j 1–4", "below trailing-504 median", 0.50),
    ("E4", "E4_h2", "DFA-1 standard [10, 126]", 1008, "standard [10, 126], q −5 … 5", "h(2) below trailing median", 0.50),
]  # fmt: skip
BASELINES = [
    ("B1", "B1_vol", "σ̂ of returns over 21; quantile vs trailing 504", "top two quintiles", 0.40),
    ("B2", "B2_vix", "VIX as of the decision; quantile vs trailing 504", "top tercile", 0.33),
    ("B3", "B3_ac1", "lag-1 autocorrelation over 63", "< 0", "≈ 0.5, reported"),
    ("B4", "B4_phi", "AR(1) φ of d*_t over 126; quantile vs trailing 504", "below median", 0.50),
    ("B5", "B5_vr5", "VR(5) over 252", "< 1", "≈ 0.5, reported"),
    ("B6", "B6_mom", "region net P&L over 63", "> 0", "≈ 0.5, reported"),
]  # fmt: skip


def table(header, rows) -> str:
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    return "\n".join(out + ["| " + " | ".join(str(c) for c in r) + " |" for r in rows])


def frozen(mw: pd.DataFrame, tab: pd.DataFrame) -> list[dict]:
    worst = tab.groupby(["estimator", "n"])["sd"].max()
    out = []
    for label, col, est, w, scale, fav, pi in SECTION_6_4:
        grid = sorted(n for (e, n) in worst.index if e == est)
        near = min(grid, key=lambda n: abs(n - w))  # 512 is read at the study's 504
        ok = [n for n in grid if near <= n <= REGISTERED_MAX and worst[(est, n)] <= cb.SD_MAX]
        sd_at = float(worst[(est, near)])
        out.append({"label": label, "column": col, "estimator": est, "window_6_4": w,
                    "window": (w if ok[0] == near else int(ok[0])) if ok else w, "scale_set": scale, "favourable": fav, "pi": pi,
                    "sd_at_6_4": round(sd_at, 4), "sd_at_window": round(float(worst[(est, ok[0])]) if ok else sd_at, 4),
                    "status": "powered" if ok else "under-powered"})  # fmt: skip
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--vintage")
    ap.add_argument("--draws", type=int, default=500)
    ap.add_argument("--out", type=Path, default=RESULTS_DIR / "reports")
    ap.add_argument("--spec", type=Path, default=SPECS_DIR / "H3_regime_gates.yaml")
    a = ap.parse_args(argv)
    t0 = time.time()
    tab = cb.noise_table(ns=(252, 504, 1008, 2016), draws=a.draws)  # 2,016: informational, beyond the grid
    mw = cb.min_window(tab)
    pers = cb.rolling_persistence()
    da = cb.delta_alpha_tails(draws=min(a.draws, 200))
    phi = cb.rw_phi_null()
    lo = cb.lo_rs_rejection(draws=min(a.draws, 500))
    fz = frozen(mw, tab)
    t_study = time.time() - t0

    timing: dict | None = None
    if a.vintage:
        from vwap_mr.features.state import long_state

        t1 = time.time()
        sts = {s: long_state(s, a.vintage) for s in ("SPY", "QQQ", "IWM", "DIA")}
        timing = {"seconds": time.time() - t1, "rows": {s: len(f) for s, f in sts.items()},
                  "nan_on_2016_01_04": {s: int(f.loc["2016-01-04"].isna().sum()) for s, f in sts.items()}}  # fmt: skip

    piv = []
    for _, grp in tab.groupby(["estimator", "innovations", "n"], sort=False):
        e, g, n = grp.iloc[0]["estimator"], grp.iloc[0]["innovations"], int(grp.iloc[0]["n"])
        cells = {h: f"{b:+.3f} / {s:.3f}" for h, b, s in zip(grp["H"], grp["bias"], grp["sd"], strict=True)}
        piv.append(
            [e, g, n, *[cells.get(h, "—") for h in (0.40, 0.45, 0.50, 0.55, 0.60)], f"{grp['ms'].mean():.2f}"]
        )
    L = ["# U8 — State estimators and their noise (synthetic; PLAN.md §7 U8)", "",
         (f"Synthetic only: no market return or P&L is read, so no look is recorded (K unchanged). {a.draws} draws per "
         f"cell; Gaussian fGn (Davies–Harte), ARFIMA(0, H − ½, 0) with Gaussian innovations, and the same with "
         f"Student-t₄ innovations (the Gaussian ARFIMA separates ARFIMA's short-lag structure from the tails); seeds "
         f"fixed in `regime/calibrate.py`. Runtime of the study {t_study:.0f} s."), "",
         "## 1. Bias / SD by estimator, innovations, window and H", "",
         ("MF-DFA h(2) on the standard scales equals DFA-1 standard exactly (tested), so E4's h(2) is the DFA-1 standard "
         "row at n 1,008. Abry–Veitch standard (j 2–6) needs ≥ 512 sessions; DFA-1 standard needs ≥ 252."), "",
         table(["estimator", "innovations", "n", "H 0.40", "0.45", "0.50", "0.55", "0.60", "ms / estimate"], piv), "",
         "## 2. Smallest window with SD ≤ 0.025 (worst over H)", "",
         ("The registered grid is 252 / 504 / 1,008 (§7 U8). The 2,016 column (8 years) is informational: it shows "
         "the window a powered estimate would need. A gate on an 8-year window barely moves, so it is never frozen. "
         "The rule is on the raw SD; the SD in H units (raw SD ÷ the slope of the mean estimate on the true H) is "
         "stricter wherever an estimator responds to H by less than one for one, and is shown beside it."),
         "",
         table(["estimator", "innovations", "min n (raw)", "min n (H units)",
                *[f"SD {c[3:]}" for c in mw.columns if c.startswith("sd@")],
                *[f"SD_H {c[4:]}" for c in mw.columns if c.startswith("sdH@")]],
               [[r["estimator"], r["innovations"], "—" if np.isnan(r["min_n"]) else int(r["min_n"]),
                 "—" if np.isnan(r.get("min_n_h", np.nan)) else int(r["min_n_h"]),
                 *[f"{r[c]:.3f}" if np.isfinite(r[c]) else "—" for c in mw.columns if c.startswith("sd@")],
                 *[f"{r[c]:.3f}" if np.isfinite(r[c]) else "—" for c in mw.columns if c.startswith("sdH@")]]
                for _, r in mw.iterrows()]), "",
         "**Lo's modified R/S (the reference test; V outside [0.809, 1.862] = reject short memory at 5 %), fGn:**", "",
         table(["H", "n", "rejection rate"], [[r["H"], r["n"], f"{r['reject']:.3f}"] for _, r in lo.iterrows()]), "",
         ("The R/S row of §1 is the classic R/S Hurst exponent (`nolds.hurst_rs` convention), the number most often "
          "quoted; Lo's V is a test statistic, not an H estimate, so it appears here as size and power."), "",
         ("## 3. Overlap: consecutive rolling estimates (one seeded 30-year fGn path, H = 0.5; the integrated "
          "autocorrelation is summed until ρ first drops below 0.05; no interval)"), "",
         table(["estimator", "window", "lag-1 autocorrelation", "independent estimates per decade"],
               [[r["estimator"], r["window"], f"{r['rho1']:.3f}", f"{r['n_eff_per_decade']:.1f}"]
                for _, r in pers.iterrows()]), "",
         "## 4. Spurious multifractality from fat tails alone (MF-DFA Δα, monofractal input)", "",
         table(["series", "n", "mean Δα", "SD", "95th pct"],
               [[r["series"], r["n"], f"{r['delta_alpha_mean']:.3f}", f"{r['delta_alpha_sd']:.3f}", f"{r['p95']:.3f}"]
                for _, r in da.iterrows()]), "",
         "## 5. B4's random-walk reference", "",
         (f"AR(1) φ of d*_t over {phi['window']} sessions on random-walk closes: mean {phi['mean']:.3f}, SD "
         f"{phi['sd']:.3f}, 5–95 % [{phi['q05']:.3f}, {phi['q95']:.3f}]. d* reverts by construction; B4 is read "
         "against this value, not against 0."), "",
         "## 6. Frozen configurations (copied into the H3 spec draft)", "",
         table(["label", "estimator", "§6.4 window", "frozen window", "SD at §6.4", "SD at frozen", "favourable", "π",
                "status"],
               [[f["label"], f["estimator"], f["window_6_4"], f["window"], f["sd_at_6_4"], f["sd_at_window"],
                 f["favourable"], f["pi"], f["status"]] for f in fz]), "",
         "Baselines B1–B6 are fixed by §6.4 (no noise study: they are not long-memory estimators).", ""]  # fmt: skip
    if timing:
        L += ["## 7. Compute", "", (f"All state columns for SPY, QQQ, IWM, DIA on the spliced long series "
              f"({', '.join(f'{s} {n}' for s, n in timing['rows'].items())} sessions): {timing['seconds']:.1f} s "
              f"(target < 60 s for 2,500 sessions × 4). NaN columns on 2016-01-04: {timing['nan_on_2016_01_04']} "
              "(the `diag` warm-up fills every window). No state value was printed or read."), ""]  # fmt: skip
    a.out.mkdir(parents=True, exist_ok=True)
    (a.out / "U08_estimators.md").write_text("\n".join(L), encoding="utf-8")
    draft = {"id": "H3_regime_gates", "status": "draft — not registered; U9 adds R1–R4, U10 completes and registers it",
             "routing": "PLAN §11 2026-10-21: under §6.2 'rejected', the registered H3 runs on diag with the vol baseline "
                        "only, as a diagnostic; this table is the frozen configuration of every state variable",
             "state_configuration": {"estimators": fz,
                                     "baselines": [dict(zip(["label", "column", "definition", "favourable", "pi"], b,
                                                            strict=True)) for b in BASELINES],
                                     "b4_random_walk_phi": phi, "trailing_quantile_window": 504}}  # fmt: skip
    a.spec.write_text("# DRAFT (U8). Not a registered spec: it lacks §8.7's required fields and the runner refuses it.\n"
                      + yaml.safe_dump(draft, sort_keys=False, allow_unicode=True), encoding="utf-8")  # fmt: skip
    print(f"report {a.out / 'U08_estimators.md'}; draft {a.spec}; {time.time() - t0:.0f} s")
    return 0


if __name__ == "__main__":
    sys.exit(main())
