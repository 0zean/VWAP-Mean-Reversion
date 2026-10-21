"""U9: regime-model detection study and frozen regime configurations (PLAN.md §7 U9) -> results/reports/
U09_detection.md, and the regime section of specs/H3_regime_gates.yaml and specs/H4_blend.yaml (drafts).

    uv run python scripts/u09_detection.py --vintage 2026-10             (full study + the diag NIG prior)
    uv run python scripts/u09_detection.py --paths 1 --n 2200 --out <tmp> --looks <tmp>/looks.jsonl   (quick check)

Synthetic study (no look): two-regime Markov-switching series with mean durations 63 and 252 sessions —
φ-switching AR(1) (φ 0.3 fast / 0.9) for R2 and its baseline B4, σ-switching noise (σ 1 / 2) for R1, R3 and B1 —
plus nulls (fGn H 0.5 for R1 / R3 / B1; constant-φ 0.6 AR(1) for R2 / B4). R1 / R2 refit every 21 sessions on the
trailing 1,008 (§6.4); R3 averages λ ∈ {63, 126, 252} with its NIG prior from the path's first 1,008 sessions
(the evaluated sessions come after).

Market data: only the NIG prior of R3, fixed from SPY `diag` 1993–2011 standardized returns (their mean and
variance), recorded as one `diag` look before it is computed."""

import argparse
import shutil
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import yaml

from vwap_mr.config import RESULTS_DIR, SPECS_DIR
from vwap_mr.experiments.looks import LOOKS_PATH, looked
from vwap_mr.regime import calibrate as cb
from vwap_mr.regime.bocpd import LAMBDAS, NIG, bocpd, bocpd_avg, prior_from
from vwap_mr.regime.hmm import hamilton_filter, rolling_regime, standardized

FIT, EVERY = 1008, 21
SPAN = FIT + 252  # the first session every model (R1 after its 252-session standardization) has a value


def table(header, rows) -> str:
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    return "\n".join(out + ["| " + " | ".join(str(c) for c in r) + " |" for r in rows])


def f(x, nd=2) -> str:
    return "—" if x is None or (isinstance(x, float) and not np.isfinite(x)) else f"{x:.{nd}f}"


def run_family(kind: str, dur, n: int, paths: int, seed: int) -> tuple[list[dict], list[dict]]:
    rows, refit_log = [], []
    for p in range(paths):
        rng = np.random.default_rng([seed, int(dur) if dur else 0, p, 0 if kind == "phi" else 1])
        if dur is None:  # nulls
            if kind == "phi":
                x = np.empty(n)
                e = rng.standard_normal(n)
                x[0] = e[0]
                for t in range(1, n):
                    x[t] = 0.6 * x[t - 1] + e[t]
            else:
                x = cb.fgn(n, 0.5, rng)
            target = np.zeros(n, bool)
        else:
            x, target = cb.ms_series(n, dur, rng, kind=kind)
        xs = pd.Series(x)
        sw = np.r_[False, target[1:] != target[:-1]]
        models = {}
        if kind == "phi":
            r2, log = rolling_regime(
                xs, pd.Series(xs.shift(1)), kind="phi", fit_window=FIT, refit_every=EVERY
            )
            models["R2 MS-AR(1)"] = r2["p_next"].to_numpy()
            models["B4 rolling φ (baseline)"] = cb.baseline_prob(xs, "phi").to_numpy()
        else:
            z = standardized(xs)
            r1, log = rolling_regime(z, kind="sigma", fit_window=FIT, refit_every=EVERY)
            models["R1 HMM"] = r1["p_next"].to_numpy()
            models["B1 rolling σ (baseline)"] = cb.baseline_prob(xs, "sigma").to_numpy()
            zv = z.to_numpy()
            start = int(np.argmax(np.isfinite(zv)))
            prior = prior_from(zv[start : start + FIT])
            b = bocpd_avg(zv[start + FIT :], prior)
            p_recent = np.full(n, np.nan)
            p_recent[start + FIT :] = b["p_recent"]
            p_recent[:SPAN] = np.nan
            cm = cb.change_metrics(p_recent, sw)
            rows.append({"family": kind, "dur": dur, "path": p, "model": "R3 BOCPD (change detector)", **cm})
        refit_log += [dict(r, family=kind, dur=dur, path=p) for r in log]
        for name, prob in models.items():
            prob = np.where(np.arange(n) >= SPAN, prob, np.nan)  # every model on the same sessions
            rows.append(
                {"family": kind, "dur": dur, "path": p, "model": name, **cb.detection_metrics(prob, target)}
            )
    return rows, refit_log


def summarise(rows: list[dict]) -> pd.DataFrame:
    df = pd.DataFrame(rows)
    df["dur"] = df["dur"].fillna(0).astype(int)
    agg = df.groupby(["family", "dur", "model"], sort=False).agg(
        paths=("path", "nunique"), n=("n", "sum"), switches=("switches", "sum"), missed=("missed", "sum"),
        delay_median=("delay_median", "median"), fa=("false_alarms_per_1000", "mean"),
        brier=("brier", "mean") if "brier" in df else ("n", "size"), share_on=("share_on", "mean"))  # fmt: skip
    return agg.reset_index()


def timing() -> dict:
    """Per-decision cost once the model is fitted: one Hamilton update and one BOCPD step (three hazards)."""
    rng = np.random.default_rng(9)
    logf = rng.normal(size=(2000, 2))
    A = np.array([[0.97, 0.03], [0.02, 0.98]])
    t0 = time.perf_counter()
    hamilton_filter(logf, A, np.array([0.5, 0.5]))
    ham = (time.perf_counter() - t0) / 2000 * 1e3
    z = rng.standard_normal(2000)
    t0 = time.perf_counter()
    for lam in LAMBDAS:
        bocpd(z, lam, NIG())
    boc = (time.perf_counter() - t0) / 2000 * 1e3
    return {"hamilton_ms": ham, "bocpd3_ms": boc}


def diag_prior(vintage: str | None, looks_path: Path) -> NIG:
    if vintage is None:
        return prior_from(np.random.default_rng(10).standard_normal(4000))
    from vwap_mr.data.secondary import load_long

    def compute() -> NIG:
        close = load_long("SPY", vintage)[0]["close"].loc[:"2011-12-31"]
        return prior_from(standardized(close.pct_change()).to_numpy())

    return looked("U9 R3 NIG prior from diag 1993-2011 (mean, var of standardized SPY returns)", 1, compute,
                  window="diag", path=looks_path)  # fmt: skip


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--vintage")
    ap.add_argument("--paths", type=int, default=3)
    ap.add_argument("--n", type=int, default=3500)
    ap.add_argument("--out", type=Path, default=RESULTS_DIR / "reports")
    ap.add_argument(
        "--looks", type=Path, default=None, help="scratch looks file (required without --vintage)"
    )
    ap.add_argument("--specs", type=Path, default=SPECS_DIR)
    a = ap.parse_args(argv)
    if a.vintage is None:
        if a.looks is None:
            ap.error("a run without --vintage needs --looks (a scratch copy)")
        shutil.copyfile(LOOKS_PATH, a.looks)
    looks_path = a.looks or LOOKS_PATH
    t0 = time.time()
    rows, refits = [], []
    for kind in ("phi", "sigma"):
        for dur in (63, 252, None):
            r, lg = run_family(kind, dur, a.n, a.paths, seed=0)
            rows += r
            refits += lg
    agg = summarise(rows)
    tm = timing()
    phi_null = cb.rw_phi_null(draws=20)
    prior = diag_prior(a.vintage, looks_path)
    t_all = time.time() - t0
    fits = [r for r in refits if "converged" in r]
    n_conv = sum(r["converged"] for r in fits)
    swaps = sum(r.get("swap_flag", False) for r in fits)

    lab = {"phi": "φ-switching AR(1), φ 0.3 / 0.9", "sigma": "σ-switching noise, σ 1 / 2"}
    out_rows = [[lab[r["family"]] if r["dur"] else
                 ("constant-φ 0.6 AR(1) null" if r["family"] == "phi" else "fGn H 0.5 null"),
                 r["dur"] or "—", r["model"], r["paths"], r["switches"], f(r["delay_median"], 0), r["missed"],
                 f(r["fa"], 1), f(r["brier"], 3) if "BOCPD" not in r["model"] else "—", f(r["share_on"], 2)]
                for r in agg.to_dict("records")]  # fmt: skip
    L = ["# U9 — Causal regime models: detection study (synthetic; PLAN.md §7 U9)", "",
         (f"{a.paths} path(s) of {a.n} sessions per cell; models refit every {EVERY} on the trailing {FIT} (R1, R2); the "
          f"evaluated span is the same for every model: sessions ≥ {SPAN} (the first fit after R1's 252-session "
          f"standardization). "
          f"Probabilities are one-step-ahead: p at t predicts the regime at t + 1. Signal: p > 0.5 (R1, R2, baselines), "
          f"P(change in the last 5 sessions) > 0.5 (R3). Runtime {t_all:.0f} s."), "",
         "## 1. Detection delay, false alarms, Brier score", "",
         table(["series", "mean duration", "model", "paths", "switches", "median delay", "missed", "false alarms / 1,000",
                "Brier", "share signalled"], out_rows), "",
         ("Delay: sessions from a switch into the target regime (fast reversion for φ, high σ for σ) to the first signal "
          "in that spell; missed: spells that ended unsignalled. R3 is a change detector: delay to p_recent > 0.5 within "
          "21 sessions of any switch, false alarm = an onset with no switch in the previous 21. On the nulls every onset "
          "is a false alarm. The baselines' Brier uses their 0/1 signal as the probability."), "",
         "## 2. Refits", "",
         (f"{len(fits)} refits; {n_conv} reported convergence ({len(fits) - n_conv} did not; kept and counted, never "
          f"dropped); R2 swap flags (|φ₁ − φ₀| < 0.05): {swaps}."), "",
         "## 3. Compute (per decision, model fitted)", "",
         (f"Hamilton update {tm['hamilton_ms']:.3f} ms; BOCPD step over three hazards {tm['bocpd3_ms']:.3f} ms; a refit "
          f"(statsmodels, 1,008 sessions) ≈ 0.2–0.3 s every 21 sessions. Target < 50 ms per decision."), "",
         "## 4. R2's random-walk reference and R3's prior", "",
         (f"AR(1) φ of d*_t (n = 5 anchor) on random walks over 126 sessions: mean {phi_null['mean']:.3f} (5–95 % "
          f"{phi_null['q05']:.3f}–{phi_null['q95']:.3f}). R2's regime φs are read against it: both regimes sit near it "
          f"under a random walk."),
         "",
         (f"R3 NIG prior ({'SPY diag 1993–2011, recorded as one diag look' if a.vintage else 'synthetic stand-in'}): "
          f"μ₀ {prior.mu0:.4f}, κ₀ {prior.kappa0:g}, α₀ {prior.alpha0:g}, β₀ {prior.beta0:.3f} (prior mean σ² "
          f"{prior.beta0 / (prior.alpha0 - 1):.3f}); centred on the sample mean, not on reversion."), ""]  # fmt: skip
    a.out.mkdir(parents=True, exist_ok=True)
    (a.out / "U09_detection.md").write_text("\n".join(L), encoding="utf-8")

    regimes = {
        "R1": {"model": "2-state Gaussian HMM (switching mean, variance)", "input": "returns / trailing-252 σ (t−252…t−1)",
               "fit_window": FIT, "refit_every": EVERY, "order": "by σ; regime 1 = high σ",
               "output": "one-step-ahead P(high-σ at t+1 | ≤ t)", "favourable": "P > 0.5"},
        "R2": {"model": "MS-AR(1), intercept form (switching c, φ, σ²)", "input": "d*_t (§8.1, n = 5)", "fit_window": FIT,
               "refit_every": EVERY, "order": "by φ; regime 0 = smaller φ = fast reversion", "swap_flag": "|φ₁ − φ₀| < 0.05",
               "output": "one-step-ahead P(fast at t+1 | ≤ t)", "favourable": "P > 0.5",
               "random_walk_phi_126": phi_null},
        "R3": {"model": "BOCPD, Normal–Inverse-Gamma", "input": "returns / trailing-252 σ", "hazards": list(LAMBDAS),
               "prior": {"mu0": prior.mu0, "kappa0": prior.kappa0, "alpha0": prior.alpha0, "beta0": prior.beta0,
                         "source": "SPY diag 1993-2011" if a.vintage else "synthetic stand-in"},
               "prune": "log-prob < max − 10", "favourable": "E[run length] > 21 and P(r_t ≤ 5) < 0.2"},
        "R4": "optional, not built (§7 U9: only if time remains after R1–R3)",
        "no_gate_before_first_fit": "NaN (counted by U10, never filled)",
    }  # fmt: skip
    h3 = a.specs / "H3_regime_gates.yaml"
    if h3.exists():
        text = h3.read_text(encoding="utf-8")
        head, body = text.split("\n", 1)
        d = yaml.safe_load(body)
        d["status"] = (
            "draft — not registered; U8 states and U9 regime models frozen; U10 completes and registers it"
        )
        d["regime_configuration"] = regimes
        h3.write_text(head + "\n" + yaml.safe_dump(d, sort_keys=False, allow_unicode=True), encoding="utf-8")
    h4 = {"id": "H4_blend", "status": "draft — not registered; U11 completes it",
          "blend": {"weight": "w_t = 0.5 + κ (P_t − 0.5)", "P_t": "R2 one-step-ahead P(fast reversion)", "kappa": 0.5,
                    "components": ["R0 (H1 region)", "TSMOM {21, 63, 126, 252}, equal weight"]},
          "regime_configuration": {"R2": regimes["R2"], "R4": regimes["R4"]}}  # fmt: skip
    (a.specs / "H4_blend.yaml").write_text("# DRAFT (U9). Not a registered spec; the runner refuses it.\n"
                                           + yaml.safe_dump(h4, sort_keys=False, allow_unicode=True), encoding="utf-8")  # fmt: skip
    print(f"report {a.out / 'U09_detection.md'}; {t_all:.0f} s")
    return 0


if __name__ == "__main__":
    sys.exit(main())
