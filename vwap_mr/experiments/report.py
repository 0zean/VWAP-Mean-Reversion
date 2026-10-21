"""Report rendering (U6): verdict, specification curve, cost curve, splits, variants; K and p·K beside every p.

Contract: `render(record)` refuses a run whose look row is not in the looks ledger (PLAN.md §0: a development-window
result is rendered only after its look is recorded). K is read from the ledger at render time, every window counted
(the 2026-10-21 ruling); p·K is the Bonferroni bound (capped at 1)."""

import math

from vwap_mr.experiments.looks import LOOKS_PATH, looks, looks_total
from vwap_mr.stats.multiplicity import bonferroni


class ReportRefused(RuntimeError):
    pass


def _f(x, nd: int = 2, sign: bool = False) -> str:
    if x is None or (isinstance(x, float) and not math.isfinite(x)):
        return "—"
    return f"{x:+.{nd}f}" if sign else f"{x:.{nd}f}"


def _p(p, k: int) -> str:
    return "—" if p is None else f"{p:.4f} ({bonferroni(p, k):.3f})"


def _table(header: list[str], rows: list[list]) -> str:
    out = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    return "\n".join(out + ["| " + " | ".join(str(c) for c in r) + " |" for r in rows])


def _ci(c: dict) -> str:
    return f"[{_f(c['ci90'][0], sign=True)}, {_f(c['ci90'][1], sign=True)}]"


def _stream_rows(items: list[dict], k: int) -> list[list]:
    return [[c["key"], c["n_obs"], _f(c["mean_bp_day"], sign=True), _ci(c), _p(c["p_boot"], k), _f(c["t_nw"]),
             _f(c["sharpe"]), _f(c["ret_ann"] * 100, sign=True), _f(c["round_trips_yr"], 1)] for c in items]  # fmt: skip


STREAM_HEADER = ["configuration", "n", "bp/day", "90 % CI", "p one-sided (p·K)", "t NW", "Sharpe", "%/yr",
                 "round trips/yr"]  # fmt: skip


def render(record: dict, looks_path=LOOKS_PATH) -> str:
    look = record["look"]
    if not any(r.get("at") == look["at"] and r.get("label") == look["label"] and r.get("n") == look["n"]
               for r in looks(looks_path)):  # fmt: skip
        raise ReportRefused(
            f"no look row {look['label']!r} at {look['at']} in {looks_path}: record the look first"
        )
    k = looks_total(looks_path)
    spec, h = record["spec"], record["headline"]
    pw, ex = spec["power"], spec["expected"]
    fl = record["floors"]
    L = [f"# {record['spec_id']} — H1 existence test of R₀ (PLAN.md §6.2)", ""]
    L += [(f"Spec `{record['spec_path']}` @ `{record['spec_sha'][:12]}`; code `{record['git_sha'][:12]}` (hash "
          f"`{record['code_hash'][:12]}`); data vintage `{record['vintage']}` (`{record['data_hash'][:12]}`). Window "
          f"{spec['window']['start']} → {spec['window']['end']}, {spec['instruments']['headline']}, "
          f"{spec['execution']['decision']} → {spec['execution']['fill']}, registered cost "
          f"{spec['cost']['registered_bp']} bp round trip. Look `{look['label']}` (n {look['n']}, {look['at']}). "
          f"**K = {k}** (every window); p·K in parentheses beside every p."), ""]  # fmt: skip
    if pw["diagnostic"]:
        L += [(f"**Registered as a diagnostic** (`power.diagnostic: true`): the expected magnitude, "
              f"{ex['magnitude_bp_day']} bp/day, is below the MDE of {pw['mde_bp_day']} bp/day (n {pw['n']}, σ_d "
              f"{pw['sigma_d_bp']} bp; power {pw.get('power_at_expected', '—')}). The verdict below is read with "
              f"that power: a rejection does not show the edge is absent."), ""]  # fmt: skip
    L += ["## 1. Verdict", ""]
    L += [_table(["statistic", "value"], [
        ["mean net excess, bp/day", _f(h["mean_bp_day"], 3, True)],
        ["annualized", f"{_f(h['ret_ann'] * 100, 2, True)} %"],
        ["one-sided p, studentized block bootstrap (p·K)", _p(h["p_boot"], k)],
        ["one-sided 90 % lower bound, bp/day", _f(h["lower90"], 3, True)],
        ["bootstrap t / Newey–West t", f"{_f(h['t_boot'])} / {_f(h['t_nw'])}"],
        ["Sharpe (excess) / PSR(0)", f"{_f(h['sharpe'])} / {_f(h['psr0'], 3)}"],
        ["gross bp/day; gross bp per round trip", f"{_f(h['gross_bp_day'], 3, True)}; {_f(h['gross_bp_rt'], 1)}"],
        ["round trips/yr; flat share", f"{_f(h['round_trips_yr'], 1)}; {_f(h['flat_share'], 3)}"],
        ["max drawdown (sessions under water)", f"{_f(h['max_dd'] * 100, 1)} % ({h['max_dd_sessions']})"],
        ["beta / alpha vs buy-and-hold (excess, ann.)", (f"{_f(h.get('beta'), 3)} / "
                                                        f"{_f(h.get('alpha_ann', float('nan')) * 100, 2, True)} %")],
    ])]  # fmt: skip
    L += ["", _table(["floor (§6.2)", "value", "required", "met"], [
        ["net excess return, %/yr", _f(fl["net_excess_ann"]["value"] * 100, 2, True),
         f"≥ {fl['net_excess_ann']['min'] * 100:g}", fl["net_excess_ann"]["ok"]],
        ["gross edge per round trip, bp", _f(fl["gross_bp_rt"]["value"], 1), f"≥ {fl['gross_bp_rt']['min']:g}",
         fl["gross_bp_rt"]["ok"]],
        ["coherence, cells with net mean > 0", fl["coherence"]["value"], f"≥ {fl['coherence']['min']} of 16",
         fl["coherence"]["ok"]],
    ])]  # fmt: skip
    t = spec["test"]
    diag = " — diagnostic" if pw["diagnostic"] else ""
    L += ["", (f"**Verdict: {record['verdict']}**{diag} (confirmed: one-sided p < {t['alpha']} with every floor; "
               f"promising: p < {t['promising_alpha']} and a positive one-sided 90 % lower bound with every floor; "
               f"otherwise rejected). Expected {ex['magnitude_bp_day']} bp/day; source: {ex['source'].strip()}"),
          "", (f"The verdict uses the raw one-sided p: H1 is alone in its stage (k = 1, §6.2). p·K = p × {k} is the "
               "looks-ledger bound over every configuration examined so far; it is printed, not used for the verdict."),
          ""]  # fmt: skip

    L += ["## 2. Specification curve (the 16 cells of R₀, registered cost; sorted by mean)", ""]
    cells = sorted(record["cells"], key=lambda c: c["mean_bp_day"])
    L += [_table(["n", "b %", "bp/day", "90 % CI", "p one-sided (p·K)", "Sharpe", "round trips/yr"],
                 [[c["n"], f"{c['b'] * 100:g}", _f(c["mean_bp_day"], sign=True), _ci(c), _p(c["p_boot"], k),
                   _f(c["sharpe"]), _f(c["round_trips_yr"], 1)] for c in cells])]  # fmt: skip
    med = sorted(c["mean_bp_day"] for c in cells)
    L += ["", (f"Coherence: {record['coherence']} of {len(cells)} cells positive; median cell "
          f"{_f((med[len(med) // 2 - 1] + med[len(med) // 2]) / 2, sign=True)} bp/day."), ""]  # fmt: skip

    L += ["## 3. Cost curve (spread + slippage per round trip; fees, borrow and carry as registered)", ""]
    L += [_table(["bp", "bp/day", "p one-sided (p·K)", "90 % lower", "Sharpe", "%/yr"],
                 [[c["bp"], _f(c["mean_bp_day"], 3, True), _p(c["p_boot"], k), _f(c["lower90"], 3, True),
                   _f(c["sharpe"]), _f(c["ret_ann"] * 100, 2, True)] for c in record["cost_curve"]])]  # fmt: skip
    be = record["break_even_bp"]
    L += ["", f"Break-even spread + slippage ≈ {_f(be, 1)} bp per round trip (secant through the curve's ends)."
          if be is not None else "No break-even: the stream is not positive at zero spread.", ""]  # fmt: skip

    L += ["## 4. Splits of the headline (reported, never used to choose)", ""]
    names = {"year": "Calendar year (net excess)", "vol_quintile": "21-session realized-vol quintile at the decision "
             "(sample quantiles; net excess)", "vix_tercile": "VIX tercile at the decision (sample quantiles; net "
             "excess)", "weekday": "Weekday of the return session (net excess)",
             "side_leg": "Legs and sides (gross, first order)"}  # fmt: skip
    for key, title in names.items():
        L += [f"**{title}**", "", _table(["bin", "n", "bp/day", "t NW"], [[r["bin"], r["n"], _f(r["mean_bp_day"],
              sign=True), _f(r["t_nw"])] for r in record["splits"][key]]), ""]  # fmt: skip

    L += ["## 5. Variants (registered cost; reported on the curve, not used for the verdict)", ""]
    L += [_table(STREAM_HEADER, _stream_rows(record["variants"], k))]
    if record["variant_input_gaps"]:
        L += ["", (f"Variant input gaps (RTH volume + cross size or RTH VWAP missing; those windows flat, counted): "
              f"{', '.join(record['variant_input_gaps'])}.")]  # fmt: skip
    L += ["", "## 6. Split instruments (R₀ at the registered cost; reported, not used for the verdict)", ""]
    L += [_table(STREAM_HEADER, _stream_rows(record["instruments"], k)), ""]

    b = record["benchmark"]
    L += ["## 7. Benchmark, account, fallbacks", ""]
    L += [(f"- Buy-and-hold {spec['instruments']['headline']} (same fill and costs): {_f(b['mean_bp_day'], 2, True)} "
          f"bp/day excess, Sharpe {_f(b['sharpe'])}."),
          (f"- Account `{h['account']['profile']}`: tradable {h['account']['tradable']}"
          f"{'; ' + '; '.join(h['account']['reasons']) if h['account']['reasons'] else ''}."),
          "- Counted cross fallbacks (RTH 5Min open / close for a missing auction print), by configuration: "
          + "; ".join(f"{c['key']} {c['fallbacks']}" for c in [h, *record["variants"], *record["instruments"]])
          + ".", ""]  # fmt: skip
    return "\n".join(L)
