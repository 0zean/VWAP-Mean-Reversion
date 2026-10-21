"""Spec registration (U6; the idea of wavelet-meta-model `families/spec.py`, rewritten for PLAN.md §8.7).

Contract: a hypothesis runs only from a committed, clean `specs/<id>.yaml` (PLAN.md §0). `load_spec` parses and
validates the §8.7 fields; `registered_sha` returns the commit that last changed the file and refuses one that is
untracked, modified or staged; `check_clean` refuses uncommitted code, so the git SHA in a ledger row describes the
code that ran. A file cannot hold its own SHA, so §8.7's `registered_sha` is recorded by the runner in every ledger
row (`spec_sha`, §8.8) rather than written into the YAML. Amendments are new files with a new id and a reason.

Budget (§6.7): 1 headline + ≤ 8 variants (≤ 4 headlines + ≤ 12 variants for `cond_mean_diff`); `budget` is the
number of trials the spec may run and must cover its headlines and variants, or the spec is refused as over budget.
"""

import hashlib
import subprocess
from dataclasses import dataclass
from datetime import date
from pathlib import Path

import yaml

from vwap_mr.config import DEV_END, DEV_START, ROOT, DecisionTime
from vwap_mr.strategies.base import FILL_FOR

REQUIRED = ("id", "mechanism", "instruments", "window", "vintage", "strategy", "execution", "cost", "account",
            "test", "floors", "expected", "power", "headlines", "variants", "splits", "budget")  # fmt: skip
TEST_KINDS = ("mean_vs_zero", "cond_mean_diff", "paired_sharpe")
CODE_PATHS = ("vwap_mr", "scripts", "specs", "pyproject.toml", "uv.lock")


class SpecError(ValueError):
    pass


@dataclass(frozen=True)
class Spec:
    path: Path
    data: dict

    @property
    def id(self) -> str:
        return self.data["id"]

    def __getitem__(self, key: str):
        return self.data[key]


def _need(cond: bool, msg: str) -> None:
    if not cond:
        raise SpecError(msg)


def _num(x) -> bool:
    return isinstance(x, int | float) and not isinstance(x, bool)


def validate(d: dict, stem: str | None = None) -> None:
    _need(isinstance(d, dict), "a spec is a YAML mapping")
    missing = [k for k in REQUIRED if k not in d]
    _need(not missing, f"spec lacks {missing}")
    _need(stem is None or d["id"] == stem, f"spec id {d['id']!r} must equal its file name {stem!r}")
    ins = d["instruments"]
    _need(isinstance(ins, dict) and isinstance(ins.get("headline"), str) and isinstance(ins.get("splits"), list),
          "instruments needs {headline: str, splits: [..]}")  # fmt: skip
    w = d["window"]
    _need(isinstance(w, dict) and isinstance(w.get("start"), date) and isinstance(w.get("end"), date),
          "window needs start and end dates")  # fmt: skip
    _need(DEV_START <= w["start"] < w["end"] <= DEV_END,
          f"window must lie inside the development window {DEV_START} → {DEV_END}")  # fmt: skip
    ex = d["execution"]
    _need(ex.get("decision") in [t.value for t in DecisionTime], f"unknown decision {ex.get('decision')!r}")
    _need(ex.get("fill") == FILL_FOR[DecisionTime(ex["decision"])],
          f"decision {ex['decision']} fills {FILL_FOR[DecisionTime(ex['decision'])]!r}, not {ex.get('fill')!r}")  # fmt: skip
    c = d["cost"]
    _need(
        _num(c.get("registered_bp")) and c["registered_bp"] >= 0, "cost.registered_bp must be a number >= 0"
    )
    _need(isinstance(c.get("curve"), list) and all(_num(x) for x in c["curve"]) and c["registered_bp"] in c["curve"],
          "cost.curve must be a list of numbers containing registered_bp")  # fmt: skip
    t = d["test"]
    _need(t.get("kind") in TEST_KINDS, f"test.kind must be one of {TEST_KINDS}")
    _need(t.get("sided") in ("one", "two"), "test.sided must be 'one' or 'two'")
    for k in ("block", "n_boot", "seed"):
        _need(
            isinstance(t.get(k), int) and not isinstance(t[k], bool) and t[k] >= 0, f"test.{k} must be an int"
        )
    _need(_num(c.get("borrow_rate")) and isinstance(c.get("fees"), bool), "cost needs borrow_rate and fees")
    if t["kind"] == "mean_vs_zero":  # §6.2's three-tier verdict and floors
        _need(_num(t.get("alpha")) and _num(t.get("promising_alpha")) and t["alpha"] < t["promising_alpha"],
              "test needs alpha < promising_alpha")  # fmt: skip
        fl = d["floors"]
        for k in ("net_excess_ann_min", "gross_rt_multiple", "coherence_min"):
            _need(_num(fl.get(k)), f"floors.{k} must be a number")
    e, p = d["expected"], d["power"]
    _need(_num(e.get("magnitude_bp_day")) and isinstance(e.get("source"), str) and e["source"].strip(),
          "expected needs magnitude_bp_day and a source")  # fmt: skip
    _need(isinstance(p.get("diagnostic"), bool), "power.diagnostic must be true or false")
    for k in ("n", "sigma_d_bp", "k", "mde_bp_day"):
        _need(_num(p.get(k)), f"power.{k} must be a number")
    heads, var = d["headlines"], d["variants"]
    n_var = 12 if t["kind"] == "cond_mean_diff" else 8
    _need(isinstance(heads, list) and 1 <= len(heads) <= 4, "headlines: 1 to 4 names")
    _need(
        t["kind"] == "cond_mean_diff" or len(heads) == 1, "only cond_mean_diff specs carry several headlines"
    )
    _need(isinstance(var, list) and len(var) <= n_var, f"variants: at most {n_var}")
    _need(len(set(heads + var)) == len(heads + var), "headline and variant names must be unique")
    _need(isinstance(d["splits"], list), "splits must be a list")
    b = d["budget"]
    _need(isinstance(b, int) and not isinstance(b, bool), "budget must be an int")
    _need(len(heads) + len(var) <= b, f"over budget: {len(heads)} headline(s) + {len(var)} variant(s) > {b}")


def load_spec(path: Path | str) -> Spec:
    path = Path(path)
    with open(path, encoding="utf-8") as f:
        d = yaml.safe_load(f)
    validate(d, path.stem)
    return Spec(path, d)


def _git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess:
    return subprocess.run(["git", *args], cwd=repo, capture_output=True, text=True, check=check)


def registered_sha(path: Path | str, repo: Path = ROOT) -> str:
    """The commit that last changed the spec file; raises if it is untracked, modified or staged."""
    rel = Path(path).resolve().relative_to(Path(repo).resolve()).as_posix()
    if _git(repo, "ls-files", "--error-unmatch", rel, check=False).returncode != 0:
        raise SpecError(f"{rel} is not committed: a spec runs only from a committed file")
    if _git(repo, "status", "--porcelain", "--", rel).stdout.strip():
        raise SpecError(f"{rel} has uncommitted changes: commit it (an amendment is a new file)")
    return _git(repo, "log", "-1", "--format=%H", "--", rel).stdout.strip()


def check_clean(repo: Path = ROOT, paths: tuple[str, ...] = CODE_PATHS) -> str:
    """HEAD, after refusing uncommitted or untracked changes under `paths` (the ledger's git SHA must be the code)."""
    dirty = _git(repo, "status", "--porcelain", "--", *paths).stdout.strip()
    if dirty:
        raise SpecError(f"uncommitted changes; commit before a registered run:\n{dirty}")
    return _git(repo, "rev-parse", "HEAD").stdout.strip()


def code_hash(root: Path = ROOT / "vwap_mr") -> str:
    """sha256 over the package's .py files (path and bytes), sorted: the code that computed a cell."""
    h = hashlib.sha256()
    for p in sorted(Path(root).rglob("*.py")):
        h.update(p.relative_to(root).as_posix().encode() + b"\0" + p.read_bytes() + b"\0")
    return h.hexdigest()
