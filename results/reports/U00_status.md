# U0 status — scaffold, protocol and hygiene

Date 2026-10-20. Branch `unit/00-scaffold` (off `dev` @ e016db4). Windows 11, uv 0.8.0, Python 3.12.13.

## Done-when

- [x] **`uv sync` from a clean checkout; pytest, ruff, ty clean; `import vwap_mr`; `vectorbt` absent; `trading/` gone.**
  Fresh `git clone -b unit/00-scaffold` into a scratch directory, then:
  ```
  uv sync                          -> + ... websockets==16.1.1, + wrapt==2.5.1 (exit 0)
  uv run pytest -q                 -> 6 passed in 1.21s   (7 after the review fixes: 7 passed in 1.01s)
  uvx ruff check .                 -> All checks passed!
  uvx ruff format --check .        -> 53 files already formatted
  uv run ty check                  -> All checks passed!
  uv run python -c "import vwap_mr, vwap_mr.config as c; print('import ok', c.HOLDOUT_START)"
                                   -> import ok 2026-01-01
  grep -c -i vectorbt uv.lock      -> 0      (plotly also absent)
  test -e trading                  -> no
  git grep -E "^\s*(from|import) trading"  -> (empty, rc=1)
  ```
  `scripts/done_gate.ps1` runs the four gate commands and stops at the first failure: `done-gate passed`.
- [x] **`git check-ignore specs/x.yaml` matches nothing; `git check-ignore alpaca_api.yaml .env` matches both.**
  ```
  git check-ignore specs/x.yaml            -> (no output, rc=1)
  git check-ignore alpaca_api.yaml .env    -> alpaca_api.yaml / .env (rc=0)
  git check-ignore results/ledger.jsonl results/looks.jsonl results/reports/x.md -> (no output, rc=1)
  ```
- [x] **`looks.jsonl` has the K₀ row; the ledger round-trips a row under lock (test).**
  `results/looks.jsonl` has 1 row, `n = 1`, label "README in-sample chart and down-market reading (prior look)",
  git SHA e016db4. `results/ledger.jsonl` is empty and tracked. Tests: `test_ledger_round_trip` (append under
  `run_lock`, NaN → null, numpy → Python, invalid row refused), `test_run_lock_excludes_second_process`,
  `test_truncated_line_is_isolated`, `test_seeded_looks_ledger`, `test_record_look_and_bonferroni`,
  `test_looks_reader_refuses_malformed_line`.
- [x] **No secret in the new files' history.**
  `git grep -I -E "(PK|SK)[A-Z0-9]{16,}"` → empty (rc=1); the same regex over `git log -p dev..HEAD` → empty.
  `alpaca_api.yaml` and `.env` were never tracked (`git log --all -- alpaca_api.yaml .env` empty, per reviewer).

## What was built

- `pyproject.toml`: vectorbt and plotly removed; pandas, scipy, statsmodels, pyarrow, pywavelets, pyyaml,
  python-dotenv, alpaca-py, requests, matplotlib added; dev group pytest, pytest-cov, ruff, ty, hypothesis, mfdfa,
  nolds. Ruff line length 110, `extend-exclude = ["*.md"]` (current ruff formats Python blocks inside Markdown and
  rewrote PLAN.md §5's interface sketch; reverted). Hatchling build so `import vwap_mr` works from any directory.
- `vwap_mr/` skeleton per PLAN.md §5: 49 modules, each a contract docstring; `config.py` holds the dates,
  paths and `DecisionTime`. 343 non-test lines total.
- `experiments/ledger.py` and `experiments/looks.py`, ported from wavelet-meta-model with the citation kept.
  These changes are deliberate: stage ranking and DSR counting dropped (PLAN.md §3.2); validation is an
  injectable hook, so looks share the lock and fsync path; `_git_sha` runs in the repo root and raises instead of
  recording "unknown"; the looks reader raises on a malformed line; `bonferroni` raises on a NaN p.
- `.gitignore`, `.env.example`, README (one paragraph), `scripts/done_gate.ps1`, `.claude/agents/adversarial-reviewer.md`
  (byte-identical copy), `specs/.gitkeep`. `trading/` deleted.

## Looks

K was 0 and is now 1 (the seed only). No market data was read.

## Adversarial review (one pass)

BREAKING 0 / SEVERE 1 / MINOR 5. Fixed 4 (the SEVERE and three MINOR; the KeyError MINOR is covered by the strict reader), deferred 2.

- SEVERE, fixed: `test_seeded_looks_ledger` asserted K == 1 on the real `results/looks.jsonl`, so it would have
  failed on the first legitimate look. It now checks that row 0 is the seed and that K ≥ 1.
- MINOR, fixed: the looks reader used `Ledger.rows()`, which skips malformed lines and only warns, so K could be
  undercounted. It now raises with the line number (test added). This also covers the `KeyError` on a row with
  no `n`.
- MINOR, fixed: `bonferroni(nan, k)` returned 1.0. It now raises.
- MINOR, deferred: on Windows, `rows()` from another process during an `append` gets `PermissionError` from
  the msvcrt byte-range lock instead of waiting. Upstream behaves the same, the window is one write plus fsync,
  and only one runner writes. Revisit in U6 if the runner reads while another process writes.
- MINOR, deferred (user action): `alpaca_api.yaml` is still in the working tree and `.env` does not exist yet.
  The user moves the credentials into `.env` and deletes the file. The file is ignored and was never tracked.
  It was not opened in this unit.
