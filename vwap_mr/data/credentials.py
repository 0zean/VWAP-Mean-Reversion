"""The one module that reads secrets (PLAN.md §0): `.env` at the repository root, or the process environment.

Values are returned to the caller and never logged, never written to `os.environ`, and never put in an error
message (a missing secret is reported by name only)."""

import os

from dotenv import dotenv_values

from vwap_mr.config import ROOT

ENV_PATH = ROOT / ".env"


class MissingCredentialsError(RuntimeError):
    pass


def credential(*names: str) -> tuple[str, ...]:
    """Each named secret from the environment, else from `.env`; raises naming the missing ones."""
    file_vals = dotenv_values(ENV_PATH) if ENV_PATH.exists() else {}
    vals = [os.environ.get(n) or file_vals.get(n) for n in names]
    missing = [n for n, v in zip(names, vals, strict=True) if not v]
    if missing:
        raise MissingCredentialsError(
            f"credentials missing: set {', '.join(missing)} in the environment or {ENV_PATH}"
        )
    return tuple(v for v in vals if v)
