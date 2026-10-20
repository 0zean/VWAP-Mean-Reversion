"""Holm step-down and the Bonferroni bound with K looks (U4, SPEC.md §6; Holm 1979; ported from
0zean/wavelet-meta-model `experiments/report.py::holm` and `families/looks.py::bonferroni`)."""

import math

import numpy as np


def holm(p) -> np.ndarray:
    """Holm step-down adjusted p-values: p̃_(i) = max_{j ≤ i} min(1, (m − j + 1)·p_(j)), in the input order.
    A NaN p raises (a missing hypothesis would shrink m)."""
    p = np.asarray(p, dtype=float)
    if p.ndim != 1 or p.size == 0:
        raise ValueError("need a 1-D array of at least one p-value")
    if np.isnan(p).any() or (p < 0).any() or (p > 1).any():
        raise ValueError(f"p-values must be in [0, 1] and not NaN; got {p}")
    order = np.argsort(p, kind="stable")
    adj = np.maximum.accumulate(p[order] * (len(p) - np.arange(len(p))))
    out = np.empty_like(adj)
    out[order] = np.minimum(adj, 1.0)
    return out


def bonferroni(p: float, k: int) -> float:
    """The Bonferroni bound on a p-value over K looks (capped at 1); a NaN p raises rather than reading as 1."""
    if math.isnan(p):
        raise ValueError("p is NaN")
    return float(min(1.0, p * max(int(k), 1)))
