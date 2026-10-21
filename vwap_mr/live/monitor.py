"""Live monitors (U13): BOCPD on live P&L and the Kalman edge tracker.

`EdgeMonitor` (wired in U7, PLAN.md §6.3): the causal local-level track of `stats.breaks.local_level_track`, one
session at a time. q̂ and σ̂²_ε are refitted on all history before session r whenever r − min_obs is a multiple of
`refit_every`, and the filter then runs forward with them, so each P(μ_t > 0) uses returns ≤ t only and equals the
batch track exactly (tested). Sessions before `min_obs` return None, which states that no fit exists yet."""

import numpy as np
from scipy.stats import norm

from vwap_mr.stats.breaks import _local_level_filter, local_level


class EdgeMonitor:
    def __init__(self, history=(), *, min_obs: int = 252, refit_every: int = 21):
        if min_obs < 10 or refit_every < 1:
            raise ValueError("need min_obs >= 10 and refit_every >= 1")
        self.min_obs, self.refit_every = min_obs, refit_every
        self.y: list[float] = []
        self.fit: dict | None = None
        self._a = self._p = 0.0
        for r in history:
            self.update(r)

    def update(self, r: float) -> dict | None:
        """Add session t's net return; returns {mu, sd, p_pos, q, q_zero} for t, or None before min_obs."""
        if not np.isfinite(r):
            raise ValueError("a live return must be finite (a missing session is a counted skip upstream)")
        n = len(self.y)  # index of the new session
        if n >= self.min_obs and (n - self.min_obs) % self.refit_every == 0:
            self.fit = local_level(np.asarray(self.y))
            _, _, af, pf = _local_level_filter(np.asarray(self.y), np.array([self.fit["q"]]))
            self._a, self._p = float(af[-1, 0]), float(pf[-1, 0])
        self.y.append(float(r))
        if self.fit is None:
            return None
        p_pred = self._p + self.fit["q"]
        k = p_pred / (p_pred + 1.0)
        self._a += k * (r - self._a)
        self._p = p_pred * (1 - k)
        sd = float(np.sqrt(self.fit["sigma2_eps"] * self._p))
        return {"mu": self._a, "sd": sd, "p_pos": float(norm.cdf(self._a / sd)), "q": self.fit["q"],
                "q_zero": self.fit["q_zero"]}  # fmt: skip
