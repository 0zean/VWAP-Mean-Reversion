"""Secondary daily SPY history 1993-2015, the `diag` source (U1, PLAN.md §4).

Contract: ratio-spliced to Alpaca at 2016-01-04 only if the overlap ratio is constant to rounding, else
raise; frames carry `attrs["source"] = "diag"`; never costed as Alpaca fills; never in a registered
headline."""
