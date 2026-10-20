# U3 checks — raw output

`PYTHONIOENCODING=utf-8 uv run python scripts/u03_checks.py --vintage 2026-10` on `unit/03-backtest` (2026-10-20). No strategy P&L is computed or printed (script docstring).

```

=== SPY
[MARKET] SPY 2019-08-12: cross missing (open False, close True); RTH 5Min fallback: first bar 09:30:00, last bar ends 15:35:00
[MARKET] SPY 2023-06-05: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
1. sessions 2514 2016-01-04 -> 2025-12-31; missing open cross 1, close cross 1; ex-dates 40; early-close decisions at 13:25: 21
2. DTB3 visible and fresh at all 2514 close decisions
[ENGINE] 1 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
3. always-long next_open: equity vs share ledger max rel err 7.86e-15; fallbacks {'open': 1, 'close': 1}
[ENGINE] 1 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 moc fill(s) at a fallback close the 15:45 decision already saw: ['2019-08-12']
3. always-long moc: equity vs share ledger max rel err 2.75e-15; vs adjusted close 2.75e-15; fallbacks {'open': 0, 'close': 1, 'close_seen_at_decision': 1}
[ENGINE] 1 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
4. random ±1, registered costs: vectorized vs loop max |Δnet| 4.44e-16, equity max rel err 4.08e-15, every ledger cell within 1e-12 rel + 1e-9 abs True; runtime vectorized 4.6 ms, loop 98.9 ms
5. view probe: vectorized vs DecisionView loop max |Δtarget| 0.00e+00; 1.4 ms vs 444 ms
6. account margin_30k: (True, []); cash_30k: (False, ['cash account cannot short: 1239 session(s) end short'])
7. fee rows used (SEC $/million, TAF $/share -> sessions):
sec   taf     
0.0   0.000166    160
5.1   0.000119    216
      0.000130     92
8.0   0.000145    213
      0.000166     98
13.0  0.000119    226
18.4  0.000119     29
20.7  0.000119    211
21.8  0.000119    349
22.1  0.000119    258
22.9  0.000130    159
      0.000145     37
23.1  0.000119    222
27.8  0.000166    244

=== QQQ
[MARKET] QQQ 2020-03-16: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
1. sessions 2514 2016-01-04 -> 2025-12-31; missing open cross 1, close cross 0; ex-dates 41; early-close decisions at 13:25: 21
2. DTB3 visible and fresh at all 2514 close decisions
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
3. always-long next_open: equity vs share ledger max rel err 7.09e-15; fallbacks {'open': 1, 'close': 0}
3. always-long moc: equity vs share ledger max rel err 6.45e-15; vs adjusted close 6.71e-15; fallbacks {'open': 0, 'close': 0, 'close_seen_at_decision': 0}
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
4. random ±1, registered costs: vectorized vs loop max |Δnet| 4.44e-16, equity max rel err 9.45e-15, every ledger cell within 1e-12 rel + 1e-9 abs True; runtime vectorized 3.7 ms, loop 96.3 ms
5. view probe: vectorized vs DecisionView loop max |Δtarget| 0.00e+00; 1.4 ms vs 447 ms
6. account margin_30k: (True, []); cash_30k: (False, ['cash account cannot short: 1239 session(s) end short'])
7. fee rows used (SEC $/million, TAF $/share -> sessions):
sec   taf     
0.0   0.000166    160
5.1   0.000119    216
      0.000130     92
8.0   0.000145    213
      0.000166     98
13.0  0.000119    226
18.4  0.000119     29
20.7  0.000119    211
21.8  0.000119    349
22.1  0.000119    258
22.9  0.000130    159
      0.000145     37
23.1  0.000119    222
27.8  0.000166    244

=== IWM
[MARKET] IWM 2017-03-20: cross missing (open False, close True); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
[MARKET] IWM 2019-08-12: cross missing (open False, close True); RTH 5Min fallback: first bar 09:30:00, last bar ends 15:35:00
[MARKET] IWM 2021-09-02: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
[MARKET] IWM 2021-09-14: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
[MARKET] IWM 2023-06-05: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
1. sessions 2514 2016-01-04 -> 2025-12-31; missing open cross 3, close cross 2; ex-dates 40; early-close decisions at 13:25: 21
2. DTB3 visible and fresh at all 2514 close decisions
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 3 session(s) marked/filled at the RTH 5Min open (missing open cross)
3. always-long next_open: equity vs share ledger max rel err 6.49e-15; fallbacks {'open': 3, 'close': 2}
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 moc fill(s) at a fallback close the 15:45 decision already saw: ['2019-08-12']
3. always-long moc: equity vs share ledger max rel err 3.42e-15; vs adjusted close 3.47e-15; fallbacks {'open': 0, 'close': 2, 'close_seen_at_decision': 1}
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 3 session(s) marked/filled at the RTH 5Min open (missing open cross)
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 3 session(s) marked/filled at the RTH 5Min open (missing open cross)
4. random ±1, registered costs: vectorized vs loop max |Δnet| 4.44e-16, equity max rel err 3.84e-15, every ledger cell within 1e-12 rel + 1e-9 abs True; runtime vectorized 3.8 ms, loop 98.1 ms
5. view probe: vectorized vs DecisionView loop max |Δtarget| 0.00e+00; 1.4 ms vs 434 ms
6. account margin_30k: (True, []); cash_30k: (False, ['cash account cannot short: 1239 session(s) end short'])
7. fee rows used (SEC $/million, TAF $/share -> sessions):
sec   taf     
0.0   0.000166    160
5.1   0.000119    216
      0.000130     92
8.0   0.000145    213
      0.000166     98
13.0  0.000119    226
18.4  0.000119     29
20.7  0.000119    211
21.8  0.000119    349
22.1  0.000119    258
22.9  0.000130    159
      0.000145     37
23.1  0.000119    222
27.8  0.000166    244

=== DIA
[MARKET] DIA 2017-03-20: cross missing (open False, close True); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
[MARKET] DIA 2019-08-12: cross missing (open False, close True); RTH 5Min fallback: first bar 09:30:00, last bar ends 15:25:00
[MARKET] DIA 2023-06-05: cross missing (open True, close False); RTH 5Min fallback: first bar 09:30:00, last bar ends 16:00:00
1. sessions 2514 2016-01-04 -> 2025-12-31; missing open cross 1, close cross 2; ex-dates 120; early-close decisions at 13:25: 21
2. DTB3 visible and fresh at all 2514 close decisions
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
3. always-long next_open: equity vs share ledger max rel err 5.34e-15; fallbacks {'open': 1, 'close': 2}
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 moc fill(s) at a fallback close the 15:45 decision already saw: ['2019-08-12']
3. always-long moc: equity vs share ledger max rel err 3.11e-15; vs adjusted close 2.46e-15; fallbacks {'open': 0, 'close': 2, 'close_seen_at_decision': 1}
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
[ENGINE] 2 session(s) marked/filled at the RTH 5Min close (missing close cross)
[ENGINE] 1 session(s) marked/filled at the RTH 5Min open (missing open cross)
4. random ±1, registered costs: vectorized vs loop max |Δnet| 4.44e-16, equity max rel err 9.88e-15, every ledger cell within 1e-12 rel + 1e-9 abs True; runtime vectorized 3.7 ms, loop 98.6 ms
5. view probe: vectorized vs DecisionView loop max |Δtarget| 0.00e+00; 1.4 ms vs 444 ms
6. account margin_30k: (True, []); cash_30k: (False, ['cash account cannot short: 1239 session(s) end short'])
7. fee rows used (SEC $/million, TAF $/share -> sessions):
sec   taf     
0.0   0.000166    160
5.1   0.000119    216
      0.000130     92
8.0   0.000145    213
      0.000166     98
13.0  0.000119    226
18.4  0.000119     29
20.7  0.000119    211
21.8  0.000119    349
22.1  0.000119    258
22.9  0.000130    159
      0.000145     37
23.1  0.000119    222
27.8  0.000166    244
```
