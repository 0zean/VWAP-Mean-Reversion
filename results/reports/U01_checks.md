# U1 checks — scripts/u01_checks.py --vintage 2026-10 (raw output)

```

## SPY
[DATA]  SPY 1DayRTH: dropping 1 session(s) missing their last bar: ['2019-08-12']
sessions: 1DayPrint 2514, 1DayRTH 2513, common 2513; print-only ['2019-08-12']
1. volume ratio print/RTH: p1 1.0892  p50 1.1944  p99 1.5383  min 1.0690  max 1.8138
   by year (median): {2016: 1.18, 2017: 1.27, 2018: 1.227, 2019: 1.223, 2020: 1.19, 2021: 1.177, 2022: 1.179, 2023: 1.159, 2024: 1.215, 2025: 1.166}
2. print close vs last RTH 5Min close, bp: median |d| 0.80, p99 |d| 8.25, max |d| 287.8 on 2020-03-12, n |d| > 10 bp: 22
   median |d| bp by year: {2016: 0.95, 2017: 0.64, 2018: 1.07, 2019: 1.04, 2020: 1.68, 2021: 0.71, 2022: 0.76, 2023: 0.49, 2024: 0.75, 2025: 0.72}
3. raw print vs Yahoo, sessions 2514: close |d| > 1.5c: 924 (max 7.13 on 2020-03-12); open |d| > 1.5c: 44; last-RTH-5Min close vs Yahoo close |d| > 1.5c: 1912
      close>1.5c  close_bp_med  close_bp_p99  open>1.5c
2016          53          0.00          3.46         14
2017         174          1.13          5.01         24
2018         208          1.43         10.81          3
2019         196          1.34          6.65          2
2020         215          1.77         65.17          1
2021          70          0.00          5.99          0
2022           8          0.00          1.75          0
2023           0          0.00          0.00          0
2024           0          0.00          0.00          0
2025           0          0.00          0.00          0
4. Yahoo ex-dates in window: 40; factor-step mismatches > 2e-4: 2
                           alpaca_step  yahoo_step      diff
2016-03-18 00:00:00-04:00     0.000011    0.005158 -0.005147
2018-06-15 00:00:00-04:00     0.000034    0.004491 -0.004457
   (Alpaca/Yahoo factor ratio - 1) median by year: {2016: '4.5e-03', 2017: '4.5e-03', 2018: '8.4e-06', 2019: '-8.1e-06', 2020: '-9.5e-06', 2021: '-1.1e-05', 2022: '-3.7e-06', 2023: '-6.6e-07', 2024: '5.7e-07', 2025: '-1.7e-06'}
5. 1DayAuction 2514 sessions; no open cross 1, no close cross 1 ['2019-08-12', '2023-06-05']; wide window open 0 close 0; multi-price open 0 close 0; ex-dates 40, overrides used 4
      open: print>1bp  open: yahoo>1bp  open: print p99 bp  close: print>1bp  close: yahoo>1bp  close: print p99 bp
2016              106              107                 4.6               142               102                  8.9
2017               89               94                 2.7               129                 0                  5.0
2018              106              104                 5.7               167                 0                 10.8
2019               73               72                 2.8               157                 1                  6.7
2020              102              102                10.8               169                 0                 65.2
2021               92               92                 2.7                54                 0                  6.0
2022              146              146                 8.3                 5                 0                  1.7
2023              109              109                 5.2                 0                 0                  0.0
2024               80               81                 2.5                 0                 0                  0.0
2025               47               48                 2.7                 0                 0                  0.0
   closing cross share of 1DayPrint volume: p1 0.0067  p50 0.0181  p99 0.0649  min 0.0045  max 0.1174

## QQQ
sessions: 1DayPrint 2514, 1DayRTH 2514, common 2514; print-only []
1. volume ratio print/RTH: p1 1.0462  p50 1.1167  p99 1.2826  min 1.0303  max 1.4486
   by year (median): {2016: 1.098, 2017: 1.11, 2018: 1.127, 2019: 1.133, 2020: 1.12, 2021: 1.11, 2022: 1.117, 2023: 1.108, 2024: 1.123, 2025: 1.123}
2. print close vs last RTH 5Min close, bp: median |d| 0.97, p99 |d| 10.86, max |d| 83.5 on 2020-03-13, n |d| > 10 bp: 36
   median |d| bp by year: {2016: 0.92, 2017: 0.71, 2018: 1.66, 2019: 1.3, 2020: 2.24, 2021: 0.9, 2022: 1.52, 2023: 0.84, 2024: 0.64, 2025: 0.64}
3. raw print vs Yahoo, sessions 2514: close |d| > 1.5c: 29 (max 0.15 on 2018-12-14); open |d| > 1.5c: 752; last-RTH-5Min close vs Yahoo close |d| > 1.5c: 1654
      close>1.5c  close_bp_med  close_bp_p99  open>1.5c
2016           9           0.0          2.52        129
2017           8           0.0          2.05        131
2018           8           0.0          3.86        165
2019           1           0.0          0.00        157
2020           2           0.0          0.00        163
2021           1           0.0          0.00          7
2022           0           0.0          0.00          0
2023           0           0.0          0.00          0
2024           0           0.0          0.00          0
2025           0           0.0          0.00          0
4. Yahoo ex-dates in window: 40; factor-step mismatches > 2e-4: 2
                           alpaca_step    yahoo_step      diff
2020-09-21 00:00:00-04:00     0.001451 -2.419305e-07  0.001451
2022-09-19 00:00:00-04:00     0.003579  1.797149e-03  0.001782
   (Alpaca/Yahoo factor ratio - 1) median by year: {2016: '-1.4e-03', 2017: '-1.4e-03', 2018: '-1.4e-03', 2019: '-1.4e-03', 2020: '-1.4e-03', 2021: '1.1e-05', 2022: '1.9e-05', 2023: '1.8e-03', 2024: '1.8e-03', 2025: '1.8e-03'}
5. 1DayAuction 2514 sessions; no open cross 1, no close cross 0 ['2020-03-16']; wide window open 0 close 0; multi-price open 0 close 7; ex-dates 41, overrides used 3
      open: print>1bp  open: yahoo>1bp  open: print p99 bp  close: print>1bp  close: yahoo>1bp  close: print p99 bp
2016              142               10                 7.1                 9                 0                  2.5
2017              133                1                 4.9                 8                 0                  2.1
2018              180               16                10.0                 8                 0                  3.9
2019              166               12                 6.5                 1                 0                  0.0
2020              189               46                14.5                 2                 0                  0.0
2021              171              172                 8.8                 1                 0                  0.0
2022              194              194                12.8                 0                 0                  0.0
2023              156              156                 8.8                 0                 0                  0.0
2024              141              141                 7.5                 0                 0                  0.0
2025              131              131                 7.3                 0                 0                  0.0
   closing cross share of 1DayPrint volume: p1 0.0023  p50 0.0092  p99 0.0394  min 0.0008  max 0.0988

## IWM
[DATA]  IWM 1DayRTH: dropping 1 session(s) missing their last bar: ['2019-08-12']
sessions: 1DayPrint 2514, 1DayRTH 2513, common 2513; print-only ['2019-08-12']
1. volume ratio print/RTH: p1 1.0453  p50 1.1174  p99 1.4165  min 1.0288  max 1.7541
   by year (median): {2016: 1.109, 2017: 1.147, 2018: 1.133, 2019: 1.127, 2020: 1.134, 2021: 1.121, 2022: 1.123, 2023: 1.088, 2024: 1.113, 2025: 1.1}
2. print close vs last RTH 5Min close, bp: median |d| 1.33, p99 |d| 11.30, max |d| 213.9 on 2020-03-16, n |d| > 10 bp: 33
   median |d| bp by year: {2016: 1.62, 2017: 0.74, 2018: 1.24, 2019: 1.29, 2020: 1.83, 2021: 1.34, 2022: 1.46, 2023: 1.62, 2024: 1.88, 2025: 0.89}
3. raw print vs Yahoo, sessions 2514: close |d| > 1.5c: 693 (max 1.89 on 2020-03-16); open |d| > 1.5c: 122; last-RTH-5Min close vs Yahoo close |d| > 1.5c: 1597
      close>1.5c  close_bp_med  close_bp_p99  open>1.5c
2016          61          0.00          5.63         20
2017         126          1.30          5.09         36
2018         129          1.17          8.41          7
2019         137          1.27          6.62         15
2020         172          1.93         59.75         34
2021          61          0.00          7.70          9
2022           7          0.00          3.98          1
2023           0          0.00          0.00          0
2024           0          0.00          0.00          0
2025           0          0.00          0.00          0
4. Yahoo ex-dates in window: 40; factor-step mismatches > 2e-4: 0
   (Alpaca/Yahoo factor ratio - 1) median by year: {2016: '-5.1e-06', 2017: '-3.8e-06', 2018: '-1.3e-06', 2019: '3.4e-06', 2020: '3.4e-06', 2021: '5.9e-06', 2022: '2.8e-06', 2023: '-1.6e-06', 2024: '-2.9e-06', 2025: '-2.4e-06'}
5. 1DayAuction 2514 sessions; no open cross 3, no close cross 2 ['2017-03-20', '2019-08-12', '2021-09-02', '2021-09-14', '2023-06-05']; wide window open 0 close 0; multi-price open 0 close 0; ex-dates 40, overrides used 0
      open: print>1bp  open: yahoo>1bp  open: print p99 bp  close: print>1bp  close: yahoo>1bp  close: print p99 bp
2016              128              114                 7.8               135               104                  9.4
2017               85               69                 3.0               126                 0                  5.1
2018              109              102                 6.9               129                 0                  8.4
2019              104               89                 3.8               137                 0                  6.6
2020               93               58                14.8               172                 0                 59.7
2021              137              129                 4.8                52                 0                  7.7
2022              142              143                 6.9                 6                 0                  4.0
2023              168              168                 6.0                 0                 0                  0.0
2024              165              164                 5.6                 0                 0                  0.0
2025              167              167                 5.8                 0                 0                  0.0
   closing cross share of 1DayPrint volume: p1 0.0050  p50 0.0178  p99 0.0791  min 0.0020  max 0.2052

## DIA
[DATA]  DIA 1DayRTH: dropping 1 session(s) missing their last bar: ['2019-08-12']
sessions: 1DayPrint 2514, 1DayRTH 2513, common 2513; print-only ['2019-08-12']
1. volume ratio print/RTH: p1 1.0122  p50 1.0545  p99 1.3271  min 1.0048  max 1.8286
   by year (median): {2016: 1.034, 2017: 1.041, 2018: 1.052, 2019: 1.061, 2020: 1.084, 2021: 1.054, 2022: 1.056, 2023: 1.044, 2024: 1.051, 2025: 1.076}
2. print close vs last RTH 5Min close, bp: median |d| 0.77, p99 |d| 6.33, max |d| 231.2 on 2020-03-16, n |d| > 10 bp: 11
   median |d| bp by year: {2016: 1.11, 2017: 0.83, 2018: 0.8, 2019: 0.79, 2020: 1.51, 2021: 0.58, 2022: 0.89, 2023: 0.59, 2024: 0.72, 2025: 0.62}
3. raw print vs Yahoo, sessions 2514: close |d| > 1.5c: 327 (max 4.62 on 2020-03-16); open |d| > 1.5c: 41; last-RTH-5Min close vs Yahoo close |d| > 1.5c: 1597
      close>1.5c  close_bp_med  close_bp_p99  open>1.5c
2016          22           0.0          2.40         10
2017          30           0.0          2.66          3
2018          25           0.0          6.85          8
2019         103           0.0          6.97          6
2020         118           0.0         13.35         11
2021          26           0.0          4.15          3
2022           3           0.0          0.45          0
2023           0           0.0          0.00          0
2024           0           0.0          0.00          0
2025           0           0.0          0.00          0
4. Yahoo ex-dates in window: 120; factor-step mismatches > 2e-4: 1
                           alpaca_step  yahoo_step      diff
2016-03-18 00:00:00-04:00      0.00004    0.002148 -0.002108
   (Alpaca/Yahoo factor ratio - 1) median by year: {2016: '1.4e-05', 2017: '5.3e-06', 2018: '-2.5e-07', 2019: '-3.8e-06', 2020: '-2.8e-06', 2021: '-6.1e-06', 2022: '-3.3e-06', 2023: '3.0e-06', 2024: '2.5e-06', 2025: '-5.5e-07'}
5. 1DayAuction 2514 sessions; no open cross 1, no close cross 2 ['2017-03-20', '2019-08-12', '2023-06-05']; wide window open 0 close 0; multi-price open 0 close 0; ex-dates 120, overrides used 3
      open: print>1bp  open: yahoo>1bp  open: print p99 bp  close: print>1bp  close: yahoo>1bp  close: print p99 bp
2016               84               76                 9.6                57                49                  4.4
2017               38               40                 8.3                20                 0                  2.7
2018               72               65                11.0                24                 0                  6.8
2019               56               54                 3.9                92                 0                  7.0
2020               78               70                18.1               106                 0                 13.4
2021               54               54                 3.3                22                 0                  4.1
2022              119              119                 7.5                 2                 0                  0.5
2023               89               89                 4.7                 0                 0                  0.0
2024              114              114                 5.2                 0                 0                  0.0
2025              101              101                 3.8                 0                 0                  0.0
   closing cross share of 1DayPrint volume: p1 0.0015  p50 0.0087  p99 0.0636  min 0.0006  max 0.1383
```

## diag splice report (python -m vwap_mr.data diag --vintage 2026-10)

```
{
  "n_overlap": 2513,
  "overlap": "2016-01-04 -> 2025-12-31",
  "raw_ratio_median": 1.0,
  "raw_ratio_yearly_median_dev": {
    "2016": 1.1688639500739839e-08,
    "2017": -2.50801912571319e-09,
    "2018": 4.223447813700432e-09,
    "2019": 4.31679425538789e-09,
    "2020": -3.7333794944416354e-09,
    "2021": 0.0,
    "2022": -2.9702973414202916e-09,
    "2023": -5.23372623018048e-09,
    "2024": 0.0,
    "2025": 0.0
  },
  "raw_daily_dev_bp": {
    "median_abs": 0.00022369490482176957,
    "p99_abs": 2.927623251275252,
    "max_abs": 11.437460269629884,
    "max_date": "2016-05-31"
  },
  "adjusted_close_corr": 0.9999999643284592,
  "yahoo_vs_own_dividend_chain_max_dev": 0.0011167979684948426,
  "factor_at_dev_start": 0.8469611357133092,
  "missing_in_yahoo": 0,
  "missing_auction_close": 1,
  "volume_ratio_print_vs_yahoo_median": 1.0153033336938733
}
[DIAG]  SPY long: 8,288 sessions 1993-01-29 -> 2025-12-31, 5,774 diag
```
