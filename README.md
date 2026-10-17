# Interest Rates and the Exchange Rate in Russia

Empirical part of a study of how Bank of Russia monetary policy affects the real effective exchange rate of the ruble (REER). The code estimates impulse responses of the REER to a contractionary monetary policy shock (a rise in RUONIA) with **local projections (LP)**. The main implementation is **LP with sign restrictions**: the monetary policy shock is identified through sign restrictions on impulse responses and on the implied policy rule, and the result is reported as the exact identified set of the REER response together with its median. Cholesky LP, LP-IV and a nonlinear (regime-switching) LP are estimated alongside for comparison.

## Data

Monthly data, January 2013 – January 2026 (`data/raw/`):

- `russian_data.xlsx` – NEER, REER, industrial production (IPP), CPI, Urals oil price, RUONIA, imports, CNY/RUB rate.
- `consensus_investing.csv` – Bank of Russia key-rate decisions with consensus forecasts (Investing.com), used to build the monetary policy surprise.

Pre-processing (`scripts/00`, `scripts/01`): the surprise (actual − consensus) is allocated to months with day weights, unscheduled meetings are excluded, IPP and CPI are seasonally adjusted with X-13ARIMA-SEATS, variables are taken in logs (rates in percent), and a first-difference data set is built for the log-difference specification.

## Model

Endogenous variables: IPP, CPI, RUONIA, REER (in this order); Urals enters as an exogenous control through its lags. The shock is a +1 p.p. rise in RUONIA (one standard deviation in the sign-restriction model). Horizons are 0–12 months. Figures are oriented so that a rise means ruble appreciation (the CNY specification uses the CNY-per-ruble rate with the same convention).

Every model is estimated in two versions:

- **linear**: full sample, 6 lags;
- **regime-dependent**: before / from March 2022, 1 and 2 lags.

### LP with sign restrictions (`scripts/20_sign_restrictions_lp.R`)

1. **Reduced form.** The innovation covariance matrix Σ comes from regressing Y_t on its lags and the lags of Urals; the LP coefficient matrices G_h (h = 0..12, G_0 = I) come from projecting Y_{t+h} on Y_t and the same controls, on a common estimation sample.
2. **Impact vector.** δ = Pq, with PP' = Σ and ‖q‖ = 1. The response to the shock is IRF_h = G_h δ. The shock is normalised to one standard deviation (δ'Σ⁻¹δ = 1) and signed so that RUONIA rises on impact.
3. **Restrictions**, all linear in δ:
   - the mean responses of IPP and CPI over h = 0..12 have the sign `price_sign` (set to −1, i.e. non-positive, in `cfg`; +1 gives non-negative responses);
   - the RUONIA response is non-negative for h = 0..3;
   - policy-rule restrictions on a = Σ⁻¹δ, ψ_i = −a_i / a_r (with the orientation a_r > 0), in three variants:

   | Variant | Restrictions |
   |---|---|
   | A (`restrA-irf-fullrule`) | IRF restrictions + ψ_IPP > 0, ψ_CPI > 0, ψ_REER < 0 |
   | B (`restrB-irf-only`) | IRF restrictions only |
   | C (`restrC-irf-partialrule`) | IRF restrictions + ψ_IPP > 0, ψ_CPI > 0 |

4. **Output.** The exact identified set of the REER response (lower and upper bounds at every horizon, computed by enumerating the faces of the constraint cone, no sampling) and the pointwise median of the response over admissible Haar draws of q. No confidence or credible intervals are produced.
5. **Regime-dependent version.** LP coefficients G_h and the innovation regression are regime-specific (the regime is assigned by the date of the shock); the covariance matrix Σ is common to both regimes.

### Other estimators

| Script | Method |
|---|---|
| `10_cholesky_lp.R` | Cholesky LP (`lpirfs::lp_lin`, Newey–West 95% bands). Specifications: levels from 2013-01; levels from 2015-01; log differences; CNY instead of REER. Regime-dependent models use `lpirfs::lp_nl` with a binary switch on `post_2022`. Also: cumulative response with block-bootstrap bands for the log-difference specification, and a per-horizon z-test of equality of the two regime responses (levels, 2013-01 sample). |
| `30_iv_lp.R` | LP-IV (2SLS, `lpirfs::lp_lin_iv`): RUONIA instrumented by the monthly monetary policy surprise; controls are lags of the endogenous variables, Urals and the instrument. The regime-dependent version is estimated separately on each sub-sample. |
| `40_lp_nl.R` | `lpirfs::lp_nl` with lags 1–3, Newey–West bands (figures `rc_l1`–`rc_l3`). |

## Usage

Run from the project root (or open `int_rates_and_exch_rates_russia.Rproj` in RStudio):

```
Rscript run_all.R            # whole pipeline
Rscript run_all.R 20 30      # only scripts 20 and 30
```

Scripts `00` and `01` must be run before the estimation scripts (they create `data/processed/`). Console output of every script is written to `outputs/logs/`.

Required R packages: `lpirfs`, `seasonal`, `x13binary`, `sandwich`, `openxlsx`, `lubridate`, `tibble`, `tidyr`, `dplyr`, `ggplot2`, `scales`. If a package is missing, the exact `install.packages()` call is printed. `ragg` is optional (used for PNG output if installed).

## Project structure

```
run_all.R                  entry point
R/                         functions (setup, data preparation, LP estimators, plotting, I/O)
scripts/                   00 surprise, 01 data, 10 Cholesky LP, 20 sign restrictions, 30 LP-IV, 40 lp_nl
data/raw/                  input data
data/processed/            generated data sets
outputs/figures/           PNG figures
outputs/tables/            CSV tables with the plotted numbers
outputs/logs/              script logs, session info
outputs/estimability_report.csv
```

## Output file names

`<method>_<lags>lags_<model>_<transform>_<sample start>_<dependent variable>[_<restriction variant>]`

- method: `cholesky_lp`, `cholesky-lp-cumulative-boot`, `signrestr_lp`, `iv_lp`
- model: `linear` or `regime2022m03` (before / from March 2022)
- transform: `levels` (logs of levels, rates in %) or `logdiff`
- sample start: `2013m01`, `2015m01`, `2013m02` (log differences); the sample always ends in `2026m01`
- dependent variable: `reer` or `cny`
- restriction variant (sign restrictions only): `restrA-irf-fullrule`, `restrB-irf-only`, `restrC-irf-partialrule`

Example: `signrestr_lp_2lags_regime2022m03_levels_2013m01_reer_restrC-irf-partialrule.png`. The regime-equality test is saved as the same name plus `_irf_difference_test.csv`.

In regime-dependent figures blue is the period before March 2022 and red is the period from March 2022. Sign-restriction figures show the identified set (band) and its median (line).

## Technical notes

- If a specification cannot be estimated, no figure is produced and the reason is written to `outputs/estimability_report.csv`; any other error stops the pipeline. Typical reasons: an empty identified set, fewer than 100 admissible draws, a weak instrument (HAC first-stage F below `min_F` = 10 in `30_iv_lp.R`), a sample that is too short (the post-March 2022 regime has only about 47 months).
- Lags and leads are built by row position, so the data must contain consecutive months; this is checked before estimation.
- Seasonal adjustment needs the X-13 binary shipped with `x13binary`.
- Run time: the bootstrap for the regime-dependent cumulative responses re-estimates `lp_nl` 1000 times, and the sign-restriction draws can reach 2·10⁶ per specification, so a full run takes a while.
- `consensus_investing.csv` is semicolon-separated, CP1251-encoded, with dates in `m/d/yy` format.
- Meetings of 24 July 2026 and 11 September 2026 are not in the raw file and are added in `scripts/00_build_mp_surprise.R` (`add_missing_2026`).
- Unscheduled meetings are excluded from the instrument on purpose (the list is set in `scripts/00_build_mp_surprise.R`).
- Generated files in `data/processed/` and `outputs/` are git-ignored.
