# cyclind

<!-- badges: start -->
[![R-CMD-check](https://github.com/madsyair/cyclind/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/madsyair/cyclind/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/madsyair/cyclind/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/madsyair/cyclind/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/madsyair/cyclind/actions/workflows/lint.yaml/badge.svg)](https://github.com/madsyair/cyclind/actions/workflows/lint.yaml)
<!-- badges: end -->

`cyclind` builds composite leading indicators (CLI), and the coincident index
that goes with them, from the selection of candidate series to a tested
composite, following the OECD system of composite leading indicators:

1. **Candidates** – a reference series and candidate series with their
   economic rationale (`cli_candidates()`).
2. **Specification** – every methodological choice in one validated object
   (`cli_spec()`).
3. **Cycles** – log transformation, outlier treatment, optional seasonal
   adjustment, one year of AR forecasts appended to stabilise the end of the
   series, and one of five filters: Hodrick-Prescott, OECD double HP,
   Christiano-Fitzgerald, Baxter-King or Hamilton (`cli_cycles()`).
   Cycles can also be taken from growth rates, smoothed with an HP
   smoother (`growth_form()`, experimental).
4. **Turning points** – Bry-Boschan and Harding-Pagan BBQ rules
   (`cli_turning_points()`).
5. **Classification** – leading, coincident, lagging or rejected, from the
   cross-correlation lead, leads at turning points, concordance and a
   Toda-Yamamoto Granger test (`cli_assess()`).
6. **Aggregation** – equal weights chained at the ragged edge, the OECD
   chained growth, principal components, Conference Board standardisation
   or a dynamic factor model (`cli_composite()`).
7. **Validation** – turning-point statistics, QPS, AUC and the
   Pesaran-Timmermann test (`cli_validate()`), plus a check of the selection
   bias of in-sample leads (`cli_selection_bias()`).
8. **Selection loop** – components are dropped until the composite passes,
   with an audit log (`cli_build()`).
9. **Quality verdict** – a scorecard of turning-point, prediction and
   out-of-sample criteria returns *good*, *acceptable* or *weak*
   (`cli_quality()`).
10. **Coincident index** – the coincident candidates aggregated the same
    way and tested for timing instead of lead (`cci_build()`,
    `cci_validate()`); `cci_as_reference()` makes the index the monthly
    reference series.

`oecd_spec()` follows the OECD documents (Gyomai and Guidetti 2012;
Gyomai, Ahmad and Astolfi 2017) where they state a rule: TRAMO outliers
with two months of forecasts, the log/level test, the double
Hodrick-Prescott filter, Bry-Boschan turning points with phases of at
least 9 and cycles of at least 24 months, the median-lead classification,
no lag-shifting and the chained average growth of the components. Where
the documents give no formula (the inversion of counter-cyclical
components, the amplitude measure), it takes the choice that comes closest
to the published CLI of Indonesia; `?oecd_spec` lists the source of each
step. `cli_presentation()` gives the three OECD forms of the composite.
The vignette explains, with simulations, where and why the package
defaults differ.

Series of mixed frequencies (daily, weekly, monthly, quarterly) and types
(flow, stock, average), read from one file per indicator with
`read_cli_series()`, are brought to one frequency with `cli_align()`, and
moving holidays such as Idulfitri are removed during seasonal adjustment with
`holiday_effect()`.

A Shiny interface runs the same steps with plots for each of them:
`run_app()` (English) or `run_app(language = "id")` (Indonesian).

## Installation

`cyclind` is not on CRAN yet. Install the development version from
[GitHub](https://github.com/madsyair/cyclind):

``` r
# install.packages("remotes")
remotes::install_github("madsyair/cyclind")

# or, with pak
# install.packages("pak")
pak::pak("madsyair/cyclind")
```

To build the vignette as well:

``` r
remotes::install_github("madsyair/cyclind", build_vignettes = TRUE)
```

The core of the package needs only base R (R >= 4.1.0). Optional features
use packages listed in `Suggests`:

``` r
install.packages("shiny")     # graphical interface, run_app()
install.packages("seasonal")  # X-13ARIMA-SEATS, cli_spec(seasonal_adjust = TRUE)
```

Check the installation:

``` r
library(cyclind)
packageVersion("cyclind")
run_app()   # opens the step-by-step interface in the browser
```

## Example

``` r
library(cyclind)

sim  <- simulate_cycles(seed = 2026)                 # known leads
meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
cand <- cli_candidates(sim, reference = "reference", meta = meta)

res <- cli_build(cand, cli_spec(freq = 12, oos_cutoffs = 2015))
res
assessment(res)   # why each candidate was kept or rejected
audit_log(res)    # decisions of the selection loop
cli_quality(res)  # is the composite good enough?
plot(res)
```

## Mixed frequencies

``` r
gdp_q <- aggregate(sim[, "reference"], nfrequency = 4, FUN = sum)
weeks <- seq(as.Date("1995-01-02"), as.Date("2024-12-23"), by = "week")
sales <- data.frame(date = weeks, value = 700)
x <- cli_align(list(gdp = gdp_q, sim[, -1], sales = sales),
               type = c(gdp = "flow", sales = "flow"), reference = "gdp")
attr(x, "alignment")          # how each series was converted
meta_x <- data.frame(name = colnames(x)[-1], rationale = "early_stage")
cand <- cli_candidates(x, reference = "gdp", meta = meta_x)
```

| Type | Examples | Higher to lower frequency | Quarterly to monthly |
|---|---|---|---|
| `flow` | GDP, production, sales | sum | Denton-Cholette (reference) or central month |
| `stock` | money, outstanding credit | end of period | last month |
| `average` | prices, rates, survey balances | mean | central month |

## Graphical interface

`run_app()` opens a Shiny application that walks through ten steps (data,
candidate metadata, specification, cycles, turning points, classification,
composite, validation with the quality scorecard, coincident index, and
export). Each step
unlocks the next one, and the export step writes an R script that reproduces
the analysis. `cyclind_app()` returns the application object for deployment on
Shiny Server or Posit Connect (`system.file("app", package = "cyclind")`).

## Quality harness

* Filters replicate `mFilter` and `neverhpfilter` within a relative
  tolerance of 1e-8.
* Bry-Boschan dates agree with `Coinprofile` on a monthly test series; BBQ
  dates agree with a golden file of `BCDating` 0.9.8 results (archived
  from CRAN in May 2026) on 46 of 50 series with `phase_rule = "bbq"` and
  on 45 with the default symmetric phase rule; on the others cyclind keeps
  the more extreme of two close turning points.
* Default classification thresholds are calibrated by Monte Carlo
  simulation (`data-raw/`): at most 1.3 percent of simulated non-leading
  series are labelled leading by `cli_spec()`, 2.5 to 7.8 percent by the
  OECD rule of `oecd_spec()` (`inst/calibration/`). The thresholds were
  tuned on the same simulation design, so these rates are optimistic.
* `oecd_spec()` reproduces the published OECD CLI of Indonesia from the
  OECD's own component cycles (correlation 0.9998); from the public
  original series the whole method gives 0.88 (`vignette("cyclind")`,
  section 8).
* `R CMD check --as-cran` runs on Windows, macOS and Linux in GitHub Actions.

## References

Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
Indicators*. OECD.

Harding, D. and Pagan, A. (2002). Dissecting the cycle: a methodological
investigation. *Journal of Monetary Economics*, 49(2), 365–381.

Marcellino, M. (2006). Leading indicators. *Handbook of Economic
Forecasting*, Vol. 1, 879–960.

## Bug reports

Please report problems at <https://github.com/madsyair/cyclind/issues>.
