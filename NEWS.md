# cyclind 0.1.0.9000

## Breaking changes

* The acceleration form is removed: `acceleration_form()`, the class
  `AccelerationForm` and its choice in the graphical interface no longer
  exist. Use `level_form()` (growth cycle) or `growth_form()` (growth rate
  cycle). Reasons:
  * it is not one of the three standard kinds of cycle (classical, growth
    cycle, growth rate cycle);
  * the "acceleration cycle" of the literature (the ABCD framework, Anas
    and Ferrara 2004) means the growth rate cycle, which `growth_form()`
    already covers;
  * the change of a growth rate is a second difference, so it is noisy, and
    the classification thresholds were never calibrated for it.
* Specifications or cycles saved with `acceleration_form()` can no longer
  be used; build them again with `growth_form()`.
* `inst/calibration/forms.csv` now holds the level and growth rate forms
  only (`data-raw/calibrate_forms.R` was run again).

## Graphical interface

* English is now the default language of the interface, in view of the
  CRAN submission: `run_app()` and `cyclind_app()` take
  `language = c("en", "id")`, and the deployment entry point
  (`system.file("app", package = "cyclind")`) reads `CYCLIND_LANGUAGE`
  with `"en"` as its default. The Indonesian interface stays available
  with `run_app(language = "id")`.
* The cycle forms in step 3 are named by their kind of cycle: "Growth
  cycle: deviation from trend" and "Growth rate cycle: growth rate"
  ("Growth cycle: deviasi dari tren" and "Growth rate cycle: laju
  pertumbuhan" in Indonesian). A new help line says what a turning point
  marks in each: output above or below its trend in a growth cycle, the
  speeding up or slowing down of growth in a growth rate cycle.
  `?CycleForm-class` cites Gyomai and Guidetti (2012, pp. 9-11).
* The subtitle of the application names all three indices: "Cyclical
  Composite Indicators: Leading, Coincident and Lagging" ("Indikator
  Komposit Siklus: Pendahulu, Sejalan, dan Tertinggal").

# cyclind 0.1.0

First release. The package builds cyclical composite indicators following
the OECD system of composite leading indicators: candidate series are
classified as leading, coincident or lagging, and the leading and
coincident components are aggregated into a composite leading indicator
(CLI) and a coincident index (CCI).

## Data input, alignment and transformation

* `cli_candidates()` collects a reference series and candidate series with
  their economic rationale (`oecd_rationales()`) and sign.
* `read_cli_data()` and `read_cli_series()` read one table or one file per
  indicator; `series_frequencies()` and `series_types()` describe them.
* `cli_align()` brings series of mixed frequencies (annual, quarterly,
  monthly) and types (stock, flow, index, rate) to one frequency.
* Log transformation, outlier treatment (`robust_outliers()`,
  `tramo_outliers()`, `treat_outliers()`), optional seasonal adjustment and
  moving-holiday effects (`holiday_effect()`, `idulfitri_dates()`).
* End-of-series extension with AR forecasts (`ar_extension()`,
  `no_extension()`, `extend_series()`).
* Cycle forms: level (`level_form()`), growth rate (`growth_form()`) and
  acceleration (`acceleration_form()`, experimental), with growth rates
  computed as statistical offices publish them; `apply_form()`.

## Cycle filters

* Hodrick-Prescott (`hp_filter()`, O(n) solver, `hp_lambda()`), OECD double
  HP (`hp_double_filter()`), HP smoother (`hp_smoother()`),
  Christiano-Fitzgerald (`cf_filter()`), Baxter-King (`bk_filter()`) and
  Hamilton (`hamilton_filter()`), all through the `CycleFilter` strategy and
  `extract_cycle()`; `cli_cycles()` applies the chosen filter.

## Turning-point dating

* Bry-Boschan (`bry_boschan()`) and Harding-Pagan BBQ (`bbq()`) rules
  through the `Dater` strategy and `date_turning_points()`;
  `cli_turning_points()`, `turning_points()`, `cycle_states()` and
  `concordance()`.

## Classification as leading, coincident or lagging

* `cli_assess()` classifies every candidate as leading, coincident, lagging
  or rejected from the lead at turning points, cross-correlation,
  concordance and a Toda-Yamamoto Granger test with fixed-b inference.
* Classifier strategies: `cyclind_classifier()` (default) and
  `oecd_classifier()` with the OECD conformity criteria; extension points
  `leading_criteria()`, `coincident_criteria()` and `classify_candidate()`.

## Aggregation into a CLI and a coincident index

* Aggregators: equal weights chained at the ragged edge (`equal_weight()`),
  the OECD chained growth (`oecd_aggregate()`), principal components
  (`principal_component()`), Conference Board standardisation
  (`conference_board()`) and a dynamic factor model (`dynamic_factor()`),
  through `aggregate_components()`; `cli_composite()`.
* `cli_build()` re-selects components until the composite passes, with an
  audit trail (`audit_log()`).
* `cci_build()`, `cci_validate()` and `cci_as_reference()` build and test a
  coincident index that can replace a quarterly reference series.

## OECD specification

* `cli_spec()` holds every methodological choice in one validated object;
  `oecd_spec()` follows the OECD method (double HP filter, median lead,
  amplitude adjustment, chained aggregation), with options for the form,
  filter and outlier engine.
* Example data from the OECD for Indonesia in `inst/extdata/oecd-idn/` and
  `vignette("oecd-indonesia")`.

## Evaluation and quality

* `cli_validate()`: turning-point statistics, QPS, AUC and ROC curve
  (`roc_curve()`), the Pesaran-Timmermann test and pseudo out-of-sample
  forecasts; `contraction_probability()`.
* `cli_selection_bias()` checks the selection bias of in-sample leads.
* `cli_quality()` returns a scorecard with a verdict of *good*, *acceptable*
  or *weak*; extension point `quality_criteria()`.
* `cli_presentation()` and plot methods for cycles, turning points and
  composites.

## Simulation and calibration

* `simulate_cycles()` generates series with a known lead, lag or no
  relation, used to calibrate the default thresholds; the evidence is in
  `inst/calibration/` and the scripts in `data-raw/`.

## Graphical interface

* `run_app()` and `cyclind_app()` start an optional 'shiny' interface in
  Indonesian or English that runs the workflow step by step and exports an
  R script that reproduces the composite.
