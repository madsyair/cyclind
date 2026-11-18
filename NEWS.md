# cyclind (development version)

## Bug fixes

* `cf_filter()` removes the drift (the line through the first and last
  observations) before the Christiano-Fitzgerald random-walk filter, as
  Christiano and Fitzgerald (2003) and `mFilter::cffilter(drift = TRUE)`
  do. Before, a trending series got a spurious cycle at both ends.
  `cf_filter(drift = FALSE)` gives the old filter; filters saved before
  the argument existed also remove the drift. Results with `cf_filter()`
  change.
* Daters with the default `phase_rule = "symmetric"` apply the end rule of
  Bry and Boschan (1971) in full: a first (last) peak is dropped when any
  value between it and the start (end) of the series is higher, and
  likewise for troughs; before, only the first (last) value counted.
  `phase_rule = "bbq"` keeps the rule of the 'BCDating' implementation
  of BBQ (the golden-file replication is unchanged: 46 and 45 of 50
  series).
* The QS seasonality statistic, used when X-13 fails, is 0 when the
  autocorrelation at the seasonal lag is not positive, as in X-13ARIMA-SEATS.
* The Conference Board aggregator takes simple differences of components
  declared `unit = "percent"`, and leaves the index and its cycle missing at
  dates without enough components instead of assuming no change.
* A candidate too short for its cycle (fewer than four years) is excluded
  with a warning, like a candidate whose seasonal adjustment failed (see
  `excluded_series()`), instead of stopping `cli_cycles()` and
  `cli_build()`; a short reference still stops the run.
* `read_cli_data()` rejects months outside 1 to 12, stops when `frequency`
  contradicts the dates, and drops blank rows at the end of a file (Excel).
* `simulate_cycles()` checks that `leads` and `counter` are named whole
  numbers; `cli_composite()` names unknown components; `cli_candidates()`
  rejects duplicated names in `meta`.
* Graphical interface: an uploaded file is read only when its path is one
  of shiny's upload files under `tempdir()`, so a crafted browser message
  can no longer make the server read other files; malformed step-1 inputs
  give an error message instead of ending the session. The exported
  script writes the seed and frequency as R values.
* `CandidateSet`, `CompositeIndex` and `CycleSeries` have validity
  methods.
* Graphical interface, from an end-to-end test of the ten steps:
  - The quality scorecard of step 8 and its download are computed from the
    result, as `cli_quality(res)` does, so they keep the row and note of a
    reference disaggregated from quarterly (semiannual, annual) data; the
    verdict box shows that note. For a manual composite the exported script
    puts the composite and its validation (with the out-of-sample check
    when step 8 ran it) into `res_manual` and reports
    `cli_quality(res_manual)`, so script and interface give the same
    scorecard.
  - The turning points of candidates inverted in step 6 are dated again on
    the inverted cycles, as in `cli_build()`; before, the `CliResult`
    downloaded from step 10 kept those of the cycles as extracted.
  - A file that cannot be read with the chosen column separator or decimal
    mark gives the error with a translated hint to check them.
  - A manual choice without components says "Choose at least one
    component" instead of "Error: Components".
  - With the OECD method, the options it does not use (filter, aggregation
    method, transformation, AR extension and the leading thresholds) are
    hidden.
  - Filter, aggregation and transformation choices and the rationale in the
    classification table are translated into Indonesian; messages of the
    package itself (warnings, printed objects) stay in English.
  - The warning on the share of outliers replaced is shown once, in step 4,
    instead of again by the out-of-sample checks of steps 8 and 9.
* When the holiday dates stop before the X-13ARIMA-SEATS forecast horizon
  (three years past the data end), the series are still adjusted with X-11
  without forecast extension, but `cli_cycles()` now warns once
  (`cyclind_warning_holiday_horizon`), naming the series, the end of the
  dates, the date X-13 needs and the call that fixes it (e.g.
  `idulfitri_dates(2008, 2029)` for data ending in 2026). Before, the
  fallback was only recorded in the steps. The graphical interface writes
  `idulfitri_dates(<first year>, <last year + 3>)` from the data instead of
  the default years, in the app and in the exported script.
* Under `oecd_classifier()` with the conformity criteria (the default), a
  coincident series must also have its cross-correlation peak strictly
  inside the coincident band (lags -1 to 1 for monthly data, lag 0 for
  quarterly data) and positive. This applies to the classification of
  candidates (otherwise "rejected", reason "ccf lag") and to the
  `coincident_criteria()` of `cci_validate()` (new criterion `ccf`). Before,
  an index with a coincident median lead but its peak at lag -8 passed.
  The leading and lagging rules and `oecd_classifier(conformity = FALSE)`
  are unchanged. In `inst/calibration/oecd-classification.csv` the overall
  accuracy of the OECD rule rises by up to 0.015 (fewer noise series
  called coincident; 0.013 lower for quarterly data with strong noise);
  in `lead-window.csv` correctly coincident series fall only with strong
  noise (by up to 0.17), the leading columns are unchanged.

## Documentation

* `?cli_validate` and `?cli_quality` say that the probability scores are
  pseudo out-of-sample: only the logit is re-estimated recursively, while
  the cycles, the contraction states and the components come from the full
  sample.
* `?Aggregator` explains the scales of the principal-component weights.
* `inst/extdata/oecd-idn/SOURCE.txt` gives the URL of the fetch script and
  the licence of the OECD data (CC BY 4.0).
* The OECD Indonesia validation (`data-raw/oecd_idn_validate.R`) uses
  TRAMO explicitly and stops without 'RJDemetra'; the vignette and the
  harness say which engine produced the stored numbers.
* `?holiday_effect`, `?idulfitri_dates`, `?cli_spec` and the vignette say
  that holiday effects are optional (`holidays = list()`) and apply only
  with `seasonal_adjust = TRUE`, that any moving holiday (Easter, Chinese
  New Year, Diwali, ...) can be given by its dates, with Idulfitri as a
  helper for Indonesia, and that the dates must reach three years past the
  data end. They also say that dates and effects are country-specific: the
  official 1 Syawal is set by each country's authority (Indonesia: *sidang
  isbat* of the Ministry of Religious Affairs; elsewhere by moon sighting
  or calculation) and can differ from the arithmetic calendar and between
  countries by one or two days, and the default windows reflect Indonesia
  (*mudik*, *cuti bersama*); an example passes a country's own dates and
  windows.
* `?coincident_criteria` and `?Classifier` describe the cross-correlation
  condition of an OECD coincident series.

# cyclind 0.2.0

## Breaking changes

* The acceleration form is removed: `acceleration_form()`, the class
  `AccelerationForm` and its choice in the graphical interface no longer
  exist. It is not one of the standard kinds of cycle, the "acceleration
  cycle" of the literature (Anas and Ferrara 2004) is the growth rate cycle
  already covered by `growth_form()`, and its thresholds were never
  calibrated. Use `level_form()` (growth cycle) or `growth_form()` (growth
  rate cycle); objects saved with `acceleration_form()` must be rebuilt.
* The defaults of `oecd_spec()` follow the 2017 OECD documentation
  (Gyomai, Ahmad and Astolfi 2017, "GAA17"):
  * turning points are dated with the new `oecd_dater()`, Bry-Boschan with
    phases of at least 9 and cycles of at least 24 months (3 and 8
    quarters, a translation by cyclind), instead of 5 and 15 months;
    `oecd_spec(dater = bry_boschan())` gives the old rule;
  * components are no longer shifted by their median lead
    (`lag_shift = FALSE`), and turning points are paired within 24 months
    lead and 9 months lag (`lead_window = c(9, 24)`);
  * only additive outliers are removed (`outlier_types = "AO"`); transitory
    changes and level shifts are detected and reported but kept.
    `tramo_outliers()` has the same default (`remove = "AO"`);
    `oecd_spec(outlier_types = c("AO", "TC"))` restores the earlier one.
* Daters have a `phase_rule`. The new default, `"symmetric"`, removes a
  phase shorter than `min_phase` so that the most extreme turning points
  are kept (the old rule could lose the deepest trough of a crisis). This
  changes the dates of `bry_boschan()` and `bbq()` in every specification;
  `phase_rule = "bbq"` keeps the old rule.
* `oecd_aggregate()` applies the 60 percent rule to the components
  available at both t and t - 1 (GAA17 p. 286, note 8).
* `cli_presentation()` leaves the trend-restored CLI and its rate of change
  missing after the last observation of the reference, as the OECD does;
  `extend_trend = TRUE` extends the trend linearly and flags those rows in
  the new column `trend_extrapolated`.
* `transform = "auto"` and `"test"` no longer take logarithms of series
  declared `unit = "percent"`, such as interest rates.
* With `seasonal_adjust = TRUE` the transformation decided by cyclind is
  passed to X-13 (`transform.function = "log"` or `"none"`), so X-13 no
  longer runs a log/level test of its own (ADR-013). Results can change.
* English is the default language of the graphical interface:
  `run_app()` and `cyclind_app()` take `language = c("en", "id")`, and the
  deployment entry point reads `CYCLIND_LANGUAGE`. Use
  `run_app(language = "id")` for the Indonesian interface.

## New features

* `oecd_spec()` takes `dater`, `aggregator`, `lag_shift`, `lead_window`,
  `min_median_lead`, `outliers`, `outlier_types` and `extension`.
* Outliers are reported: `CycleSeries` keeps the dates and types of the
  outliers found (slot `outliers`) and the warnings raised during cycle
  extraction (slot `notes`); `outlier_report()` tabulates them, and
  `show()` and the interface display them. A share of replaced outliers
  above `max_share` (default 0.05) gives a warning.
* `cycle_agreement()`: Pearson and Spearman correlations and the RMSE
  against the reference outside excluded periods, with moving-block
  bootstrap intervals from an explicit `seed`.
* `cli_spec(matching = c("nearest", "maximum"))` chooses how a reference
  turning point is paired with a candidate turning point.
* Twin candidates, whose cycles correlate at least `twin_threshold`
  (default 0.99), are reduced to one before the selection loop.
* A composite with fewer than three components gives a warning.
* `cyclind_provenance()` records the versions of cyclind, R, seasonal,
  X-13 and RJDemetra and checksums of the input; the script exported by
  the interface prints it.
* `regarima_extension()` keeps the forecasts of the outlier engine when
  outlier treatment is switched off.
* `excluded_series()` lists the candidates left out because they could not
  be seasonally adjusted.
* `cli_presentation()` returns `reference_smoothed`, the de-trended and
  smoothed reference with which the OECD compares the amplitude-adjusted
  CLI.
* The interface names the cycle forms by their kind of cycle, explains what
  a turning point marks in each, and translates the new warnings.

## Bug fixes

* An additive outlier and a transitory change on the same date are
  rewritten, without changing the fitted effect, as an AO equal to the whole
  deviation and a TC that starts one period later (ADR-012). Their
  regressors are nearly collinear, so removing the AO alone left a
  deviation that the data never showed and dated false turning points. A
  warning (`cyclind_warning_outlier_collinear`) gives the date, and the
  outlier tables have a new column `collinear`.
* A seasonal adjustment that X-13 cannot estimate no longer stops
  `cli_cycles()`, `cli_build()` or `cci_build()`: the series is used
  unadjusted when it shows no seasonality, adjusted without holiday
  regressors otherwise, and left out with a warning when that fails too.
  A reference series that cannot be adjusted stops with
  `cyclind_error_seasonal_failed`; system errors stop with
  `cyclind_error_system`.
* A failed X-13 or TRAMO model gives a warning (`cyclind_warning_x13`)
  that names the series and the error; multi-line X-13 messages are kept
  whole, without temporary file names.
* A reference cycle without turning points (a short sample, a series
  without cycles, or a growth rate cycle that the dater leaves flat) made
  `cli_assess()`, `cli_build()`, `cci_build()`, `cli_validate()`,
  `cci_validate()` and `cli_selection_bias()` fail with an error from
  `data.frame()`. They now stop with `cyclind_error_reference_turning_points`,
  whose message gives the number of turning points of the reference and
  suggests a longer sample or another cycle form. The whole call stops,
  rather than every candidate being rejected, because without a reference
  turning point no lead, concordance or contraction probability can be
  measured and the classification would be empty. A reference with some
  turning points, but fewer than the classifier needs, and a candidate whose
  span contains none of them are still assessed: the classifier rejects the
  candidate, with `matched` among the reasons. In `cli_selection_bias()` a training
  window without reference turning points gives a note for that cut-off
  instead of stopping. `cycle_agreement()` does not use turning points and
  is not affected.
* `oecd_spec(dater = ...)` no longer fails.
* Switching outliers off (`update(oecd_spec(), outlier_threshold = Inf)`)
  no longer switches the end extension off.
* `vignette("oecd-indonesia")` standardises each series of the growth rate
  plot on its own, so the plot is no longer cut.

## Documentation

* `?oecd_spec`, `?oecd_classifier`, `vignette("cyclind")` and the README
  separate the rules written in the OECD documents, with their pages, from
  the choices of cyclind (`min_matched = 3`, the 40 percent limits, the
  fixed-b concordance test, the aggregation by mean log ratios, the
  quarterly translation of the dating rules).
* `vignette("cyclind")` has a section on the kinds of cycle (growth cycle,
  growth rate cycle, classical cycle) and shows the matching rule, the
  outlier report, the dropping of twins, `cycle_agreement()` and
  `cyclind_provenance()`. `?growth_form` gives the growth rate formula of
  statistical offices and its gap to the log rate.
* Every number in the vignettes is computed inline from the code or from
  `inst/calibration/`. The steps of `vignette("oecd-indonesia")` that need
  TRAMO print results stored in `inst/vignette-results/`, so the vignettes
  build without Java or X-13; a test checks the stored file.
* The length of the end extension is documented for cyclind, the OECD and
  CIF (Vraná 2020).
* References give DOIs or URLs of primary sources.
* `inst/calibration/` was regenerated for the new defaults.

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
