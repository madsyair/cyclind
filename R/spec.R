#' Specify a CLI workflow
#'
#' Collects every methodological choice in one validated object. Arguments
#' left at `NULL` take frequency-specific defaults. The defaults of
#' `cli_spec()` are the package's own choices, not the OECD method: the
#' cycle filter (double Hodrick-Prescott, 12 to 120 months) and the
#' normalisation are those of the OECD system of composite leading
#' indicators (Gyomai and Guidetti 2012), but the outlier rule, the
#' transformation, the end-of-series extension, the turning-point rule
#' (Bry-Boschan with 5/15 months), the classification, the lead window, the
#' aggregation and the amplitude differ; [oecd_spec()] lists the
#' differences step by step. The quarterly defaults translate the monthly
#' ones to quarters, an extension of cyclind (the OECD works only with
#' monthly series). The
#' classification thresholds were calibrated with the Monte Carlo harness in
#' `data-raw/` (see `vignette("cyclind")`): on simulated monthly data they
#' classify about 87 percent of candidates correctly with at most 1.3
#' percent of non-leading (coincident, lagging or noise) series wrongly
#' labelled leading (100 samples per
#' cell, `inst/calibration/accuracy.csv`). The thresholds were tuned and
#' evaluated on the same simulation design, so these rates are optimistic.
#' With [oecd_classifier()] the share is 2.5 to 7.8 percent
#' (`inst/calibration/oecd-classification.csv`), and with the experimental
#' growth rate form up to 1.3 percent (`inst/calibration/forms.csv`).
#'
#' The defaults chain the composite at the ragged edge (`equal_weight()`)
#' and extend each series with one year of AR forecasts before the filter
#' (`ar_extension()`). `cli_spec(aggregator = equal_weight(ragged_edge =
#' "level"), extension = no_extension())` does neither.
#'
#' @param freq Data frequency: 12 (monthly) or 4 (quarterly).
#' @param filter A [CycleFilter-class]; default [hp_double_filter()].
#' @param dater A [Dater-class]; default [bry_boschan()] for monthly and
#'   [bbq()] for quarterly data.
#' @param aggregator An [Aggregator-class]; default [equal_weight()], which
#'   after the last date with all components adds the average change of the
#'   available components to the previous value
#'   (`equal_weight(ragged_edge = "level")` takes the plain mean).
#' @param transform `"auto"` (logarithm for strictly positive series),
#'   `"log"`, `"none"` or `"test"` (logarithm when the log/level test of a
#'   regARIMA model prefers it, as TRAMO does at the OECD; needs
#'   \pkg{RJDemetra} or \pkg{seasonal}; with seasonal adjustment the test
#'   is run on the unadjusted series, before the adjustment, so that one
#'   decision serves both). With `"auto"` and `"test"`,
#'   series declared in percent (`unit = "percent"` in the metadata of
#'   [cli_candidates()]: interest rates, unemployment rates, survey
#'   balances) stay in levels, since their cycle is additive; `"log"` takes
#'   the logarithm of every series. Before 0.2.0, `"auto"` also logged
#'   positive series in percent.
#' @param seasonal_adjust Seasonally adjust with X-13ARIMA-SEATS (needs the
#'   \pkg{seasonal} package)? The adjustment is made in the scale chosen by
#'   `transform` (multiplicative when the series is logged, additive
#'   otherwise), so that X-13 does not run a log/level test of its own. When
#'   X-13 fails on a series (for example a singular ARMA covariance matrix),
#'   a warning of class `cyclind_warning_seasonal_failed` names the series
#'   and the error, and: when the QS test (at level `alpha`) finds no
#'   seasonality, the series is used unadjusted; otherwise the adjustment is
#'   tried again without the holiday regressors; when that fails too, the
#'   candidate is left out of [cli_cycles()] with the reason (see
#'   [excluded_series()] and the audit log of [cli_build()]). A reference
#'   series that cannot be adjusted stops the run
#'   (`cyclind_error_seasonal_failed`), and so do errors of the system
#'   (`cyclind_error_system`).
#' @param outlier_threshold Robust z-score of period-to-period changes above
#'   which an observation is replaced by interpolation; `Inf` disables. A
#'   shortcut for `outliers = robust_outliers(outlier_threshold)`.
#' @param min_lead Minimum lead of the cross-correlation peak, in periods
#'   (default 3 months or 1 quarter). Also the minimum lag of a lagging series.
#' @param min_median_lead Minimum median lead at turning points (default 1
#'   month or 0.5 quarter, that is, the median must point to a lead).
#' @param min_consistency Minimum share of reference turning points that the
#'   candidate leads (default 0.5).
#' @param max_missed,max_extra Maximum shares of missed and extra turning
#'   points, relative to the number of reference turning points (default 0.4).
#' @param coincident_band Largest absolute lag of the cross-correlation peak
#'   of a coincident series (default 1 month or 0 quarters).
#' @param min_auc Minimum area under the ROC curve for a good composite
#'   (default 0.7, "acceptable discrimination" in Hosmer and Lemeshow 2000).
#' @param target_lead Lead range the OECD aims at, reported in the quality
#'   scorecard for information (default 6 to 9 months or 2 to 3 quarters);
#'   its lower end is the default horizon of the prediction tests.
#' @param lead_window Turning points are paired when the candidate turns at
#'   most `lead_window[1]` periods after and `lead_window[2]` periods before
#'   the reference (default `c(12, 24)` months or `c(4, 8)` quarters; the
#'   OECD uses 9 months after and 24 before, Gyomai, Ahmad and Astolfi 2017,
#'   p. 285, which [oecd_spec()] takes).
#' @param matching Rule that pairs turning points. Reference turning points
#'   are taken in time order; each is paired with a candidate turning point
#'   of the same type inside `lead_window` that is later than the candidate
#'   turning point paired before and not used yet. `"nearest"` (default)
#'   takes the one closest in time, as CIF (Vraná, 2020) does;
#'   `"maximum"` takes the most pronounced one, the highest peak or deepest
#'   trough of the candidate cycle, and the closest of equally pronounced
#'   ones. Unpaired reference turning points are missed and unpaired
#'   candidate turning points are extra. `"maximum"` keeps a small wiggle
#'   just before the reference turn from replacing the main turn of the
#'   candidate, at the cost of longer leads.
#' @param twin_threshold Candidates whose normalised cycles correlate at
#'   least this much in absolute value are twins: before the selection loop
#'   only the one with the higher peak cross-correlation with the reference
#'   is kept (default 0.99, as in CIF, Vraná 2020). The twins dropped are
#'   listed in the
#'   audit log and in a warning. 1 drops identical cycles only.
#' @param ccf_lag_max Largest lead or lag of the cross-correlation search
#'   (default 24 months or 8 quarters).
#' @param alpha Significance level of the tests (default 0.05).
#' @param granger_max_lag Largest lag order considered by the Toda-Yamamoto
#'   test (default 12 months or 4 quarters); the order is chosen by AIC.
#' @param granger_dmax Extra unrestricted lags of the Toda-Yamamoto test.
#' @param max_iter Maximum number of iterations of the selection loop.
#' @param min_components Smallest number of components of the composite.
#' @param horizon Horizon of the probability and direction tests of
#'   [cli_validate()]; `NA` uses the lower end of `target_lead`, fixed in
#'   advance rather than chosen from the data being tested.
#' @param oos_cutoffs Optional cut-off dates (decimal time, e.g. `2015`) for
#'   the selection-bias harness run by [cli_build()].
#' @param holidays A [holiday_effect()], or a list of them, whose effects
#'   X-13ARIMA-SEATS estimates and removes (needs `seasonal_adjust = TRUE`).
#'   For Indonesian data, `holiday_effect(idulfitri_dates())` removes the
#'   moving Idulfitri effect, which fixed monthly factors cannot.
#' @param classifier A [Classifier-class]; default [cyclind_classifier()]. Use
#'   [oecd_classifier()] for the OECD median-lead rule.
#' @param outliers An [OutlierTreatment-class]; default
#'   `robust_outliers(outlier_threshold)`. Use [tramo_outliers()] for the
#'   OECD treatment.
#' @param lag_shift Shift each component later by its median lead minus the
#'   shortest median lead of the components before aggregation (default
#'   `FALSE`). The OECD system supports such shifts (Gyomai and Guidetti
#'   2012, p. 9), but "in practice all the indicators currently produced are
#'   without delayed components" (Gyomai, Ahmad and Astolfi 2017, p. 287).
#'   Values moved past the last date of the data are dropped.
#' @param extension An [Extension-class]; default [ar_extension()]: each
#'   series is extended by one year of AR forecasts before the cycle filter
#'   (a choice of cyclind; `ar_extension(h = 6)` for the six months of
#'   ARIMA forecasts of CIF (Vraná, 2020); the OECD extends with 2 to 6
#'   months of TRAMO
#'   forecasts, Gyomai, Ahmad and Astolfi 2017, p. 284, see
#'   [Extension-class]). Use [no_extension()] for no extension. The
#'   extension is separate from the outlier treatment: switching outliers
#'   off with [update()] keeps it.
#' @param form A [CycleForm-class]: [level_form()] (default, the growth
#'   cycle) or [growth_form()] (growth rate cycle).
#'   Without `filter`, the growth form uses [hp_smoother()].
#'   The growth form is experimental: the thresholds were calibrated for
#'   the level form. [update()] changes a filter left at the default of the
#'   old form to the default of the new one; a filter chosen by hand is
#'   kept. The level form with [hp_smoother()] gives a warning, since that
#'   filter does not remove the trend.
#' @param amplitude How the composite index is scaled to the amplitude of
#'   the reference cycle (in percent deviation from trend): `"mad"`
#'   (default) gives the index the mean absolute deviation of the reference
#'   cycle; `"sd"` gives it the standard deviation of the reference cycle
#'   over their common dates, which reproduces the OECD amplitude-adjusted
#'   CLI (see [oecd_spec()]). Only the index changes, not the cycle.
#' @return A [CliSpec-class] object.
#' @references
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD Publishing, Paris.
#'
#' Vraná, L. (2020). *cif: Composite Indicators Framework for Business
#' Cycle Analysis*. Python package version 0.0.6.
#' <https://pypi.org/project/cif/>
#' @examples
#' spec <- cli_spec(freq = 12)
#' spec
#' update(spec, min_lead = 6, aggregator = dynamic_factor())
#' @export
cli_spec <- function(freq = 12, filter = NULL, dater = NULL,
                     aggregator = NULL,
                     transform = c("auto", "log", "none", "test"),
                     seasonal_adjust = FALSE, outlier_threshold = 5,
                     min_lead = NULL, min_median_lead = NULL,
                     min_consistency = 0.5, max_missed = 0.4,
                     max_extra = 0.4, coincident_band = NULL,
                     min_auc = 0.7, target_lead = NULL,
                     lead_window = NULL, ccf_lag_max = NULL, alpha = 0.05,
                     granger_max_lag = NULL, granger_dmax = 1,
                     max_iter = 10, min_components = 2, horizon = NA,
                     oos_cutoffs = numeric(0), holidays = list(),
                     classifier = NULL, outliers = NULL, lag_shift = FALSE,
                     extension = NULL, form = NULL,
                     amplitude = c("mad", "sd"),
                     matching = c("nearest", "maximum"),
                     twin_threshold = 0.99) {
  check_frequency(freq)
  matching <- match.arg(matching)
  monthly <- freq == 12
  if (is(holidays, "HolidayEffect")) holidays <- list(holidays)
  check_strategy(classifier, "Classifier", "classifier")
  check_strategy(outliers, "OutlierTreatment", "outliers")
  check_strategy(extension, "Extension", "extension")
  check_strategy(form, "CycleForm", "form")
  if (is.null(form)) form <- level_form()
  if (identical(amplitude, c("mad", "sd"))) amplitude <- "mad"
  spec <- new_valid(
    "CliSpec",
    freq = as.numeric(freq),
    filter = if (is.null(filter)) default_filter(form) else filter,
    dater = if (is.null(dater)) {
      if (monthly) bry_boschan() else bbq()
    } else {
      dater
    },
    aggregator = if (is.null(aggregator)) equal_weight() else aggregator,
    transform = match.arg(transform),
    seasonal_adjust = seasonal_adjust,
    outlier_threshold = as.numeric(outlier_threshold),
    min_lead = as.numeric(if (is.null(min_lead)) {
      if (monthly) 3 else 1
    } else {
      min_lead
    }),
    min_median_lead = as.numeric(if (is.null(min_median_lead)) {
      if (monthly) 1 else 0.5
    } else {
      min_median_lead
    }),
    min_consistency = as.numeric(min_consistency),
    max_missed = as.numeric(max_missed),
    max_extra = as.numeric(max_extra),
    coincident_band = as.numeric(if (is.null(coincident_band)) {
      if (monthly) 1 else 0
    } else {
      coincident_band
    }),
    min_auc = as.numeric(min_auc),
    target_lead = as.numeric(if (is.null(target_lead)) {
      if (monthly) c(6, 9) else c(2, 3)
    } else {
      target_lead
    }),
    lead_window = as.numeric(if (is.null(lead_window)) {
      if (monthly) c(12, 24) else c(4, 8)
    } else {
      lead_window
    }),
    ccf_lag_max = as.numeric(if (is.null(ccf_lag_max)) {
      if (monthly) 24 else 8
    } else {
      ccf_lag_max
    }),
    alpha = as.numeric(alpha),
    granger_max_lag = as.numeric(if (is.null(granger_max_lag)) {
      if (monthly) 12 else 4
    } else {
      granger_max_lag
    }),
    granger_dmax = as.numeric(granger_dmax),
    max_iter = as.numeric(max_iter),
    min_components = as.numeric(min_components),
    horizon = as.numeric(horizon),
    oos_cutoffs = as.numeric(oos_cutoffs),
    holidays = holidays,
    classifier = if (is.null(classifier)) cyclind_classifier() else classifier,
    outliers = if (is.null(outliers)) {
      robust_outliers(outlier_threshold)
    } else {
      outliers
    },
    lag_shift = lag_shift,
    extension = if (is.null(extension)) ar_extension() else extension,
    form = form,
    amplitude = amplitude,
    matching = matching,
    twin_threshold = as.numeric(twin_threshold)
  )
  if (is(spec@outliers, "RobustOutliers")) {
    spec@outlier_threshold <- spec@outliers@threshold
  }
  check_valid(spec)
  check_form_filter(spec)
  spec
}

# A filter that only smooths keeps the trend, so with the level form the
# "cycle" would be the smoothed trend itself.
check_form_filter <- function(spec) {
  if (form_kind(spec_form(spec)) == "level" && !removes_trend(spec@filter)) {
    cyclind_warn(paste0("The ", spec@filter@label, " does not remove the ",
                        "trend: with the level form the cycle is the smoothed ",
                        "series. That suits series that are already growth ",
                        "rates; otherwise use growth_form()."),
                 "cyclind_warning_form_filter")
  }
  invisible(spec)
}

# validObject() and new() with the error class of the package.
check_valid <- function(object) {
  tryCatch(methods::validObject(object), error = invalid_error)
  invisible(TRUE)
}

new_valid <- function(...) tryCatch(methods::new(...), error = invalid_error)

invalid_error <- function(e) {
  cyclind_abort(sub("^invalid class .*? object: ", "", conditionMessage(e),
                    perl = TRUE), "cyclind_error_argument")
}

check_strategy <- function(x, class, name) {
  if (!is.null(x) && !is(x, class)) {
    cyclind_abort(sprintf("`%s` must be a %s object.", name, class),
                  "cyclind_error_argument")
  }
  invisible(TRUE)
}

# Slots added after 0.1.0, filled with the behaviour of the version that
# saved the object (used by update() on old specifications).
upgrade_spec <- function(object) {
  old <- list(holidays = list(), classifier = cyclind_classifier(),
              outliers = robust_outliers(object@outlier_threshold),
              lag_shift = FALSE, extension = no_extension(),
              form = level_form(), amplitude = "mad", matching = "nearest",
              twin_threshold = 1)
  for (nm in names(old)) {
    if (!methods::.hasSlot(object, nm)) attr(object, nm) <- old[[nm]]
  }
  object
}

setValidity("CliSpec", function(object) {
  msg <- character(0)
  one <- function(slot) length(slot(object, slot)) == 1L
  scalar_slots <- c("freq", "outlier_threshold", "min_lead", "min_median_lead",
                    "min_consistency", "max_missed", "max_extra",
                    "coincident_band", "ccf_lag_max", "alpha",
                    "granger_max_lag", "granger_dmax", "max_iter",
                    "min_components", "horizon", "min_auc")
  for (s in scalar_slots) {
    if (!one(s)) msg <- c(msg, sprintf("`%s` must have length 1.", s))
  }
  if (length(msg) > 0L) return(msg)
  for (s in setdiff(scalar_slots, "horizon")) {
    if (is.na(slot(object, s))) {
      msg <- c(msg, sprintf("`%s` must not be NA.", s))
    }
  }
  if (length(msg) > 0L) return(msg)
  if (!object@freq %in% c(4, 12)) msg <- c(msg, "`freq` must be 4 or 12.")
  if (length(object@seasonal_adjust) != 1L || is.na(object@seasonal_adjust)) {
    msg <- c(msg, "`seasonal_adjust` must be TRUE or FALSE.")
  }
  if (!(object@outlier_threshold > 0)) {
    msg <- c(msg, "`outlier_threshold` must be positive.")
  }
  if (methods::.hasSlot(object, "form") &&
      form_kind(object@form) != "level" &&
      needs_levels(object@aggregator)) {
    msg <- c(msg, paste("The", object@aggregator@label, "aggregator needs",
                        "the level form: its changes of levels are not",
                        "defined for growth rates."))
  }
  if (!(object@min_lead >= 0)) msg <- c(msg, "`min_lead` must be >= 0.")
  if (!(object@min_consistency >= 0 && object@min_consistency <= 1)) {
    msg <- c(msg, "`min_consistency` must be in [0, 1].")
  }
  for (s in c("max_missed", "max_extra")) {
    if (!(slot(object, s) >= 0 && slot(object, s) <= 1)) {
      msg <- c(msg, sprintf("`%s` must be a share in [0, 1].", s))
    }
  }
  for (s in c("coincident_band", "granger_dmax", "min_median_lead")) {
    if (!(slot(object, s) >= 0)) msg <- c(msg, sprintf("`%s` must be >= 0.", s))
  }
  if (!(object@min_auc >= 0.5 && object@min_auc <= 1)) {
    msg <- c(msg, "`min_auc` must be in [0.5, 1].")
  }
  if (length(object@target_lead) != 2L || anyNA(object@target_lead) ||
      object@target_lead[1] > object@target_lead[2]) {
    msg <- c(msg, "`target_lead` must be an increasing pair of numbers.")
  }
  if (length(object@lead_window) != 2L || any(object@lead_window < 0)) {
    msg <- c(msg, "`lead_window` must be two non-negative numbers.")
  }
  if (!(object@ccf_lag_max >= 1)) msg <- c(msg, "`ccf_lag_max` must be >= 1.")
  if (!(object@alpha > 0 && object@alpha < 1)) {
    msg <- c(msg, "`alpha` must be in (0, 1).")
  }
  if (!(object@granger_max_lag >= 1)) {
    msg <- c(msg, "`granger_max_lag` must be >= 1.")
  }
  if (!(object@max_iter >= 1)) msg <- c(msg, "`max_iter` must be >= 1.")
  for (s in c("max_iter", "min_components", "ccf_lag_max",
              "granger_max_lag", "granger_dmax", "horizon")) {
    v <- slot(object, s)
    if (is.finite(v) && abs(v - round(v)) > 1e-8) {
      msg <- c(msg, sprintf("`%s` must be a whole number.", s))
    }
  }
  if (!(object@min_components >= 1)) {
    msg <- c(msg, "`min_components` must be >= 1.")
  }
  if (!is.na(object@horizon) &&
      !(object@horizon >= 0 && is.finite(object@horizon))) {
    msg <- c(msg, "`horizon` must be NA or a finite number >= 0.")
  }
  hol <- object@holidays
  if (!all(vapply(hol, is, logical(1), "HolidayEffect"))) {
    msg <- c(msg, "`holidays` must be holiday_effect() objects.")
  } else if (length(hol) > 0L && !isTRUE(object@seasonal_adjust)) {
    msg <- c(msg, "`holidays` need `seasonal_adjust = TRUE`.")
  }
  if (length(object@lag_shift) != 1L || is.na(object@lag_shift)) {
    msg <- c(msg, "`lag_shift` must be TRUE or FALSE.")
  }
  if (methods::.hasSlot(object, "amplitude") &&
      (length(object@amplitude) != 1L ||
         !isTRUE(object@amplitude %in% c("mad", "sd")))) {
    msg <- c(msg, "`amplitude` must be \"mad\" or \"sd\".")
  }
  if (methods::.hasSlot(object, "matching") &&
      (length(object@matching) != 1L ||
         !isTRUE(object@matching %in% c("nearest", "maximum")))) {
    msg <- c(msg, "`matching` must be \"nearest\" or \"maximum\".")
  }
  if (methods::.hasSlot(object, "twin_threshold") &&
      (length(object@twin_threshold) != 1L ||
         !isTRUE(object@twin_threshold > 0 && object@twin_threshold <= 1))) {
    msg <- c(msg, "`twin_threshold` must be in (0, 1].")
  }
  if (length(msg) == 0L) TRUE else msg
})

#' Update a specification
#'
#' @param object A [CliSpec-class].
#' @param ... Named slots to replace, e.g. `min_lead = 6`.
#' @return The updated, re-validated [CliSpec-class].
#' @examples
#' update(cli_spec(), aggregator = principal_component())
#' @export
setMethod("update", "CliSpec", function(object, ...) {
  object <- upgrade_spec(object)
  args <- list(...)
  bad <- setdiff(names(args), slotNames(object))
  if (length(args) > 0L && (is.null(names(args)) || length(bad) > 0L)) {
    cyclind_abort(
      sprintf("Unknown specification field(s): %s.",
              paste(bad, collapse = ", ")),
      "cyclind_error_argument"
    )
  }
  # a filter left at the default of the old form follows the new form
  if ("form" %in% names(args) && !"filter" %in% names(args) &&
      is(args$form, "CycleForm") &&
      identical(object@filter, default_filter(object@form))) {
    args$filter <- default_filter(args$form)
  }
  types <- methods::getSlots("CliSpec")
  old_outliers <- object@outliers
  for (nm in names(args)) {
    value <- args[[nm]]
    if (is.numeric(value) ||
        (types[[nm]] == "numeric" && is.logical(value) && all(is.na(value)))) {
      value <- as.numeric(value)
    }
    tryCatch(slot(object, nm) <- value, error = invalid_error)
  }
  # keep the outlier shortcut and the outlier treatment in step
  if ("outlier_threshold" %in% names(args) && !"outliers" %in% names(args)) {
    object@outliers <- robust_outliers(object@outlier_threshold)
  }
  if (is(object@outliers, "RobustOutliers")) {
    object@outlier_threshold <- object@outliers@threshold
  }
  # switching the outlier treatment off keeps its end extension (butir 2)
  carried <- carried_extension(old_outliers, object@outliers)
  if (!is.null(carried) && !"extension" %in% names(args) &&
      is(object@extension, "NoExtension")) {
    object@extension <- carried
  }
  check_valid(object)
  check_form_filter(object)
  object
})

#' Specification that follows the OECD method
#'
#' A [cli_spec()] set, step by step, to the OECD system of composite leading
#' indicators as documented by Gyomai and Guidetti (2012, "GG12") and
#' Gyomai, Ahmad and Astolfi (2017, "GAA17"), where the documents state the
#' rule, and to the choice that reproduces the published OECD CLI of
#' Indonesia where they do not. It is not a replication of the OECD's
#' production system: the steps marked "cyclind" below are not written in
#' the OECD documents.
#'
#' **Rules of the OECD** (written in GG12 or GAA17; `oecd_spec()` follows
#' them):
#'
#' * outliers: TRAMO finds them; additive outliers (AO) are filtered out and
#'   level shifts (LS) are analysed and removed only on expert judgement
#'   (GAA17 p. 283; transitory changes are not mentioned);
#' * log or levels: the TRAMO log/level test (GG12 p. 7; GAA17 p. 284);
#'   `transform = "test"`;
#' * end of the series: 2 to 6 months of TRAMO forecasts (GAA17 p. 284);
#' * cycle: double Hodrick-Prescott filter, 12 to 120 months (GG12 p. 7
#'   and 15; OECD 2024, question 10); [hp_double_filter()];
#' * the target is the growth cycle (deviation from trend); components are
#'   selected on it, and CLIs for the growth rate and classical cycles are
#'   presentations (GAA17 pp. 276--277);
#' * normalisation: mean and mean absolute deviation (GG12 p. 7; GAA17
#'   p. 284);
#' * turning points: simplified Bry-Boschan with phases of at least 9 and
#'   cycles of at least 24 months (GAA17 p. 285); [oecd_dater()];
#' * matching window: 24 months lead and 9 months lag (GAA17 p. 285);
#'   `lead_window = c(9, 24)`;
#' * evaluation: length and consistency of the lead, cyclical conformity
#'   (cross-correlation) and missing or extra cycles, named without
#'   thresholds (GG12 p. 8);
#' * classification: median lead at turning points, coincident within
#'   +/- 2 months (GG12 p. 9); [oecd_classifier()];
#' * lag-shifting: supported but not used in practice (GAA17 p. 287);
#'   `lag_shift = FALSE`;
#' * aggregation: chained mean of the ratios \eqn{c_{i,t}/c_{i,t-1}} with
#'   the 60 percent rule (GAA17 p. 286, note 8);
#' * amplitude adjustment: rescaled to the reference cycle, measure not
#'   given (GG12 p. 9; GAA17 p. 288);
#' * trend restored: trend of the reference times the CLI, not computed
#'   before GDP is available (GG12 p. 10; OECD 2024, question 6);
#'   [cli_presentation()].
#'
#' **Choices of cyclind** (not written in the OECD documents; each can be
#' changed):
#'
#' * `outlier_types = "AO"`: transitory changes are detected and reported
#'   but kept, as level shifts are (ADR-010), and an AO and a TC found on
#'   the same date are rewritten so that the AO is the whole deviation at
#'   that date (ADR-012, [OutlierTreatment-class]); `c("AO", "TC", "LS")`
#'   gives the fully linearised series of GG12 (p. 6);
#' * forecasts: 2 months, the lower end of the OECD range;
#' * `min_matched = 3` in [oecd_classifier()]: the OECD table of median
#'   leads sets no minimum number of matched turning points;
#' * the thresholds and tests that make the OECD evaluation criteria
#'   operational ([oecd_classifier()], `conformity = TRUE`): at most 40
#'   percent missed and 40 percent extra turning points (`max_missed`,
#'   `max_extra`), a positive cross-correlation peak, and a concordance
#'   significantly above its value under independence, tested with a
#'   fixed-b HAC test (Kiefer and Vogelsang 2005; see [cli_assess()]);
#' * aggregation: the mean of log ratios with reciprocal inversion of
#'   counter-cyclical components ([oecd_aggregate()], see below) instead
#'   of the written ratio formula;
#' * amplitude: ratio of standard deviations (see below);
#' * phase rule of the dating: the most extreme turning points are kept
#'   (`phase_rule = "symmetric"`);
#' * quarterly data: the OECD system is monthly; the month thresholds are
#'   converted to quarters (phases of 3 and cycles of 8 quarters, a lead
#'   window of 3 and 8 quarters, a coincident band of 2/3 of a quarter);
#' * the transformation decided by cyclind is also used by the X-13
#'   seasonal adjustment, and a series that X-13 cannot adjust is handled
#'   as described under `seasonal_adjust` in [cli_spec()].
#'
#' [cli_spec()] uses the package defaults instead: a robust z-score above 5
#' for outliers, `transform = "auto"`, 12 months of AR forecasts, the
#' Bry-Boschan rule with 5/15 months (GG12 annex B), a window of
#' `c(12, 24)`, [cyclind_classifier()] and [equal_weight()].
#'
#' **Turning points.** The OECD dates with the simplified Bry-Boschan rule
#' and phases of at least 9 and cycles of at least 24 months (GAA17,
#' p. 285). The 5/15-month rule of the original Bry-Boschan routine, which
#' the OECD used with the phase-average trend method before 2008 (GG12,
#' annex B), is `oecd_spec(dater = bry_boschan())`. Which turning point of a
#' too-short phase the OECD drops is not documented; cyclind keeps the most
#' extreme ones (`phase_rule = "symmetric"`, see [Dater-class]).
#'
#' **Aggregation.** GAA17 (p. 286, note 8) writes the CLI as
#' \deqn{CLI_t = \frac{\sum_i w_i \delta_{i,t,t-1} c_{i,t} / c_{i,t-1}}
#'   {\sum_i w_i \delta_{i,t,t-1}} CLI_{t-1},\quad CLI_1 = 100,}
#' with \eqn{\delta_{i,t,t-1} = 1} when component \eqn{i} is available at
#' \eqn{t} and \eqn{t-1}, computed when \eqn{\sum_i w_i \delta_{i,t,t-1}} is
#' at least 60 percent. The document does not say how a counter-cyclical
#' component enters. This written formula, with inversion \eqn{200 - c}, is
#' `oecd_aggregate(growth = "ratio")`. The default of `oecd_spec()` is
#' `oecd_aggregate(growth = "log")`: the mean of log ratios, with a
#' counter-cyclical component entering as the reciprocal
#' \eqn{100^2 / c}. It is a choice of cyclind, made because it is closer to
#' the published CLI of Indonesia: from the seven component cycles the OECD
#' publishes, the correlation with the published normalised CLI is 0.99985
#' (largest difference 0.086) against 0.99839 (0.605) for the written
#' formula, and both find all 27 of its turning points (audit of 2026-11-17;
#' `data-raw/oecd_idn_validate.R`). Most of the gain comes from the
#' reciprocal inversion, not from the logarithm; that the OECD inverts this
#' way has not been confirmed by the OECD.
#'
#' **Amplitude.** The OECD does not give the measure by which the CLI is
#' rescaled to the amplitude of the reference. On the CLI of Indonesia the
#' ratio of standard deviations over common dates (`amplitude = "sd"`)
#' matches the published amplitude-adjusted CLI and the ratio of mean
#' absolute deviations does not (about 17 percent too small), so `"sd"` is
#' the default here.
#'
#' Seasonal adjustment, which the OECD applies only to series that are not
#' already adjusted at the source, uses X-13ARIMA-SEATS (the successor of
#' X-12-ARIMA, with SEATS) when `seasonal_adjust = TRUE`. Quarterly
#' candidates are always interpolated linearly by [cli_align()], at the
#' month the OECD uses (GG12 p. 6; `method` only concerns the reference
#' series, for which the OECD method of monthly GDP is not reproduced).
#' The selection loop of [cli_build()] then evaluates composites with the
#' OECD classifier.
#'
#' The minimum median lead of the out-of-sample check is set to the
#' coincident band (2 months). Other arguments are passed to [cli_spec()].
#'
#' From the four component series of Indonesia that are public, the whole
#' method gives a composite that correlates 0.89 with the published CLI and
#' finds 10 of the 12 turning points that [oecd_dater()] dates in it over
#' the common span (row B1 of `inst/calibration/oecd-idn-composite.csv`);
#' the classification step uses the stricter operationalisation of
#' [oecd_classifier()].
#'
#' @param freq Data frequency, 12 or 4. The OECD system is monthly; for
#'   quarterly data the month thresholds are converted to quarters (an
#'   extension of cyclind, not an OECD rule).
#' @param seasonal_adjust,holidays See [cli_spec()].
#' @param engine Engine of [tramo_outliers()] and of the log/level test.
#' @param classifier An OECD [Classifier-class], by default
#'   [oecd_classifier()].
#' @param forecast Months of TRAMO forecasts appended before the filter
#'   ([tramo_outliers()]); `NULL` means 2 months (1 quarter), within the
#'   2 to 6 months of GAA17 (p. 284). Until INC-15 it was one year.
#' @param amplitude Amplitude measure of the index (see [cli_spec()]); the
#'   OECD amplitude-adjusted CLI matches standard deviations (`"sd"`).
#' @param transform Transformation before the filter (see [cli_spec()]);
#'   the OECD log/level test (`"test"`) by default. `"none"` suits series
#'   that are already growth rates or percentages.
#' @param outlier_types Outlier types whose effects are removed by
#'   [tramo_outliers()]: any of `"AO"`, `"TC"`, `"LS"`. The default `"AO"`
#'   follows the OECD practice (Gyomai, Ahmad and Astolfi 2017, p. 283:
#'   additive outliers are filtered, level shifts only on expert judgement;
#'   ADR-010); the other types are detected and reported but kept.
#'   `c("AO", "TC")` was the default before 0.2.0.
#' @param filter A [CycleFilter-class] in place of the OECD double
#'   Hodrick-Prescott filter, for example [hp_smoother()] for series that
#'   are already growth rates (FR-014.8). `NULL` keeps the OECD filter (or
#'   the default filter of the cycle form).
#' @param dater A [Dater-class]; `NULL` means [oecd_dater()] (phases of 9
#'   and cycles of 24 months, or 3 and 8 quarters). `bry_boschan()` gives
#'   the original 5/15-month rule.
#' @param aggregator An [Aggregator-class]; `NULL` means [oecd_aggregate()]
#'   (log growth, reciprocal inversion). `oecd_aggregate(growth = "ratio")`
#'   is the formula written in GAA17.
#' @param lag_shift Shift components by their median lead before
#'   aggregation? `FALSE` by default, as in OECD practice (GAA17, p. 287).
#' @param lead_window Pairing window of turning points (see [cli_spec()]);
#'   `NULL` means 9 months after and 24 before the reference (3 and 8
#'   quarters).
#' @param min_median_lead Minimum median lead; `NULL` means the coincident
#'   band of `classifier` (2 months).
#' @param outliers An [OutlierTreatment-class]; `NULL` means
#'   `tramo_outliers(engine = engine, forecast = forecast, remove =
#'   outlier_types)`. When given, `outlier_types` is ignored.
#' @param extension An [Extension-class]; `NULL` means [no_extension()]
#'   (the TRAMO forecasts of `outliers` extend the series).
#' @param ... Further arguments of [cli_spec()]. With `form =`
#'   [growth_form()] (experimental, not part of the OECD method) the
#'   filter is [hp_smoother()], as in [cli_spec()].
#' @return A [CliSpec-class].
#' @references
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD Publishing, Paris.
#'
#' OECD (2024). *Composite Leading Indicators: Frequently Asked Questions*.
#' OECD, Paris.
#'
#' Kiefer, N. M. and Vogelsang, T. J. (2005). A new asymptotic theory for
#' heteroskedasticity-autocorrelation robust tests. *Econometric Theory*,
#' 21(6), 1130--1164.
#' @seealso [cli_spec()] for the package defaults and the reasons they
#'   differ (`vignette("cyclind")`).
#' @examples
#' oecd_spec()
#' # the original Bry-Boschan rule (5/15 months), OECD before 2008
#' oecd_spec(dater = bry_boschan())
#' \donttest{
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cand <- cli_candidates(sim, "reference", meta = meta)
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   res <- cli_build(cand, oecd_spec(engine = "x13"))
#'   res
#' }
#' }
#' @export
oecd_spec <- function(freq = 12, seasonal_adjust = FALSE, holidays = list(),
                      engine = c("auto", "tramo", "x13"),
                      classifier = oecd_classifier(), forecast = NULL,
                      amplitude = c("sd", "mad"), transform = "test",
                      filter = NULL, outlier_types = "AO",
                      dater = NULL, aggregator = NULL,
                      lag_shift = FALSE, lead_window = NULL,
                      min_median_lead = NULL, outliers = NULL,
                      extension = NULL, ...) {
  check_frequency(freq)
  engine <- match.arg(engine)
  if (identical(amplitude, c("sd", "mad"))) amplitude <- "sd"
  if (is.null(forecast)) forecast <- if (freq == 12) 2 else 1
  if (!is(classifier, "OecdClassifier")) {
    cyclind_abort("`classifier` must be an oecd_classifier().",
                  "cyclind_error_argument")
  }
  cls <- classifier
  if (!is.null(filter) && !is(filter, "CycleFilter")) {
    cyclind_abort("`filter` must be a CycleFilter (see hp_double_filter()).",
                  "cyclind_error_argument")
  }
  check_strategy(dater, "Dater", "dater")
  check_strategy(aggregator, "Aggregator", "aggregator")
  # the OECD double HP filter, or the default filter of another form
  form <- list(...)$form
  filt <- if (!is.null(filter)) {
    filter
  } else if (is.null(form) || !is(form, "CycleForm")) {
    hp_double_filter()
  } else {
    default_filter(form)
  }
  monthly <- freq == 12
  cli_spec(freq = freq, filter = filt,
           dater = if (is.null(dater)) oecd_dater() else dater,
           aggregator = if (is.null(aggregator)) oecd_aggregate() else
             aggregator,
           transform = transform,
           seasonal_adjust = seasonal_adjust, holidays = holidays,
           min_median_lead = if (is.null(min_median_lead)) {
             cls@coincident * freq / 12
           } else {
             min_median_lead
           },
           classifier = cls,
           outliers = if (is.null(outliers)) {
             tramo_outliers(engine = engine, forecast = forecast,
                            remove = outlier_types)
           } else {
             outliers
           },
           lag_shift = lag_shift,
           lead_window = if (is.null(lead_window)) {
             if (monthly) c(9, 24) else c(3, 8)
           } else {
             lead_window
           },
           extension = if (is.null(extension)) no_extension() else extension,
           amplitude = amplitude, ...)
}
