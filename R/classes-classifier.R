#' Classification rules
#'
#' A `Classifier` decides, from the statistics computed by [cli_assess()],
#' whether a candidate (or the composite, in [cli_validate()]) is leading,
#' coincident, lagging or rejected.
#'
#' * `cyclind_classifier()`: the default of the package. A series is leading
#'   when it meets every criterion listed in [cli_assess()]: at least three
#'   matched turning points, a positive cross-correlation peak at a lead of
#'   at least `min_lead`, a median lead of at least `min_median_lead`,
#'   consistency, few missed and extra turning points, significant
#'   concordance and Granger causality. The thresholds are the fields of the
#'   [CliSpec-class] and were calibrated by Monte Carlo simulation.
#' * `oecd_classifier()`: the OECD classification by the median lead, in
#'   two forms. `rule = "cyclind"` (the default, with unchanged results) adds
#'   the thresholds of cyclind listed below; `rule = "document"` applies only
#'   what the OECD documents write, with the cross-check of the lag of the
#'   cross-correlation peak.
#'
#' **What the OECD documents write.** The class follows only the median lead
#' at turning points (Gyomai and Guidetti 2012, "GG12", p. 9): coincident
#' within +/- `coincident` months, leading from `coincident` months on
#' ("shorter/medium" up to `long` months, "longer" beyond) and lagging at
#' `-coincident` months or less. The OECD also evaluates candidates by the
#' length and consistency of the lead, by cyclical conformity and by missing
#' or extra cycles, but names these criteria without thresholds or tests
#' (GG12 p. 8; Gyomai, Ahmad and Astolfi 2017, "GAA17", pp. 285--286). The
#' one check the documents formulate is a cross-check: the lead "at which
#' the highest correlation occurs should not be too different from the
#' median lead" (GAA17 p. 286; GG12 p. 8 has "median lag"). The tolerance of
#' +/- 2 months for coincident behaviour allows for the phase shifts that
#' the linear interpolation of quarterly series to months introduces
#' (Fulop and Gyomai 2012, "FG12", pp. 3--4).
#'
#' The table separates the two ("c" marks a choice of cyclind):
#'
#' | Step | OECD documents | `"cyclind"` | `"document"` |
#' | --- | --- | --- | --- |
#' | class | median lead (GG12 p. 9) | same | same |
#' | matched turning points | no minimum | >= 3 (c) | >= 1 |
#' | missed, extra cycles | no threshold (GG12 p. 8) | <= 0.4 (c) | shown |
#' | conformity | no threshold (GG12 p. 8) | concordance test (c) | shown |
#' | peak lag | near median lead (GAA17 p. 286) | in band (c) | 6 months (c) |
#' | quarterly data | monthly system (GG12 p. 6) | in quarters (c) | same |
#'
#' **Choices of cyclind in `rule = "cyclind"`.** (1) `min_matched`
#' (default 3) is the smallest number of matched turning points from which a
#' median lead is computed. (2) With `conformity = TRUE` (the default) the
#' OECD evaluation criteria are made operational: a positive
#' cross-correlation peak, at most `max_missed` and `max_extra` (40 percent)
#' missed and extra turning points, and a concordance significantly above
#' its value under independence at level `alpha`, tested with a fixed-b HAC
#' test (Kiefer and Vogelsang 2005; see [cli_assess()]). A coincident series
#' must, in addition, have its cross-correlation peak strictly inside the
#' coincident band (lags -1 to 1 for monthly data, lag 0 for quarterly
#' data); otherwise it is rejected ("ccf lag"). The leading and lagging
#' rules do not use the lag of the peak. (3) Thresholds are given in months
#' and converted to quarters for quarterly data.
#'
#' These choices are stricter than OECD practice. In its 2012 evaluation
#' against GDP, the OECD found 1 missed and 5 extra turning points for 8 of
#' the reference in the CLI of Indonesia and flagged that CLI for review,
#' not for withdrawal (FG12 pp. 7--8, table 2c); dated with [oecd_dater()],
#' the CLI published now has 6 extra turning points against 8
#' (`inst/calibration/oecd-idn-composite.csv`, row "OECD published CLI").
#' The rule rejects all seven components of that CLI, for extra turning
#' points and concordance (`inst/calibration/oecd-idn-components.csv`).
#' Without the thresholds the median lead alone labels about half of all
#' independent cycles as leading; with them about one in ten (monthly) to
#' one in five (quarterly) in the simulations of
#' `inst/calibration/lead-window.csv`, since the OECD rule has no test of
#' predictive content such as the Granger test of [cyclind_classifier()]
#' (0 to 3 percent). `conformity = FALSE` gives the median-lead table taken
#' literally, with at least `min_matched` matched turning points.
#'
#' **`rule = "document"`.** The class follows the median-lead table with at
#' least one matched turning point (`min_matched = 1` unless given), and a
#' series of any class whose cross-correlation peak lies more than
#' `ccf_tolerance` months from its median lead is rejected ("ccf lag").
#' Missed and extra turning points, concordance and the size of the
#' correlation are reported for judgement only, as the OECD analyst would
#' read them. The documents give no number for "not too different", so the
#' default of 6 months is a choice of cyclind: it is the largest gap
#' between the median lead and the peak lead of the CLIs the OECD kept when
#' it moved to the GDP target, except one (0 to 6 months, Slovenia 7, in
#' FG12 tables 2a and 2b, pp. 5--6), and it keeps the CLI of Indonesia
#' itself (median 8, peak 3 months; FG12 p. 7). With it and the OECD signs
#' (`meta$sign`), 4 of the 7 OECD components of Indonesia are leading and 1
#' coincident, against none with `rule = "cyclind"`; the exchange rate
#' (median lead 9.5, peak 3 months) and producer prices fail the
#' cross-check. With 3 months (one quarter) 2 are leading
#' (`inst/calibration/oecd-idn-components.csv`, columns
#' `A_class_oecd_document_rule*`). The rule is closer to the documents and
#' therefore looser: it has neither the conformity thresholds nor a test of
#' predictive content.
#'
#' New rules are added by defining a subclass of `Classifier` and methods
#' for the generics `classify_candidate()` and `leading_criteria()`.
#'
#' @param coincident Half-width, in months, of the coincident band (OECD: 2).
#' @param long Lead, in months, above which a leading series is "longer
#'   leading" (OECD: 8).
#' @param min_matched Minimum number of matched turning points; 3 with
#'   `rule = "cyclind"`, 1 with `rule = "document"` when not given.
#' @param conformity Require cyclical conformity and few missed or extra
#'   cycles (see above). Only for `rule = "cyclind"`.
#' @param rule `"cyclind"` (the default) for the OECD table with the
#'   thresholds of cyclind, `"document"` for the rule of the OECD documents
#'   with the cross-check of the lag (an `OecdDocumentClassifier`).
#' @param ccf_tolerance Largest distance, in months, between the lead of the
#'   cross-correlation peak and the median lead with `rule = "document"`
#'   (converted to quarters for quarterly data). A choice of cyclind; see
#'   above.
#' @return A `Classifier` object.
#' @references
#' Fulop, G. and Gyomai, G. (2012). *Transition of the OECD CLI System to a
#' GDP-based Business Cycle Target*. OECD Composite Leading Indicators,
#' Background note, March 2012. OECD, Paris.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 273--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD, Paris, April 2012.
#'
#' Kiefer, N. M. and Vogelsang, T. J. (2005). A new asymptotic theory for
#' heteroskedasticity-autocorrelation robust tests. *Econometric Theory*,
#' 21(6), 1130--1164.
#' @seealso [cli_spec()], [oecd_spec()], [cli_assess()]
#' @examples
#' cyclind_classifier()
#' oecd_classifier()
#' oecd_classifier(rule = "document")
#' cli_spec(classifier = oecd_classifier())
#' @name Classifier-class
#' @aliases Classifier
#' @exportClass Classifier
NULL

setClass("Classifier", representation("VIRTUAL", label = "character"))

#' @rdname Classifier-class
setClass("CyclindClassifier", contains = "Classifier")

#' @rdname Classifier-class
setClass("OecdClassifier", contains = "Classifier",
         representation(coincident = "numeric", long = "numeric",
                        min_matched = "numeric", conformity = "logical"))

#' @rdname Classifier-class
setClass("OecdDocumentClassifier", contains = "OecdClassifier",
         representation(ccf_tolerance = "numeric"))

#' @rdname Classifier-class
#' @export
cyclind_classifier <- function() {
  new("CyclindClassifier",
      label = paste("cyclind: cross-correlation, turning points,",
                    "concordance, Granger"))
}

#' @rdname Classifier-class
#' @export
oecd_classifier <- function(coincident = 2, long = 8, min_matched = 3,
                            conformity = TRUE,
                            rule = c("cyclind", "document"),
                            ccf_tolerance = 6) {
  rule <- match.arg(rule)
  if (rule == "document") {
    if (!missing(conformity) && isTRUE(conformity)) {
      cyclind_abort(paste("`conformity` applies to `rule = \"cyclind\"`",
                          "only; the rule of the OECD documents has no",
                          "conformity thresholds. Drop `conformity`."),
                    "cyclind_error_argument")
    }
    return(oecd_document_classifier(
      coincident, long, if (missing(min_matched)) 1 else min_matched,
      ccf_tolerance))
  }
  check_oecd_table(coincident, long, min_matched)
  if (!is.logical(conformity) || length(conformity) != 1L ||
      is.na(conformity)) {
    cyclind_abort("`conformity` must be TRUE or FALSE.",
                  "cyclind_error_argument")
  }
  label <- if (conformity) {
    "OECD: median lead at turning points, cyclical conformity"
  } else {
    "OECD: median lead at turning points"
  }
  new("OecdClassifier", label = label,
      coincident = as.numeric(coincident), long = as.numeric(long),
      min_matched = as.numeric(min_matched), conformity = conformity)
}

# Arguments of the OECD median-lead table (GG12 p. 9).
check_oecd_table <- function(coincident, long, min_matched) {
  check_number(coincident, "coincident", lower = 0)
  check_number(long, "long", lower = 0)
  check_number(min_matched, "min_matched", lower = 1)
  if (long < coincident) {
    cyclind_abort("`long` must be at least `coincident`.",
                  "cyclind_error_argument")
  }
  invisible(TRUE)
}

# The rule of the OECD documents: the median-lead table and the cross-check
# of the lag of the cross-correlation peak (GG12 pp. 8-9; GAA17 p. 286).
oecd_document_classifier <- function(coincident, long, min_matched,
                                     ccf_tolerance) {
  check_oecd_table(coincident, long, min_matched)
  check_number(ccf_tolerance, "ccf_tolerance", lower = 0)
  new("OecdDocumentClassifier",
      label = paste("OECD documents: median lead at turning points,",
                    "lag of the cross-correlation peak"),
      coincident = as.numeric(coincident), long = as.numeric(long),
      min_matched = as.numeric(min_matched), conformity = FALSE,
      ccf_tolerance = as.numeric(ccf_tolerance))
}

# Conformity setting of an OECD classifier; classifiers saved before 0.6.0
# have none and keep the rule they were created with.
oecd_conformity <- function(classifier) {
  methods::.hasSlot(classifier, "conformity") &&
    isTRUE(classifier@conformity)
}

#' @rdname Classifier-class
#' @param classifier A `Classifier`.
#' @param row One-row data frame of statistics from [cli_assess()].
#' @param spec A [CliSpec-class].
#' @export
setGeneric("classify_candidate", function(classifier, row, spec) {
  standardGeneric("classify_candidate")
})

#' @rdname Classifier-class
#' @export
setGeneric("leading_criteria", function(classifier, row, spec) {
  standardGeneric("leading_criteria")
})

#' @rdname Classifier-class
#' @param object An object to print.
setMethod("show", "Classifier", function(object) {
  cat("<", class(object), "> ", object@label, "\n", sep = "")
  invisible(object)
})

#' @rdname Classifier-class
setMethod("show", "OecdClassifier", function(object) {
  cat("<", class(object), "> ", object@label, "\n", sep = "")
  cat("  coincident within +/- ", object@coincident, " months; leading from ",
      object@coincident, " months (longer over ", object@long,
      "); lagging at -", object@coincident, " months or less; at least ",
      object@min_matched, " matched turning points",
      if (oecd_conformity(object)) {
        "; cyclical conformity and few missed or extra cycles"
      }, "\n", sep = "")
  invisible(object)
})

#' @rdname Classifier-class
setMethod("show", "OecdDocumentClassifier", function(object) {
  callNextMethod()
  cat("  cross-correlation peak within ", object@ccf_tolerance,
      " months of the median lead\n", sep = "")
  invisible(object)
})

# Half-width, in periods, of the timing band of a coincident index, and the
# texts of the scorecard rules that depend on the classifier. Internal
# generics, so that a classifier sets its own band without branches here.
setGeneric("timing_band", function(classifier, spec) {
  standardGeneric("timing_band")
})
setMethod("timing_band", "Classifier", function(classifier, spec) {
  max(2 * spec@freq / 12, 1)
})
setMethod("timing_band", "OecdClassifier", function(classifier, spec) {
  oecd_band(classifier, spec)[["coincident"]]
})

# TRUE where a lead lies in the timing band: inclusive by default, strict for
# the OECD classifier (its coincident band is an open interval).
setGeneric("in_timing_band", function(classifier, lead, band) {
  standardGeneric("in_timing_band")
})
setMethod("in_timing_band", "Classifier", function(classifier, lead, band) {
  abs(lead) <= band
})
setMethod("in_timing_band", "OecdClassifier", function(classifier, lead,
                                                       band) {
  abs(lead) < band
})

setGeneric("timing_rules", function(classifier, band) {
  standardGeneric("timing_rules")
})
setMethod("timing_rules", "Classifier", function(classifier, band) {
  list(matched = ">= 3", timing = sprintf("within +/- %s", band))
})
setMethod("timing_rules", "OecdClassifier", function(classifier, band) {
  list(matched = sprintf(">= %s", classifier@min_matched),
       timing = sprintf("within (-%s, %s)", band, band),
       ccf = sprintf("within (-%s, %s), positive", band, band))
})
setMethod("timing_rules", "OecdDocumentClassifier", function(classifier,
                                                           band) {
  rules <- callNextMethod()
  rules$ccf <- sprintf("|lag - median lead| <= %s months",
                       classifier@ccf_tolerance)
  rules
})
