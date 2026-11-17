#' Outlier treatment
#'
#' An `OutlierTreatment` cleans a (transformed) series of outliers before
#' its cycle is extracted, and may extend it with forecasts.
#'
#' * `robust_outliers()`: the default of the package. An observation is an
#'   outlier when its change from the previous period and the next change
#'   are both more than `threshold` robust z-scores (deviation from the
#'   median change divided by the median absolute deviation) and of
#'   opposite sign, that is, a one-period spike. Spikes are replaced by
#'   linear interpolation. An extreme first change is replaced too (the
#'   first value becomes the second minus the median change); the last
#'   observation is left alone, because an extreme last change cannot yet
#'   be told apart from a level shift or a turn. No model and no external
#'   program are needed.
#' * `tramo_outliers()`: the OECD treatment (Gyomai and Guidetti 2012,
#'   p. 6--7). A regARIMA model with automatic outlier detection finds
#'   additive outliers (AO), transitory changes (TC) and level shifts (LS);
#'   the estimated effects of the types in `remove` are subtracted from the
#'   series, which replaces the outlying observations by their estimated
#'   values. The same model provides `forecast` periods of forecasts that
#'   extend the series before the cycle filter, which stabilises the end of
#'   the cycle. With `engine = "tramo"` the model is TRAMO, through the
#'   \pkg{RJDemetra} package (JDemetra+, needs Java), as at the OECD; with
#'   `engine = "x13"` it is the regARIMA part of X-13ARIMA-SEATS through
#'   \pkg{seasonal}, which uses the same outlier types and detection
#'   procedure (Chen and Liu 1993); `"auto"` takes TRAMO when \pkg{RJDemetra}
#'   is installed and X-13 otherwise.
#'
#' By default only additive outliers (AO) are removed, as the OECD
#' describes its practice (Gyomai, Ahmad and Astolfi 2017, p. 283: additive
#' outliers are filtered, level shifts are analysed and removed only on
#' expert judgement; ADR-010). Transitory changes (TC) and level shifts
#' (LS) are detected, so that they are not mistaken for other outliers,
#' and reported, but kept: in monthly activity data the automatic
#' procedure often labels the onset of a recession a level shift or a
#' transitory change, and removing it would erase a genuine cyclical
#' movement. `remove = c("AO", "TC")` was the default before 0.1.0.9000;
#' `remove = c("AO", "TC", "LS")` gives the fully linearised series (Gyomai
#' and Guidetti 2012, p. 6).
#'
#' **An AO and a TC on the same date.** Their regressors are nearly
#' collinear: after first differencing they correlate
#' \eqn{(2 - r)\sqrt{1 + r}/2}{(2 - r) sqrt(1 + r) / 2}, 0.96 for the
#' quarterly X-13 decay rate \eqn{r = 0.343}{r = 0.343} and 0.85 for
#' \eqn{r = 0.7}{r = 0.7}. The model then cannot tell how the deviation at
#' that date splits between them, and the two estimates can be large with
#' opposite signs; removing only the AO would leave the TC's value at that
#' date in the series, a deviation the data never showed, and can date a
#' false turning point. cyclind therefore rewrites the pair, without
#' changing the fitted effect, as an AO equal to the whole deviation at that
#' date (\eqn{a + c}) and a TC that starts one period later (\eqn{c r}):
#' with `remove = "AO"` the deviation at that date is removed and the
#' transitory change after it is kept. A warning of class
#' `cyclind_warning_outlier_collinear` names the date and the correlation,
#' the `collinear` column of the outlier table and of [outlier_report()]
#' marks the pair, and the `steps` record it (ADR-012). Pairs on adjacent
#' dates are not rewritten: each regressor then owns a date of its own.
#' Several regARIMA programs may also pick different models for the same
#' series (for example X-13 levels with ARIMA(1 2 0), TRAMO logarithms);
#' setting `transform` of the specification fixes the scale for both.
#'
#' Every treatment reports the outliers it found: [cli_cycles()] keeps
#' their dates and types in the `outliers` slot of each
#' [CycleSeries-class], [outlier_report()] tabulates them, and the
#' `show()` methods print them. When the outliers removed from a series
#' exceed `max_share` of its observations, a warning of class
#' `cyclind_warning_outlier_share` is given and recorded in the `notes`
#' slot: so many replaced values change the cycle itself, and the series or
#' the threshold deserve a look.
#'
#' When X-13ARIMA-SEATS cannot estimate the model, the series keeps its
#' outliers, a warning of class `cyclind_warning_x13` names the series and
#' the error, and the `notes` slot records it. Errors of the system (a full
#' disk, a missing X-13 binary, a file that cannot be written) stop the run
#' with an error of class `cyclind_error_system`.
#'
#' New treatments are added by defining a subclass and a method for the
#' generic `treat_outliers()`, which returns a list with the cleaned series
#' `x`, the number of outliers `n`, a description `step`, a numeric vector
#' `forecast` (possibly empty) and, optionally, a data frame `table` of the
#' outliers found (`index`, `type`, `removed`, optionally `collinear`) and a
#' character `failed` (the error message when the model failed).
#'
#' @param threshold Robust z-score above which a change is extreme.
#' @param max_share Share of the observations of a series above which the
#'   outliers removed give a warning (default 0.05, one month in twenty).
#' @param engine `"auto"`, `"tramo"` (RJDemetra) or `"x13"` (seasonal).
#' @param remove Outlier types whose effects are removed: any of `"AO"`,
#'   `"TC"`, `"LS"`.
#' @param forecast Number of periods of forecasts appended before the cycle
#'   filter; `NULL` means one year (12 months or 4 quarters). With 0 the
#'   regARIMA model gives no forecasts and the extension of the
#'   specification applies instead ([ar_extension()] by
#'   default; add `extension = no_extension()` for none). The OECD extends
#'   the series with 2 to 6 months of TRAMO forecasts (Gyomai, Ahmad and
#'   Astolfi 2017, p. 284); [oecd_spec()] uses 2 months.
#' @param critical Critical t-value of the outlier detection; `NULL` uses
#'   the program's default, which depends on the length of the series.
#' @return An `OutlierTreatment` object.
#' @references
#' Chen, C. and Liu, L.-M. (1993). Joint estimation of model parameters and
#' outlier effects in time series. *Journal of the American Statistical
#' Association*, 88(421), 284--297.
#'
#' Gómez, V. and Maravall, A. (2001). Automatic modeling methods for
#' univariate series. In D. Peña, G. C. Tiao and R. S. Tsay (eds.), *A
#' Course in Time Series Analysis*, 171--201. Wiley, New York.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD Publishing, Paris.
#' @seealso [cli_spec()], [oecd_spec()], [outlier_report()]
#' @examples
#' robust_outliers()
#' tramo_outliers(engine = "x13")
#' @name OutlierTreatment-class
#' @aliases OutlierTreatment
#' @exportClass OutlierTreatment
NULL

setClass("OutlierTreatment", representation("VIRTUAL", label = "character",
                                            max_share = "numeric"),
         prototype(max_share = 0.05))

check_max_share <- function(max_share) {
  if (!is.numeric(max_share) || length(max_share) != 1L ||
      is.na(max_share) || !(max_share > 0 && max_share <= 1)) {
    cyclind_abort("`max_share` must be a share in (0, 1].",
                  "cyclind_error_argument")
  }
  invisible(TRUE)
}

# Warning threshold of a treatment (objects saved before 0.1.0.9000 have
# none).
outlier_max_share <- function(method) {
  if (methods::.hasSlot(method, "max_share") &&
      length(method@max_share) == 1L) method@max_share else 0.05
}

#' @rdname OutlierTreatment-class
setClass("RobustOutliers", contains = "OutlierTreatment",
         representation(threshold = "numeric"))

#' @rdname OutlierTreatment-class
setClass("TramoOutliers", contains = "OutlierTreatment",
         representation(engine = "character", remove = "character",
                        forecast = "numeric", critical = "numeric"))

#' @rdname OutlierTreatment-class
#' @export
robust_outliers <- function(threshold = 5, max_share = 0.05) {
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !(threshold > 0)) {
    cyclind_abort("`threshold` must be a positive number (Inf disables).",
                  "cyclind_error_argument")
  }
  check_max_share(max_share)
  new("RobustOutliers", label = sprintf("robust z-score > %s", threshold),
      threshold = as.numeric(threshold), max_share = as.numeric(max_share))
}

#' @rdname OutlierTreatment-class
#' @export
tramo_outliers <- function(engine = c("auto", "tramo", "x13"),
                           remove = "AO", forecast = NULL,
                           critical = NULL, max_share = 0.05) {
  engine <- match.arg(engine)
  remove <- toupper(remove)
  if (length(remove) == 0L || !all(remove %in% c("AO", "TC", "LS"))) {
    cyclind_abort("`remove` must contain \"AO\", \"TC\" and/or \"LS\".",
                  "cyclind_error_argument")
  }
  if (!is.null(forecast)) {
    check_number(forecast, "forecast", lower = 0, integer = TRUE)
  }
  if (!is.null(critical)) check_number(critical, "critical", lower = 1)
  check_max_share(max_share)
  new("TramoOutliers",
      label = sprintf("TRAMO-type regARIMA (%s), remove %s", engine,
                      paste(unique(remove), collapse = ", ")),
      engine = engine, remove = unique(remove),
      forecast = if (is.null(forecast)) NA_real_ else as.numeric(forecast),
      critical = if (is.null(critical)) NA_real_ else as.numeric(critical),
      max_share = as.numeric(max_share))
}

#' @rdname OutlierTreatment-class
#' @param method An `OutlierTreatment`.
#' @param x A `ts` (already transformed, without missing values).
#' @param ... Unused.
#' @export
setGeneric("treat_outliers", function(method, x, ...) {
  standardGeneric("treat_outliers")
})

#' @rdname OutlierTreatment-class
#' @param object An object to print.
setMethod("show", "OutlierTreatment", function(object) {
  cat("<", class(object), "> ", object@label, "\n", sep = "")
  invisible(object)
})
