#' Moving-holiday effect
#'
#' @slot name Short name of the holiday.
#' @slot dates Dates of the holiday.
#' @slot windows Named list of day windows around the holiday.
#' @slot center Centring of the regressors.
#' @seealso [holiday_effect()]
#' @exportClass HolidayEffect
setClass("HolidayEffect", representation(
  name = "character", dates = "Date", windows = "list", center = "character"
))

#' Moving-holiday effects for seasonal adjustment
#'
#' A moving holiday falls in a different month from year to year, so fixed
#' monthly seasonal factors cannot remove it. `holiday_effect()` describes
#' such a holiday by its dates and by one or more windows of days around it;
#' each window becomes one regressor of the regARIMA model of
#' X-13ARIMA-SEATS (built with [seasonal::genhol()]), and the estimated
#' effect is removed from the seasonally adjusted series.
#'
#' Holiday effects are optional: [cli_spec()] and [oecd_spec()] use none by
#' default (`holidays = list()`), and they apply only with
#' `seasonal_adjust = TRUE`. Pass the effects through `holidays`. Any moving
#' holiday can be described, such as Easter, Chinese New Year (Imlek),
#' Diwali or Eid al-Adha, from dates you supply; [idulfitri_dates()] is a
#' helper for Eid al-Fitr (Idulfitri), the holiday that matters most for
#' Indonesian data.
#'
#' **Horizon.** X-13ARIMA-SEATS extends the series with three years of
#' forecasts, and the holiday regressors must cover them: give dates from
#' the first year of the data to at least three years past its last
#' observation. When the dates stop earlier, the series is adjusted with
#' X-11 without forecast extension and [cli_cycles()] warns
#' (`cyclind_warning_holiday_horizon`) once, naming the series. Dates that
#' do not cover the data themselves are an error.
#'
#' **Country-specific dates and windows.** The date and the effect of a
#' holiday depend on the country. The official date of 1 Syawal is set by
#' each country: in Indonesia by the Ministry of Religious Affairs
#' (Kementerian Agama) after the *sidang isbat*; in Malaysia, Saudi Arabia
#' and elsewhere by their own authorities, by moon sighting (*rukyat*) or
#' by calculation (*hisab*). Official dates can therefore differ from the
#' arithmetic calendar of [idulfitri_dates()], and from one country to
#' another, by one or two days; use the official dates of the country of
#' the data when you have them. The default windows describe Idulfitri in
#' Indonesia: demand rises in the two weeks before the holiday, when
#' households travel home (*mudik*), and production falls during the week
#' of collective leave (*cuti bersama*) that starts on it. Other countries
#' and holidays need their own windows, set through `windows`. Centring
#' with `"mean"` follows the advice of the 'seasonal' package for
#' Ramadan-type holidays.
#'
#' @param dates Dates of the holiday (class `Date`), covering every year of
#'   the data and at least three years past its end (the X-13 forecast
#'   horizon).
#' @param name Short name, used for the regressors (`<name>_<window>`).
#' @param windows Named list of windows, each `c(first, last)` in days
#'   relative to the holiday (negative = before). The default suits
#'   Idulfitri in Indonesia (see Details).
#' @param center Centring of the regressors, passed to [seasonal::genhol()].
#' @return A `HolidayEffect` object.
#' @references
#' Lestari, E., Widiharih, T. and Rahmawati, R. (2018). Peramalan ekspor
#' nonmigas dengan variasi kalender Islam menggunakan X-13-ARIMA-SEATS.
#' *Jurnal Gaussian*, 7(3), 236--247.
#' @seealso [idulfitri_dates()], [cli_spec()]
#' @examples
#' # Idulfitri, data from 1990 to 2027: dates up to 2030
#' eid <- holiday_effect(idulfitri_dates(1990, 2030), name = "idulfitri")
#' eid
#' # a country's own official dates and windows: replace the dates below
#' # (the arithmetic ones, for illustration) by the official ones, e.g. as
#' # Date strings read from the authority's announcements
#' own <- idulfitri_dates(2010, 2030)
#' eid_own <- holiday_effect(own, name = "eid",
#'                           windows = list(before = c(-7, -1),
#'                                          after = c(0, 2)))
#' eid_own
#' \donttest{
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   sim <- simulate_cycles(seed = 1)
#'   meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#'   spec <- cli_spec(seasonal_adjust = TRUE, holidays = eid)
#'   cyc <- cli_cycles(cli_candidates(sim, "reference", meta = meta), spec)
#'   cyc$lead_6
#' }
#' }
#' @export
holiday_effect <- function(dates, name = "holiday",
                           windows = list(before = c(-14, -1),
                                          after = c(0, 6)),
                           center = c("mean", "calendar", "none")) {
  center <- match.arg(center)
  if (!inherits(dates, "Date") || length(dates) < 2L || anyNA(dates)) {
    cyclind_abort("`dates` must be a vector of at least two Dates.",
                  "cyclind_error_argument")
  }
  if (!is.character(name) || length(name) != 1L ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    cyclind_abort("`name` must be one word of letters, digits and _.",
                  "cyclind_error_argument")
  }
  ok <- is.list(windows) && length(windows) > 0L &&
    !is.null(names(windows)) && all(nzchar(names(windows))) &&
    all(vapply(windows, function(w) {
      is.numeric(w) && length(w) == 2L && !anyNA(w) && w[1] <= w[2]
    }, logical(1)))
  if (!ok) {
    cyclind_abort("`windows` must be a named list of c(first, last) day pairs.",
                  "cyclind_error_argument")
  }
  new("HolidayEffect", name = name, dates = sort(unique(dates)),
      windows = lapply(windows, as.numeric), center = center)
}

#' Approximate dates of Idulfitri
#'
#' Dates of 1 Syawal (Eid al-Fitr, Idulfitri) from the arithmetic (tabular)
#' Islamic calendar, a helper for [holiday_effect()] with Indonesian data.
#' Holiday effects are optional and apply only with
#' `seasonal_adjust = TRUE` (see [cli_spec()]).
#'
#' The official date is set per country: in Indonesia by the Ministry of
#' Religious Affairs (Kementerian Agama) after the *sidang isbat*; in
#' Malaysia, Saudi Arabia and elsewhere by their own authorities, by moon
#' sighting (*rukyat*) or by calculation (*hisab*). It can differ from these
#' dates, and between countries, by one or two days. For monthly or
#' quarterly holiday regressors such a difference rarely matters, but the
#' official dates of the country of the data should be used when they are
#' at hand (pass them to [holiday_effect()] as `Date`s).
#'
#' X-13ARIMA-SEATS needs the holiday regressors over its forecast horizon,
#' three years past the last observation: choose `to` at least three years
#' after the last year of the data (for data ending in 2026, `to = 2029`).
#' The default years cover data from 1980 to 2037.
#'
#' @param from,to First and last Gregorian year.
#' @return A sorted vector of class `Date`.
#' @seealso [holiday_effect()]
#' @examples
#' idulfitri_dates(2020, 2026)
#' # quarterly data from 2008 to 2026Q3 need dates up to 2029
#' range(idulfitri_dates(2008, 2029))
#' @export
idulfitri_dates <- function(from = 1980, to = 2040) {
  check_number(from, "from", lower = 1, upper = 9999, integer = TRUE)
  check_number(to, "to", lower = from, upper = 9999, integer = TRUE)
  hijri <- seq(floor((from - 622) * 33 / 32) - 1, ceiling((to - 622) * 33 /
                                                             32) + 1)
  # Julian day of 1 Shawwal (month 10) of each Hijri year, tabular calendar
  jd <- 1 + ceiling(29.5 * 9) + (hijri - 1) * 354 +
    floor((3 + 11 * hijri) / 30) + 1948439.5 - 1
  d <- as.Date(jd - 2440587.5, origin = "1970-01-01")
  yr <- as.integer(format(d, "%Y"))
  sort(d[yr >= from & yr <= to])
}

# Holiday regressors for one series, or NULL without holidays.
holiday_regressors <- function(x, holidays) {
  if (length(holidays) == 0L) return(NULL)
  f <- stats::frequency(x)
  regs <- list()
  for (h in holidays) {
    for (w in names(h@windows)) {
      r <- seasonal::genhol(h@dates, start = h@windows[[w]][1],
                            end = h@windows[[w]][2], frequency = f,
                            center = h@center)
      regs[[paste(h@name, w, sep = "_")]] <- r
    }
  }
  reg <- do.call(stats::ts.union, regs)
  if (is.null(dim(reg))) reg <- stats::ts(matrix(reg, ncol = 1),
                                          start = stats::start(reg),
                                          frequency = f)
  colnames(reg) <- names(regs)
  if (stats::time(reg)[1] > stats::time(x)[1] + 1e-8 ||
      stats::time(reg)[nrow(reg)] < stats::time(x)[length(x)] - 1e-8) {
    cyclind_abort("The holiday dates do not cover the whole span of the data.",
                  "cyclind_error_argument")
  }
  reg
}

#' @rdname holiday_effect
#' @param object A `HolidayEffect`.
#' @export
setMethod("show", "HolidayEffect", function(object) {
  d <- object@dates
  cat("<HolidayEffect> '", object@name, "', ", length(d), " dates from ",
      format(min(d)), " to ", format(max(d)), "\n", sep = "")
  for (w in names(object@windows)) {
    v <- object@windows[[w]]
    cat(sprintf("  %s: days %+d to %+d\n", w, as.integer(v[1]),
                as.integer(v[2])))
  }
  invisible(object)
})
