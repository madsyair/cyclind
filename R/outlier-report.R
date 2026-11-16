#' Outliers found in each series
#'
#' Tabulates, series by series, the outliers that the outlier treatment of
#' the specification found (see [OutlierTreatment-class]): how many were
#' removed and how many detected but kept, the share of the observations
#' removed, the periods and types, and the warnings recorded while the
#' cycle was extracted (a failed X-13 model, a share of outliers above
#' `max_share`). The individual outliers are in the `outliers` slot of each
#' [CycleSeries-class].
#'
#' @param x A [CliResult-class], a named list of [CycleSeries-class] (from
#'   [cli_cycles()]) or one [CycleSeries-class].
#' @return A data frame with one row per series: `name`, `n_removed`,
#'   `n_kept`, `share` (removed outliers per observation), `periods` (type
#'   and period of each outlier, kept ones in brackets), `types` (counts by
#'   type) and `note` (warnings, empty when there were none).
#' @examples
#' cand <- cli_candidates(simulate_cycles(seed = 1), "reference")
#' outlier_report(cli_cycles(cand, cli_spec(12)))
#' @export
outlier_report <- function(x) {
  if (is(x, "CliResult")) x <- x@cycles
  if (is(x, "CycleSeries")) x <- stats::setNames(list(x), x@name)
  if (!is.list(x) || length(x) == 0L ||
      !all(vapply(x, is, logical(1), "CycleSeries"))) {
    cyclind_abort(paste("`x` must be a CliResult, a CycleSeries or a list",
                        "of them."), "cyclind_error_argument")
  }
  rows <- lapply(x, function(cs) {
    ol <- series_outliers(cs)
    n_obs <- sum(!is.na(cs@transformed))
    lab <- ifelse(ol$removed, paste(ol$type, ol$period),
                  paste0("(", ol$type, " ", ol$period, ")"))
    tps <- table(factor(ol$type, c("AO", "TC", "LS")))
    data.frame(
      name = cs@name, n_removed = sum(ol$removed),
      n_kept = sum(!ol$removed),
      share = if (n_obs > 0L) sum(ol$removed) / n_obs else NA_real_,
      periods = paste(lab, collapse = ", "),
      types = paste(names(tps)[tps > 0], tps[tps > 0], collapse = ", "),
      note = paste(series_notes(cs), collapse = " "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Outliers and notes of a cycle (objects saved before 0.1.0.9000 have
# neither).
series_outliers <- function(cs) {
  if (methods::.hasSlot(cs, "outliers")) cs@outliers else empty_outliers()
}

series_notes <- function(cs) {
  if (methods::.hasSlot(cs, "notes")) cs@notes else character(0)
}
