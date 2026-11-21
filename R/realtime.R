#' Real-time evaluation of a composite
#'
#' Replays the composite as it would have been computed at earlier dates.
#' For every origin \eqn{t} the data are cut at \eqn{t} (or replaced by the
#' data vintage of \eqn{t}, see below), the whole workflow is run again on
#' them, and the end-point value and direction of the composite cycle are
#' compared with those of the final composite `x` at the same date. This
#' answers the question the pseudo out-of-sample scores of [cli_validate()]
#' and [cli_selection_bias()] cannot: what would the analyst have seen at
#' the time? The two-sided filter revises the end of every cycle as data
#' come in; the revision of the end point is what the comparison measures.
#'
#' **Data at each origin.** When the [CandidateSet-class] holds a data
#' vintage named by the origin (`vintages` in [cli_candidates()], names
#' such as `"2015-06"`, `"2015Q2"` or decimal time), its values replace the
#' final data up to the origin, column by column (columns missing from the
#' vintage keep the final data); otherwise the final data are cut at the
#' origin (a growing window). The `source` column says which. A growing
#' window ignores data revisions and the publication lag, so it understates
#' the real-time revisions; with vintages, store each release under the
#' date at which it was available. The OECD prefers series "not subject to
#' significant revisions" and timely ones (Gyomai and Guidetti 2012, p. 5);
#' this function measures how much the composite moves when they are
#' revised.
#'
#' **Workflow at each origin.** With `reselect = TRUE` the selection of
#' [cli_build()] is run again with the specification of `x` (an expert
#' reference chronology in it is cut at the origin; see [expert_dater()]),
#' so the components can change, as in a periodic review of the OECD
#' (Gyomai, Ahmad and Astolfi 2017, p. 280). With `reselect = FALSE` the
#' components, inversions and shifts of `x` are kept and only the cycles
#' are extracted again, as in the regular monthly production.
#'
#' @param x A [CliResult-class] with a composite (the final run).
#' @param origins Origins: decimal times or periods such as `"2015-06"` or
#'   `"2015Q2"`, within the data of `x`.
#' @param reselect Run the selection again at each origin (`TRUE`), or keep
#'   the components of `x` (`FALSE`)?
#' @return A data frame with one row per origin: `origin`, `period`,
#'   `source` (`"vintage"` or `"window"`), `components` and
#'   `same_components` (the components of `x`?), `end` (the period of the
#'   last value of the real-time composite cycle), `cli_realtime` and
#'   `cli_final` (the normalised composite cycle at `end` in the real-time
#'   and final runs), `direction_realtime` and `direction_final` (sign of
#'   its last change, +1 or -1), `same_direction`, `ref_realtime` and
#'   `ref_final` (the reference cycle at its last real-time observation,
#'   as deviation from trend: in logarithms for a logged series) and
#'   `note` (why a row is empty). The
#'   attribute `"summary"` holds the number of origins compared, the
#'   correlation of real-time and final values of the composite and of the
#'   reference, and the share of origins with the same direction.
#' @references
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#'
#' Gyomai, G. and Guidetti, E. (2012). *OECD System of Composite Leading
#' Indicators*. OECD Publishing, Paris.
#' @seealso [cli_selection_bias()] for the pseudo out-of-sample check of the
#'   selection.
#' @examples
#' \donttest{
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' res <- cli_build(cli_candidates(sim, "reference", meta = meta))
#' rt <- cli_realtime(res, origins = c("2016-06", "2019-06"),
#'                    reselect = FALSE)
#' rt[, c("period", "cli_realtime", "cli_final", "same_direction")]
#' attr(rt, "summary")
#' }
#' @export
cli_realtime <- function(x, origins, reselect = TRUE) {
  if (!is(x, "CliResult") || is.null(x@composite)) {
    cyclind_abort("`x` must be a CliResult with a composite (see cli_build()).",
                  "cyclind_error_argument")
  }
  check_flag(reselect, "reselect")
  data <- x@candidates@data
  f <- stats::frequency(data)
  if (length(origins) == 0L) {
    cyclind_abort("`origins` must give at least one date.",
                  "cyclind_error_argument")
  }
  times <- sort(parse_period(as_vector(origins), f, "origins"))
  tm <- as.numeric(stats::time(data))
  off <- times < tm[1] - 1e-8 | times > tm[length(tm)] + 1e-8
  if (any(off)) {
    cyclind_abort(sprintf("The origin(s) %s lie outside the data (%s to %s).",
                          paste(fmt_time(times[off], f), collapse = ", "),
                          period_label(tm[1], f),
                          period_label(tm[length(tm)], f)),
                  "cyclind_error_argument")
  }
  rows <- lapply(times, function(t0) realtime_origin(x, t0, reselect))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "summary") <- realtime_summary(out)
  out
}

# Candidate set as available at `t0`: the vintage named `t0`, filled with
# the final data, or the final data cut at `t0`.
realtime_candidates <- function(candidates, t0) {
  f <- stats::frequency(candidates@data)
  cand <- window(candidates, end = t0)
  vint <- candidates@vintages
  if (length(vint) == 0L || is.null(names(vint))) {
    return(list(candidates = cand, source = "window"))
  }
  vt <- vapply(names(vint), function(nm) {
    tryCatch(parse_period(nm, f, "vintage names"),
             cyclind_error = function(e) NA_real_)
  }, numeric(1))
  hit <- which(!is.na(vt) & same_time(vt, t0))
  if (length(hit) == 0L) return(list(candidates = cand, source = "window"))
  v <- vint[[hit[1]]]
  dat <- cand@data
  for (nm in intersect(colnames(dat), colnames(v))) {
    col <- stats::window(v[, nm], start = stats::start(dat),
                         end = stats::end(dat), extend = TRUE)
    dat[, nm] <- as.numeric(col)
  }
  cand@data <- dat
  list(candidates = cand, source = "vintage")
}

realtime_origin <- function(x, t0, reselect) {
  f <- stats::frequency(x@candidates@data)
  final_comps <- x@composite@components
  row <- data.frame(origin = t0, period = fmt_time(t0, f), source = "",
                    components = "", same_components = NA, end = "",
                    cli_realtime = NA_real_, cli_final = NA_real_,
                    direction_realtime = NA_real_,
                    direction_final = NA_real_, same_direction = NA,
                    ref_realtime = NA_real_, ref_final = NA_real_,
                    note = "", stringsAsFactors = FALSE)
  rt <- realtime_candidates(x@candidates, t0)
  row$source <- rt$source
  spec <- update(x@spec, oos_cutoffs = numeric(0))
  run <- tryCatch(suppressWarnings(
    if (reselect) {
      res <- cli_build(rt$candidates, spec)
      list(composite = res@composite, cycles = res@cycles)
    } else {
      fixed_composite(x, rt$candidates, spec)
    }),
    cyclind_error = function(e) conditionMessage(e))
  if (is.character(run)) {
    row$note <- run
    return(row)
  }
  ref_rt <- run$cycles[[x@candidates@reference]]@cycle
  ref_fin <- x@cycles[[x@candidates@reference]]@cycle
  k <- last_valid(ref_rt)
  if (!is.na(k)) {
    row$ref_realtime <- as.numeric(ref_rt)[k]
    row$ref_final <- value_at(ref_fin, stats::time(ref_rt)[k])
  }
  if (is.null(run$composite)) {
    row$note <- "no leading candidate at the origin"
    return(row)
  }
  comps <- run$composite@components
  row$components <- paste(comps, collapse = ", ")
  row$same_components <- setequal(comps, final_comps)
  cyc <- run$composite@cycle
  k <- last_valid(cyc)
  te <- as.numeric(stats::time(cyc))[k]
  row$end <- period_label(te, f)
  row$cli_realtime <- as.numeric(cyc)[k]
  row$cli_final <- value_at(x@composite@cycle, te)
  row$direction_realtime <- sign(as.numeric(cyc)[k] - as.numeric(cyc)[k - 1L])
  row$direction_final <- sign(row$cli_final -
                                value_at(x@composite@cycle, te - 1 / f))
  row$same_direction <- row$direction_realtime == row$direction_final
  row
}

# The composite of `x` (its components, inversions and shifts) from the
# cycles of other data.
fixed_composite <- function(x, candidates, spec) {
  cycles <- cli_cycles(candidates, spec)
  for (nm in intersect(x@assessment@inverted, names(cycles))) {
    cycles[[nm]] <- invert_cycle(cycles[[nm]])
  }
  comps <- x@composite@components
  lost <- setdiff(comps, names(cycles))
  if (length(lost) > 0L) {
    cyclind_abort(sprintf("No cycle at the origin for: %s.",
                          paste(lost, collapse = ", ")),
                  "cyclind_error_short_series")
  }
  shifts <- x@composite@shifts
  comp <- cli_composite(cycles, comps, spec, cycles[[x@candidates@reference]],
                        shifts = if (length(shifts) > 0L) shifts)
  list(composite = comp, cycles = cycles)
}

last_valid <- function(x) {
  ok <- which(!is.na(as.numeric(x)))
  if (length(ok) < 2L) NA_integer_ else ok[length(ok)]
}

value_at <- function(x, tm) {
  i <- time_index(tm, x)
  if (is.na(i)) NA_real_ else as.numeric(x)[i]
}

realtime_summary <- function(out) {
  ok <- !is.na(out$cli_realtime) & !is.na(out$cli_final)
  ok_ref <- !is.na(out$ref_realtime) & !is.na(out$ref_final)
  corr <- function(a, b) {
    if (length(a) < 3L || stats::sd(a) == 0 || stats::sd(b) == 0) {
      return(NA_real_)
    }
    stats::cor(a, b)
  }
  data.frame(
    n_origins = nrow(out), n_compared = sum(ok),
    cor_composite = corr(out$cli_realtime[ok], out$cli_final[ok]),
    cor_reference = corr(out$ref_realtime[ok_ref], out$ref_final[ok_ref]),
    same_direction = if (any(ok)) mean(out$same_direction[ok]) else NA_real_,
    same_components = if (any(ok)) mean(out$same_components[ok]) else
      NA_real_
  )
}
