# Expert judgement: reference chronologies, edits of turning points and the
# log of expert decisions (ADR-014).

# ---- classes ---------------------------------------------------------------

#' @rdname expert_dater
#' @slot base The [Dater-class] of the other series.
#' @slot chronology Data frame of the expert turning points: `time`
#'   (decimal) and `type`.
#' @slot series Names of the series dated by the chronology.
#' @slot log Decisions behind the chronology (see [expert_log()]).
#' @exportClass ExpertDater
setClass("ExpertDater", contains = "Dater",
         representation(base = "Dater", chronology = "data.frame",
                        series = "character", log = "data.frame"))

#' A chronology edited by an expert
#'
#' A [TurningPoints-class] whose turning points were edited with
#' [edit_turning_points()]; the slot `log` holds the edits.
#'
#' @slot log Data frame of the edits (see [expert_log()]).
#' @seealso [edit_turning_points()]
#' @exportClass ExpertChronology
setClass("ExpertChronology", contains = "TurningPoints",
         representation(log = "data.frame"))

# ---- periods ---------------------------------------------------------------

# Decimal time of periods written as numbers (decimal time), Dates, or text
# "2008-05", "2008M05", "2008-5", "2008Q2", "2008-Q2". `freq` is needed for
# Dates and checked against the text; NA infers it from the text.
parse_period <- function(p, freq = NA, name = "period") {
  if (is.numeric(p)) {
    if (anyNA(p)) {
      cyclind_abort(sprintf("`%s` must not be missing.", name),
                    "cyclind_error_argument")
    }
    return(as.numeric(p))
  }
  if (inherits(p, "Date")) {
    if (is.na(freq)) freq <- 12
    lt <- as.POSIXlt(p)
    sub <- if (freq == 12) lt$mon + 1L else lt$mon %/% 3L + 1L
    return(lt$year + 1900 + (sub - 1) / freq)
  }
  p <- toupper(trimws(as.character(p)))
  quarterly <- grepl("^[0-9]{4}-?Q[1-4]$", p)
  monthly <- grepl("^[0-9]{4}(-|M)(0?[1-9]|1[0-2])$", p)
  # decimal times written as text (a vector that mixes periods and times)
  num <- suppressWarnings(as.numeric(p))
  decimal <- !(quarterly | monthly) & !is.na(num) & grepl("\\.", p)
  bad <- !(quarterly | monthly | decimal)
  if (any(bad)) {
    cyclind_abort(sprintf(paste("Cannot read the %s %s. Write periods as",
                                "\"2008-05\" (monthly), \"2008Q2\"",
                                "(quarterly) or decimal time."), name,
                          paste(sprintf("'%s'", p[bad]), collapse = ", ")),
                  "cyclind_error_argument")
  }
  f <- ifelse(quarterly, 4, 12)
  f[decimal] <- if (is.na(freq)) 12 else freq
  if (!is.na(freq) && any(f != freq)) {
    cyclind_abort(sprintf("The %s %s do not match the frequency %s.", name,
                          paste(sprintf("'%s'", p[f != freq]),
                                collapse = ", "), freq),
                  "cyclind_error_frequency")
  }
  year <- as.integer(substr(p, 1, 4))
  sub <- suppressWarnings(as.integer(sub("^[0-9]{4}-?[QM-]", "", p)))
  out <- year + (sub - 1) / f
  out[decimal] <- num[decimal]
  out
}

# Frequency of a set of period labels (NA when they are numbers).
period_frequency <- function(p) {
  if (!is.character(p) && !is.factor(p)) return(NA_real_)
  if (all(grepl("Q", toupper(p)))) 4 else 12
}

# Position of decimal times in a ts (NA outside it).
time_index <- function(tm, x) {
  tsp <- stats::tsp(x)
  i <- round((tm - tsp[1]) * tsp[3]) + 1
  i[i < 1 | i > length(x)] <- NA
  as.integer(i)
}

same_time <- function(a, b) abs(a - b) < 1e-6

# ---- decision log ----------------------------------------------------------

#' Log of expert decisions
#'
#' Expert judgement enters cyclind at three points, as in the OECD system:
#' the chronology of reference turning points (the dating algorithm "does
#' not always locate the most plausible turning points ... expert judgment
#' complements the identification of turning points", Gyomai, Ahmad and
#' Astolfi 2017, p. 285; Fulop and Gyomai 2012, p. 3), the treatment of
#' outliers ("it is based on expert judgement whether level shifts are
#' eliminated or not", Gyomai, Ahmad and Astolfi 2017, p. 283) and the
#' choice of components. Every such decision is recorded with its reason so
#' that a composite can be traced to the judgements behind it.
#'
#' `expert_log()` returns the decisions recorded in an object:
#'
#' * a [CliResult-class] or [CciResult-class]: the reference chronology
#'   given to [cli_build()] (`reference_tp`), its edits
#'   ([edit_turning_points()]), the outlier decisions of
#'   [tramo_outliers()] (`decisions`) with whether each matched an outlier
#'   found in the data, and a manual composite ([cli_composite()] with
#'   `reason`);
#' * an [ExpertChronology-class]: the edits of [edit_turning_points()];
#' * a [CliSpec-class]: the reference chronology of an [expert_dater()] and
#'   the outlier decisions, before they meet the data;
#' * a [CompositeIndex-class]: the manual composite.
#'
#' The log is a data frame with one row per decision: `step`
#' (`"reference turning points"`, `"outliers"` or `"composite"`),
#' `object` (the series or composite concerned), `decision`, `status`
#' (`"applied"`, `"not matched"` for an outlier decision that matched no
#' outlier found in the data, or `"pending"` before the data are seen),
#' `reason`, `who` (optional, `NA` when not given) and `when` (time stamp of
#' the decision, ISO 8601). The [show()] and `summary()` methods of a
#' result, the quality scorecard ([cli_quality()]) and the script exported
#' by [cyclind_app()] report it.
#'
#' @param x An object of cyclind (see above).
#' @param ... Unused.
#' @return A data frame of decisions (no rows when there are none).
#' @references
#' Fulop, G. and Gyomai, G. (2012). *Transition of the OECD CLI system to a
#' GDP-based business cycle target*. OECD background note, Paris.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#' @seealso [edit_turning_points()], [expert_dater()], [tramo_outliers()]
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cand <- cli_candidates(sim, "reference", meta = meta)
#' spec <- cli_spec(freq = 12)
#' cyc <- cli_cycles(cand, spec)
#' tp <- date_turning_points(spec@dater, cyc$reference@normalised,
#'                           name = "reference")
#' # the peak of 2019-09 lies below the trend: a minor cycle
#' tp2 <- edit_turning_points(tp, drop = c("2019-02", "2019-09"),
#'                            reason = "minor cycle below trend")
#' expert_log(tp2)
#' @export
setGeneric("expert_log", function(x, ...) standardGeneric("expert_log"))

decision_rows <- function(step, object, decision, reason, who = NA,
                          when = NULL, status = "applied") {
  n <- length(decision)
  if (n == 0L) return(empty_decision_log())
  if (is.null(when)) when <- decision_time()
  out <- data.frame(step = rep_len(step, n), object = rep_len(object, n),
                    decision = decision, status = rep_len(status, n),
                    reason = rep_len(as.character(reason), n),
                    who = rep_len(as.character(who), n),
                    when = rep_len(as.character(when), n),
                    stringsAsFactors = FALSE)
  out
}

decision_time <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Reasons are compulsory: one per change, or one for all.
check_reasons <- function(reason, n, what) {
  reason <- rep_len(as.character(reason), n)
  if (n > 0L && (length(reason) == 0L || anyNA(reason) ||
                 any(!nzchar(trimws(reason))))) {
    cyclind_abort(sprintf(paste("Every %s needs a reason: give `reason`, or",
                                "a `reason` column."), what),
                  "cyclind_error_reason")
  }
  reason
}

check_who <- function(who) {
  if (is.null(who)) return(NA_character_)
  if (!is.character(who) || length(who) != 1L) {
    cyclind_abort("`who` must be one string (or NULL).",
                  "cyclind_error_argument")
  }
  who
}

# ---- chronologies ----------------------------------------------------------

# A chronology as a data frame of `time`, `type` (sorted), from a
# TurningPoints object or a data frame with `period` or `time` and `type`.
as_chronology <- function(x, freq = NA, name = "reference_tp") {
  if (is(x, "TurningPoints")) {
    tm <- as.numeric(stats::time(x@series))[x@index]
    out <- data.frame(time = tm, type = x@type, stringsAsFactors = FALSE)
  } else if (is.data.frame(x)) {
    has_time <- "time" %in% names(x)
    if (!(has_time || "period" %in% names(x)) || !"type" %in% names(x)) {
      cyclind_abort(sprintf(paste("`%s` must be a TurningPoints object or a",
                                  "data frame with columns `period` (or",
                                  "`time`) and `type`."), name),
                    "cyclind_error_argument")
    }
    tm <- if (has_time) x$time else x$period
    if (is.factor(tm)) tm <- as.character(tm)
    out <- data.frame(time = parse_period(tm, freq, "periods"),
                      type = tolower(trimws(as.character(x$type))),
                      stringsAsFactors = FALSE)
  } else {
    cyclind_abort(sprintf(paste("`%s` must be a TurningPoints object or a",
                                "data frame with columns `period` (or",
                                "`time`) and `type`."), name),
                  "cyclind_error_argument")
  }
  check_chronology(out, name, if (is.na(freq)) 12 else freq)
}

check_chronology <- function(chron, name = "reference_tp", f = 12) {
  if (!all(chron$type %in% c("peak", "trough"))) {
    cyclind_abort(sprintf("`%s$type` must be \"peak\" or \"trough\".", name),
                  "cyclind_error_turning_points")
  }
  chron <- chron[order(chron$time), , drop = FALSE]
  rownames(chron) <- NULL
  if (anyDuplicated(round(chron$time * 1e6)) > 0L) {
    cyclind_abort(sprintf("`%s` has two turning points on the same date.",
                          name), "cyclind_error_turning_points")
  }
  n <- nrow(chron)
  if (n > 1L) {
    same <- which(chron$type[-1] == chron$type[-n])
    if (length(same) > 0L) {
      cyclind_abort(sprintf(paste("Peaks and troughs of `%s` must alternate;",
                                  "two %ss follow each other at %s and %s."),
                            name, chron$type[same[1]],
                            fmt_time(chron$time[same[1]], f),
                            fmt_time(chron$time[same[1] + 1L], f)),
                    "cyclind_error_turning_points")
    }
  }
  chron
}

fmt_time <- function(tm, freq) {
  if (all(abs(tm * freq - round(tm * freq)) < 1e-6)) {
    period_label(tm, freq)
  } else {
    format(tm)
  }
}

# Turning points of `x` from a chronology: positions within the non-missing
# span of `x` (a truncated series keeps the turning points it covers).
chronology_turning_points <- function(chron, x, name, params = list()) {
  idx <- time_index(chron$time, x)
  span <- non_na_span(x)
  keep <- !is.na(idx) & !anyNA(span)
  if (!anyNA(span)) keep <- keep & idx >= span[1] & idx <= span[2]
  new("TurningPoints", name = name, series = x, index = idx[keep],
      type = chron$type[keep], params = c(params, list(source = "expert")))
}

# Error when a chronology lies outside the data of the reference.
check_chronology_range <- function(chron, x, name = "reference_tp") {
  span <- non_na_span(x)
  tm <- as.numeric(stats::time(x))
  idx <- time_index(chron$time, x)
  off <- is.na(idx) | anyNA(span)
  if (!anyNA(span)) off <- off | idx < span[1] | idx > span[2]
  if (any(off)) {
    f <- stats::frequency(x)
    cyclind_abort(sprintf(paste("The turning point(s) %s of `%s` lie outside",
                                "the data of the reference (%s to %s)."),
                          paste(fmt_time(chron$time[off], f),
                                collapse = ", "), name,
                          period_label(tm[span[1]], f),
                          period_label(tm[span[2]], f)),
                  "cyclind_error_turning_points")
  }
  if (nrow(chron) == 0L) {
    cyclind_abort(sprintf("`%s` has no turning point.", name),
                  "cyclind_error_turning_points")
  }
  invisible(chron)
}

# ---- expert dater ----------------------------------------------------------

#' A dater that uses an expert chronology
#'
#' `expert_dater()` dates the series named in `series` (the reference) with
#' a chronology of turning points fixed by an expert, and every other series
#' with the `base` dater. It is the strategy behind the `reference_tp`
#' argument of [cli_build()], [cli_assess()] and [cci_build()], which put
#' it into the specification; it can also be given to [cli_spec()] or
#' [oecd_spec()] directly (`dater =`). Every step that dates the reference,
#' the classification, the validation, the probability scores and the
#' selection-bias check, then uses the expert chronology.
#'
#' The OECD dates turning points with a simplified Bry-Boschan routine but
#' notes that it "does not always locate the most plausible turning points"
#' and that "expert judgment complements the identification of turning
#' points" (Gyomai, Ahmad and Astolfi 2017, p. 285); the censoring rules
#' "are set by rules of thumb", and a good result is "closer to an
#' economist's assessment of where the major turning-points are" (Fulop and
#' Gyomai 2012, p. 3). An expert chronology records that assessment.
#'
#' The chronology must alternate peaks and troughs. When a series is
#' shorter than the chronology (a truncated window, as in
#' [cli_selection_bias()] or [cli_realtime()]), the turning points outside
#' its non-missing span are left out; no end exclusion is applied to the
#' expert turning points, so in truncated windows they are known earlier
#' than an algorithm could date them. [cli_build()] stops when a turning
#' point lies outside the data of the reference.
#'
#' @param chronology A [TurningPoints-class] (for example from
#'   [edit_turning_points()]) or a data frame with columns `period` (such
#'   as `"2008-05"` or `"2008Q2"`) or `time` (decimal time), and `type`
#'   (`"peak"` or `"trough"`).
#' @param series Names of the series dated with the chronology (the
#'   reference).
#' @param base The [Dater-class] of the other series; by default
#'   [bry_boschan()].
#' @return An `ExpertDater` object (a [Dater-class]).
#' @references
#' Fulop, G. and Gyomai, G. (2012). *Transition of the OECD CLI system to a
#' GDP-based business cycle target*. OECD background note, Paris.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#' @seealso [edit_turning_points()], [expert_log()]
#' @examples
#' chron <- data.frame(period = c("2001-02", "2002-06", "2008-05"),
#'                     type = c("peak", "trough", "peak"))
#' expert_dater(chron, series = "reference", base = oecd_dater())
#' @export
expert_dater <- function(chronology, series, base = bry_boschan()) {
  if (!is.character(series) || length(series) == 0L || anyNA(series)) {
    cyclind_abort("`series` must name the series to date by the chronology.",
                  "cyclind_error_argument")
  }
  check_strategy(base, "Dater", "base")
  if (is(base, "ExpertDater")) base <- base@base
  chron <- as_chronology(chronology, name = "chronology")
  log <- chronology_log(chronology, chron, series[1])
  new("ExpertDater", label = sprintf("expert chronology for %s; others: %s",
                                     paste(series, collapse = ", "),
                                     base@label),
      window = base@window, min_phase = base@min_phase,
      min_cycle = base@min_cycle, end_exclusion = base@end_exclusion,
      phase_rule = dater_phase_rule(base), base = base, chronology = chron,
      series = series, log = log)
}

# Decisions behind a chronology: the edits of an ExpertChronology, or one
# row saying that a chronology was supplied.
chronology_log <- function(x, chron, object) {
  if (is(x, "ExpertChronology")) {
    lg <- x@log
    lg$object <- rep_len(object, nrow(lg))
    return(lg)
  }
  reason <- NA_character_
  who <- NA_character_
  if (is.data.frame(x)) {
    if ("reason" %in% names(x)) {
      r <- unique(stats::na.omit(as.character(x$reason)))
      r <- r[nzchar(r)]
      if (length(r) > 0L) reason <- paste(r, collapse = "; ")
    }
    if ("who" %in% names(x)) {
      w <- unique(stats::na.omit(as.character(x$who)))
      if (length(w) > 0L) who <- paste(w, collapse = ", ")
    }
  }
  decision_rows("reference turning points", object,
                sprintf("chronology supplied: %d peak(s), %d trough(s)",
                        sum(chron$type == "peak"),
                        sum(chron$type == "trough")),
                reason, who)
}

#' @rdname date_turning_points
setMethod("date_turning_points", "ExpertDater",
          function(dater, x, name = "", ...) {
  if (!name %in% dater@series) {
    return(date_turning_points(dater@base, x, name = name))
  }
  chronology_turning_points(dater@chronology, x, name,
                            resolve_dater(dater@base, stats::frequency(x)))
})

setMethod("dater_defaults", "ExpertDater", function(dater, freq) {
  dater_defaults(dater@base, freq)
})

#' @rdname expert_dater
#' @param object An `ExpertDater` or `ExpertChronology` to print.
#' @export
setMethod("show", "ExpertDater", function(object) {
  ch <- object@chronology
  cat("<ExpertDater> expert chronology for ",
      paste(object@series, collapse = ", "), ": ", sum(ch$type == "peak"),
      " peak(s), ", sum(ch$type == "trough"), " trough(s)\n", sep = "")
  cat("  other series: ")
  show(object@base)
  invisible(object)
})

#' @rdname expert_log
setMethod("expert_log", "ExpertDater", function(x, ...) x@log)

# The specification with the expert chronology of the reference.
expert_reference_spec <- function(spec, reference_tp, candidates) {
  if (is.null(reference_tp)) return(spec)
  ref <- candidates@reference
  base <- spec@dater
  chron <- as_chronology(reference_tp, spec@freq)
  check_chronology_range(chron, candidates@data[, ref])
  spec@dater <- expert_dater(reference_tp, series = ref, base = base)
  spec
}

# ---- editing a chronology ---------------------------------------------------

#' Edit a chronology of turning points
#'
#' Adds, drops or moves turning points of a dated series, typically the
#' reference chronology of the dating algorithm, so that it matches an
#' expert assessment (Gyomai, Ahmad and Astolfi 2017, p. 285; Fulop and
#' Gyomai 2012, p. 3; see [expert_dater()]). Every change needs a reason,
#' and every change is recorded in the log of the result
#' ([expert_log()]). Edits can be chained: editing an
#' `ExpertChronology` keeps its log.
#'
#' Changes are applied in the order drop, move, add. The result must
#' alternate peaks and troughs, and every turning point must lie within the
#' dated series; otherwise the edit stops with an error of class
#' `cyclind_error_turning_points` that names the offending dates. A change
#' without a reason stops with an error of class `cyclind_error_reason`.
#'
#' @param tp A [TurningPoints-class], for example
#'   `turning_points(res)$reference` or the result of
#'   [date_turning_points()], or an `ExpertChronology`.
#' @param add Turning points to add: a data frame with `period` (or
#'   `time`), `type` and, optionally, `reason`.
#' @param drop Turning points to drop: periods (such as `"2019-09"`),
#'   decimal times, or a data frame with `period` (or `time`) and,
#'   optionally, `reason`.
#' @param move Turning points to move: a data frame with `from`, `to`
#'   (periods or decimal times) and, optionally, `reason`. The type is
#'   kept.
#' @param reason Reason for the changes without a `reason` of their own:
#'   one string for all, or one per change in the order drop, move, add.
#' @param who Optional name of the person who decided.
#' @return An `ExpertChronology`: a [TurningPoints-class] with the log of
#'   its edits (see [expert_log()]).
#' @references
#' Fulop, G. and Gyomai, G. (2012). *Transition of the OECD CLI system to a
#' GDP-based business cycle target*. OECD background note, Paris.
#'
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#' @seealso [cli_build()] (`reference_tp`), [expert_log()]
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' x <- cli_cycles(cli_candidates(sim, "reference"), cli_spec())$reference
#' tp <- date_turning_points(bry_boschan(), x@normalised, name = "reference")
#' tp2 <- edit_turning_points(
#'   tp, drop = c("2019-02", "2019-09"),
#'   move = data.frame(from = "2008-05", to = "2008-03"),
#'   reason = c("minor cycle below trend", "minor cycle below trend",
#'              "peak of activity in the national accounts"),
#'   who = "analyst A")
#' tp2
#' expert_log(tp2)
#' @export
edit_turning_points <- function(tp, add = NULL, drop = NULL, move = NULL,
                                reason = NULL, who = NULL) {
  if (!is(tp, "TurningPoints")) {
    cyclind_abort("`tp` must be a TurningPoints object.",
                  "cyclind_error_argument")
  }
  who <- check_who(who)
  x <- tp@series
  f <- stats::frequency(x)
  chron <- as_chronology(tp, f, name = "tp")
  drop <- edit_frame(drop, c("period", "time"), "drop", f)
  move <- edit_frame(move, c("from", "to"), "move", f)
  add <- edit_frame(add, c("period", "time"), "add", f)
  n <- c(nrow(drop), nrow(move), nrow(add))
  if (sum(n) == 0L) {
    cyclind_abort("Give at least one change in `add`, `drop` or `move`.",
                  "cyclind_error_argument")
  }
  given <- c(drop$reason, move$reason, add$reason)
  default <- if (is.null(reason)) NA_character_ else reason
  if (length(default) > 1L && length(default) != sum(n)) {
    cyclind_abort(sprintf(paste("`reason` must be one string or one per",
                                "change (%d)."), sum(n)),
                  "cyclind_error_reason")
  }
  default <- rep_len(default, sum(n))
  given[is.na(given) | !nzchar(trimws(given))] <-
    default[is.na(given) | !nzchar(trimws(given))]
  reasons <- check_reasons(given, sum(n), "change of a turning point")
  decisions <- character(0)
  for (k in seq_len(nrow(drop))) {
    hit <- which(same_time(chron$time, drop$time[k]))
    if (length(hit) == 0L) abort_no_turning_point(drop$time[k], f, "drop")
    decisions <- c(decisions, sprintf("drop %s %s", chron$type[hit],
                                      fmt_time(chron$time[hit], f)))
    chron <- chron[-hit, , drop = FALSE]
  }
  for (k in seq_len(nrow(move))) {
    hit <- which(same_time(chron$time, move$from[k]))
    if (length(hit) == 0L) abort_no_turning_point(move$from[k], f, "move")
    decisions <- c(decisions, sprintf("move %s %s to %s", chron$type[hit],
                                      fmt_time(move$from[k], f),
                                      fmt_time(move$to[k], f)))
    chron$time[hit] <- move$to[k]
  }
  if (nrow(add) > 0L) {
    type <- tolower(trimws(as.character(add$type)))
    chron <- rbind(chron, data.frame(time = add$time, type = type,
                                     stringsAsFactors = FALSE))
    decisions <- c(decisions, sprintf("add %s %s", type,
                                      fmt_time(add$time, f)))
  }
  chron <- check_chronology(chron, "tp", f)
  check_chronology_range(chron, x, "tp")
  log <- decision_rows("reference turning points", tp@name, decisions,
                       reasons, who)
  if (is(tp, "ExpertChronology")) log <- rbind(tp@log, log)
  new("ExpertChronology", name = tp@name, series = x,
      index = time_index(chron$time, x), type = chron$type,
      params = c(tp@params, list(source = "expert")), log = log)
}

abort_no_turning_point <- function(tm, f, what) {
  cyclind_abort(sprintf("There is no turning point at %s to %s.",
                        fmt_time(tm, f), what),
                "cyclind_error_turning_points")
}

# The changes of edit_turning_points() as a data frame with decimal times
# and a `reason` column.
edit_frame <- function(x, cols, what, f) {
  if (is.null(x)) {
    out <- data.frame(time = numeric(0), reason = character(0))
    if (what == "move") out <- data.frame(from = numeric(0), to = numeric(0),
                                          reason = character(0))
    if (what == "add") out$type <- character(0)
    return(out)
  }
  if (!is.data.frame(x)) {
    if (what != "drop") {
      cyclind_abort(sprintf("`%s` must be a data frame.", what),
                    "cyclind_error_argument")
    }
    x <- data.frame(period = x, stringsAsFactors = FALSE)
    if (is.numeric(x$period)) names(x) <- "time"
  }
  reason <- if ("reason" %in% names(x)) as.character(x$reason) else
    rep(NA_character_, nrow(x))
  if (what == "move") {
    if (!all(c("from", "to") %in% names(x))) {
      cyclind_abort("`move` needs columns `from` and `to`.",
                    "cyclind_error_argument")
    }
    return(data.frame(from = parse_period(as_vector(x$from), f, "periods"),
                      to = parse_period(as_vector(x$to), f, "periods"),
                      reason = reason, stringsAsFactors = FALSE))
  }
  tm <- if ("time" %in% names(x)) x$time else x$period
  if (is.null(tm)) {
    cyclind_abort(sprintf("`%s` needs a column `period` or `time`.", what),
                  "cyclind_error_argument")
  }
  out <- data.frame(time = parse_period(as_vector(tm), f, "periods"),
                    reason = reason, stringsAsFactors = FALSE)
  if (what == "add") {
    if (!"type" %in% names(x)) {
      cyclind_abort("`add` needs a column `type` (\"peak\" or \"trough\").",
                    "cyclind_error_argument")
    }
    out$type <- as.character(x$type)
  }
  out
}

as_vector <- function(v) if (is.factor(v)) as.character(v) else v

#' @rdname expert_dater
#' @export
setMethod("show", "ExpertChronology", function(object) {
  methods::callNextMethod()
  cat(nrow(object@log), " expert decision(s); see expert_log()\n", sep = "")
  invisible(object)
})

#' @rdname expert_log
setMethod("expert_log", "ExpertChronology", function(x, ...) x@log)

#' @rdname expert_log
setMethod("expert_log", "CliSpec", function(x, ...) {
  out <- if (is(x@dater, "ExpertDater")) x@dater@log else
    empty_decision_log()
  out$status <- rep_len("applied", nrow(out))
  od <- outlier_decision_log(spec_outliers(x))
  rbind(out, od)
})

#' @rdname expert_log
setMethod("expert_log", "CompositeIndex", function(x, ...) {
  composite_decisions(x)
})

#' @rdname expert_log
setMethod("expert_log", "CliResult", function(x, ...) {
  out <- if (methods::.hasSlot(x, "decisions")) x@decisions else
    empty_decision_log()
  if (!is.null(x@composite)) out <- rbind(out, composite_decisions(x@composite))
  rownames(out) <- NULL
  out
})

composite_decisions <- function(x) {
  if (methods::.hasSlot(x, "decisions") && nrow(x@decisions) > 0L) {
    x@decisions
  } else {
    empty_decision_log()
  }
}

# Decisions of a finished run: the reference chronology and the outlier
# decisions, with whether each matched an outlier found in the data.
result_decisions <- function(spec, cycles) {
  out <- if (is(spec@dater, "ExpertDater")) spec@dater@log else
    empty_decision_log()
  od <- outlier_decision_log(spec_outliers(spec), cycles)
  out <- rbind(out, od)
  rownames(out) <- NULL
  out
}
