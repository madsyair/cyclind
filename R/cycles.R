#' Extract the cycles of all series
#'
#' Applies, series by series on its own non-missing span: interpolation of
#' internal gaps, optional seasonal adjustment of the original series with
#' X-13ARIMA-SEATS in the scale of the transformation (see
#' `seasonal_adjust` in [cli_spec()] for series that X-13 cannot adjust),
#' the transformation (`"auto"` takes logarithms of strictly
#' positive series, except series in percent such as interest rates, see
#' `unit` in [cli_candidates()]), outlier replacement, the cycle form of the
#' specification (the level by default, or the growth rate, see
#' [growth_form()]), the extension of the series end with forecasts
#' ([ar_extension()] by default), the cycle filter and the OECD
#' normalisation (subtract the mean, divide by the mean absolute deviation).
#' Every step is recorded in the `steps` slot.
#'
#' @param candidates A [CandidateSet-class].
#' @param spec A [CliSpec-class].
#' @return A named list of [CycleSeries-class], reference first. Candidates
#'   whose seasonal adjustment failed beyond repair are left out; the
#'   attribute `"excluded"` (see [excluded_series()]) gives their names and
#'   the reason.
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cand <- cli_candidates(sim, "reference", meta = meta)
#' cyc <- cli_cycles(cand, cli_spec(freq = 12))
#' cyc$lead_6
#' @export
cli_cycles <- function(candidates, spec) {
  check_inputs(candidates, spec)
  data <- candidates@data
  ref <- candidates@reference
  nms <- c(ref, candidate_names(candidates))
  out <- list()
  excluded <- data.frame(name = character(0), reason = character(0),
                         stringsAsFactors = FALSE)
  for (nm in nms) {
    cs <- tryCatch(
      cycle_one(data[, nm], nm, spec, unit = series_unit(candidates, nm)),
      cyclind_error_seasonal_failed = function(e) e
    )
    if (!inherits(cs, "error")) {
      out[[nm]] <- cs
      next
    }
    reason <- conditionMessage(cs)
    if (identical(nm, ref)) {
      # without the reference there is nothing to compare with
      cyclind_abort(sprintf("Series '%s' (the reference): %s", nm, reason),
                    c("cyclind_error_seasonal_failed", "cyclind_error_x13"))
    }
    cyclind_warn(sprintf(paste("Series '%s': %s; the series is excluded",
                               "from the candidates."), nm, reason),
                 "cyclind_warning_seasonal_failed")
    excluded[nrow(excluded) + 1L, ] <- list(nm, reason)
  }
  if (nrow(excluded) > 0L) attr(out, "excluded") <- excluded
  out
}

#' Series excluded from the cycles
#'
#' A candidate whose seasonal adjustment fails, and cannot be replaced by
#' the unadjusted series or by an adjustment without holiday regressors,
#' is left out of [cli_cycles()] (see [cli_spec()], `seasonal_adjust`).
#' This function lists such series and the reason.
#'
#' @param cycles The output of [cli_cycles()], or a [CliResult-class].
#' @return A data frame with columns `name` and `reason` (no rows when
#'   no series was excluded).
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cyc <- cli_cycles(cli_candidates(sim, "reference", meta = meta),
#'                   cli_spec(12))
#' excluded_series(cyc)
#' @export
excluded_series <- function(cycles) {
  if (is(cycles, "CliResult")) cycles <- cycles@cycles
  ex <- attr(cycles, "excluded")
  if (is.null(ex)) {
    ex <- data.frame(name = character(0), reason = character(0),
                     stringsAsFactors = FALSE)
  }
  ex
}

# Candidates without the series that cli_cycles() excluded.
drop_excluded <- function(candidates, cycles) {
  ex <- excluded_series(cycles)$name
  if (length(ex) == 0L) return(candidates)
  keep <- setdiff(candidate_names(candidates), ex)
  if (length(keep) == 0L) {
    cyclind_abort(paste("No candidate is left: the seasonal adjustment of",
                        "every candidate failed (see the warnings)."),
                  "cyclind_error_seasonal_failed")
  }
  candidates[keep]
}

# Audit rows (iteration 0) of the excluded series.
exclusion_audit <- function(cycles) {
  ex <- excluded_series(cycles)
  if (nrow(ex) == 0L) return(NULL)
  rows <- lapply(seq_len(nrow(ex)), function(k) {
    row <- empty_audit(sprintf("excluded %s: %s", ex$name[k], ex$reason[k]))
    row$n_components <- NA_integer_
    row
  })
  do.call(rbind, rows)
}

check_inputs <- function(candidates, spec) {
  if (!is(candidates, "CandidateSet")) {
    cyclind_abort("`candidates` must be a CandidateSet (see cli_candidates()).",
                  "cyclind_error_argument")
  }
  if (!is(spec, "CliSpec")) {
    cyclind_abort("`spec` must be a CliSpec (see cli_spec()).",
                  "cyclind_error_argument")
  }
  if (stats::frequency(candidates@data) != spec@freq) {
    cyclind_abort("The frequency of the data and of `spec` differ.",
                  "cyclind_error_frequency")
  }
  invisible(TRUE)
}

cycle_one <- function(x, name, spec, unit = "level") {
  x <- as_ts_like(as.numeric(x), x)
  store <- new.env(parent = emptyenv())
  store$notes <- character(0)
  # X-13 warnings name the series and are kept in the result
  record <- function(cnd) {
    msg <- conditionMessage(cnd)
    if (!grepl(sprintf("'%s'", name), msg, fixed = TRUE)) {
      msg <- sprintf("Series '%s': %s", name, msg)
    }
    store$notes <- c(store$notes, msg)
    cyclind_warn(msg, setdiff(class(cnd), c("cyclind_warning", "warning",
                                            "condition")))
    invokeRestart("muffleWarning")
  }
  out <- withCallingHandlers(
    cycle_one_body(x, name, spec, unit),
    cyclind_warning_x13 = record, cyclind_warning_outlier_share = record,
    cyclind_warning_outlier_collinear = record,
    cyclind_warning_seasonal_failed = record
  )
  out@notes <- unique(store$notes)
  out
}

cycle_one_body <- function(x, name, spec, unit) {
  steps <- character(0)
  span <- non_na_span(x)
  if (anyNA(span) || span[2] - span[1] + 1L < 4L * spec@freq) {
    cyclind_abort(
      sprintf("Series '%s' has fewer than four years of observations.", name),
      "cyclind_error_short_series"
    )
  }
  core <- stats::window(x, start = stats::time(x)[span[1]],
                        end = stats::time(x)[span[2]])
  if (anyNA(core)) {
    core <- as_ts_like(fill_internal_na(as.numeric(core)), core)
    steps <- c(steps, "interpolated internal gaps")
  }
  outliers <- spec_outliers(spec)
  engine <- if (methods::.hasSlot(outliers, "engine")) outliers@engine else
    "auto"
  # one decision on logarithms (FR-018), made before the seasonal
  # adjustment so that X-13 adjusts in the same scale
  decision <- transform_decision(core, spec@transform, name, engine,
                                 unit = unit)
  if (spec@seasonal_adjust) {
    hol <- if (methods::.hasSlot(spec, "holidays")) spec@holidays else list()
    core <- seasonal_adjust_x13(core, hol, log = decision$log,
                                alpha = spec@alpha)
    steps <- c(steps, attr(core, "step"))
    attr(core, "step") <- NULL
  }
  core <- if (decision$log) log(core) else core
  steps <- c(steps, decision$step)
  out <- treat_outliers(outliers, core)
  steps <- c(steps, out$step)
  found <- located_outliers(out$table, span[1] + seq_along(core) - 1L, x)
  check_outlier_share(found, length(core), outliers, name)
  core <- out$x
  fc <- out$forecast
  # cycle form (F-08): after seasonal adjustment, transformation and
  # outliers, before the extension and the filter; forecasts of the outlier
  # treatment are put in the same form
  form <- spec_form(spec)
  logged <- any(grepl("^log", steps))
  whole <- stats::ts(c(as.numeric(core), fc), start = stats::start(core),
                     frequency = stats::frequency(core))
  # a series in percent (FR-015) is differenced in percentage points, also
  # when the transformation took its logarithm
  in_points <- identical(unit, "percent") && form_kind(form) != "level"
  if (in_points && logged) whole <- exp(whole)
  whole <- apply_form(form, whole, logged && !in_points)
  if (in_points) logged <- "percent"
  drop <- sum(cumprod(is.na(as.numeric(whole))))
  keep <- length(core) - drop
  if (keep < 3L * spec@freq) {
    cyclind_abort(sprintf(paste("Series '%s' has fewer than three years of",
                                "observations in the %s form."), name,
                          form@label), "cyclind_error_short_series")
  }
  steps <- c(steps, form_step(form, spec@freq, logged))
  core <- stats::window(whole, start = stats::time(whole)[drop + 1L],
                        end = stats::time(whole)[length(core)])
  fc <- utils::tail(as.numeric(whole), length(fc))
  span[1] <- span[1] + drop
  kind <- "outlier treatment"
  if (length(fc) == 0L) {
    ext <- spec_extension(spec)
    fc <- extend_series(ext, core)
    kind <- ext@label
  }
  h <- length(fc)
  if (h > 0L) {
    ext <- stats::ts(c(as.numeric(core), fc), start = stats::start(core),
                     frequency = stats::frequency(core))
    cyc <- utils::head(as.numeric(extract_cycle(spec@filter, ext)),
                       length(core))
    steps <- c(steps, sprintf(
      "cycle: %s (series extended by %d-period forecasts: %s)",
      spec@filter@label, h, kind))
  } else {
    cyc <- extract_cycle(spec@filter, core)
    steps <- c(steps, sprintf("cycle: %s", spec@filter@label))
  }
  place <- function(v) {
    full <- rep(NA_real_, length(x))
    full[span[1]:span[2]] <- as.numeric(v)
    as_ts_like(full, x)
  }
  new("CycleSeries", name = name, original = x, transformed = place(core),
      cycle = place(cyc), normalised = place(normalise_cycle(cyc)),
      steps = c(steps, "normalised (mean 0, mean absolute deviation 1)"),
      outliers = found)
}

# Outliers of a treatment with their dates: `pos` maps positions of the
# treated span to positions of the full series `x`.
located_outliers <- function(table, pos, x) {
  if (is.null(table) || nrow(table) == 0L) return(empty_outliers())
  table <- table[!is.na(table$index), , drop = FALSE]
  if (nrow(table) == 0L) return(empty_outliers())
  tm <- as.numeric(stats::time(x))[pos[table$index]]
  f <- stats::frequency(x)
  collinear <- if (is.null(table$collinear)) FALSE else table$collinear
  out <- data.frame(time = tm,
                    period = period_label(tm, f),
                    type = table$type, removed = table$removed,
                    collinear = rep_len(as.logical(collinear), nrow(table)),
                    stringsAsFactors = FALSE)
  out <- out[order(out$time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

empty_outliers <- function() {
  data.frame(time = numeric(0), period = character(0), type = character(0),
             removed = logical(0), collinear = logical(0),
             stringsAsFactors = FALSE)
}

# Warning when the outliers removed exceed the share allowed by the
# treatment (butir 1).
check_outlier_share <- function(found, n, method, name) {
  limit <- outlier_max_share(method)
  share <- sum(found$removed) / n
  if (share > limit) {
    cyclind_warn(sprintf(paste("Series '%s': the share of outliers removed",
                               "(%d of %d observations, %.1f%%) exceeds",
                               "max_share = %s; check the series or the",
                               "outlier treatment."), name,
                         sum(found$removed), n, 100 * share, limit),
                 "cyclind_warning_outlier_share")
  }
  invisible(share)
}

# Outlier treatment of a specification (specifications saved before 0.1.3
# have only the robust threshold).
spec_outliers <- function(spec) {
  if (methods::.hasSlot(spec, "outliers")) spec@outliers else
    robust_outliers(spec@outlier_threshold)
}

# End-of-series extension of a specification (none before 0.2.0).
spec_extension <- function(spec) {
  if (methods::.hasSlot(spec, "extension")) spec@extension else
    no_extension()
}

apply_transform <- function(x, transform, name, engine = "auto",
                            unit = "level") {
  d <- transform_decision(x, transform, name, engine, unit)
  out <- if (d$log) log(x) else x
  attr(out, "step") <- d$step
  out
}

# Logarithms or levels (FR-018): list(log = TRUE/FALSE, step = text).
transform_decision <- function(x, transform, name, engine = "auto",
                               unit = "level") {
  positive <- all(x > 0)
  # a rate, a percentage or a balance (unit "percent", e.g. an interest
  # rate) has no multiplicative trend: "auto" and "test" keep it in levels
  # (butir 4); only an explicit "log" takes its logarithm
  if (identical(unit, "percent") && transform %in% c("auto", "test")) {
    return(list(log = FALSE, step = "levels (no log: series in percent)"))
  }
  if (transform == "test") {
    test <- if (positive) log_test(x, engine) else FALSE
    failed <- attr(test, "failed")
    use_log <- isTRUE(test)
    step <- if (!is.null(failed)) {
      cyclind_warn(sprintf(paste("The regARIMA log/level test (X-13 or",
                                 "TRAMO) failed for series '%s' (%s); the",
                                 "series is kept in levels."), name, failed),
                   "cyclind_warning_x13")
      sprintf("levels (regARIMA log/level test failed: %s)", failed)
    } else if (use_log) {
      "log (regARIMA log/level test)"
    } else {
      "levels (regARIMA log/level test)"
    }
    return(list(log = use_log, step = step))
  }
  if (transform == "log" && !positive) {
    cyclind_abort(sprintf("Series '%s' has non-positive values; cannot log.",
                          name), "cyclind_error_transform")
  }
  auto_log <- transform == "auto" && positive && !identical(unit, "percent")
  if (transform == "log" || auto_log) {
    list(log = TRUE, step = "log")
  } else {
    list(log = FALSE, step = "levels (no log)")
  }
}

# Replace observations whose change is an extreme robust outlier.
replace_outliers <- function(x, threshold) {
  if (!is.finite(threshold)) return(list(x = x, n = 0L))
  v <- as.numeric(x)
  d <- diff(v)
  s <- stats::mad(d)
  if (!is.finite(s) || s == 0) return(list(x = x, n = 0L))
  z <- abs(d - stats::median(d)) / s
  # a spike shows as two consecutive extreme changes of opposite sign
  flag <- which(z[-length(z)] > threshold & z[-1] > threshold &
                  sign(d[-length(d)]) != sign(d[-1])) + 1L
  # the first observation shows only one change: an extreme first change
  # is treated as a spike. The last observation is left alone, because an
  # extreme last change cannot be told apart from a level shift or a turn
  # until the next observation arrives, and it is the newest signal.
  m <- length(d)
  first <- z[1] > threshold && !(m > 1L && z[2] > threshold)
  n_flag <- length(flag) + first
  if (n_flag == 0L) return(list(x = x, n = 0L))
  v[flag] <- NA
  v <- fill_internal_na(v)
  if (first) v[1] <- v[2] - stats::median(d)
  list(x = as_ts_like(v, x), n = as.integer(n_flag),
       index = sort(c(if (first) 1L, as.integer(flag))))
}

# Seasonal adjustment with X-13ARIMA-SEATS in the scale decided by
# cyclind (`log`: TRUE, FALSE or NULL for X-13's own test). When X-13 fails
# on the model (FR-019, ADR-013): warn, then keep the series when the QS
# test finds no seasonality, else try again without the holiday regressors,
# else stop with `cyclind_error_seasonal_failed` (cli_cycles() then
# excludes the candidate). System errors stop at once (run_x13()).
seasonal_adjust_x13 <- function(x, holidays = list(), log = NULL,
                                alpha = 0.05) {
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    cyclind_abort("Seasonal adjustment needs the 'seasonal' package.",
                  "cyclind_error_dependency")
  }
  tf <- if (is.null(log)) "auto" else if (isTRUE(log)) "log" else "none"
  scale <- switch(tf, auto = "", log = ", log", none = ", levels")
  reg <- holiday_regressors(x, holidays)
  hol_names <- paste(vapply(holidays, function(h) h@name, ""),
                     collapse = ", ")
  if (is.null(reg)) {
    fit <- run_x13(suppressMessages(seasonal::seas(x,
                                                   transform.function = tf)))
    step <- sprintf("seasonally adjusted (X-13ARIMA-SEATS%s)", scale)
  } else {
    # X-13 needs the regressors over its forecast horizon (three years);
    # without them, use X-11 without forecast extension.
    covered <- stats::time(reg)[nrow(reg)] >=
      stats::time(x)[length(x)] + 3 - 1e-8
    args <- list(x, xreg = reg, regression.usertype = "holiday",
                 transform.function = tf)
    if (!covered) args <- c(args, list(x11 = "", forecast.maxlead = 0))
    fit <- run_x13(suppressMessages(do.call(seasonal::seas, args)))
    step <- sprintf("seasonally adjusted (X-13ARIMA-SEATS%s%s, holidays: %s)",
                    scale, if (covered) "" else ", X-11 without forecasts",
                    hol_names)
  }
  if (inherits(fit, "error")) {
    return(seasonal_fallback(x, one_line(conditionMessage(fit)), reg,
                             hol_names, tf, scale, alpha))
  }
  out <- as_ts_like(as.numeric(seasonal::final(fit)), x)
  attr(out, "step") <- step
  out
}

# What to do when X-13 could not adjust a series (FR-019).
seasonal_fallback <- function(x, error, reg, hol_names, tf, scale, alpha) {
  qs <- qs_test(x, log = identical(tf, "log") ||
                  (identical(tf, "auto") && all(x > 0)))
  qs_text <- sprintf("QS = %.2f, p = %.3f", qs$statistic, qs$p_value)
  failed <- sprintf("seasonal adjustment with X-13ARIMA-SEATS failed (%s)",
                    error)
  if (qs$p_value > alpha) {
    cyclind_warn(sprintf(paste("%s; the QS test finds no seasonality (%s),",
                               "so the series is used without seasonal",
                               "adjustment."), failed, qs_text),
                 "cyclind_warning_seasonal_failed")
    out <- x
    attr(out, "step") <- sprintf(paste("not seasonally adjusted: X-13 failed",
                                       "(%s); QS test, no seasonality (%s)"),
                                 error, qs_text)
    return(out)
  }
  again <- NULL
  if (!is.null(reg)) {
    fit <- run_x13(suppressMessages(seasonal::seas(x,
                                                   transform.function = tf)))
    if (!inherits(fit, "error")) {
      cyclind_warn(sprintf(paste("%s; the QS test finds seasonality (%s),",
                                 "so the series was adjusted again without",
                                 "the holiday regressors (%s)."), failed,
                           qs_text, hol_names),
                   "cyclind_warning_seasonal_failed")
      out <- as_ts_like(as.numeric(seasonal::final(fit)), x)
      attr(out, "step") <- sprintf(paste(
        "seasonally adjusted (X-13ARIMA-SEATS%s, without holiday regressors",
        "%s: they made X-13 fail: %s)"), scale, hol_names, error)
      return(out)
    }
    again <- one_line(conditionMessage(fit))
  }
  cyclind_abort(sprintf("%s%s; the QS test finds seasonality (%s)", failed,
                        if (is.null(again)) "" else
                          sprintf(", and again without the holiday %s (%s)",
                                  "regressors", again),
                        qs_text),
                c("cyclind_error_seasonal_failed", "cyclind_error_x13"))
}

# QS test for seasonality (F-13): Ljung-Box statistic on the positive
# autocorrelations at the seasonal lags s and 2s of the first differences
# (of the logarithms when `log`), with a chi-squared(2) p-value, as the
# `qsori` diagnostic of X-13ARIMA-SEATS; zero when the autocorrelation at
# lag s is not positive.
qs_test <- function(x, log = FALSE) {
  s <- stats::frequency(x)
  v <- as.numeric(x)
  v <- v[!is.na(v)]
  if (log) v <- base::log(v)
  z <- diff(v)
  m <- length(z)
  none <- list(statistic = 0, p_value = 1)
  if (m <= 2L * s + 1L || !(stats::sd(z) > 0)) return(none)
  r <- stats::acf(z, lag.max = 2L * s, plot = FALSE)$acf[-1]
  # X-13ARIMA-SEATS sets QS to zero when the autocorrelation at lag s is
  # not positive, whatever the one at lag 2s (audit S3)
  if (!(r[s] > 0)) return(none)
  rs <- r[s]
  r2 <- max(0, r[2L * s])
  q <- m * (m + 2) * (rs^2 / (m - s) + r2^2 / (m - 2L * s))
  list(statistic = q, p_value = stats::pchisq(q, 2, lower.tail = FALSE))
}
