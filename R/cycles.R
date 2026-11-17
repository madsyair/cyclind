#' Extract the cycles of all series
#'
#' Applies, series by series on its own non-missing span: interpolation of
#' internal gaps, optional seasonal adjustment of the original series with
#' X-13ARIMA-SEATS, the transformation (`"auto"` takes logarithms of strictly
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
#' @return A named list of [CycleSeries-class], reference first.
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
  nms <- c(candidates@reference, candidate_names(candidates))
  out <- lapply(nms, function(nm) {
    cycle_one(data[, nm], nm, spec, unit = series_unit(candidates, nm))
  })
  stats::setNames(out, nms)
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
    cyclind_warning_x13 = record, cyclind_warning_outlier_share = record
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
  if (spec@seasonal_adjust) {
    hol <- if (methods::.hasSlot(spec, "holidays")) spec@holidays else list()
    core <- seasonal_adjust_x13(core, hol)
    steps <- c(steps, attr(core, "step"))
    attr(core, "step") <- NULL
  }
  outliers <- spec_outliers(spec)
  engine <- if (methods::.hasSlot(outliers, "engine")) outliers@engine else
    "auto"
  core <- apply_transform(core, spec@transform, name, engine, unit = unit)
  steps <- c(steps, attr(core, "step"))
  attr(core, "step") <- NULL
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
  out <- data.frame(time = tm,
                    period = period_label(tm, f),
                    type = table$type, removed = table$removed,
                    stringsAsFactors = FALSE)
  out <- out[order(out$time), , drop = FALSE]
  rownames(out) <- NULL
  out
}

empty_outliers <- function() {
  data.frame(time = numeric(0), period = character(0), type = character(0),
             removed = logical(0), stringsAsFactors = FALSE)
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
  positive <- all(x > 0)
  # a rate, a percentage or a balance (unit "percent", e.g. an interest
  # rate) has no multiplicative trend: "auto" and "test" keep it in levels
  # (butir 4); only an explicit "log" takes its logarithm
  if (identical(unit, "percent") && transform %in% c("auto", "test")) {
    attr(x, "step") <- "levels (no log: series in percent)"
    return(x)
  }
  if (transform == "test") {
    test <- if (positive) log_test(x, engine) else FALSE
    failed <- attr(test, "failed")
    use_log <- isTRUE(test)
    out <- if (use_log) log(x) else x
    attr(out, "step") <- if (!is.null(failed)) {
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
    return(out)
  }
  if (transform == "log" && !positive) {
    cyclind_abort(sprintf("Series '%s' has non-positive values; cannot log.",
                          name), "cyclind_error_transform")
  }
  auto_log <- transform == "auto" && positive && !identical(unit, "percent")
  if (transform == "log" || auto_log) {
    out <- log(x)
    attr(out, "step") <- "log"
  } else {
    out <- x
    attr(out, "step") <- "levels (no log)"
  }
  out
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

seasonal_adjust_x13 <- function(x, holidays = list()) {
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    cyclind_abort("Seasonal adjustment needs the 'seasonal' package.",
                  "cyclind_error_dependency")
  }
  reg <- holiday_regressors(x, holidays)
  if (is.null(reg)) {
    fit <- run_x13(suppressMessages(seasonal::seas(x)))
    step <- "seasonally adjusted (X-13ARIMA-SEATS)"
  } else {
    # X-13 needs the regressors over its forecast horizon (three years);
    # without them, use X-11 without forecast extension.
    covered <- stats::time(reg)[nrow(reg)] >=
      stats::time(x)[length(x)] + 3 - 1e-8
    args <- list(x, xreg = reg, regression.usertype = "holiday")
    if (!covered) args <- c(args, list(x11 = "", forecast.maxlead = 0))
    fit <- run_x13(suppressMessages(do.call(seasonal::seas, args)))
    step <- sprintf("seasonally adjusted (X-13ARIMA-SEATS%s, holidays: %s)",
                    if (covered) "" else ", X-11 without forecasts",
                    paste(vapply(holidays, function(h) h@name, ""),
                          collapse = ", "))
  }
  if (inherits(fit, "error")) {
    # without the adjustment the cycle would carry the seasonal pattern:
    # stop rather than go on with an unadjusted series
    cyclind_abort(paste("Seasonal adjustment with X-13ARIMA-SEATS failed:",
                        one_line(conditionMessage(fit))),
                  "cyclind_error_x13")
  }
  out <- as_ts_like(as.numeric(seasonal::final(fit)), x)
  attr(out, "step") <- step
  out
}
