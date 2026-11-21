# Outlier treatment methods, the regARIMA engines and the log/level test.

has_rjdemetra <- function() requireNamespace("RJDemetra", quietly = TRUE)

has_seasonal <- function() requireNamespace("seasonal", quietly = TRUE)

# "auto" means TRAMO when RJDemetra is installed and X-13 otherwise.
resolve_engine <- function(engine) {
  if (engine == "auto") engine <- if (has_rjdemetra()) "tramo" else "x13"
  if (engine == "tramo" && !has_rjdemetra()) {
    cyclind_abort(paste("The TRAMO engine needs the 'RJDemetra' package (and",
                        "Java); use engine = \"x13\" otherwise."),
                  "cyclind_error_dependency")
  }
  if (engine == "x13" && !has_seasonal()) {
    cyclind_abort("The X-13 engine needs the 'seasonal' package.",
                  "cyclind_error_dependency")
  }
  engine
}

#' @rdname OutlierTreatment-class
setMethod("treat_outliers", "RobustOutliers", function(method, x, ...) {
  out <- replace_outliers(x, method@threshold)
  out$step <- if (out$n > 0L) {
    sprintf("replaced %d outlier(s)", out$n)
  } else {
    character(0)
  }
  out$forecast <- numeric(0)
  # a one-period spike is an additive outlier
  idx <- if (is.null(out$index)) integer(0) else out$index
  out$table <- outlier_table(idx, rep("AO", length(idx)), TRUE)
  out$index <- NULL
  out
})

# Outliers found by a treatment: position in the treated series, type and
# whether the effect was removed.
outlier_table <- function(index = integer(0), type = character(0),
                          removed = logical(0), coef = NULL) {
  out <- data.frame(index = as.integer(index), type = as.character(type),
                    removed = rep_len(as.logical(removed), length(index)),
                    stringsAsFactors = FALSE)
  if (!is.null(coef)) out$coef <- rep_len(as.numeric(coef), length(index))
  out
}

#' @rdname OutlierTreatment-class
setMethod("treat_outliers", "TramoOutliers", function(method, x, ...) {
  engine <- resolve_engine(method@engine)
  f <- stats::frequency(x)
  h <- if (is.na(method@forecast)) f else method@forecast
  fit <- if (engine == "tramo") {
    regarima_tramo(x, h, method@critical)
  } else {
    regarima_x13(x, h, method@critical)
  }
  keep <- fit$types %in% method@remove
  effect <- if (any(keep)) {
    rowSums(fit$effects[, keep, drop = FALSE])
  } else {
    rep(0, length(x))
  }
  n <- sum(fit$count[method@remove], na.rm = TRUE)
  label <- if (engine == "tramo") "TRAMO" else "X-13 regARIMA"
  found <- paste(sprintf("%s %d", names(fit$count), fit$count),
                 collapse = ", ")
  tab <- if (is.null(fit$table)) outlier_table() else fit$table
  tab$removed <- tab$type %in% method@remove
  # AO and TC on the same date (F-12, ADR-012): the AO takes their joint
  # deviation at that date, the TC the decay after it
  pairs <- collinear_pairs(tab)
  effect <- effect + collinear_shift(pairs, length(x), method@remove)
  tab$collinear <- tab$type %in% c("AO", "TC") & tab$index %in% pairs$index
  rate <- if (is.null(fit$tc_rate)) tc_rate(engine, f) else fit$tc_rate
  # X-13 writes a level shift as -1 before its date and 0 after it
  dec <- apply_outlier_decisions(tab, pairs, outlier_decisions(method),
                                 list(...)$name, x, rate, max(h, 0),
                                 ls_before = engine == "x13")
  n <- n + sum(dec$removed & !tab$removed) - sum(!dec$removed & tab$removed)
  tab$removed <- dec$removed
  effect <- effect + dec$effect
  tab$coef <- NULL
  step <- if (is.null(fit$failed)) {
    sprintf("outliers (%s): found %s; removed %s", label, found,
            paste(method@remove, collapse = ", "))
  } else {
    sprintf("outliers (%s): model failed, none removed (%s)", label,
            fit$failed)
  }
  if (nrow(pairs) > 0L) {
    step <- paste0(step, "; ", warn_collinear(pairs, x, rate))
  }
  if (length(dec$step) > 0L) step <- paste0(step, "; ", dec$step)
  list(x = as_ts_like(as.numeric(x) - effect, x), n = as.integer(n),
       step = step, table = tab, failed = fit$failed,
       forecast = if (h > 0) {
         clean_forecast(utils::head(fit$forecast, h), fit$effects,
                        method@remove, fit$tc_rate) - dec$forecast
       } else {
         numeric(0)
       })
})

# ---- expert decisions on single outliers -----------------------------------

# Decisions of a treatment (objects saved before they existed have none).
outlier_decisions <- function(method) {
  if (methods::.hasSlot(method, "decisions") && nrow(method@decisions) > 0L) {
    method@decisions
  } else {
    empty_outlier_decisions()
  }
}

# Check and normalise a table of outlier decisions (see tramo_outliers()).
check_outlier_decisions <- function(decisions) {
  if (is.null(decisions)) return(empty_outlier_decisions())
  need <- c("series", "type", "action", "reason")
  if (!is.data.frame(decisions) || !all(need %in% names(decisions)) ||
      !any(c("period", "time") %in% names(decisions))) {
    cyclind_abort(paste("`decisions` must be a data frame with columns",
                        "`series`, `period` (or `time`), `type`, `action`",
                        "and `reason`."), "cyclind_error_argument")
  }
  n <- nrow(decisions)
  type <- toupper(trimws(as.character(decisions$type)))
  action <- tolower(trimws(as.character(decisions$action)))
  if (!all(type %in% c("AO", "TC", "LS"))) {
    cyclind_abort("`decisions$type` must be \"AO\", \"TC\" or \"LS\".",
                  "cyclind_error_argument")
  }
  if (!all(action %in% c("remove", "keep"))) {
    cyclind_abort("`decisions$action` must be \"remove\" or \"keep\".",
                  "cyclind_error_argument")
  }
  reason <- check_reasons(decisions$reason, n, "outlier decision")
  tm <- if ("time" %in% names(decisions)) decisions$time else
    as_vector(decisions$period)
  freq <- period_frequency(tm)
  time <- parse_period(tm, freq, "periods of `decisions`")
  period <- if (is.na(freq)) format(time) else period_label(time, freq)
  series <- as.character(decisions$series)
  key <- paste(series, round(time * 1e6), type)
  if (anyDuplicated(key) > 0L) {
    dup <- key[duplicated(key)][1]
    k <- match(dup, key)
    cyclind_abort(sprintf("`decisions` has two rows for %s %s of '%s'.",
                          type[k], period[k], series[k]),
                  "cyclind_error_argument")
  }
  who <- if ("who" %in% names(decisions)) as.character(decisions$who) else
    rep(NA_character_, n)
  data.frame(series = series, time = time, period = period, type = type,
             action = action, reason = reason, who = who,
             when = rep(decision_time(), n), stringsAsFactors = FALSE)
}

# Apply the decisions of one series to the outliers found (`tab`): which
# are removed, and the change of the removed effect over the series and
# its `h` forecasts, relative to the global rule.
apply_outlier_decisions <- function(tab, pairs, decisions, name, x, rate,
                                    h, ls_before = FALSE) {
  n <- length(x)
  out <- list(removed = tab$removed, effect = numeric(n),
              forecast = numeric(h), step = character(0))
  if (is.null(name) || nrow(decisions) == 0L) return(out)
  d <- decisions[decisions$series == name, , drop = FALSE]
  if (nrow(d) == 0L) return(out)
  tm <- as.numeric(stats::time(x))
  found_time <- tm[tab$index]
  eff <- numeric(n + h)
  done <- character(0)
  unmatched <- logical(nrow(d))
  for (k in seq_len(nrow(d))) {
    hit <- which(!is.na(tab$index) & same_time(found_time, d$time[k]) &
                   tab$type == d$type[k])
    if (length(hit) == 0L) {
      unmatched[k] <- TRUE
      next
    }
    want <- d$action[k] == "remove"
    for (j in hit[out$removed[hit] != want]) {
      e <- single_outlier_effect(tab, j, pairs, n + h, rate, ls_before)
      eff <- eff + if (want) e else -e
      out$removed[j] <- want
    }
    done <- c(done, sprintf("%s %s %s", if (want) "removed" else "kept",
                            d$type[k], d$period[k]))
  }
  if (any(unmatched)) warn_unmatched_decisions(d[unmatched, ], tab, x, name)
  out$effect <- eff[seq_len(n)]
  out$forecast <- eff[n + seq_len(h)]
  if (length(done) > 0L) {
    out$step <- sprintf("expert decisions: %s", paste(done, collapse = ", "))
  }
  out
}

# Effect of one outlier over `len` periods; an AO and a TC on the same date
# take their rewritten effects (ADR-012): the AO the joint deviation at that
# date, the TC the decay after it. A level shift is 1 from its date on
# (TRAMO), or -1 before its date (`ls_before`, X-13), as each engine writes
# its regressor.
single_outlier_effect <- function(tab, j, pairs, len, rate,
                                  ls_before = FALSE) {
  t0 <- tab$index[j]
  coef <- if (is.null(tab$coef)) NA_real_ else tab$coef[j]
  if (is.na(coef)) {
    cyclind_abort(sprintf(paste("The effect of the %s outlier at position %d",
                                "is not known; the decision cannot be",
                                "applied."), tab$type[j], t0),
                  "cyclind_error_outliers")
  }
  tt <- seq_len(len)
  ao <- as.numeric(tt == t0)
  ls <- if (ls_before) -as.numeric(tt < t0) else as.numeric(tt >= t0)
  reg <- switch(tab$type[j], AO = ao, LS = ls,
                TC = ifelse(tt >= t0, rate^(tt - t0), 0))
  e <- coef * reg
  if (tab$type[j] %in% c("AO", "TC") && t0 %in% pairs$index) {
    c_tc <- pairs$coef_tc[match(t0, pairs$index)]
    e <- e + if (tab$type[j] == "AO") c_tc * ao else -c_tc * ao
  }
  e
}

warn_unmatched_decisions <- function(d, tab, x, name) {
  f <- stats::frequency(x)
  tm <- as.numeric(stats::time(x))
  ok <- !is.na(tab$index)
  found <- if (any(ok)) {
    paste(sprintf("%s %s", tab$type[ok], period_label(tm[tab$index[ok]], f)),
          collapse = ", ")
  } else {
    "none"
  }
  cyclind_warn(sprintf(paste("Series '%s': the model found no outlier for the",
                             "expert decision(s) %s, which have no effect.",
                             "Outliers found: %s."), name,
                       paste(sprintf("%s %s %s", d$action, d$type, d$period),
                             collapse = ", "), found),
               "cyclind_warning_outlier_decision")
}

# Decisions of a treatment for the log: `status` is "pending" before the
# data are seen, then "applied" when an outlier of that series, date and
# type was found, and "not matched" otherwise.
outlier_decision_log <- function(method, cycles = NULL) {
  d <- outlier_decisions(method)
  if (nrow(d) == 0L) return(empty_decision_log())
  status <- if (is.null(cycles)) {
    rep("pending", nrow(d))
  } else {
    vapply(seq_len(nrow(d)), function(k) {
      cs <- cycles[[d$series[k]]]
      if (is.null(cs)) return("not matched")
      o <- cs@outliers
      hit <- same_time(o$time, d$time[k]) & o$type == d$type[k]
      if (any(hit)) "applied" else "not matched"
    }, character(1))
  }
  out <- decision_rows("outliers", d$series,
                       sprintf("%s %s %s", d$action, d$type, d$period),
                       d$reason, d$who, d$when, status)
  out
}

# Decisions for series that are not in the data (cli_cycles()).
warn_unknown_decision_series <- function(spec, names) {
  d <- outlier_decisions(spec_outliers(spec))
  unknown <- setdiff(unique(d$series), names)
  if (length(unknown) > 0L) {
    cyclind_warn(sprintf(paste("Outlier decisions name series that are not",
                               "in the data: %s; they have no effect."),
                         paste(unknown, collapse = ", ")),
                 "cyclind_warning_outlier_decision")
  }
  invisible(unknown)
}

# Same-date pairs of an additive outlier and a transitory change: date
# (`index`) and coefficient of the TC (`coef_tc`). Their regressors
# correlate 0.85 to 0.97 after differencing (F-12), so the model cannot
# tell how the deviation at that date splits between the two.
collinear_pairs <- function(tab) {
  none <- data.frame(index = integer(0), coef_tc = numeric(0))
  if (is.null(tab) || nrow(tab) == 0L || is.null(tab$coef)) return(none)
  ok <- !is.na(tab$index) & !is.na(tab$coef)
  ao <- tab$index[ok & tab$type == "AO"]
  tc <- ok & tab$type == "TC" & tab$index %in% ao
  if (!any(tc)) return(none)
  data.frame(index = tab$index[tc], coef_tc = tab$coef[tc])
}

# Correction of the removed effect for same-date pairs (F-12): an AO of
# a + c at t0 and a TC of c * r from t0 + 1 give the same fitted effect as
# the AO a and the TC c at t0. Removing the AO only removes c more at t0;
# removing the TC only removes c less; both or neither, no change.
collinear_shift <- function(pairs, n, remove) {
  out <- numeric(n)
  sgn <- ("AO" %in% remove) - ("TC" %in% remove)
  if (sgn == 0 || nrow(pairs) == 0L) return(out)
  for (k in seq_len(nrow(pairs))) {
    i <- pairs$index[k]
    out[i] <- out[i] + sgn * pairs$coef_tc[k]
  }
  out
}

# Correlation of the AO and TC regressors at date t0 of a series of length
# n after first differencing; (2 - r) sqrt(1 + r) / 2 for long series.
pair_correlation <- function(n, t0, rate) {
  tt <- seq_len(n)
  ao <- as.numeric(tt == t0)
  tc <- ifelse(tt >= t0, rate^(tt - t0), 0)
  stats::cor(diff(ao), diff(tc))
}

# Warning for same-date pairs; returns the text recorded in the steps.
warn_collinear <- function(pairs, x, rate) {
  f <- stats::frequency(x)
  per <- period_label(as.numeric(stats::time(x))[pairs$index], f)
  rho <- vapply(pairs$index, function(i) pair_correlation(length(x), i, rate),
                numeric(1))
  cyclind_warn(sprintf(paste(
    "An additive outlier (AO) and a transitory change (TC) were found on",
    "the same date (%s); their regressors correlate %s after first",
    "differencing, so the model cannot split the deviation between them.",
    "The AO is taken as the whole deviation at that date and the TC as the",
    "decay after it (ADR-012)."),
    paste(per, collapse = ", "),
    paste(sprintf("%.2f", rho), collapse = ", ")),
    "cyclind_warning_outlier_collinear")
  sprintf("AO+TC on the same date at %s split into the deviation at that %s",
          paste(per, collapse = ", "),
          "date (AO) and the decay after it (TC)")
}

# Decay rate of a transitory change: TRAMO uses 0.7 at every frequency,
# X-13ARIMA-SEATS uses 0.7^(12/f) (0.343 for quarterly data).
tc_rate <- function(engine, f) if (engine == "x13") 0.7^(12 / f) else 0.7

# Effect matrix of outliers named like "AO (5-2011)" (JDemetra+ style; the
# period of quarterly data is a Roman numeral, "AO (IV-2009)").
outlier_effects <- function(names, coef, x, tc_rate = 0.7) {
  n <- length(x)
  f <- stats::frequency(x)
  st <- stats::start(x)
  types <- substr(names, 1, 2)
  eff <- matrix(0, n, 3, dimnames = list(NULL, c("AO", "TC", "LS")))
  for (k in seq_along(names)) {
    p <- strsplit(gsub(".*\\((.*)\\).*", "\\1", names[k]), "-")[[1]]
    per <- outlier_period(p[1])
    year <- suppressWarnings(as.integer(p[2]))
    t0 <- (year - st[1]) * f + (per - st[2]) + 1L
    if (length(p) != 2L || is.na(t0) || per < 1L || per > f) {
      cyclind_abort(sprintf("Cannot read the date of the outlier '%s'.",
                            names[k]), "cyclind_error_outliers")
    }
    if (t0 < 1L || t0 > n) next
    tt <- seq_len(n)
    reg <- switch(types[k],
                  AO = as.numeric(tt == t0),
                  LS = as.numeric(tt >= t0),
                  TC = ifelse(tt >= t0, tc_rate^(tt - t0), 0))
    eff[, types[k]] <- eff[, types[k]] + coef[k] * reg
  }
  eff
}

# Period of an outlier date: a number, or a Roman numeral.
outlier_period <- function(p) {
  if (grepl("^[0-9]+$", p)) return(as.integer(p))
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
             "XI", "XII")
  match(toupper(p), roman)
}

# Forecasts of the series without the removed outliers: a level shift
# continues unchanged and a transitory change keeps decaying at `tc_rate`
# (the rate of the engine, see tc_rate()) beyond the last observation, so
# the clean series and its forecasts join without a jump.
clean_forecast <- function(fc, effects, remove, tc_rate = 0.7) {
  if (length(fc) == 0L) return(fc)
  n <- nrow(effects)
  h <- seq_along(fc)
  ls <- if ("LS" %in% remove) effects[n, "LS"] else 0
  tc <- if ("TC" %in% remove) effects[n, "TC"] * tc_rate^h else 0
  fc - ls - tc
}

count_types <- function(types) {
  vapply(c("AO", "TC", "LS"), function(t) sum(types == t), integer(1))
}

# TRAMO through JDemetra+ (RJDemetra); automatic ARIMA model, no calendar
# effects, outliers AO, TC and LS, no transformation (x is already logged
# when appropriate).
regarima_tramo <- function(x, h, critical) {
  args <- list(spec = "TR3", transform.function = "None",
               fcst.horizon = max(1, h), outlier.enabled = TRUE,
               outlier.ao = TRUE, outlier.tc = TRUE, outlier.ls = TRUE,
               outlier.so = FALSE)
  if (!is.na(critical)) {
    args <- c(args, list(outlier.usedefcv = FALSE, outlier.cv = critical))
  }
  sp <- do.call(RJDemetra::regarima_spec_tramoseats, args)
  m <- run_x13(RJDemetra::regarima(x, sp))
  if (inherits(m, "error")) return(regarima_failed(x, h, conditionMessage(m)))
  if (is.null(m$forecast)) return(regarima_failed(x, h, "no forecasts"))
  rc <- m$regression.coefficients
  nm <- if (is.null(rc)) character(0) else rownames(rc)
  is_out <- grepl("^(AO|TC|LS) \\(", nm)
  nm <- nm[is_out]
  coef <- if (length(nm) > 0L) rc[is_out, 1] else numeric(0)
  tab <- tramo_outlier_table(nm, x)
  if (nrow(tab) > 0L) tab$coef <- as.numeric(coef)
  list(effects = outlier_effects(nm, coef, x),
       types = c("AO", "TC", "LS"), count = count_types(substr(nm, 1, 2)),
       forecast = as.numeric(m$forecast[, 1]),
       tc_rate = tc_rate("tramo", stats::frequency(x)),
       table = tab)
}

# Positions and types of outliers named like "AO (5-2011)" (JDemetra+).
tramo_outlier_table <- function(names, x) {
  if (length(names) == 0L) return(outlier_table())
  f <- stats::frequency(x)
  st <- stats::start(x)
  idx <- vapply(names, function(nm) {
    p <- strsplit(gsub(".*\\((.*)\\).*", "\\1", nm), "-")[[1]]
    per <- outlier_period(p[1])
    year <- suppressWarnings(as.integer(p[2]))
    t0 <- (year - st[1]) * f + (per - st[2]) + 1L
    if (length(p) != 2L || is.na(t0) || t0 < 1L || t0 > length(x)) {
      NA_integer_
    } else {
      as.integer(t0)
    }
  }, integer(1), USE.NAMES = FALSE)
  outlier_table(idx, substr(names, 1, 2), FALSE)
}

# Positions and types of the X-13 outlier regressors among the coefficient
# names, e.g. "AO2012.Jun" (monthly) or "LS2009.4" (quarterly).
x13_outlier_table <- function(names, x, coef = NULL) {
  hit <- grepl("^(AO|LS|TC)[0-9]{4}\\.", names)
  names <- names[hit]
  if (!is.null(coef)) coef <- as.numeric(coef)[hit]
  if (length(names) == 0L) return(outlier_table())
  f <- stats::frequency(x)
  st <- stats::start(x)
  year <- as.integer(substr(names, 3, 6))
  p <- substring(names, 8)
  per <- ifelse(grepl("^[0-9]+$", p), suppressWarnings(as.integer(p)),
                match(tolower(p), tolower(month.abb)))
  idx <- as.integer((year - st[1]) * f + (per - st[2]) + 1L)
  idx[!is.na(idx) & (idx < 1L | idx > length(x))] <- NA_integer_
  outlier_table(idx, toupper(substr(names, 1, 2)), FALSE, coef)
}

# When the regARIMA model cannot be estimated: no outliers removed, and
# forecasts from an IMA(1,1) model fitted with stats::arima(). The warning
# carries the error of the program, and the result records it.
regarima_failed <- function(x, h, message = "estimation failed") {
  message <- one_line(message)
  cyclind_warn(paste0("The regARIMA model (X-13 or TRAMO) could not be ",
                      "estimated (", message, "); no outliers were removed ",
                      "from this series."),
               c("cyclind_warning_x13", "cyclind_warning_outliers"))
  fc <- tryCatch({
    fit <- stats::arima(as.numeric(x), order = c(0, 1, 1))
    as.numeric(stats::predict(fit, n.ahead = max(1, h))$pred)
  }, error = function(e) rep(as.numeric(x)[length(x)], max(1, h)))
  list(effects = matrix(0, length(x), 3,
                        dimnames = list(NULL, c("AO", "TC", "LS"))),
       types = c("AO", "TC", "LS"), count = count_types(character(0)),
       forecast = fc, tc_rate = 0.7, table = outlier_table(),
       failed = message)
}

# The first line of an error message, without the X-13 banner.
one_line <- function(message) {
  lines <- trimws(strsplit(message, "\n")[[1]])
  lines <- lines[nzchar(lines) & !grepl("^(X-13 run failed|Errors:)$", lines)]
  if (length(lines) == 0L) return("unknown error")
  # an X-13 error item wraps over several lines; stop at the next item or
  # at the line that names the temporary files
  out <- lines[1]
  for (l in lines[-1]) {
    if (grepl("^- |Program error|halt execution|iofile", l)) break
    out <- paste(out, l)
  }
  sub("^- ", "", out)
}

# Errors of the system (disk, files, permissions, missing binary) as
# opposed to errors of the model (no convergence, singular matrices).
x13_failure_kind <- function(e) {
  msg <- conditionMessage(e)
  system <- paste("No space left|cannot open|cannot write|cannot create",
                  "error writing|Permission denied|Read-only file system",
                  "not found|could not find|cannot find|binary|disk quota",
                  "cannot allocate", sep = "|")
  if (grepl(system, msg, ignore.case = TRUE)) "system" else "model"
}

# Run an X-13 (or TRAMO) call: system errors stop with
# `cyclind_error_system`; model errors return the condition.
run_x13 <- function(expr) {
  tryCatch(expr, error = function(e) {
    if (x13_failure_kind(e) == "system") {
      cyclind_abort(paste0("X-13ARIMA-SEATS could not run because of a ",
                           "system error: ", one_line(conditionMessage(e))),
                    "cyclind_error_system")
    }
    e
  })
}

# regARIMA part of X-13ARIMA-SEATS through 'seasonal'.
regarima_x13 <- function(x, h, critical) {
  args <- list(x, x11 = NULL, seats = NULL, transform.function = "none",
               regression.aictest = NULL,
               outlier.types = c("ao", "ls", "tc"),
               forecast.maxlead = max(1, h), forecast.save = "fct",
               regression.save = c("ao", "ls", "tc"))
  if (!is.na(critical)) args <- c(args, list(outlier.critical = critical))
  m <- run_x13(suppressMessages(do.call(seasonal::seas, args)))
  if (inherits(m, "error")) {
    # automatic model identification can fail to converge; retry with the
    # IMA(1,1) model, then give up on outliers with a warning
    args$arima.model <- "(0 1 1)"
    m <- run_x13(suppressMessages(do.call(seasonal::seas, args)))
  }
  if (inherits(m, "error")) {
    return(regarima_failed(x, h, conditionMessage(m)))
  }
  n <- length(x)
  get_tab <- function(nm) {
    v <- tryCatch(suppressMessages(seasonal::series(m, nm)),
                  error = function(e) NULL)
    if (is.null(v)) return(rep(0, n))
    v <- as.numeric(stats::window(v, start = stats::start(x),
                                  end = stats::end(x)))
    v[is.na(v)] <- 0
    v
  }
  eff <- cbind(AO = get_tab("regression.aoutlier"),
               TC = get_tab("regression.temporarychange"),
               LS = get_tab("regression.levelshift"))
  cfv <- stats::coef(m)
  cf <- names(cfv)
  types <- toupper(substr(cf[grepl("^(AO|LS|TC)[0-9]", cf)], 1, 2))
  fc <- suppressMessages(seasonal::series(m, "forecast.forecasts"))
  list(effects = eff, types = c("AO", "TC", "LS"),
       count = count_types(types), forecast = as.numeric(fc[, 1]),
       tc_rate = tc_rate("x13", stats::frequency(x)),
       table = x13_outlier_table(cf, x, cfv))
}

# Log/level test of the regARIMA model (TRAMO or X-13); TRUE for logs. When
# the model fails the result is NA with the error message as attribute
# "failed"; system errors stop.
log_test <- function(x, engine = "auto") {
  engine <- resolve_engine(engine)
  if (engine == "tramo") {
    sp <- RJDemetra::regarima_spec_tramoseats(spec = "TR3",
                                              transform.function = "Auto")
    m <- run_x13(RJDemetra::regarima(x, sp))
    if (inherits(m, "error")) return(failed_test(m))
    isTRUE(as.logical(m$model$spec_rslt[["Log transformation"]]))
  } else {
    m <- run_x13(suppressMessages(seasonal::seas(x, x11 = NULL, seats = NULL,
                                                 transform.function = "auto",
                                                 regression.aictest = NULL,
                                                 outlier = NULL)))
    if (inherits(m, "error")) return(failed_test(m))
    identical(seasonal::transformfunction(m), "log")
  }
}

failed_test <- function(e) {
  structure(NA, failed = one_line(conditionMessage(e)))
}
