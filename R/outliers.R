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
                          removed = logical(0)) {
  data.frame(index = as.integer(index), type = as.character(type),
             removed = rep_len(as.logical(removed), length(index)),
             stringsAsFactors = FALSE)
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
  step <- if (is.null(fit$failed)) {
    sprintf("outliers (%s): found %s; removed %s", label, found,
            paste(method@remove, collapse = ", "))
  } else {
    sprintf("outliers (%s): model failed, none removed (%s)", label,
            fit$failed)
  }
  list(x = as_ts_like(as.numeric(x) - effect, x), n = as.integer(n),
       step = step, table = tab, failed = fit$failed,
       forecast = if (h > 0) {
         clean_forecast(utils::head(fit$forecast, h), fit$effects,
                        method@remove, fit$tc_rate)
       } else {
         numeric(0)
       })
})

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
  list(effects = outlier_effects(nm, coef, x),
       types = c("AO", "TC", "LS"), count = count_types(substr(nm, 1, 2)),
       forecast = as.numeric(m$forecast[, 1]),
       tc_rate = tc_rate("tramo", stats::frequency(x)),
       table = tramo_outlier_table(nm, x))
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
x13_outlier_table <- function(names, x) {
  names <- names[grepl("^(AO|LS|TC)[0-9]{4}\\.", names)]
  if (length(names) == 0L) return(outlier_table())
  f <- stats::frequency(x)
  st <- stats::start(x)
  year <- as.integer(substr(names, 3, 6))
  p <- substring(names, 8)
  per <- ifelse(grepl("^[0-9]+$", p), suppressWarnings(as.integer(p)),
                match(tolower(p), tolower(month.abb)))
  idx <- as.integer((year - st[1]) * f + (per - st[2]) + 1L)
  idx[!is.na(idx) & (idx < 1L | idx > length(x))] <- NA_integer_
  outlier_table(idx, toupper(substr(names, 1, 2)), FALSE)
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
  sub("^- ", "", lines[1])
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
  cf <- names(stats::coef(m))
  types <- toupper(substr(cf[grepl("^(AO|LS|TC)[0-9]", cf)], 1, 2))
  fc <- suppressMessages(seasonal::series(m, "forecast.forecasts"))
  list(effects = eff, types = c("AO", "TC", "LS"),
       count = count_types(types), forecast = as.numeric(fc[, 1]),
       tc_rate = tc_rate("x13", stats::frequency(x)),
       table = x13_outlier_table(cf, x))
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
