# X-13 fixes (INC-20): collinear AO/TC pairs (FR-017), the transformation
# passed to X-13 (FR-018) and failed seasonal adjustments (FR-019).

# ---- FR-017: AO and TC on the same date -----------------------------------

test_that("an AO/TC pair is split into the value at t0 and the decay", {
  skip_if_not_installed("seasonal")
  x <- pair_series()
  a <- 3
  c <- -6
  r <- 0.343
  local_mocked_bindings(regarima_x13 = function(x, h, critical) {
    fake_pair_fit(x, a, c, 19L, r)
  })
  tt <- seq_along(x)
  tc <- c * ifelse(tt >= 19, r^(tt - 19), 0)
  # AO removed, TC kept: the whole deviation at t0, a + c
  expect_warning(
    out <- treat_outliers(tramo_outliers(engine = "x13", forecast = 0), x),
    class = "cyclind_warning_outlier_collinear")
  expect_equal(as.numeric(x - out$x), (a + c) * (tt == 19), tolerance = 1e-10)
  expect_true(all(out$table$collinear))
  expect_equal(out$table$removed, c(TRUE, FALSE))
  expect_match(out$step, "AO\\+TC")
  # TC removed, AO kept: the decay after t0 only
  out_tc <- suppressWarnings(treat_outliers(
    tramo_outliers(engine = "x13", remove = "TC", forecast = 0), x))
  expect_equal(as.numeric(x - out_tc$x), tc - c * (tt == 19),
               tolerance = 1e-10)
  # both or neither: the effect of the model is unchanged
  out_both <- suppressWarnings(treat_outliers(
    tramo_outliers(engine = "x13", remove = c("AO", "TC"), forecast = 0), x))
  expect_equal(as.numeric(x - out_both$x), a * (tt == 19) + tc,
               tolerance = 1e-10)
  out_ls <- suppressWarnings(treat_outliers(
    tramo_outliers(engine = "x13", remove = "LS", forecast = 0), x))
  expect_equal(as.numeric(x - out_ls$x), rep(0, length(x)))
})

test_that("the warning gives the period and the regressor correlation", {
  skip_if_not_installed("seasonal")
  x <- pair_series()
  local_mocked_bindings(regarima_x13 = function(x, h, critical) {
    fake_pair_fit(x, 3, -6, 19L)
  })
  expect_warning(
    treat_outliers(tramo_outliers(engine = "x13", forecast = 0), x),
    "2019Q3.*0\\.96")
  r <- 0.343
  expect_equal(cyclind:::pair_correlation(200L, 100L, r),
               (2 - r) * sqrt(1 + r) / 2, tolerance = 0.01)
})

test_that("cycles and the outlier report record collinear pairs", {
  skip_if_not_installed("seasonal")
  x <- pair_series()
  local_mocked_bindings(regarima_x13 = function(x, h, critical) {
    fake_pair_fit(x, 3, -6, 19L)
  })
  cand <- cli_candidates(cbind(ref = x, z = x + 0.01 * sin(seq_along(x))),
                         "ref", meta = data.frame(name = "z",
                                                  rationale = "expectations"))
  s <- oecd_spec(freq = 4, engine = "x13", transform = "none")
  w <- character(0)
  cyc <- withCallingHandlers(
    cli_cycles(cand, s),
    cyclind_warning_outlier_collinear = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("'ref'", w)))
  expect_true(all(cyc$ref@outliers$collinear))
  expect_match(cyc$ref@notes, "same date", all = FALSE)
  rep <- outlier_report(cyc)
  expect_true("collinear" %in% names(rep))
  expect_equal(rep$collinear[rep$name == "ref"], "AO+TC 2019Q3")
  # objects saved before the column existed are still read
  old <- cyc$ref
  old@outliers$collinear <- NULL
  expect_equal(outlier_report(old)$collinear, "")
})

test_that("X-13 no longer dates a false trough at a collinear pair", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  skip_if_not_installed("RJDemetra")
  x <- pair_series()
  s13 <- oecd_spec(freq = 4, transform = "none", engine = "x13")
  str <- oecd_spec(freq = 4, transform = "none", engine = "tramo")
  expect_warning(cyclind:::cycle_one(x, "ref", s13),
                 class = "cyclind_warning_outlier_collinear")
  tp13 <- reference_tps(x, s13)
  expect_false(grepl("trough 2019Q3", tp13))
  expect_equal(tp13, reference_tps(x, str))
})

# ---- FR-018: the transformation is passed to X-13 --------------------------

test_that("the decided transformation reaches the X-13 adjustment", {
  skip_if_not_installed("seasonal")
  x <- ts(100 + 0.5 * seq_len(48) + rep(c(3, -1, 1, -3), 12),
          start = 2015, frequency = 4)
  seen <- list()
  local_mocked_bindings(
    seas = function(x, ...) {
      seen[[length(seen) + 1L]] <<- list(...)
      stop("X-13 run failed\n\nErrors:\n- captured")
    },
    .package = "seasonal")
  run <- function(tr) {
    # the mocked X-13 fails, so the seasonal series cannot be adjusted
    expect_error(suppressWarnings(cyclind:::cycle_one(
      x, "x", cli_spec(4, transform = tr, seasonal_adjust = TRUE))),
      class = "cyclind_error_seasonal_failed")
  }
  run("log")
  expect_equal(seen[[1]]$transform.function, "log")
  seen <- list()
  run("none")
  expect_equal(seen[[1]]$transform.function, "none")
  seen <- list()
  run("auto")
  expect_equal(seen[[1]]$transform.function, "log")
})

test_that("X-13 adjusts in logs when cyclind takes logarithms", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  # additive seasonality: X-13 alone would choose levels
  x <- ts(100 + 0.5 * seq_len(48) + rep(c(3, -1, 1, -3), 12) +
            with_seed(1, cumsum(stats::rnorm(48, 0, 0.5))),
          start = 2015, frequency = 4)
  auto <- suppressMessages(seasonal::seas(x))
  expect_equal(seasonal::transformfunction(auto), "none")
  cs <- cyclind:::cycle_one(x, "x", cli_spec(4, transform = "log",
                                              seasonal_adjust = TRUE))
  expect_true("seasonally adjusted (X-13ARIMA-SEATS, log)" %in% cs@steps)
  direct <- suppressMessages(seasonal::seas(x, transform.function = "log"))
  expect_equal(as.numeric(exp(cs@transformed)),
               as.numeric(seasonal::final(direct)), tolerance = 1e-6)
})

test_that("with logarithms X-13 and TRAMO date the same turning points", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  skip_if_not_installed("RJDemetra")
  x <- pair_series(season = 2)
  sp <- function(engine) {
    oecd_spec(freq = 4, transform = "log", engine = engine,
              seasonal_adjust = TRUE)
  }
  expect_equal(reference_tps(x, sp("x13")), reference_tps(x, sp("tramo")))
})

# ---- FR-019: failed seasonal adjustments -----------------------------------

test_that("the QS statistic reproduces X-13", {
  qs <- cyclind:::qs_test(UKgas, log = FALSE)
  expect_equal(qs$statistic, 176.5144, tolerance = 1e-6)
  expect_lt(qs$p_value, 1e-10)
  flat <- cyclind:::qs_test(ts(1:40, frequency = 4), log = FALSE)
  expect_equal(flat$statistic, 0)
  expect_equal(flat$p_value, 1)
  expect_lt(cyclind:::qs_test(step_pattern(), log = FALSE)$p_value, 0.001)
  expect_gt(cyclind:::qs_test(policy_rate(), log = FALSE)$p_value, 0.05)
  short <- cyclind:::qs_test(ts(rnorm(8), frequency = 4), log = FALSE)
  expect_equal(short$p_value, 1)
})

test_that("a failed adjustment without seasonality keeps the series", {
  skip_if_not_installed("seasonal")
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- ARMA ",
                              "covariance matrix is singular"),
    .package = "seasonal")
  x <- policy_rate()
  expect_warning(out <- cyclind:::seasonal_adjust_x13(x, log = FALSE),
                 class = "cyclind_warning_seasonal_failed")
  expect_equal(as.numeric(out), as.numeric(x))
  expect_match(attr(out, "step"), "not seasonally adjusted.*QS")
})

test_that("a failed adjustment is tried again without the holidays", {
  skip_if_not_installed("seasonal")
  e <- holiday_effect(idulfitri_dates(2005, 2030), name = "idulfitri")
  local_mocked_bindings(
    seas = function(x, ...) {
      if ("xreg" %in% names(list(...))) {
        stop("X-13 run failed\n\nErrors:\n- ARMA covariance singular")
      }
      structure(list(fit = x * 0 + 7), class = "fake_seas")
    },
    final = function(m) m$fit,
    .package = "seasonal")
  x <- step_pattern()
  expect_warning(out <- cyclind:::seasonal_adjust_x13(x, list(e), log = TRUE),
                 "without the holiday regressors",
                 class = "cyclind_warning_seasonal_failed")
  expect_equal(as.numeric(out), rep(7, 48))
  expect_match(attr(out, "step"), "without holiday regressors")
})

test_that("a series that cannot be adjusted is excluded with its reason", {
  skip_if_not_installed("seasonal")
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- ARMA ",
                              "covariance matrix is singular"),
    .package = "seasonal")
  cand <- sim_candidates(seed = 1, freq = 4, n_years = 20)
  dat <- cand@data
  # lead_2 gets a strong fixed seasonal pattern; the others have none
  dat[, "lead_2"] <- dat[, "lead_2"] * rep(c(1.2, 0.8, 1.1, 0.9),
                                           length.out = nrow(dat))
  cand@data <- dat
  s <- cli_spec(4, seasonal_adjust = TRUE)
  w <- character(0)
  keep_w <- function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }
  cyc <- withCallingHandlers(cli_cycles(cand, s),
                             cyclind_warning_seasonal_failed = keep_w)
  expect_false("lead_2" %in% names(cyc))
  ex <- excluded_series(cyc)
  expect_equal(ex$name, "lead_2")
  expect_match(ex$reason, "singular")
  expect_true(any(grepl("'lead_2'.*excluded", w)))
  expect_match(cyc$reference@notes, "seasonal", all = FALSE)
  res <- suppressWarnings(cli_build(cand, s))
  expect_false("lead_2" %in% res@assessment@table$name)
  expect_match(audit_log(res)$action[1], "^excluded lead_2")
  expect_output(show(res), paste0("excluded \\(failed seasonal adjustment",
                                  " or too short\\): lead_2"))
  expect_equal(excluded_series(res)$name, "lead_2")
  a <- suppressWarnings(cli_assess(cand, s, cyc))
  expect_false("lead_2" %in% a@table$name)
  # a list of cycles without the record of the exclusion is refused
  expect_error(cli_assess(cand, s, unclass(cyc[names(cyc)])),
               class = "cyclind_error_argument")
  cci <- suppressWarnings(cci_build(cand, s))
  expect_match(audit_log(cci)$action[1], "^excluded lead_2")
  expect_equal(nrow(excluded_series(list())), 0L)
})

test_that("no candidate left after the exclusions stops the build", {
  skip_if_not_installed("seasonal")
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- ARMA ",
                              "covariance matrix is singular"),
    .package = "seasonal")
  cand <- sim_candidates(seed = 1, freq = 4, n_years = 20)
  cand <- cand["lead_2"]
  dat <- cand@data
  dat[, "lead_2"] <- dat[, "lead_2"] * rep(c(1.2, 0.8, 1.1, 0.9),
                                           length.out = nrow(dat))
  cand@data <- dat
  expect_error(suppressWarnings(cli_build(cand, cli_spec(4,
                                          seasonal_adjust = TRUE))),
               "No candidate is left",
               class = "cyclind_error_seasonal_failed")
})

test_that("a reference that cannot be adjusted stops the run", {
  skip_if_not_installed("seasonal")
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- ARMA ",
                              "covariance matrix is singular"),
    .package = "seasonal")
  cand <- sim_candidates(seed = 1, freq = 4, n_years = 20)
  dat <- cand@data
  dat[, "reference"] <- dat[, "reference"] *
    rep(c(1.2, 0.8, 1.1, 0.9), length.out = nrow(dat))
  cand@data <- dat
  expect_error(suppressWarnings(cli_cycles(cand, cli_spec(4,
                                           seasonal_adjust = TRUE))),
               "reference", class = "cyclind_error_seasonal_failed")
})

test_that("system errors still stop a seasonal adjustment", {
  skip_if_not_installed("seasonal")
  local_mocked_bindings(
    seas = function(...) stop("cannot open file 'iofile.spc': ",
                              "No space left on device"),
    .package = "seasonal")
  expect_error(cyclind:::seasonal_adjust_x13(step_pattern(), log = FALSE),
               class = "cyclind_error_system")
})

test_that("real X-13 failures are handled without stopping", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  # no seasonality: kept as it is
  expect_warning(
    pr <- cyclind:::seasonal_adjust_x13(policy_rate(), log = FALSE),
    class = "cyclind_warning_seasonal_failed")
  expect_equal(as.numeric(pr), as.numeric(policy_rate()))
  # fails only with the holiday regressors: adjusted without them
  e <- holiday_effect(idulfitri_dates(2005, 2030), name = "idulfitri")
  expect_warning(
    hs <- cyclind:::seasonal_adjust_x13(holiday_series(e), list(e),
                                        log = TRUE),
    "seasonal ARMA order due to model estimation errors",
    class = "cyclind_warning_seasonal_failed")
  expect_match(attr(hs, "step"), "without holiday regressors idulfitri")
  expect_false(grepl("iofile", attr(hs, "step")))
  # seasonal and no holiday regressors: cannot be adjusted
  expect_error(suppressWarnings(
    cyclind:::seasonal_adjust_x13(step_pattern(), log = FALSE)),
    class = "cyclind_error_seasonal_failed")
})
