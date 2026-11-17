# Transparent outliers (butir 1) and visible X-13 failures (butir 3).

spiky <- function(at = c(40, 80), n = 120) {
  x <- ts(10 + sin(2 * pi * (1:n) / 40), frequency = 12, start = 2000)
  x[at] <- x[at] + 5
  x
}

test_that("robust outliers report their dates and type", {
  x <- spiky()
  out <- treat_outliers(robust_outliers(5), x)
  expect_equal(out$n, 2L)
  expect_equal(out$table$index, c(40L, 80L))
  expect_equal(out$table$type, c("AO", "AO"))
  expect_true(all(out$table$removed))
  none <- treat_outliers(robust_outliers(Inf), x)
  expect_equal(nrow(none$table), 0L)
})

test_that("cycles keep the outliers of each series", {
  cand <- sim_candidates(seed = 2)
  dat <- cand@data
  dat[50, "lead_6"] <- dat[50, "lead_6"] * 3
  cand@data <- dat
  cyc <- cli_cycles(cand, cli_spec(12))
  ol <- cyc$lead_6@outliers
  expect_s3_class(ol, "data.frame")
  expect_named(ol, c("time", "period", "type", "removed"))
  expect_true(stats::time(dat)[50] %in% ol$time)
  expect_equal(nrow(cyc$reference@outliers), 0L)
  expect_output(show(cyc$lead_6), "outliers: 1 replaced")
  rep <- outlier_report(cyc)
  expect_named(rep, c("name", "n_removed", "n_kept", "share", "periods",
                      "types", "note"))
  expect_equal(rep$n_removed[rep$name == "lead_6"], 1L)
})

test_that("a high share of outliers gives a warning that is recorded", {
  cand <- sim_candidates(seed = 2)
  dat <- cand@data
  for (i in c(30, 60, 90, 120)) dat[i, "lead_6"] <- dat[i, "lead_6"] * 3
  cand@data <- dat
  s <- cli_spec(12, outliers = robust_outliers(5, max_share = 0.01))
  expect_warning(cyc <- cli_cycles(cand, s),
                 class = "cyclind_warning_outlier_share")
  expect_match(cyc$lead_6@notes, "share of outliers")
  expect_error(robust_outliers(5, max_share = 0),
               class = "cyclind_error_argument")
  expect_error(tramo_outliers(max_share = 2),
               class = "cyclind_error_argument")
})

test_that("the OECD 2017 default removes additive outliers only", {
  expect_equal(tramo_outliers()@remove, "AO")
  expect_equal(oecd_spec()@outliers@remove, "AO")
  s <- oecd_spec(outlier_types = c("AO", "TC"))
  expect_equal(s@outliers@remove, c("AO", "TC"))
  expect_error(oecd_spec(outlier_types = "XX"),
               class = "cyclind_error_argument")
})

test_that("X-13 and TRAMO outlier names are read into dates", {
  x <- ts(1:120, frequency = 12, start = 2010)
  tab <- cyclind:::x13_outlier_table(c("AO2012.Jun", "LS2016.Aug",
                                       "AR-Nonseasonal-01"), x)
  expect_equal(tab$index, c(30L, 80L))
  expect_equal(tab$type, c("AO", "LS"))
  q <- ts(1:60, frequency = 4, start = 2005)
  tq <- cyclind:::x13_outlier_table("TC2009.4", q)
  expect_equal(tq$index, 20L)
  tt <- cyclind:::tramo_outlier_table(c("AO (5-2011)", "LS (IV-2009)"),
                                      ts(1:80, frequency = 12, start = 2008))
  expect_equal(tt$index, c(41L, 16L))
  expect_equal(nrow(cyclind:::x13_outlier_table(character(0), x)), 0L)
})

test_that("X-13 outliers are reported with the types kept", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  set.seed(5)
  x <- stats::ts(cumsum(rnorm(240, 0, 0.004)) + 4.6, start = c(2000, 1),
                 frequency = 12)
  x[100] <- x[100] + 0.1
  x[200:240] <- x[200:240] + 0.08
  out <- treat_outliers(tramo_outliers(engine = "x13"), x)
  expect_true(100L %in% out$table$index[out$table$type == "AO"])
  expect_true(all(out$table$removed == (out$table$type == "AO")))
})

# ---- butir 3: X-13 failures -------------------------------------------------

test_that("a failed X-13 model warns explicitly and is recorded", {
  skip_if_not_installed("seasonal")
  x <- ts(cumsum(rnorm(60)), frequency = 12)
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- Estimation ",
                              "failed to converge"),
    .package = "seasonal")
  expect_warning(f <- cyclind:::regarima_x13(x, 3, NA),
                 class = "cyclind_warning_x13")
  expect_match(f$failed, "converge")
  cand <- sim_candidates(seed = 2)
  s <- cli_spec(12, outliers = tramo_outliers(engine = "x13", forecast = 0))
  w <- character(0)
  cyc <- withCallingHandlers(
    cli_cycles(cand, s),
    cyclind_warning_x13 = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("'lead_6'", w)))
  expect_match(cyc$lead_6@notes, "X-13", all = FALSE)
  expect_match(outlier_report(cyc)$note[2], "X-13")
})

test_that("system errors of X-13 stop the run", {
  skip_if_not_installed("seasonal")
  x <- ts(cumsum(rnorm(60)), frequency = 12)
  local_mocked_bindings(
    seas = function(...) stop("cannot open file 'iofile.spc': ",
                              "No space left on device"),
    .package = "seasonal")
  expect_error(cyclind:::regarima_x13(x, 3, NA),
               class = "cyclind_error_system")
  expect_error(cyclind:::log_test(exp(x / 10), "x13"),
               class = "cyclind_error_system")
  expect_error(cyclind:::seasonal_adjust_x13(exp(x / 10)),
               class = "cyclind_error_system")
})

test_that("a failed log/level test warns and keeps levels", {
  skip_if_not_installed("seasonal")
  x <- ts(exp(cumsum(rnorm(60, 0, 0.01))), frequency = 12)
  local_mocked_bindings(
    seas = function(...) stop("X-13 run failed\n\nErrors:\n- bad model"),
    .package = "seasonal")
  expect_warning(out <- cyclind:::apply_transform(x, "test", "x", "x13"),
                 class = "cyclind_warning_x13")
  expect_match(attr(out, "step"), "levels")
  expect_error(cyclind:::seasonal_adjust_x13(x),
               class = "cyclind_error_x13")
})

test_that("system and model failures are told apart", {
  kind <- cyclind:::x13_failure_kind
  expect_equal(kind(simpleError("No space left on device")), "system")
  expect_equal(kind(simpleError("Permission denied")), "system")
  expect_equal(kind(simpleError("X-13 binary not found")), "system")
  expect_equal(kind(simpleError("X-13 run failed: convergence")), "model")
})

test_that("results show the outliers", {
  cand <- sim_candidates(seed = 2)
  dat <- cand@data
  dat[50, "lead_6"] <- dat[50, "lead_6"] * 3
  cand@data <- dat
  res <- suppressWarnings(cli_build(cand))
  expect_output(show(res), "outliers: 1 replaced in 1 series")
  expect_equal(outlier_report(res)$n_removed[outlier_report(res)$name ==
                                                "lead_6"], 1L)
  expect_error(outlier_report(1), class = "cyclind_error_argument")
})
