# Fixes from the audit of 2026-11-18 (INC-21). Each block names the
# finding (S = statistical, E = engineering).

# ---- S1: the Christiano-Fitzgerald filter removes the drift ---------------

test_that("S1: the CF random-walk filter removes the drift by default", {
  x <- ts(0.01 * (1:120), frequency = 4, start = 1990)
  out <- extract_cycle(cf_filter(), x)
  expect_lt(max(abs(out)), 1e-10)
  # the old filter, without drift removal, is still available
  old <- extract_cycle(cf_filter(drift = FALSE), x)
  expect_gt(max(abs(old)), 1e-3)
  expect_true(cf_filter()@drift)
  expect_error(cf_filter(drift = NA), class = "cyclind_error_argument")
  expect_error(cf_filter(drift = "yes"), class = "cyclind_error_argument")
})

test_that("S1: the CF filter with drift replicates mFilter", {
  skip_if_not_installed("mFilter")
  set.seed(12)
  x <- ts(cumsum(rnorm(200, 0.3)) + 100, frequency = 4, start = 1970)
  ours <- extract_cycle(cf_filter(6, 32), x)
  ref <- mFilter::cffilter(x, pl = 6, pu = 32, root = TRUE,
                           drift = TRUE)$cycle
  expect_equal(as.numeric(ours), as.numeric(ref), tolerance = 1e-8)
})

test_that("S1: CF filters saved before the drift slot remove the drift", {
  f <- cf_filter()
  attr(f, "drift") <- NULL
  expect_false(methods::.hasSlot(f, "drift"))
  x <- ts(0.01 * (1:120), frequency = 4)
  expect_lt(max(abs(extract_cycle(f, x))), 1e-10)
})

# ---- S3: QS statistic is zero when the seasonal autocorrelation is <= 0 ---

test_that("S3: the QS statistic is 0 when r_s is not positive (X-13)", {
  set.seed(101)
  z <- ts(cumsum(rnorm(120)), frequency = 12, start = 2000)
  r <- stats::acf(diff(z), lag.max = 24, plot = FALSE)$acf[-1]
  expect_lt(r[12], 0)
  expect_gt(r[24], 0)
  qs <- cyclind:::qs_test(z)
  expect_equal(qs$statistic, 0)
  expect_equal(qs$p_value, 1)
  # a seasonal series still gives a large statistic
  s <- ts(rep(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5), 10) +
            cumsum(rnorm(120, 0, 0.1)), frequency = 12)
  expect_gt(cyclind:::qs_test(s)$statistic, 10)
})

# ---- S5: Conference Board changes of series in percent ---------------------

test_that("S5: Conference Board uses simple differences for percent series", {
  expect_equal(cyclind:::symmetric_change(c(4, 5, 6), percent = TRUE),
               c(NA, 1, 1))
  expect_equal(cyclind:::symmetric_change(c(4, 5)),
               c(NA, 200 * 1 / 9))
  sim <- simulate_cycles(seed = 3)
  # an interest rate in percent, positive throughout
  sim[, "lead_3"] <- 5 + (sim[, "lead_3"] - mean(sim[, "lead_3"])) / 10
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage",
                     unit = c("level", "percent", rep("level", 5)))
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(12, aggregator = conference_board())
  cyc <- cli_cycles(cand, spec)
  expect_true("unit: percent" %in% cyc$lead_3@steps)
  expect_false("unit: percent" %in% cyc$lead_6@steps)
  cb <- cli_composite(cyc, c("lead_6", "lead_3"), spec, cyc$reference)
  lv <- cyclind:::cycle_level(cyc$lead_3)
  ch <- cbind(cyclind:::symmetric_change(cyclind:::cycle_level(cyc$lead_6)),
              c(NA, diff(lv)))
  s <- apply(ch, 2, stats::sd, na.rm = TRUE)
  w <- (1 / s) / sum(1 / s)
  expect_equal(unname(cb@weights), unname(w), tolerance = 1e-12)
})

# ---- S6: internal gaps of the Conference Board index -----------------------

test_that("S6: a gap in the Conference Board contributions stays missing", {
  sim <- simulate_cycles(seed = 3)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(12, aggregator = conference_board(min_coverage = 1))
  cyc <- cli_cycles(cand, spec)
  gap <- 150:152
  cyc$lead_6@normalised[gap] <- NA
  cyc$lead_6@transformed[gap] <- NA
  cb <- cli_composite(cyc, c("lead_6", "lead_3"), spec, cyc$reference)
  # the change into and out of the gap is unknown
  expect_true(all(is.na(cb@index[gap])))
  expect_true(all(is.na(cb@cycle[gap])))
  expect_false(anyNA(cb@index[c(1:149, 154:length(cb@index))][-1]))
  expect_false(anyNA(cb@cycle[c(1:149, 154:length(cb@cycle))][-1]))
})

# ---- S4: end rule of Bry and Boschan (1971) --------------------------------

test_that("S4: a first peak lower than an earlier value is dropped", {
  y <- sin(2 * pi * (1:120) / 40)
  y[1:4] <- c(0, 2, 2.5, 2)
  x <- ts(y, frequency = 12, start = 2000)
  tp <- date_turning_points(bry_boschan(), x)
  # y[3] = 2.5 exceeds the peak at i = 10 although y[1] = 0 does not
  expect_false(10L %in% tp@index)
  expect_equal(tp@index[1], 30L)
  expect_equal(tp@type[1], "trough")
  # the same at the end, for a trough
  rev_tp <- date_turning_points(bry_boschan(), ts(-rev(y), frequency = 12))
  expect_false((121L - 10L) %in% rev_tp@index)
  # the BBQ phase rule keeps the end rule of 'BCDating' (y[1] and y[n])
  old <- date_turning_points(bry_boschan(phase_rule = "bbq"), x)
  expect_equal(old@index[1], 10L)
})
