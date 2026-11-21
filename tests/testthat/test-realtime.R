# Real-time evaluation (L-M5, S-M2): growing windows and vintages.

test_that("cli_realtime compares end points with the final composite", {
  skip_on_cran()
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand)
  rt <- cli_realtime(res, origins = c("2019-06", 2016 + 5 / 12),
                     reselect = FALSE)
  expect_identical(rt$period, c("2016-06", "2019-06"))
  expect_identical(rt$source, c("window", "window"))
  expect_true(all(rt$same_components))
  expect_identical(rt$end, rt$period)
  # the final value is the full-sample composite at the end date
  fin <- res@composite@cycle
  expect_equal(rt$cli_final[1], as.numeric(fin)[time(fin) > 2016.41 &
                                                  time(fin) < 2016.42])
  expect_true(all(rt$direction_realtime %in% c(-1, 1)))
  expect_identical(rt$same_direction,
                   rt$direction_realtime == rt$direction_final)
  s <- attr(rt, "summary")
  expect_identical(s$n_origins, 2L)
  expect_identical(s$n_compared, 2L)
  expect_true(is.na(s$cor_composite))
  # with the selection run again
  rt2 <- suppressWarnings(cli_realtime(res, origins = "2016-06"))
  expect_identical(nrow(rt2), 1L)
  expect_true(nzchar(rt2$components))
})

test_that("a vintage named by the origin replaces the data", {
  skip_on_cran()
  sim <- simulate_cycles(seed = 1)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  vint <- window(sim, end = c(2016, 6))
  n <- nrow(vint)
  vint[, "lead_6"] <- vint[, "lead_6"] * (1 + 0.05 * (seq_len(n) > n - 12))
  cand <- cli_candidates(sim, "reference", meta = meta,
                         vintages = list("2016-06" = vint[, -8],
                                         "bad name" = vint))
  res <- cli_build(cand)
  rt <- cli_realtime(res, origins = c("2016-06", "2017-06"),
                     reselect = FALSE)
  expect_identical(rt$source, c("vintage", "window"))
  plain <- cli_realtime(cli_build(sim_candidates(seed = 1)),
                        origins = "2016-06", reselect = FALSE)
  expect_false(isTRUE(all.equal(rt$cli_realtime[1], plain$cli_realtime)))
  rc <- cyclind:::realtime_candidates(cand, 2016 + 5 / 12)
  expect_equal(as.numeric(rc$candidates@data[, "lead_6"]),
               as.numeric(vint[, "lead_6"]))
  # columns missing from the vintage keep the final data
  expect_equal(as.numeric(rc$candidates@data[, "noise_2"]),
               as.numeric(window(sim[, "noise_2"], end = c(2016, 6))))
})

test_that("cli_realtime checks its arguments and explains empty rows", {
  skip_on_cran()
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand)
  expect_error(cli_realtime(cand, 2016), class = "cyclind_error_argument")
  expect_error(cli_realtime(res, numeric(0)), class = "cyclind_error_argument")
  expect_error(cli_realtime(res, "1990-01"), "outside the data",
               class = "cyclind_error_argument")
  expect_error(cli_realtime(res, 2016, reselect = NA),
               class = "cyclind_error_argument")
  short <- cli_realtime(res, origins = "1997-01", reselect = FALSE)
  expect_true(nzchar(short$note))
  expect_true(is.na(short$cli_realtime))
  empty <- res
  empty@composite <- NULL
  expect_error(cli_realtime(empty, 2016), class = "cyclind_error_argument")
})
