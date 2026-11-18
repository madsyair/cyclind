# Moving holidays (Idulfitri) in seasonal adjustment.

test_that("tabular Idulfitri dates are within two days of official ones", {
  official <- as.Date(c("2019-06-05", "2022-05-02", "2023-04-22",
                        "2024-04-10", "2025-03-31"))
  d <- idulfitri_dates(2019, 2025)
  expect_length(d, 7)
  for (k in seq_along(official)) {
    expect_lte(min(abs(as.numeric(d - official[k]))), 2)
  }
  expect_true(all(diff(as.numeric(d)) > 350 & diff(as.numeric(d)) < 360))
})

test_that("holiday_effect validates its input", {
  e <- holiday_effect(idulfitri_dates(2000, 2010), name = "idulfitri")
  expect_s4_class(e, "HolidayEffect")
  expect_output(show(e), "before: days -14 to -1")
  expect_error(holiday_effect("2020-01-01"), class = "cyclind_error_argument")
  expect_error(holiday_effect(idulfitri_dates(2000, 2010), name = "two words"),
               class = "cyclind_error_argument")
  expect_error(holiday_effect(idulfitri_dates(2000, 2010),
                              windows = list(c(3, 1))),
               class = "cyclind_error_argument")
})

test_that("holidays need seasonal adjustment in the specification", {
  e <- holiday_effect(idulfitri_dates(1990, 2030), name = "idulfitri")
  expect_error(cli_spec(holidays = e), "seasonal_adjust")
  expect_error(cli_spec(seasonal_adjust = TRUE, holidays = list(1)),
               "holiday_effect")
  s <- cli_spec(seasonal_adjust = TRUE, holidays = e)
  expect_length(s@holidays, 1)
  expect_output(show(s), "holiday regressors \\(idulfitri\\)")
  expect_output(show(cli_spec(seasonal_adjust = TRUE)), "X-13ARIMA-SEATS")
})

test_that("X-13 removes a simulated Idulfitri effect", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  sim <- simulate_cycles(seed = 5)
  x <- sim[, "lead_3"]
  dates <- idulfitri_dates(1990, 2030)
  days <- seq(as.Date("1995-01-01"), as.Date("2024-12-31"), by = "day")
  eff <- rep(1, length(days))
  for (d in as.list(dates)) eff[days >= d & days <= d + 6] <- 0.6
  y <- x * tapply(eff, format(days, "%Y-%m"), mean)
  e <- holiday_effect(dates, name = "idulfitri")
  adj <- cyclind:::seasonal_adjust_x13(y, list(e))
  expect_match(attr(adj, "step"), "holidays: idulfitri")
  expect_lt(sd(log(adj) - log(x)), sd(log(y) - log(x)))
  # dates that end with the data: X-11 without forecasts
  short <- holiday_effect(idulfitri_dates(1994, 2024), name = "idulfitri")
  expect_warning(adj2 <- cyclind:::seasonal_adjust_x13(y, list(short)),
                 class = "cyclind_warning_holiday_horizon")
  expect_match(attr(adj2, "step"), "X-11 without forecasts")
  late <- holiday_effect(idulfitri_dates(2000, 2030), name = "idulfitri")
  expect_error(cyclind:::seasonal_adjust_x13(y, list(late)),
               class = "cyclind_error_argument")
})

test_that("cli_cycles records the holiday adjustment", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  sim <- simulate_cycles(seed = 1, n_years = 10)
  meta <- data.frame(name = "lead_6", rationale = "early_stage")
  cand <- cli_candidates(sim[, c("reference", "lead_6")], "reference",
                         meta = meta)
  e <- holiday_effect(idulfitri_dates(1990, 2030), name = "idulfitri")
  cyc <- cli_cycles(cand, cli_spec(seasonal_adjust = TRUE, holidays = e))
  expect_match(cyc$lead_6@steps[1], "holidays: idulfitri")
})

test_that("cli_cycles warns once when the holidays miss the X-13 horizon", {
  # audit: holiday dates ending before the X-13 forecast horizon switched
  # every series silently to X-11 without forecasts
  skip_on_cran()
  skip_if_not_installed("seasonal")
  sim <- simulate_cycles(seed = 1, n_years = 10, freq = 4)
  meta <- data.frame(name = c("lead_2", "lead_1"), rationale = "early_stage")
  cand <- cli_candidates(sim[, c("reference", "lead_2", "lead_1")],
                         "reference", meta = meta)
  end_year <- floor(stats::time(sim)[nrow(sim)])
  short <- holiday_effect(idulfitri_dates(1990, end_year + 1),
                          name = "idulfitri")
  spec <- cli_spec(4, seasonal_adjust = TRUE, holidays = short)
  caught <- list()
  cyc <- withCallingHandlers(cli_cycles(cand, spec), warning = function(w) {
    caught[[length(caught) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  horizon <- Filter(function(w) {
    inherits(w, "cyclind_warning_holiday_horizon")
  }, caught)
  expect_length(horizon, 1L)
  msg <- conditionMessage(horizon[[1]])
  expect_match(msg, "3 series (reference, lead_2, lead_1)", fixed = TRUE)
  expect_match(msg, "X-11 without forecast", fixed = TRUE)
  expect_match(msg, sprintf("idulfitri_dates(1990, %d)", end_year + 3),
               fixed = TRUE)
  expect_match(cyc$lead_2@steps[1], "X-11 without forecasts")
  # dates far enough ahead: no warning
  long <- holiday_effect(idulfitri_dates(1990, end_year + 3),
                         name = "idulfitri")
  expect_no_warning(cli_cycles(cand, update(spec, holidays = list(long))),
                    class = "cyclind_warning_holiday_horizon")
})
