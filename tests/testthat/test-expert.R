# Expert judgement (ADR-014): reference chronology, edits, outlier
# decisions, the decision log and manual composites. Simulated data only.

ref_tp_of <- function(cand, spec = cli_spec(freq = 12)) {
  cyc <- cli_cycles(cand, spec)
  date_turning_points(spec@dater, cyc$reference@normalised,
                      name = "reference")
}

test_that("parse_period reads months, quarters, Dates and numbers", {
  pp <- cyclind:::parse_period
  expect_equal(pp(c("2008-05", "2008M05", "2008-5")), rep(2008 + 4 / 12, 3))
  expect_equal(pp(c("2008Q2", "2008-Q2")), c(2008.25, 2008.25))
  expect_equal(pp(2008.5), 2008.5)
  expect_equal(pp(c("2008-05", "2008.5")), c(2008 + 4 / 12, 2008.5))
  expect_equal(pp(as.Date("2008-05-15"), 12), 2008 + 4 / 12)
  expect_equal(pp(as.Date("2008-05-15"), 4), 2008.25)
  expect_error(pp("May 2008"), class = "cyclind_error_argument")
  expect_error(pp("2008Q2", 12), class = "cyclind_error_frequency")
  expect_error(pp(NA_real_), class = "cyclind_error_argument")
})

test_that("an algorithm chronology given as reference_tp changes nothing", {
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand)
  res2 <- cli_build(cand, reference_tp = turning_points(res)$reference)
  expect_identical(res2@assessment, res@assessment)
  expect_identical(res2@composite@components, res@composite@components)
  expect_equal(res2@composite@index, res@composite@index)
  expect_equal(res2@validation@turning_points, res@validation@turning_points)
  expect_equal(res2@audit, res@audit)
  expect_s4_class(res2@spec@dater, "ExpertDater")
  # the default run has no expert decisions
  expect_identical(nrow(expert_log(res)), 0L)
  expect_identical(nrow(expert_log(res2)), 1L)
  expect_match(expert_log(res2)$decision, "chronology supplied")
})

test_that("an edited chronology dates the reference everywhere", {
  cand <- sim_candidates(seed = 1)
  tp <- ref_tp_of(cand)
  tp2 <- edit_turning_points(tp, drop = c("2019-02", "2019-09"),
                             reason = "minor cycle below trend",
                             who = "analyst")
  expect_s4_class(tp2, "ExpertChronology")
  expect_identical(length(tp2@index), length(tp@index) - 2L)
  res <- cli_build(cand, reference_tp = tp2)
  ref <- as.data.frame(turning_points(res)$reference)
  expect_false(any(ref$period %in% c("2019-02", "2019-09")))
  expect_identical(res@validation@turning_points$n_ref, length(tp2@index))
  lg <- expert_log(res)
  expect_identical(lg$decision, c("drop trough 2019-02", "drop peak 2019-09"))
  expect_identical(unique(lg$who), "analyst")
  expect_identical(unique(lg$status), "applied")
  expect_true(all(c("step", "object", "decision", "status", "reason", "who",
                    "when") %in% names(lg)))
  # the assessment alone uses it too
  a <- cli_assess(cand, cli_spec(), reference_tp = tp2)
  expect_identical(a@table$n_ref[1], length(tp2@index))
  # and the scorecard and printed result report it
  q <- cli_quality(res)
  expect_true(any(q@table$criterion == "expert decisions (see expert_log())"))
  expect_match(q@note, "2 expert decision")
  expect_output(show(res), "expert decisions: 2")
  expect_output(summary(res), "Expert decisions")
})

test_that("a data frame chronology is read and checked", {
  cand <- sim_candidates(seed = 1)
  tp <- as.data.frame(ref_tp_of(cand))
  df <- data.frame(period = tp$period, type = tp$type,
                   reason = "national accounts", who = "B")
  res <- cli_build(cand, reference_tp = df)
  expect_identical(expert_log(res)$reason, "national accounts")
  # not alternating
  bad <- df
  bad$type[2] <- bad$type[1]
  expect_error(cli_build(cand, reference_tp = bad),
               class = "cyclind_error_turning_points")
  # outside the data
  out <- rbind(df[1, ], df)
  out$period[1] <- "1980-01"
  out$type[1] <- if (df$type[1] == "peak") "trough" else "peak"
  expect_error(cli_build(cand, reference_tp = out),
               "outside the data", class = "cyclind_error_turning_points")
  expect_error(cli_build(cand, reference_tp = data.frame(x = 1)),
               class = "cyclind_error_argument")
  expect_error(cli_build(cand, reference_tp = df[0, ]),
               class = "cyclind_error_turning_points")
  expect_error(cli_assess(cand, cli_spec(), reference_tp = "2001-01"),
               class = "cyclind_error_argument")
  dup <- df[c(1, 1), ]
  expect_error(cyclind:::check_chronology(
    data.frame(time = c(2001, 2001), type = c("peak", "trough"))),
    "same date")
  bad_type <- df
  bad_type$type[1] <- "top"
  expect_error(cli_build(cand, reference_tp = bad_type),
               class = "cyclind_error_turning_points")
})

test_that("edit_turning_points adds, drops and moves with reasons", {
  x <- sine_ts(240, 48)
  tp <- date_turning_points(bry_boschan(), x, name = "ref")
  df <- as.data.frame(tp)
  # move the first turning point by two months
  to <- df$time[1] + 2 / 12
  e1 <- edit_turning_points(tp, move = data.frame(from = df$period[1],
                                                  to = to, reason = "r1"))
  expect_equal(as.data.frame(e1)$time[1], to)
  expect_match(expert_log(e1)$decision, "^move")
  # drop the last pair, then add it back: chained edits keep the log
  n <- nrow(df)
  e2 <- edit_turning_points(e1, drop = df$time[(n - 1):n], reason = "r2")
  e3 <- edit_turning_points(
    e2, add = data.frame(period = df$period[(n - 1):n], type = df$type[(n - 1):n]),
    reason = "r3", who = "C")
  expect_identical(as.data.frame(e3)$type, df$type)
  expect_identical(nrow(expert_log(e3)), 5L)
  expect_identical(expert_log(e3)$reason, c("r1", "r2", "r2", "r3", "r3"))
  expect_output(show(e3), "5 expert decision")
  # one reason per change
  e4 <- edit_turning_points(tp, drop = data.frame(period = df$period[n],
                                                  reason = "own"),
                            move = data.frame(from = df$period[1],
                                              to = to),
                            reason = c("d", "m"))
  expect_identical(expert_log(e4)$reason, c("own", "m"))
})

test_that("edit_turning_points refuses bad edits", {
  x <- sine_ts(240, 48)
  tp <- date_turning_points(bry_boschan(), x, name = "ref")
  df <- as.data.frame(tp)
  expect_error(edit_turning_points(tp, drop = df$period[1]),
               class = "cyclind_error_reason")
  expect_error(edit_turning_points(tp, drop = df$period[1], reason = ""),
               class = "cyclind_error_reason")
  expect_error(edit_turning_points(tp, drop = df$period[2],
                                   reason = c("a", "b")),
               class = "cyclind_error_reason")
  expect_error(edit_turning_points(tp, drop = df$period[2], reason = "x"),
               "alternate", class = "cyclind_error_turning_points")
  expect_error(edit_turning_points(tp, drop = "2001-07", reason = "x"),
               "no turning point", class = "cyclind_error_turning_points")
  expect_error(edit_turning_points(tp, move = data.frame(from = "2001-07",
                                                         to = "2001-08"),
                                   reason = "x"),
               class = "cyclind_error_turning_points")
  expect_error(edit_turning_points(tp, move = data.frame(from = df$period[1],
                                                         to = "2030-01"),
                                   reason = "x"),
               "outside", class = "cyclind_error_turning_points")
  expect_error(edit_turning_points(tp, reason = "x"),
               class = "cyclind_error_argument")
  expect_error(edit_turning_points("tp", drop = 1), class = "cyclind_error_argument")
  expect_error(edit_turning_points(tp, add = "2001-01", reason = "x"),
               class = "cyclind_error_argument")
  expect_error(edit_turning_points(tp, add = data.frame(period = "2001-01"),
                                   reason = "x"),
               class = "cyclind_error_argument")
  expect_error(edit_turning_points(tp, add = data.frame(type = "peak"),
                                   reason = "x"),
               class = "cyclind_error_argument")
  expect_error(edit_turning_points(tp, move = data.frame(from = 1),
                                   reason = "x"),
               class = "cyclind_error_argument")
  expect_error(edit_turning_points(tp, drop = df$period[1], reason = "x",
                                   who = 1),
               class = "cyclind_error_argument")
})

test_that("expert_dater dates only the named series and shows itself", {
  x <- sine_ts(240, 48)
  chron <- data.frame(period = c("2003-01", "2005-01"),
                      type = c("peak", "trough"))
  d <- expert_dater(chron, series = "ref", base = bbq())
  expect_s4_class(d, "Dater")
  tp_ref <- date_turning_points(d, x, name = "ref")
  expect_identical(as.data.frame(tp_ref)$period, c("2003-01", "2005-01"))
  tp_other <- date_turning_points(d, x, name = "other")
  expect_equal(tp_other@index, date_turning_points(bbq(), x)@index)
  # a truncated series keeps the turning points it covers
  short <- window(x, end = 2004)
  expect_identical(length(date_turning_points(d, short, name = "ref")@index),
                   1L)
  # a nested expert dater keeps the base of the inner one
  d2 <- expert_dater(chron, series = "ref", base = d)
  expect_s4_class(d2@base, "BBQ")
  expect_output(show(d), "ExpertDater")
  expect_identical(nrow(expert_log(d)), 1L)
  expect_error(expert_dater(chron, series = 1), class = "cyclind_error_argument")
  spec <- cli_spec(dater = d)
  expect_identical(nrow(expert_log(spec)), 1L)
  expect_equal(cyclind:::resolve_dater(d, 12),
               cyclind:::resolve_dater(bbq(), 12))
})

test_that("the expert chronology reaches cci_build and the selection bias", {
  cand <- sim_candidates(seed = 1)
  tp <- edit_turning_points(ref_tp_of(cand), drop = c("2019-02", "2019-09"),
                            reason = "minor cycle")
  res <- suppressWarnings(cli_build(cand))
  cci <- suppressWarnings(cci_build(res, reference_tp = tp))
  expect_s4_class(cci@spec@dater, "ExpertDater")
  expect_identical(nrow(expert_log(cci)), 2L)
  expect_output(show(cci), "expert decisions")
  sb <- suppressWarnings(cli_selection_bias(
    cand, cci@spec, cutoffs = 2016))
  expect_identical(nrow(sb), 1L)
})

test_that("a manual composite with a reason is logged", {
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand)
  cyc <- cycles(res)
  plain <- cli_composite(cyc, c("lead_6", "lead_3"), res@spec,
                         cyc$reference)
  expect_identical(nrow(expert_log(plain)), 0L)
  man <- cli_composite(cyc, c("lead_6", "lead_3"), res@spec, cyc$reference,
                       shifts = c(lead_3 = 3),
                       reason = "drop the counter-cyclical series",
                       who = "D")
  expect_equal(man@index, cli_composite(cyc, c("lead_6", "lead_3"),
                                        res@spec, cyc$reference,
                                        shifts = c(lead_3 = 3))@index)
  lg <- expert_log(man)
  expect_identical(lg$step, "composite")
  expect_match(lg$decision, "lead_6, lead_3 \\(shifts: lead_3 = 3\\)")
  res@composite <- man
  expect_identical(nrow(expert_log(res)), 1L)
  expect_error(cli_composite(cyc, "lead_6", res@spec, who = "D"),
               class = "cyclind_error_reason")
})

test_that("outlier decisions are checked", {
  dec <- data.frame(series = "a", period = "2020-04", type = "ls",
                    action = "Remove", reason = "lockdown")
  tr <- tramo_outliers(engine = "x13", decisions = dec)
  expect_identical(tr@decisions$type, "LS")
  expect_identical(tr@decisions$action, "remove")
  expect_equal(tr@decisions$time, 2020.25)
  expect_match(tr@label, "1 expert decision")
  lg <- expert_log(cli_spec(outliers = tr))
  expect_identical(lg$status, "pending")
  expect_identical(lg$decision, "remove LS 2020-04")
  expect_error(tramo_outliers(decisions = data.frame(series = "a")),
               class = "cyclind_error_argument")
  bad <- dec
  bad$type <- "XX"
  expect_error(tramo_outliers(decisions = bad), class = "cyclind_error_argument")
  bad <- dec
  bad$action <- "drop"
  expect_error(tramo_outliers(decisions = bad), class = "cyclind_error_argument")
  bad <- dec
  bad$reason <- NA
  expect_error(tramo_outliers(decisions = bad), class = "cyclind_error_reason")
  expect_error(tramo_outliers(decisions = rbind(dec, dec)),
               "two rows", class = "cyclind_error_argument")
  q <- data.frame(series = "a", time = 2020.25, type = "AO",
                  action = "keep", reason = "r")
  expect_identical(tramo_outliers(decisions = q)@decisions$period, "2020.25")
  expect_error(oecd_spec(decisions = dec, outliers = tr),
               class = "cyclind_error_argument")
  expect_identical(nrow(oecd_spec(decisions = dec)@outliers@decisions), 1L)
})

test_that("outlier decisions override the global rule for one outlier", {
  # a pure function test with a known fit: an AO and an LS
  x <- ts(rep(0, 24), start = 2000, frequency = 12)
  tab <- data.frame(index = c(5L, 12L), type = c("AO", "LS"),
                    removed = c(TRUE, FALSE), coef = c(2, 3))
  pairs <- cyclind:::collinear_pairs(tab)
  dec <- cyclind:::check_outlier_decisions(data.frame(
    series = "s", period = c("2000-12", "2000-05"), type = c("LS", "AO"),
    action = c("remove", "keep"), reason = "r"))
  out <- cyclind:::apply_outlier_decisions(tab, pairs, dec, "s", x, 0.7, 2)
  expect_identical(out$removed, c(FALSE, TRUE))
  expect_equal(out$effect, -2 * (seq_len(24) == 5) + 3 * (seq_len(24) >= 12))
  expect_equal(out$forecast, c(3, 3))
  expect_match(out$step, "removed LS 2000-12, kept AO 2000-05")
  # other series and no decisions: no change
  none <- cyclind:::apply_outlier_decisions(tab, pairs, dec, "t", x, 0.7, 2)
  expect_identical(none$removed, tab$removed)
  expect_equal(none$effect, numeric(24))
  # a decision that matches no outlier warns and lists those found
  dec2 <- cyclind:::check_outlier_decisions(data.frame(
    series = "s", period = "2000-03", type = "TC", action = "remove",
    reason = "r"))
  expect_warning(cyclind:::apply_outlier_decisions(tab, pairs, dec2, "s", x,
                                                   0.7, 0),
                 "AO 2000-05, LS 2000-12",
                 class = "cyclind_warning_outlier_decision")
  # AO and TC on the same date: the rewritten effects (ADR-012)
  tab2 <- data.frame(index = c(5L, 5L), type = c("AO", "TC"),
                     removed = c(TRUE, FALSE), coef = c(2, -1))
  pairs2 <- cyclind:::collinear_pairs(tab2)
  e_ao <- cyclind:::single_outlier_effect(tab2, 1L, pairs2, 10, 0.5)
  e_tc <- cyclind:::single_outlier_effect(tab2, 2L, pairs2, 10, 0.5)
  tt <- seq_len(10)
  expect_equal(e_ao + e_tc, 2 * (tt == 5) - 1 * ifelse(tt >= 5,
                                                       0.5^(tt - 5), 0))
  expect_equal(e_ao, (2 - 1) * (tt == 5))
  # X-13 writes a level shift as -1 before its date
  tab3 <- data.frame(index = 4L, type = "LS", removed = FALSE, coef = 2)
  expect_equal(cyclind:::single_outlier_effect(tab3, 1L, pairs2, 6, 0.7,
                                               ls_before = TRUE),
               c(-2, -2, -2, 0, 0, 0))
  tab2$coef <- NULL
  expect_error(cyclind:::single_outlier_effect(tab2, 1L, pairs2, 10, 0.5),
               class = "cyclind_error_outliers")
})

test_that("an outlier decision equals the global rule for that outlier", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  set.seed(5)
  x <- stats::ts(cumsum(rnorm(240, 0, 0.004)) + 4.6, start = c(2000, 1),
                 frequency = 12)
  x[200:240] <- x[200:240] + 0.08
  rule <- treat_outliers(tramo_outliers(engine = "x13",
                                        remove = c("AO", "LS")), x)
  skip_if(!grepl("LS 1", rule$step), "X-13 found exactly one level shift")
  base <- treat_outliers(tramo_outliers(engine = "x13", remove = "AO"), x)
  ls_at <- rule$table$index[rule$table$type == "LS"]
  per <- cyclind:::period_label(as.numeric(time(x))[ls_at], 12)
  dec <- data.frame(series = "s", period = per, type = "LS",
                    action = "remove", reason = "test")
  out <- treat_outliers(tramo_outliers(engine = "x13", remove = "AO",
                                       decisions = dec), x, name = "s")
  expect_equal(as.numeric(out$x), as.numeric(rule$x), tolerance = 1e-6)
  expect_equal(out$forecast, rule$forecast, tolerance = 1e-6)
  expect_identical(out$n, rule$n)
  expect_match(out$step, "expert decisions: removed LS")
  # without the series name the decisions are not applied
  same <- treat_outliers(tramo_outliers(engine = "x13", remove = "AO",
                                        decisions = dec), x)
  expect_equal(as.numeric(same$x), as.numeric(base$x))
})

test_that("outlier decisions reach the cycles and the log of a result", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  sim <- simulate_cycles(seed = 1)
  sim[300:nrow(sim), "lead_3"] <- sim[300:nrow(sim), "lead_3"] + 15
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim[, c("reference", "lead_6", "lead_3")],
                         "reference", meta = meta[1:2, ])
  spec <- cli_spec(outliers = tramo_outliers(engine = "x13"))
  cyc <- suppressWarnings(cli_cycles(cand, spec))
  ls <- cyc$lead_3@outliers[cyc$lead_3@outliers$type == "LS", ]
  skip_if(nrow(ls) == 0L, "X-13 found no level shift")
  dec <- data.frame(series = c("lead_3", "lead_3", "nope"),
                    period = c(ls$period[1], "1996-01", "1996-01"),
                    type = "LS", action = "remove",
                    reason = "test", who = "E")
  spec2 <- cli_spec(outliers = tramo_outliers(engine = "x13",
                                              decisions = dec))
  expect_warning(expect_warning(
    cyc2 <- cli_cycles(cand, spec2), "not in the data: nope"),
    "1996-01", class = "cyclind_warning_outlier_decision")
  o2 <- cyc2$lead_3@outliers
  expect_true(o2$removed[o2$type == "LS" & o2$period == ls$period[1]])
  expect_true(any(grepl("expert decisions", cyc2$lead_3@steps)))
  expect_true(any(grepl("1996-01", cyc2$lead_3@notes)))
  lg <- cyclind:::result_decisions(spec2, cyc2)
  expect_identical(lg$status, c("applied", "not matched", "not matched"))
  expect_identical(unique(lg$who), "E")
})
