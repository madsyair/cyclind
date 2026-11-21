# Specification of the OECD options (development version 0.1.3), following
# Gyomai and
# Guidetti (2012), "OECD System of Composite Leading Indicators":
#
# * Classification (p. 9): by the median lead/lag at turning points;
#   coincident within +/- 2 months, leading from 2 months (shorter/medium
#   up to 8, longer over 8), lagging at -2 months or less. Thresholds are
#   in months and converted to periods (x freq / 12).
# * Aggregation (p. 9): average the growth rates of the components, chain
#   the average growth into the index; a date is computed only when at least
#   60 percent of the components are available. Components are the
#   normalised cycles plus 100 (p. 7).
# * Lag-shifting (p. 9): components are lagged so that their leads align;
#   here every component is moved later by (its median lead - the shortest
#   median lead), rounded to whole periods.
# * Outliers (p. 6-7): TRAMO detects additive outliers, transitory changes
#   and level shifts and the outliers are replaced by estimated values;
#   TRAMO also decides log or levels and gives short-horizon forecasts that
#   extend the series before the HP filter.
# * Presentation (p. 9-11): amplitude-adjusted CLI, trend-restored CLI
#   (trend of the reference times the amplitude-adjusted CLI / 100) and its
#   12-month rate of change.

spec <- cli_spec(12)
cyc <- cli_cycles(sim_candidates(seed = 1), spec)
comps <- c("lead_6", "lead_3")

row_with <- function(median_lead, n_matched = 5) {
  data.frame(name = "x", n_ref = 6, n_matched = n_matched, missed = 1,
             extra = 0, median_lead = median_lead, mean_lead = median_lead,
             sd_lead = 1, pct_led = 0.8, ccf_lag = median_lead, ccf_r = 0.7,
             concordance = 0.8, concordance_expected = 0.5,
             concordance_p = 0.01, granger_p = 0.01)
}

test_that("the OECD classifier follows the median-lead table", {
  s <- update(spec, classifier = oecd_classifier())
  cls <- function(l, ...) cyclind:::classify_row(row_with(l, ...), s)$class
  expect_equal(cls(6), "leading")
  expect_equal(cls(2), "leading")
  expect_equal(cls(12), "leading")
  expect_equal(cls(1.5), "coincident")
  expect_equal(cls(0), "coincident")
  expect_equal(cls(-1.5), "coincident")
  expect_equal(cls(-2), "lagging")
  expect_equal(cls(-7), "lagging")
  expect_equal(cls(6, n_matched = 2), "rejected")
  expect_equal(cls(NA), "rejected")
  expect_match(cyclind:::classify_row(row_with(12), s)$reasons, "longer")
  expect_equal(cyclind:::classify_row(row_with(5), s)$reasons, "")
})

test_that("OECD thresholds are in months and follow the frequency", {
  sq <- cli_spec(4, classifier = oecd_classifier())
  cls <- function(l) cyclind:::classify_row(row_with(l), sq)$class
  expect_equal(cls(1), "leading")       # 3 months
  expect_equal(cls(0.5), "coincident")  # 1.5 months
  expect_equal(cls(-1), "lagging")
  expect_error(oecd_classifier(coincident = -1),
               class = "cyclind_error_argument")
  expect_error(oecd_classifier(coincident = 9, long = 8),
               class = "cyclind_error_argument")
})

test_that("an OECD coincident series needs its cross-correlation peak near 0", {
  # audit: a quarterly index with a coincident median lead but its
  # cross-correlation peak at lag -8 was accepted as coincident
  s <- update(spec, classifier = oecd_classifier())
  sq <- cli_spec(4, classifier = oecd_classifier())
  far <- transform(row_with(0), ccf_lag = -8)
  out <- cyclind:::classify_row(far, sq)
  expect_equal(out$class, "rejected")
  expect_match(out$reasons, "ccf lag")
  expect_equal(cyclind:::classify_row(far, s)$class, "rejected")
  expect_equal(cyclind:::classify_row(transform(far, ccf_lag = 1), s)$class,
               "coincident")
  expect_equal(cyclind:::classify_row(transform(far, ccf_lag = 1), sq)$class,
               "rejected")
  # the median-lead table taken literally keeps the old rule
  lit <- update(sq, classifier = oecd_classifier(conformity = FALSE))
  expect_equal(cyclind:::classify_row(far, lit)$class, "coincident")
  # the leading and lagging rules are unchanged
  expect_equal(cyclind:::classify_row(transform(row_with(6), ccf_lag = -8),
                                      s)$class, "leading")
  expect_equal(cyclind:::classify_row(transform(row_with(-6), ccf_lag = 8),
                                      s)$class, "lagging")
  # the same condition in the validation of a coincident index
  chk <- coincident_criteria(sq@classifier, far, sq)
  expect_named(chk, c("matched", "ccf", "timing"))
  expect_false(chk[["ccf"]])
  expect_true(chk[["timing"]])
  expect_true(coincident_criteria(sq@classifier, row_with(0), sq)[["ccf"]])
  expect_false(coincident_criteria(
    sq@classifier, transform(row_with(0), ccf_r = -0.5), sq)[["ccf"]])
  expect_named(coincident_criteria(lit@classifier, far, lit),
               c("matched", "timing"))
})

test_that("the default classifier is unchanged", {
  expect_s4_class(spec@classifier, "CyclindClassifier")
  r <- row_with(6)
  expect_equal(cyclind:::classify_row(r, spec)$class, "leading")
  r$granger_p <- 0.5
  expect_equal(cyclind:::classify_row(r, spec)$class, "rejected")
  expect_equal(cyclind:::classify_row(r, update(spec,
    classifier = oecd_classifier()))$class, "leading")
})

# O-M1 (review of the OECD expert, 2026-11): the rule of the OECD documents.
# The class follows the median lead alone (GG12 p. 9), without the
# thresholds of cyclind (three matched turning points, 40 percent missed or
# extra, concordance); the only cross-check the documents write is that the
# lead of the cross-correlation peak "should not be too different from the
# median lead" (GG12 p. 8; GAA17 p. 286), here within `ccf_tolerance`
# months (a choice of cyclind; the OECD gives no number).
test_that("O-M1: oecd_classifier(rule = 'document') reads the median lead", {
  d <- oecd_classifier(rule = "document")
  expect_s4_class(d, "OecdDocumentClassifier")
  expect_s4_class(d, "OecdClassifier")
  expect_equal(d@ccf_tolerance, 6)
  expect_equal(d@min_matched, 1)
  s <- update(spec, classifier = d)
  cls <- function(r, sp = s) cyclind:::classify_row(r, sp)
  # none of the cyclind thresholds: one matched turning point, many missed
  # and extra ones, no significant concordance, a weak correlation
  loose <- transform(row_with(6, n_matched = 1), missed = 4, extra = 6,
                     concordance_p = 0.9, ccf_r = 0.1)
  expect_equal(cls(loose)$class, "leading")
  expect_equal(cls(loose)$reasons, "")
  expect_equal(cyclind:::classify_row(loose, update(
    spec, classifier = oecd_classifier()))$class, "rejected")
  # the median-lead table of GG12 p. 9
  expect_equal(cls(row_with(12))$class, "leading")
  expect_match(cls(row_with(12))$reasons, "longer")
  expect_equal(cls(row_with(1.5))$class, "coincident")
  expect_equal(cls(row_with(-3))$class, "lagging")
  expect_equal(cls(row_with(NA))$class, "rejected")
  expect_equal(cls(row_with(6, n_matched = 0))$class, "rejected")
  # a long median lead with the cross-correlation peak at lag 0
  far <- transform(row_with(10.5), ccf_lag = 0)
  expect_equal(cls(far)$class, "rejected")
  expect_equal(cls(far)$reasons, "ccf lag")
  expect_equal(cyclind:::classify_row(far, update(
    spec, classifier = oecd_classifier()))$class, "leading")
  expect_equal(cls(transform(row_with(10.5), ccf_lag = 4.5))$class,
               "leading")
  expect_equal(cls(transform(row_with(-1), ccf_lag = 8))$class, "rejected")
  expect_equal(cls(transform(row_with(6), ccf_lag = NA))$class, "rejected")
  # the tolerance is in months and can be changed
  s3 <- update(spec, classifier = oecd_classifier(rule = "document",
                                                  ccf_tolerance = 3))
  expect_equal(cls(transform(row_with(10.5), ccf_lag = 4.5), s3)$class,
               "rejected")
  # quarterly: 6 months are 2 quarters (review L-M1: lead 3.5 quarters with
  # the peak at 0 fails, lead 3 with the peak at 1 passes)
  sq <- cli_spec(4, classifier = d)
  expect_equal(cls(transform(row_with(3.5), ccf_lag = 0), sq)$class,
               "rejected")
  expect_equal(cls(transform(row_with(3), ccf_lag = 1), sq)$class, "leading")
  expect_equal(cls(transform(row_with(0.5), ccf_lag = 0), sq)$class,
               "coincident")
  # criteria, scorecard and coincident validation report the cross-check
  chk <- leading_criteria(d, far, s)
  expect_named(chk, c("matched", "lead", "ccf_fit"))
  expect_equal(unname(chk), c(TRUE, TRUE, FALSE))
  q <- cyclind:::quality_criteria(d, far, s)
  fit <- q[q$criterion == "cross-correlation lead minus median lead", ]
  expect_equal(fit$value, -10.5)
  expect_false(fit$passed)
  expect_equal(fit$threshold, "within +/- 6")
  cc <- coincident_criteria(d, transform(row_with(0), ccf_lag = -8), s)
  expect_named(cc, c("matched", "ccf", "timing"))
  expect_false(cc[["ccf"]])
  expect_true(coincident_criteria(d, row_with(0), s)[["ccf"]])
  rules <- cyclind:::coincident_quality(d, row_with(0), sq)
  expect_true(any(grepl("median lead| <= 6 months", rules$threshold,
                        fixed = TRUE)))
  # printing
  expect_output(show(d), "OECD documents")
  expect_output(show(d), "6 months")
  expect_output(show(s), "cross-correlation peak within 6 months")
  # arguments
  expect_error(oecd_classifier(rule = "document", ccf_tolerance = -1),
               class = "cyclind_error_argument")
  expect_error(oecd_classifier(rule = "document", conformity = TRUE),
               class = "cyclind_error_argument")
  expect_error(oecd_classifier(rule = "other"))
  expect_equal(oecd_classifier(rule = "document", min_matched = 3)@min_matched,
               3)
  expect_s4_class(oecd_spec(classifier = d)@classifier,
                  "OecdDocumentClassifier")
})

test_that("O-M1: the default OECD classifier is unchanged", {
  expect_identical(oecd_classifier(rule = "cyclind"), oecd_classifier())
  expect_identical(class(oecd_classifier())[[1]], "OecdClassifier")
  expect_identical(
    oecd_classifier(),
    new("OecdClassifier",
        label = "OECD: median lead at turning points, cyclical conformity",
        coincident = 2, long = 8, min_matched = 3, conformity = TRUE))
  expect_false(methods::.hasSlot(oecd_classifier(), "ccf_tolerance"))
})

test_that("OECD aggregation chains the average growth of the components", {
  s <- update(spec, aggregator = oecd_aggregate(growth = "ratio"))
  z <- cbind(a = c(1, 2, 1, 0), b = c(0, 1, 3, 2))
  out <- aggregate_components(s@aggregator, z, NULL, s)
  lv <- z + 100
  g <- rowMeans(lv[-1, ] / lv[-4, ] - 1)
  expect_equal(out$cycle, 100.5 * cumprod(c(1, 1 + g)))
  expect_equal(unname(out$weights), c(0.5, 0.5))
  # identical components: the chained index reproduces the component
  same <- cbind(a = c(1, 2, 1, 0), b = c(1, 2, 1, 0))
  expect_equal(aggregate_components(s@aggregator, same, NULL, s)$cycle,
               c(1, 2, 1, 0) + 100)
})

test_that("OECD aggregation uses available growth rates at a ragged edge", {
  s <- update(spec, aggregator = oecd_aggregate(growth = "ratio"))
  z <- cbind(a = c(0, 1, 2, 3), b = c(0, 1, 2, 3), c = c(0, 1, 2, NA))
  out <- aggregate_components(s@aggregator, z, NULL, s)$cycle
  expect_equal(out[4] / out[3], 103 / 102)
  # below 60 percent coverage the index is missing; chaining resumes
  z2 <- cbind(a = c(0, 1, NA, 3, 4), b = c(0, 1, NA, NA, 4),
              c = c(0, 1, NA, 3, 4))
  out2 <- aggregate_components(s@aggregator, z2, NULL, s)$cycle
  expect_true(is.na(out2[3]))
  expect_false(is.na(out2[4]))
  expect_equal(out2[5] / out2[4], 104 / 103)
  expect_true(all(is.na(aggregate_components(
    s@aggregator, cbind(a = c(NA, 1), b = c(NA, NA)), NULL, s)$cycle)))
})

test_that("FR-014.2: oecd_aggregate(growth = 'log') chains average log growth", {
  s <- update(spec, aggregator = oecd_aggregate())
  expect_equal(s@aggregator@growth, "log")
  z <- cbind(a = c(1, 2, 1, 0), b = c(0, 1, 3, 2))
  out <- aggregate_components(s@aggregator, z, NULL, s)
  lv <- z + 100
  g <- rowMeans(log(lv[-1, ] / lv[-4, ]))
  expect_equal(out$cycle, 100.5 * cumprod(c(1, exp(g))), tolerance = 1e-12)
  same <- cbind(a = c(1, 2, 1, 0), b = c(1, 2, 1, 0))
  expect_equal(aggregate_components(s@aggregator, same, NULL, s)$cycle,
               c(1, 2, 1, 0) + 100, tolerance = 1e-12)
  # an inverted component enters as the reciprocal of its original index:
  # its log growth is minus that of the original series (F-10)
  orig <- c(1, 2, 1, 0)
  inv <- cbind(a = orig, b = -orig)
  lev <- matrix(NA_real_, 4, 2)
  attr(lev, "sign") <- c(1, -1)
  out_inv <- aggregate_components(s@aggregator, inv, lev, s)$cycle
  # the two log growth rates cancel: the index stays at its first value,
  # the mean of 101 and the reciprocal index 100^2 / 101
  expect_equal(out_inv, rep((101 + 100^2 / 101) / 2, 4), tolerance = 1e-12)
  # the ragged edge and the 60 percent rule work as with "ratio"
  z2 <- cbind(a = c(0, 1, NA, 3, 4), b = c(0, 1, NA, NA, 4),
              c = c(0, 1, NA, 3, 4))
  out2 <- aggregate_components(s@aggregator, z2, NULL, s)$cycle
  expect_true(is.na(out2[3]))
  expect_equal(out2[5] / out2[4], 104 / 103, tolerance = 1e-12)
})

test_that("P7: the 60 percent rule counts components at t and t - 1", {
  # Gyomai, Ahmad and Astolfi (2017, p. 286, note 8): CLI_t is computed when
  # sum_i w_i delta_{i,t,t-1} >= 60 percent, delta = 1 when component i is
  # available at t and at t - 1. At t = 3 three of five components are
  # available (60 percent) but only one of them also at t = 2 (20 percent).
  z <- cbind(a = c(0, 1, 2, 3), b = c(0, 1, NA, 3), c = c(0, 1, NA, 3),
             d = c(0, NA, 2, 3), e = c(0, NA, 2, 3))
  for (g in c("ratio", "log")) {
    ag <- oecd_aggregate(growth = g)
    out <- aggregate_components(ag, z, NULL, update(spec, aggregator = ag))
    expect_false(is.na(out$cycle[2]))
    expect_true(is.na(out$cycle[3]))
    # the chain restarts at the mean level of the components after the gap
    expect_equal(out$cycle[4], 103)
  }
})

test_that("FR-014.2: growth argument and objects saved before it", {
  expect_error(oecd_aggregate(growth = "geometric"),
               class = "cyclind_error_argument")
  old <- oecd_aggregate()
  attr(old, "growth") <- NULL
  s <- update(spec, aggregator = old)
  z <- cbind(a = c(1, 2, 1, 0), b = c(0, 1, 3, 2))
  expect_equal(aggregate_components(old, z, NULL, s)$cycle,
               aggregate_components(oecd_aggregate(growth = "ratio"), z,
                                    NULL, s)$cycle)
  expect_output(show(oecd_aggregate()), "log")
})

test_that("FR-014.1: amplitude by standard deviations", {
  expect_equal(cli_spec()@amplitude, "mad")
  expect_equal(oecd_spec()@amplitude, "sd")
  expect_error(cli_spec(amplitude = "range"), class = "cyclind_error_argument")
  expect_error(update(spec, amplitude = c("sd", "mad")),
               class = "cyclind_error_argument")
  ref <- cyc$reference
  ci_mad <- cli_composite(cyc, comps, spec, ref)
  ci_sd <- cli_composite(cyc, comps, update(spec, amplitude = "sd"), ref)
  expect_equal(ci_mad@cycle, ci_sd@cycle)
  r <- 100 * as.numeric(ref@cycle)
  c0 <- as.numeric(ci_sd@cycle)
  ok <- !is.na(r) & !is.na(c0)
  expect_equal(ci_sd@amplitude, stats::sd(r[ok]) / stats::sd(c0[ok]))
  expect_equal(stats::sd(as.numeric(ci_sd@index)[ok] - 100),
               stats::sd(r[ok]), tolerance = 1e-10)
  # default unchanged: mean absolute deviation of the reference cycle
  expect_equal(ci_mad@amplitude, mean_abs_dev(r))
  # a specification saved before the slot behaves as "mad"
  old <- spec
  attr(old, "amplitude") <- NULL
  expect_equal(cli_composite(cyc, comps, old, ref)@amplitude,
               ci_mad@amplitude)
  expect_equal(update(old, min_lead = 3)@amplitude, "mad")
  expect_output(show(update(spec, amplitude = "sd")), "standard deviation")
})

test_that("FR-014.3: oecd_spec forecasts two months ahead", {
  expect_equal(oecd_spec()@outliers@forecast, 2)
  expect_equal(oecd_spec(4)@outliers@forecast, 1)
  expect_equal(oecd_spec(forecast = 0)@outliers@forecast, 0)
  expect_equal(oecd_spec(forecast = 12)@outliers@forecast, 12)
  expect_error(oecd_spec(forecast = -1), class = "cyclind_error_argument")
})

test_that("cli_composite applies lag shifts", {
  ci <- cli_composite(cyc, comps, spec, shifts = c(lead_6 = 3, lead_3 = 0))
  z6 <- as.numeric(cyc$lead_6@normalised)
  z3 <- as.numeric(cyc$lead_3@normalised)
  manual <- rowMeans(cbind(cyclind:::shift_vec(z6, 3L), z3))
  expect_equal(as.numeric(ci@cycle), cyclind:::normalise_cycle(manual))
  expect_equal(ci@shifts, c(lead_6 = 3, lead_3 = 0))
  expect_error(cli_composite(cyc, comps, spec, shifts = c(lead_6 = -1)),
               class = "cyclind_error_argument")
  expect_length(cli_composite(cyc, comps, spec)@shifts, 0L)
})

test_that("lag shifts align the median leads in cli_build", {
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand, update(spec, lag_shift = TRUE))
  sh <- res@composite@shifts
  tab <- res@assessment@table
  lead <- tab$median_lead[match(res@composite@components, tab$name)]
  expect_equal(unname(sh), round(lead - min(lead)))
  expect_equal(min(sh), 0)
  expect_match(paste(capture.output(res@composite), collapse = " "),
               "shift")
})

test_that("robust outliers keep the 0.1.1 behaviour", {
  x <- ts(sin(1:120 / 6) + 10, frequency = 12)
  x[60] <- x[60] + 5
  out <- treat_outliers(robust_outliers(5), x)
  expect_equal(out$n, 1L)
  expect_length(out$forecast, 0L)
  expect_equal(cli_spec(12, outlier_threshold = 3)@outliers@threshold, 3)
  expect_equal(update(spec, outlier_threshold = 4)@outliers@threshold, 4)
  expect_error(robust_outliers(0), class = "cyclind_error_argument")
})

test_that("TRAMO-type outliers are removed with X-13 regARIMA", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  set.seed(3)
  x <- ts(log(100 + 10 * sin(2 * pi * (1:180) / 50)) +
            cumsum(rnorm(180, 0, 0.003)), frequency = 12, start = 2000)
  y <- x
  y[90] <- y[90] + 0.15
  out <- treat_outliers(tramo_outliers(engine = "x13", forecast = 12), y)
  expect_gte(out$n, 1L)
  expect_lt(abs(out$x[90] - x[90]), 0.05)
  expect_length(out$forecast, 12L)
  expect_match(out$step, "X-13")
  keep <- treat_outliers(tramo_outliers(engine = "x13", remove = "LS",
                                        forecast = 0), y)
  expect_equal(as.numeric(keep$x)[90], as.numeric(y)[90], tolerance = 1e-6)
  expect_length(keep$forecast, 0L)
})

test_that("TRAMO engine runs through RJDemetra when installed", {
  skip_if_not_installed("RJDemetra")
  skip_on_cran()
  set.seed(3)
  x <- ts(log(100 + 10 * sin(2 * pi * (1:180) / 50)) +
            cumsum(rnorm(180, 0, 0.003)), frequency = 12, start = 2000)
  x[90] <- x[90] + 0.15
  out <- treat_outliers(tramo_outliers(engine = "tramo", forecast = 6), x)
  expect_gte(out$n, 1L)
  expect_length(out$forecast, 6L)
  expect_match(out$step, "TRAMO")
  expect_true(cyclind:::log_test(exp(x), "tramo"))
})

test_that("engine auto falls back to X-13 without RJDemetra", {
  local_mocked_bindings(has_rjdemetra = function() FALSE,
                        has_seasonal = function() TRUE)
  expect_equal(cyclind:::resolve_engine("auto"), "x13")
  expect_error(cyclind:::resolve_engine("tramo"),
               class = "cyclind_error_dependency")
  local_mocked_bindings(has_rjdemetra = function() TRUE)
  expect_equal(cyclind:::resolve_engine("auto"), "tramo")
})

test_that("a failed regARIMA fit falls back with a warning", {
  x <- ts(cumsum(rnorm(60)), frequency = 12)
  expect_warning(out <- cyclind:::regarima_failed(x, 6),
                 class = "cyclind_warning_outliers")
  expect_true(all(out$effects == 0))
  expect_length(out$forecast, 6L)
  expect_equal(sum(out$count), 0)
  skip_if_not_installed("seasonal")
  local_mocked_bindings(seas = function(...) stop("no convergence"),
                        .package = "seasonal")
  expect_warning(f <- cyclind:::regarima_x13(x, 3, NA),
                 class = "cyclind_warning_outliers")
  expect_length(f$forecast, 3L)
})

test_that("tramo_outliers validates its arguments", {
  expect_error(tramo_outliers(remove = "XX"), class = "cyclind_error_argument")
  expect_error(tramo_outliers(forecast = -1), class = "cyclind_error_argument")
  expect_error(tramo_outliers(critical = 0), class = "cyclind_error_argument")
  expect_s4_class(tramo_outliers(), "TramoOutliers")
})

test_that("forecasts extend the series before the cycle filter", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  cand <- sim_candidates(seed = 2)
  s <- update(spec, outliers = tramo_outliers(engine = "x13", forecast = 12))
  cs <- cli_cycles(cand, s)$lead_6
  expect_true(any(grepl("12-period forecasts", cs@steps)))
  expect_equal(length(cs@cycle), length(cand@data[, "lead_6"]))
})

test_that("transform = 'test' lets the regARIMA test choose logs", {
  skip_if_not_installed("seasonal")
  skip_on_cran()
  ap <- AirPassengers
  expect_true(cyclind:::log_test(ap, "x13"))
  out <- cyclind:::apply_transform(ap, "test", "ap")
  expect_match(attr(out, "step"), "log")
  neg <- ts(sin(1:120 / 5), frequency = 12)
  expect_match(attr(cyclind:::apply_transform(neg, "test", "neg"), "step"),
               "levels")
})

test_that("oecd_spec bundles the OECD choices", {
  s <- oecd_spec(12)
  expect_s4_class(s@classifier, "OecdClassifier")
  expect_s4_class(s@aggregator, "OecdAggregate")
  expect_s4_class(s@outliers, "TramoOutliers")
  # Gyomai, Ahmad and Astolfi (2017, p. 287): no component is shifted
  expect_false(s@lag_shift)
  expect_equal(s@transform, "test")
  expect_s4_class(s@filter, "HPDoubleFilter")
  expect_equal(s@min_median_lead, 2)
  expect_equal(oecd_spec(4)@min_median_lead, 2 / 3)
  expect_equal(oecd_spec(12, max_iter = 3)@max_iter, 3)
  expect_output(show(s), "OECD")
})

test_that("P1: oecd_spec() dates with phases of 9 and cycles of 24 months", {
  s <- oecd_spec(12)
  expect_s4_class(s@dater, "OecdDater")
  r <- cyclind:::resolve_dater(s@dater, 12)
  expect_equal(c(r$min_phase, r$min_cycle), c(9, 24))
  expect_identical(r$phase_rule, "symmetric")
  rq <- cyclind:::resolve_dater(oecd_spec(4)@dater, 4)
  expect_equal(c(rq$min_phase, rq$min_cycle), c(3, 8))
  # the dater can be changed, e.g. to the original 5/15 rule (OECD before
  # 2008), and no longer collides with the argument set inside oecd_spec()
  old <- oecd_spec(dater = bry_boschan())
  expect_s4_class(old@dater, "BryBoschan")
  expect_equal(cyclind:::resolve_dater(old@dater, 12)$min_cycle, 15)
  expect_error(oecd_spec(dater = "bb"), class = "cyclind_error_argument")
})

test_that("P5/P6: lag shifting is an option, the lag window is 9 months", {
  expect_true(oecd_spec(lag_shift = TRUE)@lag_shift)
  # 24 months lead and 9 months lag (Gyomai et al. 2017, p. 285)
  expect_equal(oecd_spec()@lead_window, c(9, 24))
  expect_equal(oecd_spec(4)@lead_window, c(3, 8))
  expect_equal(oecd_spec(lead_window = c(12, 24))@lead_window, c(12, 24))
  # the other choices fixed by oecd_spec() can be changed as well
  expect_identical(
    oecd_spec(aggregator = oecd_aggregate(growth = "ratio"))@aggregator@growth,
    "ratio")
  expect_equal(oecd_spec(min_median_lead = 1)@min_median_lead, 1)
  expect_s4_class(oecd_spec(extension = ar_extension())@extension,
                  "ArExtension")
  expect_s4_class(oecd_spec(outliers = robust_outliers(4))@outliers,
                  "RobustOutliers")
})

test_that("cli_build runs with the OECD classifier and aggregation", {
  cand <- sim_candidates(seed = 1)
  s <- update(spec, classifier = oecd_classifier(),
              aggregator = oecd_aggregate(), lag_shift = TRUE)
  res <- cli_build(cand, s)
  expect_s4_class(res, "CliResult")
  expect_true("lead_6" %in% leaders(res))
  q <- cli_quality(res)
  expect_true(any(grepl("median lead", q@table$criterion)))
  expect_false(any(grepl("Granger", q@table$criterion)))
})

test_that("cli_presentation gives the three OECD forms", {
  cand <- sim_candidates(seed = 1)
  res <- cli_build(cand)
  pr <- cli_presentation(res)
  expect_named(pr, c("time", "amplitude_adjusted", "trend_restored",
                     "rate_of_change", "reference",
                     "reference_amplitude_adjusted", "reference_trend",
                     "reference_rate_of_change", "reference_smoothed",
                     "trend_extrapolated"))
  expect_equal(pr$reference_amplitude_adjusted,
               100 * pr$reference / pr$reference_trend)
  # P9: the reference de-trended and smoothed but not normalised, the
  # comparison series of the amplitude-adjusted CLI (Gyomai et al. 2017,
  # p. 288)
  ref <- cycles(res)$reference
  expect_true(cyclind:::is_logged(ref))
  expect_equal(pr$reference_smoothed, 100 * exp(as.numeric(ref@cycle)))
  expect_lt(stats::sd(diff(pr$reference_smoothed), na.rm = TRUE),
            stats::sd(diff(pr$reference_amplitude_adjusted), na.rm = TRUE))
  comp <- res@composite
  expect_equal(pr$amplitude_adjusted,
               100 + as.numeric(comp@cycle) * comp@amplitude)
  ok <- !is.na(pr$trend_restored) & !is.na(pr$reference_trend)
  expect_equal(pr$trend_restored[ok],
               pr$reference_trend[ok] * pr$amplitude_adjusted[ok] / 100)
  i <- which(!is.na(pr$trend_restored))
  i <- i[i > 12][1]
  expect_equal(pr$rate_of_change[i],
               100 * (pr$trend_restored[i] / pr$trend_restored[i - 12] - 1))
  # P8: as at the OECD (CLI FAQ 2024, no. 6), no trend-restored CLI after
  # the last reference observation
  sim <- simulate_cycles(seed = 1)
  sim[(nrow(sim) - 5L):nrow(sim), "reference"] <- NA
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  res <- cli_build(cli_candidates(sim, "reference", meta = meta))
  pr <- cli_presentation(res)
  last_cli <- max(which(!is.na(pr$amplitude_adjusted)))
  last_ref <- max(which(!is.na(pr$reference)))
  expect_gt(last_cli, last_ref)
  expect_true(all(is.na(pr$trend_restored[(last_ref + 1L):last_cli])))
  expect_true(all(is.na(pr$rate_of_change[(last_ref + 1L):last_cli])))
  expect_false(is.na(pr$trend_restored[last_ref]))
  expect_false(any(pr$trend_extrapolated))
  # extend_trend = TRUE extends the trend linearly and flags those rows
  ext <- cli_presentation(res, extend_trend = TRUE)
  expect_false(is.na(ext$trend_restored[last_cli]))
  expect_true(all(ext$trend_extrapolated[(last_ref + 1L):last_cli]))
  expect_false(any(ext$trend_extrapolated[seq_len(last_ref)][
    !is.na(pr$reference[seq_len(last_ref)])]))
  expect_equal(ext$trend_restored[1:last_ref], pr$trend_restored[1:last_ref])
  expect_error(cli_presentation(res, extend_trend = NA),
               class = "cyclind_error_argument")
  expect_error(cli_presentation(1), class = "cyclind_error_argument")
})

test_that("R3/R4: the forms of a reference that is not logged", {
  # a balance that crosses zero is not logged
  sim <- simulate_cycles(seed = 4, n_years = 20)
  bal <- sim
  bal[, "reference"] <- sim[, "reference"] - mean(sim[, "reference"])
  meta <- data.frame(name = colnames(bal)[-1], rationale = "early_stage")
  res <- cli_build(cli_candidates(bal, "reference", meta = meta),
                   cli_spec(12, transform = "none"))
  pr <- cli_presentation(res)
  ref <- cycles(res)$reference
  expect_false(cyclind:::is_logged(ref))
  scale <- mean(abs(as.numeric(ref@transformed)), na.rm = TRUE)
  ok <- !is.na(pr$trend_restored)
  expect_equal(pr$trend_restored[ok], pr$reference_trend[ok] +
                 (pr$amplitude_adjusted[ok] - 100) / 100 * scale)
  expect_equal(pr$reference_amplitude_adjusted,
               100 + 100 * (pr$reference - pr$reference_trend) / scale)
  expect_equal(pr$reference_smoothed,
               100 + 100 * as.numeric(ref@cycle) / scale)
  # the de-trended reference is on the scale of the composite (the old
  # ratio to a trend near zero ran into the thousands)
  ratio <- stats::sd(pr$reference_amplitude_adjusted, na.rm = TRUE) /
    stats::sd(pr$amplitude_adjusted, na.rm = TRUE)
  expect_true(ratio > 0.5 && ratio < 2)
})

# ---- replication: the OECD CLI of Indonesia ---------------------------------
# The OECD publishes the Indonesian CLI with its reference series and its
# seven components as normalised cycles (SDMX dataflow DF_CLI; snapshot and
# experiments in data-raw/oecd_idn_fetch.R and data-raw/oecd_idn_validate.R).
# The published components are not inverted; the OECD inverts the exchange
# rate, the short-term interest rate and producer prices before aggregating.
oecd_idn <- function() {
  f <- readRDS(test_path("fixtures", "oecd-idn.rds"))
  x <- stats::ts(f$data, start = f$start, frequency = 12)
  as_cycle <- function(nm) {
    v <- x[, nm] - 100
    new("CycleSeries", name = nm, original = v, transformed = v, cycle = v,
        normalised = normalise_cycle(v), steps = "OECD normalised cycle")
  }
  comps <- grep("^LOCO", colnames(x), value = TRUE)
  list(x = x, comps = comps,
       cycles = stats::setNames(lapply(c("RS", comps), as_cycle),
                                c("RS", comps)),
       li = as_cycle("LI"))
}

test_that("FR-014.4: oecd_spec() reproduces the OECD CLI of Indonesia", {
  o <- oecd_idn()
  cyc <- o$cycles
  for (nm in c("LOCOEX", "LOCOST", "LOCOPP")) {
    cyc[[nm]] <- invert_cycle(cyc[[nm]])
  }
  # the reference cycle as the OECD publishes it: ratio to trend (percent)
  rt <- o$x[, "RS_RT"]
  ref <- new("CycleSeries", name = "RS", original = rt, transformed = rt,
             cycle = rt - 100, normalised = normalise_cycle(rt - 100),
             steps = "OECD ratio to trend")
  s <- oecd_spec()
  ci <- cli_composite(cyc, o$comps, s, ref)
  li <- as.numeric(o$li@normalised)
  ok <- !is.na(ci@cycle) & !is.na(li)
  # chained log growth, reciprocal inversion: the published cycle
  expect_gt(stats::cor(as.numeric(ci@cycle)[ok], li[ok]), 0.9995)
  tp <- function(v) {
    as.data.frame(date_turning_points(s@dater, v))[, c("period", "type")]
  }
  expect_equal(tp(ci@cycle), tp(o$li@normalised))
  # amplitude by standard deviations: the published amplitude-adjusted CLI
  aa <- as.numeric(o$x[, "LI_AA"])
  ok_aa <- ok & !is.na(aa)
  expect_lt(mean(abs(as.numeric(ci@index)[ok_aa] - aa[ok_aa])), 0.1)
  expect_gt(mean(abs(as.numeric(
    cli_composite(cyc, o$comps, update(s, amplitude = "mad"), ref)@index
  )[ok_aa] - aa[ok_aa])), 0.2)
  # the 0.7.0 rule is close, a plain average is clearly worse
  ratio <- cli_composite(cyc, o$comps,
                         update(s, aggregator = oecd_aggregate(
                           growth = "ratio")), ref)
  ew <- cli_composite(cyc, o$comps, update(s, aggregator = equal_weight()),
                      ref)
  r_ratio <- stats::cor(as.numeric(ratio@cycle)[ok], li[ok])
  expect_gt(r_ratio, 0.995)
  expect_lt(r_ratio, stats::cor(as.numeric(ci@cycle)[ok], li[ok]))
  expect_lt(stats::cor(as.numeric(ew@cycle)[ok], li[ok]), r_ratio)
})

test_that("signs of the Indonesian components: data and meta$sign", {
  o <- oecd_idn()
  meta <- data.frame(name = o$comps, rationale = "early_stage")
  cand <- cli_candidates(o$x[, c("RS", o$comps)], "RS", meta = meta)
  a <- cli_assess(cand, oecd_spec(dater = bry_boschan()), cycles = o$cycles)
  # dated with 5/15 months, cyclind inverts the exchange rate and the
  # interest rate as the OECD does; producer prices barely co-move with GDP
  # (r < 0.1), so their sign cannot come from the data
  expect_setequal(a@inverted, c("LOCOEX", "LOCOST"))
  # dated with 9/24 months the reference cycle is longer (73 against 41
  # months), the window in which the sign is read widens to 18 months and
  # takes in the opposite phase of the interest rate: only the exchange rate
  # is read as the OECD reads it, so declare the signs
  a <- cli_assess(cand, oecd_spec(), cycles = o$cycles)
  expect_true("LOCOEX" %in% a@inverted)
  expect_false("LOCOST" %in% a@inverted)
  meta$sign <- ifelse(o$comps %in% c("LOCOEX", "LOCOST", "LOCOPP"), -1, 1)
  cand <- cli_candidates(o$x[, c("RS", o$comps)], "RS", meta = meta)
  a <- cli_assess(cand, oecd_spec(), cycles = o$cycles)
  expect_setequal(a@inverted, c("LOCOEX", "LOCOST", "LOCOPP"))
  expect_true(all(as.data.frame(a)$sign_source == "meta"))
})

test_that("O-M1: the document rule on the OECD components of Indonesia", {
  # experiment A of data-raw/oecd_idn_validate.R: the OECD cycles go in as
  # they are, with the signs found by cyclind and with the OECD signs
  o <- oecd_idn()
  meta <- data.frame(name = o$comps, rationale = "early_stage")
  assess <- function(cls, meta) {
    cand <- cli_candidates(o$x[, c("RS", o$comps)], "RS", meta = meta)
    as.data.frame(cli_assess(cand, oecd_spec(classifier = cls),
                             cycles = o$cycles))
  }
  doc <- assess(oecd_classifier(rule = "document"), meta)
  old <- assess(oecd_classifier(), meta)
  stored <- utils::read.csv(system.file("calibration",
                                        "oecd-idn-components.csv",
                                        package = "cyclind"))
  stored <- stored[match(doc$name, stored$component), ]
  # the default rule rejects all seven, as before
  expect_true(all(old$class == "rejected"))
  expect_identical(old$class, stored$A_class_oecd_rule)
  expect_identical(old$reasons, stored$A_reasons_oecd_rule)
  # the document rule rejects none for missed or extra turning points or
  # concordance; only the cross-check of the lag rejects
  expect_false(any(grepl("missed|extra|concordance", doc$reasons)))
  expect_true(all(doc$reasons[doc$class == "rejected"] == "ccf lag"))
  expect_identical(doc$class, stored$A_class_oecd_document_rule)
  expect_identical(doc$reasons, stored$A_reasons_oecd_document_rule)
  expect_equal(doc$ccf_lag, stored$A_ccf_lag_oecd_spec)
  fit <- abs(doc$ccf_lag - doc$median_lead) <= 6
  expect_identical(doc$class != "rejected", fit)
  # signs found by cyclind: 3 leading and 1 coincident (the interest rate
  # is not inverted and peaks 16 months after GDP)
  expect_equal(sum(doc$class == "leading"), 3L)
  expect_equal(sum(doc$class == "coincident"), 1L)
  # with the OECD signs (meta$sign): 4 leading and 1 coincident; the
  # exchange rate (median lead 9.5, peak 3) and producer prices fail the
  # cross-check
  meta$sign <- ifelse(o$comps %in% c("LOCOEX", "LOCOST", "LOCOPP"), -1, 1)
  doc_sign <- assess(oecd_classifier(rule = "document"), meta)
  expect_identical(doc_sign$class,
                   stored$A_class_oecd_document_rule_oecd_signs)
  expect_equal(sum(doc_sign$class == "leading"), 4L)
  expect_equal(sum(doc_sign$class == "coincident"), 1L)
  expect_setequal(doc_sign$name[doc_sign$class == "rejected"],
                  c("LOCOEX", "LOCOPP"))
  # one quarter of tolerance keeps only 2 leading
  doc3 <- assess(oecd_classifier(rule = "document", ccf_tolerance = 3), meta)
  expect_equal(sum(doc3$class == "leading"), 2L)
  expect_equal(sum(doc3$class == "coincident"), 0L)
})

test_that("FR-014.6: the OECD example files of Indonesia read and align", {
  dir <- system.file("extdata", "oecd-idn", package = "cyclind")
  files <- file.path(dir, c("gdp.csv", "share_prices.csv",
                            "call_money_rate.csv", "rupiah_per_usd.csv",
                            "consumer_confidence.csv"))
  expect_true(all(file.exists(files)))
  s <- read_cli_series(files)
  expect_identical(unname(vapply(s, attr, "", "frequency")),
                   c("quarterly", rep("monthly", 4)))
  x <- cli_align(s, type = c(gdp = "flow", call_money_rate = "average"),
                 reference = "gdp")
  expect_identical(stats::frequency(x), 12)
  o <- read_cli_data(file.path(dir, "oecd_cli.csv"))
  expect_true(all(c("cli_amplitude_adjusted", "cli_normalised",
                    "cli_trend_restored", "cli_growth_rate",
                    "reference_normalised") %in% colnames(o)))
})

test_that("FR-014.8: oecd_spec takes a filter and a transformation", {
  smoother_warning <- "does not remove the trend"
  expect_warning(s <- oecd_spec(4, transform = "none",
                                filter = hp_smoother(4)), smoother_warning)
  expect_identical(s@transform, "none")
  expect_s4_class(s@filter, "HPSmoother")
  expect_warning(ref <- update(oecd_spec(4), transform = "none",
                               filter = hp_smoother(4)), smoother_warning)
  expect_identical(s, ref)
  # the defaults are those of the OECD method
  expect_identical(oecd_spec()@transform, "test")
  expect_s4_class(oecd_spec()@filter, "HPDoubleFilter")
  expect_s4_class(oecd_spec(form = growth_form())@filter, "HPSmoother")
  expect_error(oecd_spec(filter = "hp"), class = "cyclind_error_argument")
})
