# Fixes from the comparison with the OECD package CIF (butir 4 to 10).

# ---- butir 4: transform = "auto" keeps rates in levels ----------------------

test_that("auto does not log a series in percent (interest rates)", {
  rate <- ts(6 + 2 * sin(2 * pi * (1:120) / 40), frequency = 12,
             start = 2000)
  lv <- cyclind:::apply_transform(rate, "auto", "rate", unit = "level")
  pc <- cyclind:::apply_transform(rate, "auto", "rate", unit = "percent")
  expect_equal(attr(lv, "step"), "log")
  expect_equal(as.numeric(pc), as.numeric(rate))
  expect_match(attr(pc, "step"), "percent")
  # an explicit log is still a log
  expect_equal(attr(cyclind:::apply_transform(rate, "log", "rate",
                                              unit = "percent"), "step"),
               "log")
  x <- cbind(ref = ts(exp(cumsum(rnorm(120, 0.01, 0.01))), frequency = 12,
                      start = 2000), bunga = rate)
  meta <- data.frame(name = "bunga", rationale = "prime_mover",
                     unit = "percent")
  cy <- cli_cycles(cli_candidates(x, "ref", meta = meta), cli_spec(12))
  expect_false(any(grepl("^log", cy$bunga@steps)))
  expect_true(any(grepl("^log", cy$ref@steps)))
})

# ---- butir 6: matching rule of turning points -------------------------------

tp_object <- function(index, type, values, n = 120) {
  y <- rep(0, n)
  y[index] <- values
  new("TurningPoints", name = "x", series = ts(y, frequency = 12),
      index = as.integer(index), type = type, params = list())
}

test_that("turning points can be matched to the nearest or the largest", {
  ref <- tp_object(50L, "peak", 1)
  # two candidate peaks in the window: a large one 12 months before the
  # reference peak, a small one 3 months before
  cand <- tp_object(c(38L, 42L, 47L), c("peak", "trough", "peak"),
                    c(2, -0.5, 1))
  near <- cyclind:::match_turning_points(ref, cand, c(12, 24), 0)
  expect_equal(near$pairs$lead, 3)
  expect_equal(near$n_extra, 2L)
  big <- cyclind:::match_turning_points(ref, cand, c(12, 24), 0, "maximum")
  expect_equal(big$pairs$lead, 12)
  expect_equal(big$pairs$cand_index, 38L)
  # equal values: the nearest one wins
  tie <- tp_object(c(38L, 42L, 47L), c("peak", "trough", "peak"),
                   c(1, -0.5, 1))
  expect_equal(cyclind:::match_turning_points(ref, tie, c(12, 24), 0,
                                              "maximum")$pairs$lead, 3)
  # troughs: the deepest
  rt <- tp_object(50L, "trough", -1)
  ct <- tp_object(c(40L, 44L, 48L), c("trough", "peak", "trough"),
                  c(-3, 0.5, -1))
  expect_equal(cyclind:::match_turning_points(rt, ct, c(12, 24), 0,
                                              "maximum")$pairs$lead, 10)
})

test_that("the matching rule is part of the specification", {
  expect_equal(cli_spec()@matching, "nearest")
  s <- cli_spec(matching = "maximum")
  expect_equal(s@matching, "maximum")
  expect_output(show(s), "matching:    largest")
  expect_error(cli_spec(matching = "first"))
  expect_error(update(cli_spec(), matching = "first"),
               class = "cyclind_error_argument")
  old <- cli_spec()
  attr(old, "matching") <- NULL
  expect_equal(cyclind:::spec_matching(old), "nearest")
  cand <- sim_candidates(seed = 1)
  a1 <- cli_assess(cand, cli_spec(12))
  a2 <- cli_assess(cand, cli_spec(12, matching = "maximum"))
  expect_equal(nrow(a1@table), nrow(a2@table))
  expect_true(all(a2@table$n_matched <= a2@table$n_ref))
})

# ---- butir 7: few components ------------------------------------------------

test_that("a selected composite of fewer than 3 components warns", {
  cand <- sim_candidates(seed = 1)[c("lead_6", "lead_3")]
  expect_warning(res <- cli_build(cand),
                 class = "cyclind_warning_few_components")
  expect_lt(length(components(res)), 3L)
  full <- sim_candidates(seed = 1)
  w <- character(0)
  res <- withCallingHandlers(cli_build(full), warning = function(cnd) {
    w <<- c(w, class(cnd)[1])
    invokeRestart("muffleWarning")
  })
  if (length(components(res)) >= 3L) {
    expect_false("cyclind_warning_few_components" %in% w)
  }
})

# ---- butir 8: twin candidates -----------------------------------------------

test_that("twin candidates (|r| >= 0.99) are dropped and reported", {
  sim <- simulate_cycles(seed = 1)
  set.seed(4)
  twin <- sim[, "lead_6"] * 1.001 + rnorm(nrow(sim), 0, 1e-4)
  x <- cbind(sim, twin = twin)
  colnames(x) <- c(colnames(sim), "twin")
  meta <- data.frame(name = colnames(x)[-1], rationale = "early_stage")
  cand <- cli_candidates(x, "reference", meta = meta)
  expect_warning(res <- cli_build(cand),
                 class = "cyclind_warning_twins")
  comps <- components(res)
  expect_false(all(c("lead_6", "twin") %in% comps))
  au <- audit_log(res)
  expect_true(any(grepl("^twin dropped", au$action)))
  expect_equal(au$iteration[grepl("^twin dropped", au$action)], 0L)
  # switched off with a threshold of 1 (only identical cycles)
  s <- cli_spec(12, twin_threshold = 1)
  res1 <- suppressWarnings(cli_build(cand, s))
  expect_false(any(grepl("^twin", audit_log(res1)$action)))
  expect_error(cli_spec(twin_threshold = 0), class = "cyclind_error_argument")
  expect_error(cli_spec(twin_threshold = 1.5),
               class = "cyclind_error_argument")
  tw <- cyclind:::find_twins(c("a", "b"), list(
    a = new("CycleSeries", name = "a", original = ts(1:30),
            transformed = ts(1:30), cycle = ts(1:30),
            normalised = ts(sin(1:30)), steps = character(0)),
    b = new("CycleSeries", name = "b", original = ts(1:30),
            transformed = ts(1:30), cycle = ts(1:30),
            normalised = ts(2 * sin(1:30)), steps = character(0))),
    c(a = 0.5, b = 0.9), 0.99)
  expect_equal(tw$name, "a")
  expect_equal(tw$twin_of, "b")
  p <- cyclind:::phrase_translator("id")
  expect_equal(p("twin dropped x (|r| 0.995 with y)"),
               "kembar dibuang x (|r| 0.995 dengan y)")
})
