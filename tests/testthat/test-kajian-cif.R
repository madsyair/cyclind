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
