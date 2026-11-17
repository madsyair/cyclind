check_censoring <- function(tp, n) {
  r <- tp@params
  idx <- tp@index
  if (length(idx) == 0L) return(invisible(TRUE))
  expect_true(all(idx > r$end_exclusion & idx <= n - r$end_exclusion))
  if (length(idx) > 1L) {
    expect_true(all(diff(idx) >= r$min_phase))
    expect_false(any(tp@type[-1] == tp@type[-length(tp@type)]))
  }
  for (ty in c("peak", "trough")) {
    same <- idx[tp@type == ty]
    if (length(same) > 1L) expect_true(all(diff(same) >= r$min_cycle))
  }
}

test_that("turning points of a sine wave are exact", {
  x <- sine_ts(n = 240, period = 48)
  tp <- date_turning_points(bry_boschan(), x)
  expect_equal(tp@index[tp@type == "peak"], c(12L, 60L, 108L, 156L, 204L))
  expect_equal(tp@index[tp@type == "trough"], c(36L, 84L, 132L, 180L, 228L))
})

test_that("censoring rules hold on random walks", {
  for (s in 1:15) {
    set.seed(s)
    xm <- ts(cumsum(rnorm(240)), frequency = 12)
    xq <- ts(cumsum(rnorm(120)), frequency = 4)
    check_censoring(date_turning_points(bry_boschan(), xm), 240)
    check_censoring(date_turning_points(bbq(), xq), 120)
  }
})

test_that("BBQ replicates BCDating (golden file)", {
  golden <- readRDS(test_path("fixtures", "bcdating_bbq.rds"))
  # phase_rule = "bbq" is the rule of cyclind 0.1.0, kept for this
  # replication; the symmetric default differs on one more series (ADR-004)
  for (rule in c("bbq", "symmetric")) {
    same <- 0L
    for (g in golden) {
      y <- ts(g$y, frequency = 4, start = 1980)
      tp <- date_turning_points(bbq(phase_rule = rule), y)
      ref <- sort(c(g$peaks, g$troughs))
      if (identical(as.integer(ref), tp@index)) {
        same <- same + 1L
      } else {
        # differences come from BCDating keeping the earlier of two close
        # turning points; cyclind keeps the more extreme one
        ours_p <- tp@index[tp@type == "peak"]
        ref_p <- g$peaks
        expect_gte(max(g$y[ours_p]), max(g$y[ref_p]) - 1e-8)
        expect_lte(abs(length(tp@index) - length(ref)), 2)
      }
    }
    # 46 of the 50 series give identical dates with the 0.1.0 rule
    # (development version 0.8.0), 45 with the symmetric rule
    expect_gte(same, if (rule == "bbq") 46L else 45L)
  }
})

test_that("Bry-Boschan matches Coinprofile on a smooth monthly series", {
  skip_if_not_installed("Coinprofile")
  set.seed(7)
  n <- 300
  y <- 100 + 5 * sin(2 * pi * (1:n) / 50) + cumsum(rnorm(n, 0, 0.2))
  ref <- Coinprofile::TP_BryBoschan(y, frequ = 12, year = 2000, month = 1)
  tp <- date_turning_points(bry_boschan(), ts(y, frequency = 12, start = 2000))
  expect_equal(tp@index, as.integer(ref$Time))
})

test_that("constant and missing series give no turning points", {
  expect_length(date_turning_points(bbq(), ts(rep(1, 40), frequency = 4))@index,
                0L)
  expect_length(date_turning_points(bbq(), ts(rep(NA_real_, 40),
                                              frequency = 4))@index, 0L)
  x <- ts(c(rep(NA, 10), sin(2 * pi * (1:100) / 20), rep(NA, 5)), frequency = 4)
  tp <- date_turning_points(bbq(), x)
  expect_true(all(tp@index > 10 & tp@index <= 110))
  x[50] <- NA
  expect_error(date_turning_points(bbq(), x), class = "cyclind_error_missing")
})

test_that("cycle states follow the turning points", {
  x <- sine_ts(n = 120, period = 40, freq = 12)
  tp <- date_turning_points(bry_boschan(), x)
  s <- as.numeric(cycle_states(tp))
  expect_equal(s[1:10], rep(1, 10))
  expect_equal(s[11], 0)
  expect_equal(s[30], 0)
  expect_equal(s[31], 1)
  expect_error(cycle_states(1:3), class = "cyclind_error_argument")
  empty <- date_turning_points(bbq(), ts(rep(1, 20), frequency = 4))
  expect_true(all(is.na(cycle_states(empty))))
})

test_that("dater arguments and defaults", {
  expect_s4_class(bry_boschan(), "BryBoschan")
  expect_error(bbq(window = 0), class = "cyclind_error_argument")
  expect_equal(cyclind:::resolve_dater(bbq(min_cycle = 6), 4)$min_cycle, 6)
  expect_equal(cyclind:::resolve_dater(bry_boschan(), 12)$min_cycle, 15)
  expect_error(new("TurningPoints", name = "x", series = ts(1:3),
                   index = c(2L, 1L), type = c("peak", "trough")),
               "increasing")
  expect_error(new("TurningPoints", name = "x", series = ts(1:3),
                   index = c(1L, 2L), type = c("peak", "peak")),
               "alternate")
})

test_that("turning points convert to a data frame", {
  tp <- date_turning_points(bry_boschan(), sine_ts())
  df <- as.data.frame(tp)
  expect_named(df, c("time", "period", "type", "index", "value"))
  expect_equal(df$period[1], "2000-12")
})

# Piecewise-linear path through the given turning points (positions and
# values), so that the dated turning points are known exactly.
path_through <- function(at, value, n) {
  stats::approx(c(1, at, n), c(0, value, 0), xout = seq_len(n))$y
}

test_that("a phase of exactly min_phase is kept (inclusive bound)", {
  # troughs and peaks alternate every 5 and 10 months: phases of 5 months
  # exactly, cycles of 15 months exactly
  at <- c(12, 17, 27, 32, 42, 47, 57, 62)
  value <- rep(c(-1, 1), 4)
  y <- path_through(at, value, 75)
  x <- ts(y, frequency = 12, start = 2000)
  tp <- date_turning_points(bry_boschan(), x)
  expect_equal(tp@index, as.integer(at))
  expect_true(any(diff(tp@index) == 5L))
  # one month more and the short phases go
  longer <- date_turning_points(bry_boschan(min_phase = 6), x)
  expect_false(any(diff(longer@index) < 6L))
  expect_lt(length(longer@index), length(at))
})

test_that("a cycle of exactly min_cycle is kept (inclusive bound)", {
  at <- c(12, 17, 27, 32, 42, 47, 57, 62)
  y <- path_through(at, rep(c(-1, 1), 4), 75)
  x <- ts(y, frequency = 12, start = 2000)
  tp <- date_turning_points(bry_boschan(), x)
  peaks <- tp@index[tp@type == "peak"]
  troughs <- tp@index[tp@type == "trough"]
  expect_equal(unique(diff(peaks)), 15L)
  expect_equal(unique(diff(troughs)), 15L)
  shorter <- date_turning_points(bry_boschan(min_cycle = 16), x)
  p2 <- shorter@index[shorter@type == "peak"]
  expect_true(all(diff(p2) >= 16L))
})

test_that("quarterly bounds are inclusive too (BBQ 2/5)", {
  # phases of 2 and 3 quarters, cycles of 5 quarters
  at <- c(5, 7, 10, 12, 15, 17, 20, 22)
  y <- path_through(at, rep(c(-1, 1), 4), 28)
  x <- ts(y, frequency = 4, start = 2000)
  tp <- date_turning_points(bbq(), x)
  expect_equal(tp@index, as.integer(at))
  expect_equal(unique(diff(tp@index[tp@type == "peak"])), 5L)
})

test_that("the OECD 9/24 bounds are inclusive too", {
  # phases of exactly 9 and 15 months: cycles of exactly 24 months
  at <- c(12, 21, 36, 45, 60, 69, 84, 93)
  y <- path_through(at, rep(c(-1, 1), 4), 110)
  x <- ts(y, frequency = 12, start = 2000)
  oecd <- bry_boschan(min_phase = 9, min_cycle = 24)
  tp <- date_turning_points(oecd, x)
  expect_equal(tp@index, as.integer(at))
  expect_equal(min(diff(tp@index)), 9L)
  expect_equal(unique(diff(tp@index[tp@type == "peak"])), 24L)
  # the dater of oecd_spec() (symmetric phase rule) keeps them as well
  expect_equal(date_turning_points(oecd_dater(), x)@index, as.integer(at))
  expect_lt(length(date_turning_points(bry_boschan(min_phase = 10,
                                                   min_cycle = 24),
                                       x)@index), length(at))
  expect_lt(length(date_turning_points(bry_boschan(min_phase = 9,
                                                   min_cycle = 25),
                                       x)@index), length(at))
})

# ---- phase rule (audit P2) --------------------------------------------------
# The audit example: a moderate trough (t = 30), a small peak (t = 60), the
# deepest trough five months later (t = 66) and a peak (t = 100); the phase
# 60 -> 66 is shorter than 9 months.
audit_blip <- function() {
  tt <- 1:130
  y <- -0.6 * exp(-((tt - 30) / 8)^2) + 0.25 * exp(-((tt - 60) / 3)^2) -
    2.0 * exp(-((tt - 66) / 4)^2) + 1.2 * exp(-((tt - 100) / 12)^2)
  ts(y, frequency = 12)
}

# Turning points of the reversed series, mapped back to forward time.
reversed_tp <- function(dater, x) {
  n <- length(x)
  r <- date_turning_points(dater, ts(rev(as.numeric(x)),
                                     frequency = frequency(x)))
  o <- order(n + 1L - r@index)
  list(index = (n + 1L - r@index)[o], type = r@type[o])
}

test_that("P2: the symmetric phase rule keeps the deepest trough", {
  x <- audit_blip()
  tp <- date_turning_points(bry_boschan(min_phase = 9, min_cycle = 24), x)
  expect_identical(tp@index, c(66L, 100L))
  expect_identical(tp@type, c("trough", "peak"))
  expect_identical(tp@params$phase_rule, "symmetric")
  # the earlier rule drops the later turning point of the short phase and
  # so loses the deepest trough
  old <- date_turning_points(
    bry_boschan(min_phase = 9, min_cycle = 24, phase_rule = "bbq"), x)
  expect_identical(old@index, c(30L, 100L))
  expect_identical(oecd_dater()@phase_rule, "symmetric")
})

test_that("P2: reversing the series reverses the turning points", {
  daters <- list(bry_boschan(), bry_boschan(min_phase = 9, min_cycle = 24),
                 oecd_dater())
  series <- list(audit_blip())
  for (s in 1:20) {
    set.seed(s)
    series[[length(series) + 1L]] <- ts(cumsum(rnorm(300)) +
      8 * sin(2 * pi * (1:300) / 70), frequency = 12)
  }
  for (x in series) {
    for (d in daters) {
      f <- date_turning_points(d, x)
      r <- reversed_tp(d, x)
      expect_identical(f@index, r$index)
      # a peak stays a peak when time runs backwards
      expect_identical(f@type, r$type)
    }
  }
  for (s in 1:20) {
    set.seed(100 + s)
    xq <- ts(cumsum(rnorm(140)) + 6 * sin(2 * pi * (1:140) / 24),
             frequency = 4)
    for (d in list(bbq(), oecd_dater())) {
      expect_identical(date_turning_points(d, xq)@index,
                       reversed_tp(d, xq)$index)
    }
  }
})

test_that("P2: the symmetric rule respects the censoring rules", {
  for (s in 1:15) {
    set.seed(s)
    xm <- ts(cumsum(rnorm(240)), frequency = 12)
    xq <- ts(cumsum(rnorm(120)), frequency = 4)
    check_censoring(date_turning_points(oecd_dater(), xm), 240)
    check_censoring(date_turning_points(oecd_dater(), xq), 120)
    check_censoring(date_turning_points(bbq(phase_rule = "bbq"), xq), 120)
  }
})

test_that("P1: oecd_dater() uses phases of 9 and cycles of 24 months", {
  d <- oecd_dater()
  expect_s4_class(d, "Dater")
  m <- cyclind:::resolve_dater(d, 12)
  expect_equal(c(m$min_phase, m$min_cycle), c(9, 24))
  q <- cyclind:::resolve_dater(d, 4)
  expect_equal(c(q$min_phase, q$min_cycle), c(3, 8))
  expect_equal(cyclind:::resolve_dater(oecd_dater(min_cycle = 30), 12)$min_cycle,
               30)
  expect_error(bry_boschan(phase_rule = "later"),
               class = "cyclind_error_argument")
  # objects saved before the phase rule existed keep the earlier rule
  old <- bry_boschan()
  attr(old, "phase_rule") <- NULL
  expect_identical(cyclind:::resolve_dater(old, 12)$phase_rule, "bbq")
})
