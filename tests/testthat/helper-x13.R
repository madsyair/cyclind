# Synthetic series for the X-13 fixes (FR-017 to FR-019). No real data.

# Evaluate `expr` with a fixed seed and restore the random number stream.
with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit(if (is.null(old)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", old, envir = globalenv())
  })
  set.seed(seed)
  expr
}

# Quarterly series, 2015 Q1 to 2026 Q4: an I(2) trend, a cycle of 18
# quarters and, in 2019 Q3 (t0 = 19), an additive outlier `a` and a
# transitory change `c` decaying at `r`. X-13 fits ARIMA(1 2 0) to it and
# finds the AO and the TC on the same date (FR-017). `season` adds an
# additive seasonal pattern.
pair_series <- function(a = 3, c = -6, seed = 4, n = 48, t0 = 19,
                        r = 0.343, season = 0, event = TRUE) {
  tt <- seq_len(n)
  noise <- with_seed(seed, cumsum(cumsum(stats::rnorm(n, 0, 0.15))))
  base <- 100 + 0.8 * tt + 3 * sin(2 * pi * tt / 18) + noise
  e <- a * (tt == t0) + c * ifelse(tt >= t0, r^(tt - t0), 0)
  s <- season * rep(c(1, -0.5, 0.5, -1), length.out = n)
  ts(base + if (event) e else 0, start = 2015, frequency = 4) + s
}

# Turning points of the reference cycle of `x` under `spec`, as text.
reference_tps <- function(x, spec) {
  z <- x + 0.01 * sin(seq_along(x))
  cand <- cli_candidates(cbind(ref = x, z = z), "ref",
                         meta = data.frame(name = "z",
                                           rationale = "expectations"))
  cyc <- suppressWarnings(cli_cycles(cand, spec))
  tp <- as.data.frame(cli_turning_points(cyc["ref"], spec)$ref)
  paste(tp$type, tp$period, collapse = ", ")
}

# A regARIMA fit with an AO (a) and a TC (c) on the same date t0.
fake_pair_fit <- function(x, a, c, t0, r = 0.343) {
  tt <- seq_along(x)
  eff <- cbind(AO = a * (tt == t0), TC = c * ifelse(tt >= t0, r^(tt - t0), 0),
               LS = 0)
  list(effects = eff, types = c("AO", "TC", "LS"),
       count = c(AO = 1L, TC = 1L, LS = 0L), forecast = rep(0, 4),
       tc_rate = r,
       table = data.frame(index = c(t0, t0), type = c("AO", "TC"),
                          removed = FALSE, coef = c(a, c),
                          stringsAsFactors = FALSE))
}

# Quarterly series that X-13 cannot adjust (checked with X-13 1.1-61):
# a policy rate (no seasonality; "Regression matrix singular") and a fixed
# seasonal step pattern ("Differencing has annihilated the series").
policy_rate <- function() {
  ts(c(rep(4.75, 10), rep(4.5, 8), rep(4.25, 12), rep(3.5, 8),
       rep(5.75, 10)), start = 2015, frequency = 4)
}
step_pattern <- function() {
  ts(rep(c(3.5, 3.5, 3.75, 3.75), 12), start = 2015, frequency = 4)
}

# Quarterly series that X-13 adjusts only without the Idulfitri regressors
# ("Cannot make a choice of seasonal ARMA order"): the regressor itself is
# part of the series.
holiday_series <- function(holiday) {
  x0 <- ts(rep(1, 48), start = 2015, frequency = 4)
  reg <- cyclind:::holiday_regressors(x0, list(holiday))[, 1]
  r <- as.numeric(stats::window(reg, start = 2015, end = c(2026, 4)))
  tt <- seq_len(48)
  ts(100 + cumsum(sin(tt)) + 5 * r, start = 2015, frequency = 4)
}
