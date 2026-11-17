# Evidence for ADR-012: AO and TC found on the same date by X-13.
# Synthetic quarterly series (2015 Q1 to 2026 Q4): an I(2) trend, a cycle
# of 18 quarters and, in 2019 Q3, an additive outlier `a` and a transitory
# change `c` (decay 0.343, the X-13 rate). For every series where X-13
# (levels, transform = "none") finds an AO and a TC on the same date, the
# turning points of the reference cycle are compared:
#   old    X-13, removing the AO alone (before ADR-012)
#   new    X-13, the pair rewritten (ADR-012)
#   tramo  TRAMO with the same specification
#   clean  the series without the event (robust_outliers(Inf))
#   tc     the series with the TC only (the AO removed exactly)
# Needs 'seasonal' and 'RJDemetra' (Java). Run from the package root:
#   Rscript data-raw/x13_collinear.R
# Output: inst/calibration/x13-collinear.csv (one row per series with a
# pair; the last rows are the shares of agreement).
devtools::load_all(quiet = TRUE)

make <- function(a, c, seed, part = c("all", "none", "tc")) {
  part <- match.arg(part)
  n <- 48
  tt <- seq_len(n)
  set.seed(seed)
  base <- 100 + 0.8 * tt + 3 * sin(2 * pi * tt / 18) +
    cumsum(cumsum(stats::rnorm(n, 0, 0.15)))
  ao <- a * (tt == 19)
  tc <- c * ifelse(tt >= 19, 0.343^(tt - 19), 0)
  e <- switch(part, all = ao + tc, none = 0, tc = tc)
  stats::ts(base + e, start = 2015, frequency = 4)
}

tps <- function(x, spec) {
  z <- x + 0.01 * sin(seq_along(x))
  cand <- cli_candidates(cbind(ref = x, z = z), "ref",
                         meta = data.frame(name = "z",
                                           rationale = "expectations"))
  cyc <- suppressWarnings(cli_cycles(cand, spec))
  tp <- as.data.frame(cli_turning_points(cyc["ref"], spec)$ref)
  paste(tp$type, tp$period, collapse = ", ")
}

spec <- function(engine) {
  oecd_spec(freq = 4, transform = "none", engine = engine)
}
plain <- oecd_spec(freq = 4, transform = "none",
                   outliers = robust_outliers(Inf))
new_shift <- collinear_shift
old_shift <- function(pairs, n, remove) numeric(n)

rows <- list()
for (a in c(-6, -3, 3, 6)) for (c in c(-9, -6, -3, 3, 6, 9)) {
  for (seed in 1:4) {
    x <- make(a, c, seed)
    tab <- regarima_x13(x, 1, NA)$table
    if (nrow(collinear_pairs(tab)) == 0L) next
    assignInNamespace("collinear_shift", old_shift, "cyclind")
    old <- tps(x, spec("x13"))
    assignInNamespace("collinear_shift", new_shift, "cyclind")
    rows[[length(rows) + 1L]] <- data.frame(
      a = a, c = c, seed = seed, old = old, new = tps(x, spec("x13")),
      tramo = tps(x, spec("tramo")),
      clean = tps(make(a, c, seed, "none"), plain),
      tc = tps(make(a, c, seed, "tc"), plain), stringsAsFactors = FALSE)
  }
}
d <- do.call(rbind, rows)
share <- function(rule, target) mean(d[[rule]] == d[[target]])
summary_rows <- do.call(rbind, lapply(c("tramo", "clean", "tc"), function(t) {
  data.frame(a = NA, c = NA, seed = NA,
             old = sprintf("%.3f", share("old", t)),
             new = sprintf("%.3f", share("new", t)),
             tramo = if (t == "tramo") "1.000" else
               sprintf("%.3f", share("tramo", t)),
             clean = paste("share equal to", t), tc = sprintf("n = %d",
                                                              nrow(d)))
}))
out <- rbind(d, summary_rows)
utils::write.csv(out, "inst/calibration/x13-collinear.csv", row.names = FALSE)
print(utils::tail(out, 3))
