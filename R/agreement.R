#' Agreement of a cycle with the reference, robust to crisis periods
#'
#' Compares a cycle (a composite, a candidate or the output of another
#' program) with the reference cycle by the Pearson correlation, the
#' Spearman rank correlation and the root mean squared error, on the dates
#' that both cover and that lie outside the excluded periods. A crisis such
#' as 2020 can dominate a correlation on its own; `exclude` measures the
#' agreement without it, and the rank correlation is not moved by a few
#' extreme values. Given a second cycle `y` (for instance the same
#' composite from another program), the function compares both with the
#' reference on the same dates and reports the differences `x - y`.
#'
#' The root mean squared error is computed after normalising each series to
#' mean 0 and mean absolute deviation 1 on the dates compared, as the OECD
#' normalises cycles, so that it measures shape and timing rather than
#' units.
#'
#' With `reps > 0`, a paired moving-block bootstrap gives percentile
#' intervals: blocks of `block` consecutive dates (of the dates compared)
#' are drawn with replacement, and the same blocks are used for `x`, `y`
#' and the reference, so that the interval of a difference accounts for the
#' correlation of the two errors. The draws come from a Park-Miller
#' generator started at `seed` inside the function: the random-number
#' stream of the session is neither used nor changed, and the same `seed`
#' gives the same interval.
#'
#' @param x A `ts`, a [CycleSeries-class] (its normalised cycle) or a
#'   [CompositeIndex-class] (its cycle).
#' @param reference The reference cycle, in the same forms.
#' @param y Optional second cycle compared with `x`.
#' @param exclude Periods left out: a pair `c(start, end)` of decimal times
#'   (both included, e.g. `c(2020, 2021 + 11 / 12)` for 2020-01 to
#'   2021-12), a list of such pairs, or a two-column matrix.
#' @param reps Number of bootstrap replications; 0 (default) gives point
#'   estimates only.
#' @param block Block length of the bootstrap in periods; `NULL` means two
#'   years (24 months or 8 quarters).
#' @param level Confidence level of the intervals.
#' @param seed Whole number starting the bootstrap draws; needed when
#'   `reps > 0`.
#' @return A data frame with one row per statistic (`"pearson"`,
#'   `"spearman"`, `"rmse"`) and columns `x`, `y`, `difference` (`x - y`),
#'   `lower` and `upper` (the interval of the difference, or of `x` when
#'   `y` is `NULL`). Attributes `n` (dates compared), `n_excluded`, `block`,
#'   `reps`, `seed` and `level` describe the computation.
#' @references
#' Künsch, H. R. (1989). The jackknife and the bootstrap for general
#' stationary observations. *The Annals of Statistics*, 17(3), 1217--1241.
#'
#' Park, S. K. and Miller, K. W. (1988). Random number generators: good
#' ones are hard to find. *Communications of the ACM*, 31(10), 1192--1201.
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cand <- cli_candidates(sim, "reference", meta = meta)
#' cyc <- cli_cycles(cand, cli_spec(12))
#' cycle_agreement(cyc$lead_6, cyc$reference, exclude = c(2008, 2009.5))
#' cycle_agreement(cyc$lead_6, cyc$reference, y = cyc$lead_3, reps = 99,
#'                 seed = 1)
#' @export
cycle_agreement <- function(x, reference, y = NULL, exclude = NULL,
                            reps = 0, block = NULL, level = 0.95,
                            seed = NULL) {
  check_number(reps, "reps", lower = 0, integer = TRUE)
  check_number(level, "level", lower = 0.5, upper = 0.999)
  if (reps > 0) {
    if (is.null(seed)) {
      cyclind_abort(paste("`seed` is needed for the bootstrap; the package",
                          "does not use the random numbers of the session."),
                    "cyclind_error_argument")
    }
    check_number(seed, "seed", integer = TRUE)
  }
  series <- list(x = agreement_ts(x, "x"),
                 reference = agreement_ts(reference, "reference"))
  if (!is.null(y)) series$y <- agreement_ts(y, "y")
  freq <- unique(vapply(series, stats::frequency, numeric(1)))
  if (length(freq) != 1L) {
    cyclind_abort("The series must have the same frequency.",
                  "cyclind_error_frequency")
  }
  m <- do.call(stats::ts.intersect, unname(series))
  m <- matrix(as.numeric(m), ncol = length(series),
              dimnames = list(NULL, names(series)))
  tm <- if (nrow(m) > 0L) {
    as.numeric(stats::time(do.call(stats::ts.intersect, unname(series))))
  } else {
    numeric(0)
  }
  ok <- stats::complete.cases(m)
  drop <- excluded(tm, exclude) & ok
  keep <- ok & !drop
  m <- m[keep, , drop = FALSE]
  if (nrow(m) < 2L * freq) {
    cyclind_abort(sprintf(paste("At least %d common dates outside the",
                                "excluded periods are needed."), 2L * freq),
                  "cyclind_error_short_series")
  }
  est <- agreement_stats(m)
  if (is.null(block)) block <- 2 * freq
  check_number(block, "block", lower = 1, upper = nrow(m), integer = TRUE)
  lower <- upper <- rep(NA_real_, 3L)
  if (reps > 0) {
    boot <- block_bootstrap(m, reps, block, seed)
    a <- (1 - level) / 2
    q <- apply(boot, 2, stats::quantile, probs = c(a, 1 - a), na.rm = TRUE,
               names = FALSE)
    lower <- q[1, ]
    upper <- q[2, ]
  }
  out <- data.frame(statistic = c("pearson", "spearman", "rmse"),
                    x = est$x, y = if (is.null(est$y)) NA_real_ else est$y,
                    difference = if (is.null(est$y)) NA_real_ else
                      est$x - est$y,
                    lower = lower, upper = upper, stringsAsFactors = FALSE)
  attr(out, "n") <- nrow(m)
  attr(out, "n_excluded") <- sum(drop)
  attr(out, "block") <- block
  attr(out, "reps") <- reps
  attr(out, "seed") <- seed
  attr(out, "level") <- level
  out
}

agreement_ts <- function(x, name) {
  if (is(x, "CycleSeries")) return(x@normalised)
  if (is(x, "CompositeIndex")) return(x@cycle)
  if (!stats::is.ts(x) || is.matrix(x)) {
    cyclind_abort(sprintf(paste("`%s` must be a univariate ts, a CycleSeries",
                                "or a CompositeIndex."), name),
                  "cyclind_error_argument")
  }
  x
}

# Dates inside any excluded period (both ends included).
excluded <- function(tm, exclude) {
  out <- logical(length(tm))
  if (is.null(exclude)) return(out)
  if (is.matrix(exclude)) {
    exclude <- lapply(seq_len(nrow(exclude)), function(i) exclude[i, ])
  }
  if (!is.list(exclude)) exclude <- list(exclude)
  for (w in exclude) {
    if (!is.numeric(w) || length(w) != 2L || anyNA(w) || w[1] > w[2]) {
      cyclind_abort(paste("Each period in `exclude` must be c(start, end) in",
                          "decimal time with start <= end."),
                    "cyclind_error_argument")
    }
    out <- out | (tm >= w[1] - 1e-8 & tm <= w[2] + 1e-8)
  }
  out
}

# Pearson, Spearman and RMSE of x (and y) against the reference.
agreement_stats <- function(m) {
  one <- function(v, r) {
    nv <- normalise_cycle(v)
    nr <- normalise_cycle(r)
    c(suppressWarnings(stats::cor(v, r)),
      suppressWarnings(stats::cor(v, r, method = "spearman")),
      sqrt(mean((nv - nr)^2)))
  }
  r <- m[, "reference"]
  list(x = one(m[, "x"], r),
       y = if ("y" %in% colnames(m)) one(m[, "y"], r))
}

# Statistics of `reps` moving-block resamples of the rows of m: the
# difference x - y when there is a y, otherwise those of x.
block_bootstrap <- function(m, reps, block, seed) {
  n <- nrow(m)
  n_blocks <- ceiling(n / block)
  starts <- floor(pm_uniform(reps * n_blocks, seed) * (n - block + 1)) + 1
  starts <- matrix(starts, nrow = reps)
  paired <- "y" %in% colnames(m)
  t(vapply(seq_len(reps), function(b) {
    idx <- unlist(lapply(starts[b, ], function(s) s:(s + block - 1L)))
    st <- agreement_stats(m[idx[seq_len(n)], , drop = FALSE])
    if (paired) st$x - st$y else st$x
  }, numeric(3)))
}

# Park-Miller minimal standard generator (multiplier 16807, modulus
# 2^31 - 1): `n` uniform numbers in (0, 1) from `seed`, without the random
# numbers of the session. 16807 * (2^31 - 2) < 2^53, so the products are
# exact in double precision.
pm_uniform <- function(n, seed) {
  m <- 2147483647
  state <- abs(as.numeric(seed)) %% m
  if (state == 0) state <- 1
  out <- numeric(n)
  for (i in seq_len(n)) {
    state <- (16807 * state) %% m
    out[i] <- state / m
  }
  out
}
