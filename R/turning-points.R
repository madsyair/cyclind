# Bry-Boschan / Harding-Pagan censoring algorithm on a numeric vector.
# Turning points are kept as a data frame with columns `i` (position) and
# `peak` (TRUE for a peak, FALSE for a trough), sorted by position.

tp_candidates <- function(y, k) {
  n <- length(y)
  peak <- trough <- logical(n)
  if (n > 2L * k) {
    for (i in (k + 1L):(n - k)) {
      win <- y[(i - k):(i + k)]
      peak[i] <- all(y[i] >= win)
      trough[i] <- all(y[i] <= win)
    }
  }
  # a flat stretch is neither a peak nor a trough
  flat <- peak & trough
  peak[flat] <- FALSE
  trough[flat] <- FALSE
  tp <- data.frame(i = c(which(peak), which(trough)),
                   peak = c(rep(TRUE, sum(peak)), rep(FALSE, sum(trough))))
  tp[order(tp$i), , drop = FALSE]
}

# Of consecutive turning points of the same type keep the more extreme one.
tp_alternate <- function(tp, y) {
  j <- 1L
  while (j < nrow(tp)) {
    if (tp$peak[j] == tp$peak[j + 1L]) {
      a <- y[tp$i[j]]
      b <- y[tp$i[j + 1L]]
      drop <- if (tp$peak[j]) {
        if (a >= b) j + 1L else j
      } else {
        if (a <= b) j + 1L else j
      }
      tp <- tp[-drop, , drop = FALSE]
    } else {
      j <- j + 1L
    }
  }
  rownames(tp) <- NULL
  tp
}

# Peak-to-peak and trough-to-trough distances of at least `min_cycle`.
tp_enforce_cycle <- function(tp, y, min_cycle) {
  for (is_peak in c(TRUE, FALSE)) {
    repeat {
      same <- which(tp$peak == is_peak)
      gaps <- diff(tp$i[same])
      bad <- which(gaps < min_cycle)
      if (length(bad) == 0L) break
      a <- same[bad[1]]
      b <- same[bad[1] + 1L]
      va <- y[tp$i[a]]
      vb <- y[tp$i[b]]
      drop <- if (is_peak) {
        if (va >= vb) b else a
      } else {
        if (va <= vb) b else a
      }
      tp <- tp_alternate(tp[-drop, , drop = FALSE], y)
    }
  }
  tp
}

# Cycles shorter than `min_cycle`, rule "symmetric" (ADR-004): of each
# short cycle the less extreme of its two peaks (troughs) is the candidate
# for removal, as in tp_enforce_cycle(); of all candidates, peaks and
# troughs alike, the removal that keeps the largest total amplitude is
# applied first. tp_enforce_cycle() takes the first short cycle in time,
# peaks before troughs, so its result can depend on the direction of time.
tp_cycle_symmetric <- function(tp, y, min_cycle) {
  repeat {
    cand <- integer(0)
    for (is_peak in c(TRUE, FALSE)) {
      same <- which(tp$peak == is_peak)
      bad <- which(diff(tp$i[same]) < min_cycle)
      for (b in bad) {
        a <- same[b]
        c2 <- same[b + 1L]
        va <- y[tp$i[a]]
        vb <- y[tp$i[c2]]
        cand <- c(cand, if ((va >= vb) == is_peak) c2 else a)
      }
    }
    if (length(cand) == 0L) break
    best <- NULL
    best_amp <- -Inf
    for (k in unique(cand)) {
      trial <- tp_alternate(tp[-k, , drop = FALSE], y)
      amp <- tp_amplitude(trial, y)
      if (amp > best_amp + 1e-12) {
        best <- trial
        best_amp <- amp
      }
    }
    tp <- best
  }
  tp
}

# No turning point within `e` observations of the ends.
tp_enforce_ends <- function(tp, n, e) {
  keep <- tp$i > e & tp$i <= n - e
  tp[keep, , drop = FALSE]
}

# First (last) peak must exceed the values before (after) it; troughs
# likewise. With `strict` (Bry and Boschan 1971, step "eliminate peaks
# (troughs) at both ends of the series which are lower (higher) than values
# closer to the end") every value between the turning point and the end
# counts; otherwise only the end value y[1] or y[n], as the BBQ program in
# 'BCDating' (BCDating.enfvc) does.
tp_enforce_extremes <- function(tp, y, strict = TRUE) {
  n <- length(y)
  beyond <- function(i, first, is_peak) {
    ends <- if (strict) {
      if (first) y[seq_len(i - 1L)] else y[seq.int(i + 1L, length.out = n - i)]
    } else {
      if (first) y[1] else y[n]
    }
    if (length(ends) == 0L) return(FALSE)
    if (is_peak) any(ends > y[i]) else any(ends < y[i])
  }
  repeat {
    changed <- FALSE
    if (nrow(tp) > 0L && beyond(tp$i[1], TRUE, tp$peak[1])) {
      tp <- tp[-1L, , drop = FALSE]
      changed <- TRUE
    }
    l <- nrow(tp)
    if (l > 0L && beyond(tp$i[l], FALSE, tp$peak[l])) {
      tp <- tp[-l, , drop = FALSE]
      changed <- TRUE
    }
    if (!changed) break
  }
  tp
}

# Phases shorter than `min_phase`, rule "bbq" (cyclind 0.1.0): drop the
# later turning point of the pair. Depends on the direction of time.
tp_phase_later <- function(tp, y, min_phase) {
  j <- 1L
  while (j < nrow(tp)) {
    if (tp$i[j + 1L] - tp$i[j] < min_phase) {
      tp <- tp_alternate(tp[-(j + 1L), , drop = FALSE], y)
    } else {
      j <- j + 1L
    }
  }
  tp
}

# Total amplitude of the turning-point path: the sum of the absolute
# differences between consecutive turning points.
tp_amplitude <- function(tp, y) sum(abs(diff(y[tp$i])))

# Phases shorter than `min_phase`, rule "symmetric" (ADR-004): for every
# short phase both of its turning points are candidates for removal (the
# alternation then drops the less extreme of the two neighbours of the
# other type); of all candidates, the removal that keeps the largest total
# amplitude is applied, and the search repeats until no phase is short.
# Every step depends only on the values and the set of short phases, not on
# the direction of time.
tp_phase_symmetric <- function(tp, y, min_phase) {
  repeat {
    short <- which(diff(tp$i) < min_phase)
    if (length(short) == 0L) break
    cand <- unique(c(short, short + 1L))
    best <- NULL
    best_amp <- -Inf
    for (k in cand) {
      trial <- tp_alternate(tp[-k, , drop = FALSE], y)
      amp <- tp_amplitude(trial, y)
      if (amp > best_amp + 1e-12) {
        best <- trial
        best_amp <- amp
      }
    }
    tp <- best
  }
  tp
}

tp_enforce_phase <- function(tp, y, min_phase, rule = "symmetric") {
  if (identical(rule, "bbq")) {
    tp_phase_later(tp, y, min_phase)
  } else {
    tp_phase_symmetric(tp, y, min_phase)
  }
}

tp_core <- function(y, window, min_phase, min_cycle, end_exclusion,
                    phase_rule = "symmetric") {
  n <- length(y)
  # the end rule of Bry and Boschan (1971) with the symmetric phase rule,
  # the one of 'BCDating' with the BBQ rule that replicates it (audit S4)
  strict <- !identical(phase_rule, "bbq")
  tp <- tp_alternate(tp_candidates(y, window), y)
  for (iter in seq_len(100L)) {
    before <- tp
    tp <- if (identical(phase_rule, "bbq")) {
      tp_enforce_cycle(tp, y, min_cycle)
    } else {
      tp_cycle_symmetric(tp, y, min_cycle)
    }
    tp <- tp_alternate(tp_enforce_ends(tp, n, end_exclusion), y)
    tp <- tp_alternate(tp_enforce_extremes(tp, y, strict), y)
    tp <- tp_enforce_phase(tp, y, min_phase, phase_rule)
    tp <- tp_alternate(tp_enforce_extremes(tp, y, strict), y)
    if (identical(before, tp)) break
  }
  tp
}

date_with_rules <- function(dater, x, name) {
  check_ts(x)
  span <- non_na_span(x)
  rules <- resolve_dater(dater, stats::frequency(x))
  if (anyNA(span)) {
    tp <- data.frame(i = integer(0), peak = logical(0))
  } else {
    y <- as.numeric(x)[span[1]:span[2]]
    if (anyNA(y)) {
      cyclind_abort("Internal missing values are not allowed when dating.",
                    "cyclind_error_missing")
    }
    tp <- do.call(tp_core, c(list(y = y), rules))
    tp$i <- tp$i + span[1] - 1L
  }
  new("TurningPoints", name = name, series = x, index = as.integer(tp$i),
      type = c("trough", "peak")[tp$peak + 1L], params = rules)
}

#' @rdname date_turning_points
setMethod("date_turning_points", "BryBoschan",
          function(dater, x, name = "", ...) date_with_rules(dater, x, name))

#' @rdname date_turning_points
setMethod("date_turning_points", "BBQ",
          function(dater, x, name = "", ...) date_with_rules(dater, x, name))

#' Expansion and contraction states implied by turning points
#'
#' Returns 1 in expansions (after a trough up to and including the next peak)
#' and 0 in contractions. Observations before the first turning point take the
#' state that the first turning point ends; observations outside the
#' non-missing span of the series are `NA`.
#'
#' @param tp A [TurningPoints-class] object.
#' @return An integer `ts` aligned with the dated series.
#' @examples
#' x <- ts(sin(2 * pi * (1:120) / 40), frequency = 4)
#' cycle_states(date_turning_points(bbq(), x))
#' @export
cycle_states <- function(tp) {
  if (!is(tp, "TurningPoints")) {
    cyclind_abort("`tp` must be a TurningPoints object.",
                  "cyclind_error_argument")
  }
  x <- tp@series
  n <- length(x)
  s <- rep(NA_integer_, n)
  span <- non_na_span(x)
  if (anyNA(span) || length(tp@index) == 0L) return(as_ts_like(s, x))
  first_is_peak <- tp@type[1] == "peak"
  state <- if (first_is_peak) 1L else 0L
  breaks <- c(tp@index, span[2])
  pos <- span[1]
  for (b in seq_along(breaks)) {
    s[pos:breaks[b]] <- state
    pos <- breaks[b] + 1L
    state <- 1L - state
    if (pos > span[2]) break
  }
  as_ts_like(s, x)
}
