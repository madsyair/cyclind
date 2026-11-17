#' Build a composite leading indicator
#'
#' Runs the whole workflow: cycle extraction ([cli_cycles()]), turning-point
#' dating ([cli_turning_points()]), classification ([cli_assess()]),
#' aggregation of the leading candidates ([cli_composite()]) and validation
#' ([cli_validate()]). When the composite fails the leading criteria, the
#' selection loop removes, one at a time, the component whose removal gives
#' the best composite (backward elimination) and tests again. The loop stops
#' when the composite passes, when `spec@min_components` components remain or
#' after `spec@max_iter` iterations. Every iteration is written to the audit
#' log ([audit_log()]).
#'
#' When `spec@lag_shift` is `TRUE`, every composite is built from components
#' shifted so that their median leads align on the shortest one (OECD
#' lag-shifting; see [cli_composite()]).
#'
#' Composites are ranked by the score
#' \deqn{10 \times \textrm{criteria met} + \min(\textrm{lead}, 2\,
#' \textrm{min\_lead}) + 5 \times \textrm{concordance} - 5 \times
#' (\textrm{missed share} + \textrm{extra share}),}
#' where the lead is that of the cross-correlation peak.
#'
#' Before the loop, twin candidates whose normalised cycles correlate at
#' least `spec@twin_threshold` (0.99) in absolute value are reduced to the
#' one with the higher peak cross-correlation with the reference; the twins
#' dropped are listed in a warning (`cyclind_warning_twins`) and in the
#' audit log (iteration 0). Twins add the same information twice and give
#' it double weight. A selected composite of fewer than three components
#' gives a warning (`cyclind_warning_few_components`): it is little more
#' than the cycle of its components and is fragile when one of them is
#' revised.
#'
#' When `spec@oos_cutoffs` is not empty the selection-bias harness
#' ([cli_selection_bias()]) is run as well and stored in the validation.
#'
#' @param candidates A [CandidateSet-class].
#' @param spec A [CliSpec-class]; by default [cli_spec()] at the frequency of
#'   the data.
#' @return A [CliResult-class].
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' res <- cli_build(cli_candidates(sim, "reference", meta = meta))
#' res
#' leaders(res)
#' audit_log(res)
#' @export
cli_build <- function(candidates, spec = NULL) {
  if (is.null(spec) && is(candidates, "CandidateSet")) {
    spec <- cli_spec(freq = stats::frequency(candidates@data))
  }
  check_inputs(candidates, spec)
  cycles <- cli_cycles(candidates, spec)
  candidates <- drop_excluded(candidates, cycles)
  assessment <- cli_assess(candidates, spec, cycles)
  for (nm in assessment@inverted) cycles[[nm]] <- invert_cycle(cycles[[nm]])
  tps <- cli_turning_points(cycles, spec)
  sel <- select_components(cycles, assessment, spec, candidates@reference)
  sel$audit <- rbind(exclusion_audit(cycles), sel$audit)
  validation <- sel$validation
  if (!is.null(validation) && length(spec@oos_cutoffs) > 0L) {
    validation@out_of_sample <- cli_selection_bias(candidates, spec,
                                                   spec@oos_cutoffs)
    validation@quality <- cli_quality(validation, spec)
  }
  new("CliResult", spec = spec, candidates = candidates, cycles = cycles,
      turning_points = tps, assessment = assessment,
      composite = sel$composite, validation = validation, audit = sel$audit)
}

composite_score <- function(val, spec) {
  row <- val@turning_points
  n_ok <- length(leading_checks(row, spec)) - length(val@failed)
  share <- if (row$n_ref > 0L) (row$missed + row$extra) / row$n_ref else 1
  lead <- if (is.na(row$ccf_lag)) 0 else min(row$ccf_lag, 2 * spec@min_lead)
  conc <- if (is.na(row$concordance)) 0 else row$concordance
  10 * n_ok + lead + 5 * conc - 5 * share
}

audit_row <- function(iter, action, comps, val, score, spec) {
  tp <- val@turning_points
  data.frame(
    iteration = iter, action = action, n_components = length(comps),
    components = paste(comps, collapse = ", "),
    ccf_lag = tp$ccf_lag, median_lead = tp$median_lead,
    pct_led = tp$pct_led, missed = tp$missed, extra = tp$extra,
    concordance = tp$concordance, passed = val@passed, score = score,
    stringsAsFactors = FALSE
  )
}

empty_audit <- function(action) {
  data.frame(iteration = 0L, action = action, n_components = 0L,
             components = "", ccf_lag = NA_real_, median_lead = NA_real_,
             pct_led = NA_real_, missed = NA_real_, extra = NA_real_,
             concordance = NA_real_, passed = FALSE, score = NA_real_,
             stringsAsFactors = FALSE)
}

# Backward-elimination loop over the leading candidates.
select_components <- function(cycles, assessment, spec, reference) {
  ref <- cycles[[reference]]
  comps <- leaders(assessment)
  if (length(comps) == 0L) {
    cyclind_warn("No candidate was classified as leading; no composite built.",
                 "cyclind_warning_no_leaders")
    return(list(composite = NULL, validation = NULL,
                audit = empty_audit("no leading candidates")))
  }
  twins <- drop_twins(comps, cycles, assessment, spec)
  comps <- twins$keep
  shift_on <- methods::.hasSlot(spec, "lag_shift") && isTRUE(spec@lag_shift)
  evaluate <- function(cs) {
    sh <- if (shift_on) lag_shifts(cs, assessment) else NULL
    comp <- cli_composite(cycles, cs, spec, ref, shifts = sh)
    val <- validate_leading(comp, ref, spec, prediction = FALSE)
    list(composite = comp, validation = val, score = composite_score(val, spec))
  }
  sel <- elimination_loop(comps, evaluate, spec,
                          "initial composite of all leading candidates")
  sel$audit <- rbind(twins$audit, sel$audit)
  # the prediction group only for the chosen composite
  sel$validation <- cli_validate(sel$composite, ref, spec)
  sel
}

# Twin threshold of a specification (objects saved before 0.2.0 keep
# every candidate but identical ones).
spec_twin_threshold <- function(spec) {
  if (methods::.hasSlot(spec, "twin_threshold") &&
      length(spec@twin_threshold) == 1L) spec@twin_threshold else 1
}

# Candidates whose cycles correlate at least `threshold` in absolute value
# with a better candidate (higher `score`), in order of the score: data
# frame of `name`, `twin_of` and `r` (butir 8).
find_twins <- function(comps, cycles, score, threshold) {
  out <- data.frame(name = character(0), twin_of = character(0),
                    r = numeric(0), stringsAsFactors = FALSE)
  if (length(comps) < 2L) return(out)
  s <- score[comps]
  s[is.na(s)] <- -Inf
  ord <- comps[order(-s, seq_along(comps))]
  kept <- ord[1]
  for (nm in ord[-1]) {
    a <- as.numeric(cycles[[nm]]@normalised)
    r <- vapply(kept, function(k) {
      b <- as.numeric(cycles[[k]]@normalised)
      ok <- !is.na(a) & !is.na(b)
      if (sum(ok) < 3L || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) {
        return(0)
      }
      stats::cor(a[ok], b[ok])
    }, numeric(1))
    hit <- which(abs(r) >= threshold)
    if (length(hit) > 0L) {
      out[nrow(out) + 1L, ] <- list(nm, kept[hit[1]], r[hit[1]])
    } else {
      kept <- c(kept, nm)
    }
  }
  out
}

# Drop twin candidates before the selection loop, with a warning and audit
# rows.
drop_twins <- function(comps, cycles, assessment, spec) {
  tab <- assessment@table
  score <- stats::setNames(tab$ccf_r, tab$name)
  tw <- find_twins(comps, cycles, score, spec_twin_threshold(spec))
  if (nrow(tw) == 0L) return(list(keep = comps, audit = NULL))
  action <- sprintf("twin dropped %s (|r| %.3f with %s)", tw$name,
                    abs(tw$r), tw$twin_of)
  cyclind_warn(sprintf(paste("Twin candidates dropped before the selection",
                             "(|r| >= %s): %s."), spec_twin_threshold(spec),
                       paste(sprintf("%s (twin of %s)", tw$name, tw$twin_of),
                             collapse = ", ")),
               "cyclind_warning_twins")
  audit <- do.call(rbind, lapply(action, function(a) {
    row <- empty_audit(a)
    row$n_components <- NA_integer_
    row
  }))
  list(keep = setdiff(comps, tw$name), audit = audit)
}

# A composite of fewer than three components (butir 7).
warn_few_components <- function(comps) {
  if (length(comps) > 0L && length(comps) < 3L) {
    cyclind_warn(sprintf(paste("The selected composite has %d component(s):",
                               "%s. With fewer than three it is little more",
                               "than the cycle of its components."),
                         length(comps), paste(comps, collapse = ", ")),
                 "cyclind_warning_few_components")
  }
  invisible(comps)
}

# Backward elimination shared by the leading and coincident composites:
# `evaluate(components)` returns the composite, its validation and its score.
elimination_loop <- function(comps, evaluate, spec, first_action) {
  current <- evaluate(comps)
  rows <- list(audit_row(1L, first_action, comps, current$validation,
                         current$score, spec))
  iter <- 1L
  repeat {
    if (current$validation@passed) {
      rows[[length(rows) + 1L]] <- audit_row(iter, "accepted: all criteria met",
                                             comps, current$validation,
                                             current$score, spec)
      break
    }
    if (length(comps) <= spec@min_components) {
      rows[[length(rows) + 1L]] <- audit_row(
        iter, "stopped: minimum number of components reached", comps,
        current$validation, current$score, spec)
      break
    }
    if (iter >= spec@max_iter) {
      rows[[length(rows) + 1L]] <- audit_row(
        iter, "stopped: maximum number of iterations", comps,
        current$validation, current$score, spec)
      break
    }
    iter <- iter + 1L
    trials <- lapply(comps, function(drop) evaluate(setdiff(comps, drop)))
    scores <- vapply(trials, function(t) t$score, numeric(1))
    best <- which.max(scores)
    dropped <- comps[best]
    comps <- setdiff(comps, dropped)
    current <- trials[[best]]
    rows[[length(rows) + 1L]] <- audit_row(iter, sprintf("dropped %s", dropped),
                                           comps, current$validation,
                                           current$score, spec)
  }
  audit <- do.call(rbind, rows)
  rownames(audit) <- NULL
  warn_few_components(comps)
  list(composite = current$composite, validation = current$validation,
       audit = audit)
}
