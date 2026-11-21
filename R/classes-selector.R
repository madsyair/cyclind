#' Component selection strategies
#'
#' A `Selector` decides how [cli_build()] searches the leading candidates
#' for the components of the composite. It is set in the specification
#' (`selector` in [cli_spec()] and [oecd_spec()]).
#'
#' * `backward_selection()` (the default): the selection loop of
#'   [cli_build()]. It starts from all leading candidates and removes, one
#'   at a time, the component whose removal gives the best composite until
#'   the composite meets the leading criteria.
#' * `loss_selection()`: a heuristic search for the combination of
#'   components that minimises a loss function, after the OECD workflow:
#'   "With a heuristic search several combinations of the series from the
#'   short-list are composed into competing CLIs ... A loss function
#'   combining statistical properties and practical considerations on the
#'   actual implementation is used to assign scores to each CLI variant.
#'   The loop of Components selection, Evaluation, Aggregation is repeated
#'   until the score is minimised" (Gyomai, Ahmad and Astolfi 2017,
#'   pp. 279--280). The OECD does not publish its loss function; the one
#'   below, and its weights, are a choice of cyclind.
#'
#' **Loss.** For a composite with turning-point statistics as in
#' [cli_validate()],
#' \deqn{L = w_1 \frac{\textrm{missed}}{n_\textrm{ref}} + w_2
#' \frac{\textrm{extra}}{n_\textrm{ref}} + w_3 \frac{\textrm{sd of
#' lead}}{f} + w_4 (1 - r) + w_5 \frac{\max(0, \textrm{target} -
#' \textrm{median lead})}{f},}
#' where \eqn{n_\textrm{ref}} is the number of reference turning points,
#' the standard deviation of the lead at matched turning points is in years
#' (\eqn{f} periods a year), \eqn{r} is the peak cross-correlation with the
#' reference, and the last term penalises a median lead shorter than the
#' lower end of `target_lead` of the specification (6 months by default).
#' The terms follow the evaluation measures of the OECD (missed and extra
#' cycles, consistency of the lead measured by its standard deviation,
#' cyclical conformity measured by the cross-correlation; Gyomai, Ahmad and
#' Astolfi 2017, pp. 285--286, and the lead the OECD aims at, p. 278).
#' Missing statistics count as the worst value (a share of 1, a standard
#' deviation of one year, a correlation of 0). The weights are named
#' `missed`, `extra`, `sd_lead`, `correlation` and `lead`, all 1 by default.
#'
#' **Search.** With `search = "stepwise"`, starting from all leading
#' candidates (twins removed, as in [cli_build()]), every composite that
#' drops one component or adds back one dropped candidate is evaluated, and
#' the best move is taken while it lowers the loss (forward-backward
#' search). With `search = "random"`, `max_eval` random combinations of at
#' least `min_components` candidates are evaluated, drawn from the session
#' random-number generator (call [set.seed()] first for a reproducible
#' search). Both evaluate at most `max_eval` distinct combinations and keep
#' the one with the smallest loss. Each move is written to the audit log
#' with its loss.
#'
#' New strategies are added by a subclass and a method for the generic
#' `search_components()`, which receives the candidate names, a function
#' `evaluate(components)` returning the composite, its validation and its
#' score, and the specification, and returns a list with the `composite`,
#' its `validation` and the `audit` rows.
#'
#' @param weights Named weights of the loss terms (see **Loss**); missing
#'   names keep the weight 1.
#' @param search `"stepwise"` (forward-backward) or `"random"`.
#' @param max_eval Largest number of combinations evaluated.
#' @return A `Selector` object.
#' @references
#' Gyomai, G., Ahmad, N. and Astolfi, R. (2017). The OECD system of composite
#' leading indicators. In *Handbook on Cyclical Composite Indicators*,
#' chapter 10, 271--291. Eurostat, Luxembourg.
#' @seealso [cli_build()], [cli_spec()]
#' @examples
#' backward_selection()
#' loss_selection(weights = c(missed = 2, correlation = 0.5))
#' \donttest{
#' sim <- simulate_cycles(seed = 1)
#' meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
#' cand <- cli_candidates(sim, "reference", meta = meta)
#' res <- cli_build(cand, cli_spec(selector = loss_selection()))
#' audit_log(res)[, c("action", "components")]
#' }
#' @name Selector-class
#' @aliases Selector
#' @exportClass Selector
NULL

setClass("Selector", representation("VIRTUAL", label = "character"))

#' @rdname Selector-class
setClass("BackwardSelection", contains = "Selector")

#' @rdname Selector-class
setClass("LossSelection", contains = "Selector",
         representation(weights = "numeric", search = "character",
                        max_eval = "numeric"))

loss_terms <- c("missed", "extra", "sd_lead", "correlation", "lead")

#' @rdname Selector-class
#' @export
backward_selection <- function() {
  new("BackwardSelection", label = "backward elimination")
}

#' @rdname Selector-class
#' @export
loss_selection <- function(weights = NULL,
                           search = c("stepwise", "random"),
                           max_eval = 200) {
  search <- match.arg(search)
  w <- stats::setNames(rep(1, length(loss_terms)), loss_terms)
  if (!is.null(weights)) {
    if (!is.numeric(weights) || is.null(names(weights)) ||
        !all(names(weights) %in% loss_terms) || anyNA(weights) ||
        any(weights < 0)) {
      cyclind_abort(sprintf(paste("`weights` must be non-negative numbers",
                                  "named among %s."),
                            paste(loss_terms, collapse = ", ")),
                    "cyclind_error_argument")
    }
    w[names(weights)] <- weights
  }
  check_number(max_eval, "max_eval", lower = 1, integer = TRUE)
  new("LossSelection",
      label = sprintf("loss-function search (%s, at most %d composites)",
                      search, as.integer(max_eval)),
      weights = w, search = search, max_eval = as.numeric(max_eval))
}

#' @rdname Selector-class
#' @param selector A `Selector`.
#' @param comps Names of the candidate components.
#' @param evaluate Function of a vector of component names returning a list
#'   with `composite`, `validation` and `score`.
#' @param spec A [CliSpec-class].
#' @param first_action Text of the first audit row.
#' @export
setGeneric("search_components",
           function(selector, comps, evaluate, spec, first_action) {
  standardGeneric("search_components")
})

#' @rdname Selector-class
setMethod("search_components", "BackwardSelection",
          function(selector, comps, evaluate, spec, first_action) {
  elimination_loop(comps, evaluate, spec, first_action)
})

#' @rdname Selector-class
setMethod("search_components", "LossSelection",
          function(selector, comps, evaluate, spec, first_action) {
  loss_search(selector, comps, evaluate, spec, first_action)
})

#' @rdname Selector-class
#' @param object A `Selector`.
setMethod("show", "Selector", function(object) {
  cat("<", class(object), "> ", object@label, "\n", sep = "")
  if (is(object, "LossSelection")) {
    cat("  weights: ", paste(names(object@weights), object@weights,
                             sep = " = ", collapse = ", "), "\n", sep = "")
  }
  invisible(object)
})

# Selector of a specification (objects saved before it existed use the
# selection loop).
spec_selector <- function(spec) {
  if (methods::.hasSlot(spec, "selector") && is(spec@selector, "Selector")) {
    spec@selector
  } else {
    backward_selection()
  }
}

# Loss of a composite from its turning-point statistics (see ?Selector).
composite_loss <- function(row, spec, weights) {
  f <- spec@freq
  share <- function(k) {
    if (is.na(k) || is.na(row$n_ref) || row$n_ref == 0) 1 else k / row$n_ref
  }
  sd_lead <- if (is.na(row$sd_lead)) f else row$sd_lead
  r <- if (is.na(row$ccf_r)) 0 else row$ccf_r
  med <- if (is.na(row$median_lead)) 0 else row$median_lead
  terms <- c(missed = share(row$missed), extra = share(row$extra),
             sd_lead = sd_lead / f, correlation = 1 - r,
             lead = max(0, spec@target_lead[1] - med) / f)
  sum(weights[names(terms)] * terms)
}

# Heuristic search of a LossSelection (stepwise or random).
loss_search <- function(selector, comps, evaluate, spec, first_action) {
  min_k <- min(length(comps), max(1, spec@min_components))
  cache <- new.env(parent = emptyenv())
  eval_set <- function(cs) {
    cs <- comps[comps %in% cs]
    key <- paste(cs, collapse = "\r")
    if (is.null(cache[[key]])) {
      if (length(ls(cache)) >= selector@max_eval) return(NULL)
      ev <- evaluate(cs)
      ev$loss <- composite_loss(ev$validation@turning_points, spec,
                                selector@weights)
      ev$components <- cs
      cache[[key]] <- ev
    }
    cache[[key]]
  }
  current <- eval_set(comps)
  rows <- list(loss_audit_row(1L, first_action, current, spec))
  current <- if (selector@search == "stepwise") {
    stepwise_moves(current, comps, eval_set, min_k, rows, spec)
  } else {
    random_draws(current, comps, eval_set, min_k, selector@max_eval, rows,
                 spec)
  }
  rows <- current$rows
  best <- current$best
  iter <- length(rows)
  rows[[iter + 1L]] <- loss_audit_row(
    iter, sprintf("chosen: smallest loss of %d composites evaluated",
                  length(ls(cache))), best, spec)
  audit <- do.call(rbind, rows)
  rownames(audit) <- NULL
  warn_few_components(best$components)
  list(composite = best$composite, validation = best$validation,
       audit = audit)
}

loss_audit_row <- function(iter, action, ev, spec) {
  audit_row(iter, sprintf("%s (loss %.3f)", action, ev$loss), ev$components,
            ev$validation, ev$score, spec)
}

# Forward-backward search: take the best single drop or addition while it
# lowers the loss.
stepwise_moves <- function(current, comps, eval_set, min_k, rows, spec) {
  iter <- 1L
  repeat {
    cs <- current$components
    drops <- list()
    if (length(cs) > min_k) {
      drops <- lapply(cs, function(d) {
        list(set = setdiff(cs, d), action = d, kind = "dropped")
      })
    }
    adds <- lapply(setdiff(comps, cs), function(a) {
      list(set = c(cs, a), action = a, kind = "added")
    })
    moves <- c(drops, adds)
    trials <- lapply(moves, function(m) eval_set(m$set))
    ok <- !vapply(trials, is.null, logical(1))
    if (!any(ok)) break
    losses <- vapply(trials[ok], function(t) t$loss, numeric(1))
    k <- which.min(losses)
    if (losses[k] >= current$loss - 1e-12) break
    iter <- iter + 1L
    move <- moves[ok][[k]]
    current <- trials[ok][[k]]
    rows[[length(rows) + 1L]] <- loss_audit_row(
      iter, sprintf("%s %s", move$kind, move$action), current, spec)
  }
  list(best = current, rows = rows)
}

# Random search: draws of at least `min_k` candidates from the session RNG.
random_draws <- function(current, comps, eval_set, min_k, max_eval, rows,
                         spec) {
  best <- current
  n <- length(comps)
  iter <- 1L
  for (draw in seq_len(max_eval)) {
    if (n <= min_k) break
    sizes <- min_k:n
    k <- sizes[sample.int(length(sizes), 1L)]
    ev <- eval_set(sample(comps, k))
    if (is.null(ev)) break
    if (ev$loss < best$loss - 1e-12) {
      iter <- iter + 1L
      best <- ev
      rows[[length(rows) + 1L]] <- loss_audit_row(
        iter, sprintf("random draw %d improves", draw), best, spec)
    }
  }
  list(best = best, rows = rows)
}
