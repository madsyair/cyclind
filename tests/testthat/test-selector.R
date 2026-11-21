# Component selection strategies (ADR-014): the default loop and the
# loss-function search after GAA17 pp. 279-280.

test_that("the default selector is the selection loop", {
  spec <- cli_spec()
  expect_s4_class(spec@selector, "BackwardSelection")
  expect_s4_class(cyclind:::spec_selector(spec), "BackwardSelection")
  old <- spec
  attr(old, "selector") <- NULL
  expect_s4_class(cyclind:::spec_selector(old), "BackwardSelection")
  expect_output(show(spec), "loop:")
  expect_output(show(backward_selection()), "backward elimination")
  expect_error(cli_spec(selector = "loss"), class = "cyclind_error_argument")
})

test_that("loss_selection checks its arguments", {
  s <- loss_selection(weights = c(missed = 2))
  expect_equal(s@weights[["missed"]], 2)
  expect_equal(s@weights[["extra"]], 1)
  expect_output(show(s), "weights: missed = 2")
  expect_error(loss_selection(weights = c(other = 1)),
               class = "cyclind_error_argument")
  expect_error(loss_selection(weights = c(missed = -1)),
               class = "cyclind_error_argument")
  expect_error(loss_selection(weights = 1), class = "cyclind_error_argument")
  expect_error(loss_selection(max_eval = 0), class = "cyclind_error_argument")
  expect_output(show(cli_spec(selector = s)), "selection:   loss-function")
})

test_that("the loss follows its formula", {
  spec <- cli_spec(freq = 12)
  w <- loss_selection()@weights
  row <- data.frame(n_ref = 10, missed = 2, extra = 1, sd_lead = 6,
                    ccf_r = 0.8, median_lead = 3)
  expect_equal(cyclind:::composite_loss(row, spec, w),
               0.2 + 0.1 + 0.5 + 0.2 + 3 / 12)
  na_row <- data.frame(n_ref = 0, missed = NA, extra = 0, sd_lead = NA,
                       ccf_r = NA, median_lead = NA)
  expect_equal(cyclind:::composite_loss(na_row, spec, w),
               1 + 1 + 1 + 1 + 6 / 12)
  w0 <- w
  w0[] <- 0
  expect_equal(cyclind:::composite_loss(row, spec, w0), 0)
})

test_that("the stepwise search lowers the loss and logs each move", {
  cand <- sim_candidates(seed = 1)
  spec <- cli_spec(selector = loss_selection())
  res <- suppressWarnings(cli_build(cand, spec))
  au <- audit_log(res)
  expect_match(au$action[1], "^initial composite .* \\(loss [0-9.]+\\)$")
  expect_match(au$action[nrow(au)], "^chosen: smallest loss")
  loss <- as.numeric(sub(".*\\(loss ([0-9.]+)\\)$", "\\1", au$action))
  expect_true(all(diff(loss[-length(loss)]) < 0))
  expect_identical(au$components[nrow(au)],
                   paste(components(res), collapse = ", "))
  expect_true(length(components(res)) >= spec@min_components)
  # the default loop is unchanged
  def <- cli_build(cand)
  expect_identical(audit_log(def)$action[1],
                   "initial composite of all leading candidates")
})

test_that("the random search uses the session RNG and max_eval", {
  cand <- sim_candidates(seed = 1)
  spec <- cli_spec(selector = loss_selection(search = "random",
                                             max_eval = 6))
  set.seed(11)
  a <- suppressWarnings(cli_build(cand, spec))
  set.seed(11)
  b <- suppressWarnings(cli_build(cand, spec))
  expect_identical(components(a), components(b))
  n_eval <- as.integer(sub(".* of ([0-9]+) composites.*", "\\1",
                           audit_log(a)$action[nrow(audit_log(a))]))
  expect_lte(n_eval, 6L)
  # with as many candidates as the minimum, nothing is drawn
  ev <- function(cs) {
    v <- new("CliValidation", turning_points = data.frame(
      n_ref = 4, missed = 0, extra = 0, sd_lead = 1, ccf_r = 0.9,
      median_lead = 6, ccf_lag = 6, median_lead = 6, pct_led = 1,
      concordance = 0.9), probability = data.frame(),
      out_of_sample = data.frame(), passed = TRUE, failed = character(0))
    list(composite = NULL, validation = v, score = 1)
  }
  out <- suppressWarnings(cyclind:::loss_search(
    loss_selection(search = "random"), c("a", "b"), ev, cli_spec(), "start"))
  expect_identical(nrow(out$audit), 2L)
})

test_that("a stepwise search adds a candidate back when it helps", {
  # the loss of a composite falls with each component, except 'c'
  ev <- function(cs) {
    v <- new("CliValidation", turning_points = data.frame(
      n_ref = 10, missed = 5 - length(setdiff(cs, "c")) + 2 * ("c" %in% cs),
      extra = 0, sd_lead = 1, ccf_r = 0.9, median_lead = 6, ccf_lag = 6,
      pct_led = 1, concordance = 0.9), probability = data.frame(),
      out_of_sample = data.frame(), passed = FALSE, failed = character(0))
    list(composite = cs, validation = v, score = 0)
  }
  spec <- cli_spec(min_components = 1)
  out <- suppressWarnings(cyclind:::loss_search(
    loss_selection(), c("a", "b", "c"), ev, spec, "start"))
  expect_identical(out$composite, c("a", "b"))
  # a cap on the evaluations stops the search
  out1 <- cyclind:::loss_search(loss_selection(max_eval = 1),
                                c("a", "b", "c"), ev, spec, "start")
  expect_identical(out1$composite, c("a", "b", "c"))
})
