# A reference series without turning points (a short sample, a series
# without cycles, or a growth rate cycle that the dater leaves flat) must
# stop with a clear error, not with an error from data.frame().

# A Bry-Boschan dater that finds no turning point in the reference series
# and dates the other series as usual. The class is removed at the end of
# this file.
setClass("TestFlatReferenceDater", contains = "BryBoschan",
         where = globalenv())
setMethod("date_turning_points", "TestFlatReferenceDater",
          function(dater, x, name = "", ...) {
  tp <- methods::callNextMethod()
  if (identical(name, "reference")) {
    tp@index <- integer(0)
    tp@type <- character(0)
  }
  tp
}, where = globalenv())

flat_reference_dater <- function() {
  new("TestFlatReferenceDater", bry_boschan())
}

flat_reference_setup <- function() {
  dater <- flat_reference_dater()
  sim <- simulate_cycles(seed = 2026)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  list(cand = cand, spec = cli_spec(freq = 12, dater = dater))
}

test_that("matching handles a reference without turning points", {
  x <- stats::ts(sin(seq_len(120) / 6), start = c(2000, 1), frequency = 12)
  ref <- new("TurningPoints", name = "reference", series = x,
             index = integer(0), type = character(0), params = list())
  cand <- new("TurningPoints", name = "a", series = x,
              index = c(10L, 30L), type = c("peak", "trough"),
              params = list())
  m <- match_turning_points(ref, cand, c(12, 24), 6)
  expect_identical(nrow(m$pairs), 0L)
  expect_named(m$pairs, c("ref_index", "type", "cand_index", "lead"))
  expect_identical(m$n_extra, 2L)
})

test_that("matching handles reference turning points outside the candidate", {
  x <- stats::ts(sin(seq_len(120) / 6), start = c(2000, 1), frequency = 12)
  short <- x
  short[1:60] <- NA
  ref <- new("TurningPoints", name = "reference", series = x,
             index = c(10L, 30L), type = c("peak", "trough"),
             params = list())
  cand <- new("TurningPoints", name = "a", series = short,
              index = c(80L, 100L), type = c("peak", "trough"),
              params = list())
  m <- match_turning_points(ref, cand, c(12, 24), 6)
  expect_identical(nrow(m$pairs), 0L)
  expect_identical(m$n_extra, 2L)
})

test_that("cli_assess() rejects a reference without turning points", {
  s <- flat_reference_setup()
  err <- expect_error(cli_assess(s$cand, s$spec),
                      class = "cyclind_error_reference_turning_points")
  expect_match(conditionMessage(err), "'reference' has 0 turning points")
  expect_match(conditionMessage(err), "longer sample")
  expect_match(conditionMessage(err), "cycle form")
})

test_that("cli_build() and cci_build() stop with the same error", {
  s <- flat_reference_setup()
  expect_error(cli_build(s$cand, s$spec),
               class = "cyclind_error_reference_turning_points")
  expect_error(cci_build(s$cand, s$spec),
               class = "cyclind_error_reference_turning_points")
})

test_that("cli_validate() and cci_validate() reject a flat reference", {
  sim <- simulate_cycles(seed = 2026)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  res <- suppressWarnings(cli_build(cand, cli_spec(freq = 12)))
  s <- flat_reference_setup()
  ref <- cycles(res)[["reference"]]
  expect_error(cli_validate(composite(res), ref, s$spec),
               class = "cyclind_error_reference_turning_points")
  expect_error(cci_validate(composite(res), ref, s$spec),
               class = "cyclind_error_reference_turning_points")
})

test_that("cli_selection_bias() rejects a flat reference", {
  s <- flat_reference_setup()
  expect_error(cli_selection_bias(s$cand, s$spec, cutoffs = 2015),
               class = "cyclind_error_reference_turning_points")
})

test_that("a cut-off whose reference has no turning point gets a note", {
  sim <- simulate_cycles(seed = 2026)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(freq = 12)
  cyc <- cli_cycles(cand, spec)
  ref <- cyc[["reference"]]
  ref_tp <- date_turning_points(spec@dater, ref@normalised, name = ref@name)
  # the full reference has turning points; the training window, dated by
  # the flat dater, has none
  out <- suppressWarnings(
    one_cutoff(cand, update(spec, dater = flat_reference_dater()), 2015,
               cyc, ref, ref_tp)
  )
  expect_match(out$note, "no turning point of the reference")
})

test_that("cycle_agreement() does not need turning points", {
  s <- flat_reference_setup()
  cyc <- cli_cycles(s$cand, s$spec)
  agr <- cycle_agreement(cyc[["lead_3"]], cyc[["reference"]])
  expect_true(is.data.frame(agr) || is.list(agr))
})

removeMethod("date_turning_points", "TestFlatReferenceDater",
             where = globalenv())
removeClass("TestFlatReferenceDater", where = globalenv())
