# Fixes from the audit of 2026-11-18 (INC-21). Each block names the
# finding (S = statistical, E = engineering).

# ---- S1: the Christiano-Fitzgerald filter removes the drift ---------------

test_that("S1: the CF random-walk filter removes the drift by default", {
  x <- ts(0.01 * (1:120), frequency = 4, start = 1990)
  out <- extract_cycle(cf_filter(), x)
  expect_lt(max(abs(out)), 1e-10)
  # the old filter, without drift removal, is still available
  old <- extract_cycle(cf_filter(drift = FALSE), x)
  expect_gt(max(abs(old)), 1e-3)
  expect_true(cf_filter()@drift)
  expect_error(cf_filter(drift = NA), class = "cyclind_error_argument")
  expect_error(cf_filter(drift = "yes"), class = "cyclind_error_argument")
})

test_that("S1: the CF filter with drift replicates mFilter", {
  skip_if_not_installed("mFilter")
  set.seed(12)
  x <- ts(cumsum(rnorm(200, 0.3)) + 100, frequency = 4, start = 1970)
  ours <- extract_cycle(cf_filter(6, 32), x)
  ref <- mFilter::cffilter(x, pl = 6, pu = 32, root = TRUE,
                           drift = TRUE)$cycle
  expect_equal(as.numeric(ours), as.numeric(ref), tolerance = 1e-8)
})

test_that("S1: CF filters saved before the drift slot remove the drift", {
  f <- cf_filter()
  attr(f, "drift") <- NULL
  expect_false(methods::.hasSlot(f, "drift"))
  x <- ts(0.01 * (1:120), frequency = 4)
  expect_lt(max(abs(extract_cycle(f, x))), 1e-10)
})

# ---- S3: QS statistic is zero when the seasonal autocorrelation is <= 0 ---

test_that("S3: the QS statistic is 0 when r_s is not positive (X-13)", {
  set.seed(101)
  z <- ts(cumsum(rnorm(120)), frequency = 12, start = 2000)
  r <- stats::acf(diff(z), lag.max = 24, plot = FALSE)$acf[-1]
  expect_lt(r[12], 0)
  expect_gt(r[24], 0)
  qs <- cyclind:::qs_test(z)
  expect_equal(qs$statistic, 0)
  expect_equal(qs$p_value, 1)
  # a seasonal series still gives a large statistic
  s <- ts(rep(c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5), 10) +
            cumsum(rnorm(120, 0, 0.1)), frequency = 12)
  expect_gt(cyclind:::qs_test(s)$statistic, 10)
})

# ---- S5: Conference Board changes of series in percent ---------------------

test_that("S5: Conference Board uses simple differences for percent series", {
  expect_equal(cyclind:::symmetric_change(c(4, 5, 6), percent = TRUE),
               c(NA, 1, 1))
  expect_equal(cyclind:::symmetric_change(c(4, 5)),
               c(NA, 200 * 1 / 9))
  sim <- simulate_cycles(seed = 3)
  # an interest rate in percent, positive throughout
  sim[, "lead_3"] <- 5 + (sim[, "lead_3"] - mean(sim[, "lead_3"])) / 10
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage",
                     unit = c("level", "percent", rep("level", 5)))
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(12, aggregator = conference_board())
  cyc <- cli_cycles(cand, spec)
  expect_true("unit: percent" %in% cyc$lead_3@steps)
  expect_false("unit: percent" %in% cyc$lead_6@steps)
  cb <- cli_composite(cyc, c("lead_6", "lead_3"), spec, cyc$reference)
  lv <- cyclind:::cycle_level(cyc$lead_3)
  ch <- cbind(cyclind:::symmetric_change(cyclind:::cycle_level(cyc$lead_6)),
              c(NA, diff(lv)))
  s <- apply(ch, 2, stats::sd, na.rm = TRUE)
  w <- (1 / s) / sum(1 / s)
  expect_equal(unname(cb@weights), unname(w), tolerance = 1e-12)
})

# ---- S6: internal gaps of the Conference Board index -----------------------

test_that("S6: a gap in the Conference Board contributions stays missing", {
  sim <- simulate_cycles(seed = 3)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(12, aggregator = conference_board(min_coverage = 1))
  cyc <- cli_cycles(cand, spec)
  gap <- 150:152
  cyc$lead_6@normalised[gap] <- NA
  cyc$lead_6@transformed[gap] <- NA
  cb <- cli_composite(cyc, c("lead_6", "lead_3"), spec, cyc$reference)
  # the change into and out of the gap is unknown
  expect_true(all(is.na(cb@index[gap])))
  expect_true(all(is.na(cb@cycle[gap])))
  expect_false(anyNA(cb@index[c(1:149, 154:length(cb@index))][-1]))
  expect_false(anyNA(cb@cycle[c(1:149, 154:length(cb@cycle))][-1]))
})

# ---- S4: end rule of Bry and Boschan (1971) --------------------------------

test_that("S4: a first peak lower than an earlier value is dropped", {
  y <- sin(2 * pi * (1:120) / 40)
  y[1:4] <- c(0, 2, 2.5, 2)
  x <- ts(y, frequency = 12, start = 2000)
  tp <- date_turning_points(bry_boschan(), x)
  # y[3] = 2.5 exceeds the peak at i = 10 although y[1] = 0 does not
  expect_false(10L %in% tp@index)
  expect_equal(tp@index[1], 30L)
  expect_equal(tp@type[1], "trough")
  # the same at the end, for a trough
  rev_tp <- date_turning_points(bry_boschan(), ts(-rev(y), frequency = 12))
  expect_false((121L - 10L) %in% rev_tp@index)
  # the BBQ phase rule keeps the end rule of 'BCDating' (y[1] and y[n])
  old <- date_turning_points(bry_boschan(phase_rule = "bbq"), x)
  expect_equal(old@index[1], 10L)
})

# ---- E2: a short candidate is excluded, not fatal --------------------------

test_that("E2: a candidate shorter than four years is excluded", {
  sim <- simulate_cycles(seed = 1)
  sim[1:(nrow(sim) - 36L), "noise_1"] <- NA
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(12)
  expect_warning(cyc <- cli_cycles(cand, spec),
                 "'noise_1': it has fewer than four years of observations;",
                 class = "cyclind_warning_short_series")
  expect_false("noise_1" %in% names(cyc))
  ex <- excluded_series(cyc)
  expect_equal(ex$name, "noise_1")
  expect_match(ex$reason, "fewer than four years")
  res <- suppressWarnings(cli_build(cand, spec))
  expect_false("noise_1" %in% res@assessment@table$name)
  expect_match(audit_log(res)$action[1], "^excluded noise_1")
  # the reference still stops the run
  short_ref <- sim
  short_ref[1:(nrow(sim) - 36L), "reference"] <- NA
  cand2 <- cli_candidates(short_ref, "reference", meta = meta)
  expect_error(cli_cycles(cand2, spec), "'reference' has fewer",
               class = "cyclind_error_short_series")
  # every candidate too short
  only <- cand["noise_1"]
  expect_error(suppressWarnings(cli_build(only, spec)), "No candidate is left",
               class = "cyclind_error_short_series")
})

# ---- E3: Chow-Lin checks its arguments before the dependency ---------------

test_that("E3: Chow-Lin without an indicator is an argument error", {
  sim <- simulate_cycles(seed = 4, n_years = 12)
  q <- stats::aggregate(sim[, "reference"], nfrequency = 4, FUN = sum)
  expect_error(cyclind:::regression_disagg(q, "flow", NULL),
               class = "cyclind_error_argument")
})

# ---- E4, E5, E6: dates of read_cli_data() ----------------------------------

test_that("E4: months outside 1 to 12 are rejected", {
  # both sequences are consecutive when months are counted as numbers
  bad <- list(c("2000-11", "2001-00", "2001-01"),
              c("2000-12", "2000-13", "2001-02"))
  for (d in bad) {
    df <- data.frame(date = d, x = 1:3, y = 3:1)
    expect_error(read_cli_data(df), "between 1 and 12",
                 class = "cyclind_error_dates")
  }
  df <- data.frame(date = c("2000-11", "2000-12", "2001-01"), x = 1:3,
                   y = 3:1)
  expect_equal(start(read_cli_data(df)), c(2000, 11))
})

test_that("E5: a frequency that contradicts the dates is an error", {
  q <- data.frame(date = c("2001Q1", "2001Q2", "2001Q3"), x = 1:3, y = 3:1)
  expect_error(read_cli_data(q, frequency = 12), "quarterly",
               class = "cyclind_error_frequency")
  expect_equal(frequency(read_cli_data(q, frequency = 4)), 4)
  m <- data.frame(date = c("2001-01", "2001-02", "2001-03"), x = 1:3, y = 3:1)
  expect_equal(frequency(read_cli_data(m, frequency = 12)), 12)
  # quarterly data dated by the first month of the quarter stay allowed
  qm <- data.frame(date = c("2001-01-01", "2001-04-01", "2001-07-01"),
                   x = 1:3, y = 3:1)
  expect_equal(start(read_cli_data(qm, frequency = 4)), c(2001, 1))
})

test_that("E6: trailing blank rows (Excel) are dropped", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c("date,x,y", "2001Q1,1,3", "2001Q2,2,2", "2001Q3,3,1", ",,",
               ",,", ""), f)
  out <- read_cli_data(f)
  expect_equal(nrow(out), 3L)
  expect_equal(frequency(out), 4)
  # a blank row inside the data is still an error
  writeLines(c("date,x,y", "2001Q1,1,3", ",,", "2001Q3,3,1"), f)
  expect_error(read_cli_data(f), class = "cyclind_error_dates")
})

# ---- E7: leads of simulate_cycles() ----------------------------------------

test_that("E7: leads must be named whole numbers", {
  for (bad in list(c(6, 3), c(a = 1.5), c(a = NA), list(a = 1),
                   c(a = "1"), c(a = 1, a = 2), c(a = Inf),
                   stats::setNames(1, ""))) {
    expect_error(simulate_cycles(leads = bad, seed = 1),
                 "`leads` must be", class = "cyclind_error_argument")
  }
  expect_error(simulate_cycles(counter = c(2.5), seed = 1),
               "`counter` must be", class = "cyclind_error_argument")
  expect_error(simulate_cycles(leads = c(a = 1), counter = c(a = 2),
                               seed = 1),
               "names", class = "cyclind_error_argument")
  sim <- simulate_cycles(leads = c(a = 2L), counter = c(b = 1), seed = 1)
  expect_equal(colnames(sim)[1:3], c("reference", "a", "b"))
  sim <- simulate_cycles(leads = c(a = 2), counter = numeric(0), n_noise = 0,
                         seed = 1)
  expect_equal(colnames(sim), c("reference", "a"))
})

# ---- nits ------------------------------------------------------------------

test_that("shift_vec() needs a whole number of periods", {
  expect_equal(cyclind:::shift_vec(1:4 + 0, 1), c(NA, 1, 2, 3))
  expect_error(cyclind:::shift_vec(1:4, 1.5), class = "cyclind_error_argument")
  expect_error(cyclind:::shift_vec(1:4, NA), class = "cyclind_error_argument")
})

test_that("cli_composite() names unknown components", {
  sim <- simulate_cycles(seed = 1)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  spec <- cli_spec(12)
  cyc <- cli_cycles(cli_candidates(sim, "reference", meta = meta), spec)
  expect_error(cli_composite(cyc, c("lead_6", "lead_9", "foo"), spec),
               "Unknown component\\(s\\): lead_9, foo",
               class = "cyclind_error_argument")
  expect_error(cli_composite(cyc, character(0), spec),
               class = "cyclind_error_argument")
})

test_that("duplicated names in `meta` are an error", {
  sim <- simulate_cycles(seed = 1)
  meta <- data.frame(name = c(colnames(sim)[-1], "lead_6"),
                     rationale = "early_stage")
  expect_error(cli_candidates(sim, "reference", meta = meta),
               "lead_6", class = "cyclind_error_argument")
  meta <- data.frame(name = c("reference", "reference"), unit = "level")
  expect_error(cli_candidates(sim, "reference", meta = meta),
               "reference", class = "cyclind_error_argument")
})

test_that("the script writes the seed and frequency as R values", {
  lines <- cyclind:::data_lines(list(source = "example_m", frequency = 12,
                                     seed = "1); system('x'"))
  expect_true(any(grepl("seed = \"1); system('x'\")", lines, fixed = TRUE)))
  lines <- cyclind:::data_lines(list(source = "example_m", frequency = 12,
                                     seed = 7))
  expect_true(any(grepl("simulate_cycles(freq = 12, seed = 7)", lines,
                        fixed = TRUE)))
})

# ---- E8: validity of the core classes --------------------------------------

test_that("E8: CandidateSet, CompositeIndex and CycleSeries are validated", {
  sim <- simulate_cycles(seed = 1)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  expect_true(validObject(cand))
  bad <- cand
  bad@reference <- "gdp"
  expect_error(validObject(bad), "reference")
  bad <- cand
  bad@meta$name[1] <- "foo"
  expect_error(validObject(bad), "meta\\$name")
  bad <- cand
  bad@meta <- rbind(bad@meta, bad@meta[1, ])
  expect_error(validObject(bad), "duplicated")
  spec <- cli_spec(12)
  cyc <- cli_cycles(cand, spec)
  expect_true(validObject(cyc$lead_6))
  bad <- cyc$lead_6
  bad@cycle <- ts(1:3)
  expect_error(validObject(bad), "same length")
  comp <- cli_composite(cyc, c("lead_6", "lead_3"), spec, cyc$reference)
  expect_true(validObject(comp))
  bad <- comp
  bad@weights <- c(lead_6 = 1)
  expect_error(validObject(bad), "one value per component")
  bad <- comp
  names(bad@weights) <- c("a", "b")
  expect_error(validObject(bad), "names of `weights`")
})

# ---- E1, E9: uploads and malformed inputs of the interface -----------------

test_that("E1: an upload path outside shiny's upload directory is refused", {
  skip_if_not_installed("shiny")
  secret <- tempfile("secret", tmpdir = tempfile("outside"))
  dir.create(dirname(secret))
  writeLines(c("date,a,b", "2001Q1,1,2", "2001Q2,2,3", "2001Q3,3,4"),
             secret)
  good <- as_upload(secret, "data.csv")
  expect_identical(cyclind:::checked_upload(good)$name, "data.csv")
  hostile <- list(
    # a file the R process can read, anywhere on disk
    data.frame(name = "x.csv", datapath = secret),
    data.frame(name = "x.csv", datapath = file.path(R.home(), "COPYING")),
    # relative traversal out of an upload directory
    data.frame(name = "x.csv",
               datapath = file.path(dirname(good$datapath), "..", "..",
                                    basename(secret))),
    # not a data frame, or without the expected columns
    list(name = "x.csv", datapath = good$datapath),
    data.frame(name = "x.csv", path = good$datapath),
    data.frame(name = character(0), datapath = character(0))
  )
  for (h in hostile) {
    expect_error(cyclind:::checked_upload(h), "not valid",
                 class = "cyclind_error_argument")
  }
  for (h in hostile[c(1, 4)]) {
    shiny::testServer(cyclind:::app_server("en"), {
      session$setInputs(source = "upload", freq = "auto", start = "",
                        csv_sep = "auto", csv_dec = "auto", file = h)
      session$setInputs(run1 = 1)
      expect_null(rv$data)
      expect_false(1L %in% rv$done)
      session$setInputs(source = "upload_multi", target_freq = "4",
                        files = h, run1 = 2)
      expect_null(rv$series)
      expect_false(1L %in% rv$done)
    })
  }
  shiny::testServer(cyclind:::app_server("en"), {
    session$setInputs(source = "upload", freq = "auto", start = "",
                      csv_sep = "auto", csv_dec = "auto", file = good)
    session$setInputs(run1 = 1)
    expect_equal(colnames(rv$data), c("a", "b"))
  })
})

test_that("E9: malformed step-1 inputs do not crash the session", {
  skip_if_not_installed("shiny")
  shiny::testServer(cyclind:::app_server("en"), {
    for (bad in list(list(source = c("upload", "x")),
                     list(source = "elsewhere"),
                     list(source = "example_m", csv_sep = list(1)),
                     list(source = "example_m", csv_sep = c("comma", "tab")),
                     list(source = "upload_multi", target_freq = "7",
                          files = multi_files()))) {
      do.call(session$setInputs, c(bad, list(run1 = sample.int(1e6, 1))))
      expect_false(1L %in% rv$done)
    }
    session$setInputs(source = "example_m", csv_sep = "auto", seed = 1,
                      run1 = 1e7)
    expect_true(1L %in% rv$done)
  })
})
