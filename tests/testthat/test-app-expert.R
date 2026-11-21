# Expert judgement in the graphical interface (ADR-014): the expert
# chronology of step 5, the reason of a manual composite, the log of step
# 10, the outlier decisions of step 3, and the script that reproduces them.

test_that("small helpers of the expert inputs", {
  expect_null(cyclind:::parse_period_list(""))
  expect_null(cyclind:::parse_period_list(NULL))
  expect_identical(cyclind:::parse_period_list("2019-02, 2019-09;"),
                   c("2019-02", "2019-09"))
  expect_identical(cyclind:::parse_tp_list("2008-05 Peak; 2009-03 trough"),
                   data.frame(period = c("2008-05", "2009-03"),
                              type = c("peak", "trough")))
  expect_error(cyclind:::parse_tp_list("2008-05"),
               class = "cyclind_error_argument")
  code <- cyclind:::table_code("x", data.frame(a = c("p", "q"), b = 1:2))
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_identical(env$x, data.frame(a = c("p", "q"), b = 1:2))
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  writeLines(c("Period;Type", "2001-01;peak"), f)
  tab <- cyclind:::read_expert_table(f, "type")
  expect_identical(tab$period, "2001-01")
  expect_error(cyclind:::read_expert_table(f, "reason"), "reason",
               class = "cyclind_error_argument")
  expect_false(cyclind:::has_expert_state(list()))
  expect_true(cyclind:::has_expert_state(list(manual_reason = "r")))
  # the specification code of a table of decisions
  inp <- list(method = "cyclind", outlier_method = "tramo", filter = "hp",
              aggregator = "equal", transform = "auto", seasonal = FALSE,
              alpha = 0.05)
  expect_match(cyclind:::spec_call(inp, 12, decisions = TRUE),
               "tramo_outliers(decisions = decisions)", fixed = TRUE)
  inp$method <- "oecd"
  expect_match(cyclind:::spec_call(inp, 12, decisions = TRUE),
               "decisions = decisions", fixed = TRUE)
})

test_that("an expert chronology, a manual reason and the log in the app", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  shiny::testServer(cyclind:::app_server("id"), {
    session$userData$data <- function() rv$data
    run_all_steps(session, cutoffs = "")
    ref <- as.data.frame(rv$tps$reference)
    k <- which.max(ifelse(ref$type == "trough", ref$value, -Inf))
    drop <- paste(ref$period[c(k - 1, k)], collapse = ", ")
    # a change without a reason is refused
    session$setInputs(tp_drop = drop, tp_add = "", tp_reason = "",
                      tp_who = "", tp_apply = 1)
    expect_null(rv$reference_tp)
    session$setInputs(tp_reason = "slowdown above trend", tp_who = "A",
                      tp_apply = 2)
    expect_s4_class(rv$reference_tp, "ExpertChronology")
    expect_identical(length(rv$tps$reference@index), nrow(ref) - 2L)
    expect_false(6L %in% rv$done)
    expect_match(output$tp_log, "titik balik acuan")
    session$setInputs(run6 = 2)
    expect_identical(rv$assessment@table$n_ref[1], nrow(ref) - 2L)
    session$setInputs(comp_mode = "manual",
                      manual_components = c("lead_6", "lead_3"),
                      manual_reason = "keep two clean leaders", run7 = 2)
    lg <- expert_log(result())
    expect_identical(lg$step, c(rep("reference turning points", 2),
                                "composite"))
    expect_match(output$expert_log, "keep two clean leaders")
    expect_true(file.exists(output$dl_expert_log))
    expect_identical(nrow(utils::read.csv(output$dl_expert_log)), 3L)
    expect_match(cli_quality(result())@note, "3 expert decision")
    script <- script()
    expect_match(script, "edit_turning_points(", fixed = TRUE)
    expect_match(script, "reference_tp = reference_tp", fixed = TRUE)
    expect_match(script, "expert_log(res_manual)", fixed = TRUE)
    env <- new.env()
    utils::capture.output(eval(parse(text = script), envir = env))
    expect_equal(as.numeric(env$cli@index), as.numeric(rv$composite@index))
    expect_identical(expert_log(env$res_manual)$decision, lg$decision)
    # back to the algorithm's chronology
    session$setInputs(tp_reset = 1)
    expect_null(rv$reference_tp)
    expect_identical(length(rv$tps$reference@index), nrow(ref))
  })
})

test_that("an uploaded chronology is used in the app and the script", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  shiny::testServer(cyclind:::app_server("en"), {
    session$userData$data <- function() rv$data
    run_all_steps(session, cutoffs = "")
    ref <- as.data.frame(rv$tps$reference)
    utils::write.csv(data.frame(period = ref$period[-(1:2)],
                                type = ref$type[-(1:2)],
                                reason = "national accounts"), f,
                     row.names = FALSE)
    session$setInputs(ref_tp_file = as_upload(f), tp_drop = "", tp_add = "",
                      tp_apply = 1)
    expect_identical(length(rv$tps$reference@index), nrow(ref) - 2L)
    session$setInputs(run6 = 2, comp_mode = "auto", run7 = 2)
    expect_match(output$expert_log, "chronology supplied")
    script <- script()
    expect_match(script, "reference_tp <- data.frame(", fixed = TRUE)
    env <- new.env()
    utils::capture.output(eval(parse(text = script), envir = env))
    expect_equal(components(env$res), rv$composite@components)
    expect_equal(as.numeric(env$res@composite@index),
                 as.numeric(rv$composite@index))
  })
})

test_that("outlier decisions of step 3 reach the spec and the script", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("seasonal")
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  utils::write.csv(data.frame(series = "lead_6", period = "2010-01",
                              type = "LS", action = "remove",
                              reason = "test"), f, row.names = FALSE)
  shiny::testServer(cyclind:::app_server("en"), {
    session$userData$data <- function() rv$data
    session$setInputs(source = "example_m", seed = 1, run1 = 1)
    session$setInputs(reference = "reference")
    n <- length(setdiff(colnames(rv$data), "reference"))
    do.call(session$setInputs, stats::setNames(
      as.list(rep("early_stage", n)), paste0("rat_", seq_len(n))))
    session$setInputs(run2 = 1)
    session$setInputs(method = "oecd", oecd_engine = "x13", seasonal = FALSE,
                      alpha = 0.05, min_auc = 0.7, min_components = 2,
                      max_iter = 10, oos_cutoffs = "",
                      decisions_file = as_upload(f), run3 = 1)
    expect_identical(nrow(rv$spec@outliers@decisions), 1L)
    expect_identical(rv$decisions$series, "lead_6")
    expect_identical(expert_log(rv$spec)$status, "pending")
    state <- current_state()
    lines <- cyclind:::analysis_lines(state)
    expect_true(any(grepl("decisions <- data.frame(", lines, fixed = TRUE)))
    expect_true(any(grepl("expert decisions on single outliers", lines)))
    # a bad table is refused with a message
    utils::write.csv(data.frame(series = "lead_6", period = "2010-01"), f,
                     row.names = FALSE)
    session$setInputs(decisions_file = as_upload(f), run3 = 2)
    expect_identical(rv$decisions$series, "lead_6")
  })
})
