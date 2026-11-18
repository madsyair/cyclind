# INC-22: fixes from the end-to-end test of the interface (2026-11-18).

# Messages shown with shiny::showNotification() inside `code`.
capture_notifications <- function(code) {
  shown <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown <<- c(shown, paste(as.character(ui), collapse = " "))
      invisible(NULL)
    },
    .package = "shiny"
  )
  force(code)
  shown
}

# The display conditions of the conditional panels around an input, read
# from the HTML of the interface (without 'htmltools', which is not in
# Suggests).
display_conditions <- function(ui, id) {
  html <- as.character(ui)
  at <- regexpr(sprintf('id="%s"', id), html, fixed = TRUE)
  before <- substr(html, 1L, at)
  tags <- gregexpr("<div[^>]*>|</div>", before)[[1]]
  text <- regmatches(before, list(tags))[[1]]
  open <- character(0)
  for (tag in text) {
    if (startsWith(tag, "</")) {
      open <- open[-length(open)]
    } else {
      cond <- regmatches(tag, regexec('data-display-if="([^"]*)"', tag))[[1]]
      open <- c(open, if (length(cond) == 2L) cond[2] else "")
    }
  }
  gsub("&#39;", "'", open[nzchar(open)], fixed = TRUE)
}

# Steps 1 to 7 for the files of multi_files(): a quarterly reference
# disaggregated to months.
run_multi_steps <- function(session, files, mode, cutoffs = "") {
  session$setInputs(source = "upload_multi", csv_sep = "auto",
                    csv_dec = "auto", target_freq = "12", files = files)
  session$setInputs(run1 = 1)
  session$setInputs(reference = "pdb")
  session$setInputs(rat_1 = "early_stage", rat_2 = "early_stage",
                    rat_3 = "rapid_response", rat_4 = "rapid_response",
                    rat_5 = "expectations")
  session$setInputs(ref_type = "average", typ_5 = "flow", run2 = 1)
  session$setInputs(method = "cyclind", filter = "hp_double",
                    aggregator = "equal", extend = TRUE,
                    transform = "auto", seasonal = FALSE, outlier = 5,
                    min_lead = 3, min_median_lead = 1,
                    min_consistency = 0.5, max_missed = 0.4,
                    max_extra = 0.4, alpha = 0.05, min_auc = 0.7,
                    min_components = 2, max_iter = 10,
                    oos_cutoffs = cutoffs, run3 = 1)
  session$setInputs(run4 = 1)
  session$setInputs(cyc_series = "lead_6", tp_series = "pdb",
                    as_series = "lead_6", run5 = 1)
  session$setInputs(run6 = 1)
  session$setInputs(comp_mode = mode,
                    manual_components = c("lead_6", "lead_3"), run7 = 1)
}

test_that("INC-22 (1): the scorecard notes a disaggregated reference", {
  skip_if_not_installed("shiny")
  files <- multi_files()
  for (mode in c("auto", "manual")) {
    shiny::testServer(cyclind:::app_server("en"), {
      run_multi_steps(session, files, mode, cutoffs = "2012")
      session$setInputs(run8 = 1)
      expect_true(all(1:7 %in% rv$done), info = mode)
      q <- quality()
      expect_true("reference disaggregated from quarterly data" %in%
                    q@table$criterion, info = mode)
      expect_match(as.character(output$verdict$html),
                   "disaggregated from quarterly data", fixed = TRUE)
      expect_equal(nrow(utils::read.csv(output$dl_quality)), nrow(q@table))
      # the exported script gives the same scorecard
      script <- script()
      old <- setwd(dirname(files$datapath[1]))
      env <- new.env()
      utils::capture.output(eval(parse(text = script), envir = env))
      setwd(old)
      res <- if (mode == "auto") env$res else env$res_manual
      expect_equal(cli_quality(res)@table, q@table, info = mode)
      expect_equal(cli_quality(res)@note, q@note, info = mode)
    })
  }
  # the note is translated
  shiny::testServer(cyclind:::app_server("id"), {
    run_multi_steps(session, files, "auto")
    expect_match(as.character(output$verdict$html), "disagregasi",
                 fixed = TRUE)
  })
})

test_that("INC-22 (2): turning points of inverted candidates as cli_build()", {
  skip_if_not_installed("shiny")
  shiny::testServer(cyclind:::app_server("en"), {
    session$userData$data <- function() rv$data
    run_all_steps(session, cutoffs = "")
    expect_true(length(rv$assessment@inverted) > 0L)
    res <- cli_build(rv$cand, rv$spec)
    expect_equal(result()@turning_points, res@turning_points)
    expect_equal(readRDS(output$dl_rds)@turning_points, res@turning_points)
    # step 5 still shows the cycles as extracted
    nm <- rv$assessment@inverted[1]
    expect_equal(rv$tps[[nm]],
                 cli_turning_points(rv$cycles[nm], rv$spec)[[nm]])
  })
})

test_that("INC-22 (3): a wrong column separator gives a hint", {
  skip_if_not_installed("shiny")
  f <- tempfile(fileext = ".csv")
  writeLines(c("tanggal;pdb;kredit", "2001Q1;100,5;20,1",
               "2001Q2;101,2;20,9", "2001Q3;102,0;21,5"), f)
  for (lang in c("en", "id")) {
    shown <- capture_notifications(
      shiny::testServer(cyclind:::app_server(lang), {
        session$setInputs(source = "upload", freq = "auto", start = "",
                          csv_sep = "comma", csv_dec = "auto",
                          file = as_upload(f, "pdb.csv"))
        session$setInputs(run1 = 1)
        expect_false(1L %in% rv$done)
      })
    )
    expect_true(any(grepl(cyclind:::app_text[[lang]]$read_hint, shown,
                          fixed = TRUE)), info = lang)
  }
  # several files
  shown <- capture_notifications(
    shiny::testServer(cyclind:::app_server("en"), {
      session$setInputs(source = "upload_multi", csv_sep = "comma",
                        csv_dec = "auto", target_freq = "4",
                        files = as_upload(f, "pdb.csv"))
      session$setInputs(run1 = 1)
      expect_false(1L %in% rv$done)
    })
  )
  expect_true(any(grepl(cyclind:::app_text$en$read_hint, shown,
                        fixed = TRUE)))
})

test_that("INC-22 (4): manual mode without components says so", {
  skip_if_not_installed("shiny")
  for (lang in c("en", "id")) {
    shown <- capture_notifications(
      shiny::testServer(cyclind:::app_server(lang), {
        session$userData$data <- function() rv$data
        run_all_steps(session, cutoffs = "")
        session$setInputs(comp_mode = "manual",
                          manual_components = character(0), run7 = 2)
        session$setInputs(cci_mode = "manual",
                          cci_components = character(0), run9 = 1)
        expect_null(rv$cci)
      })
    )
    hits <- grepl(cyclind:::app_text[[lang]]$need_component, shown,
                  fixed = TRUE)
    expect_equal(sum(hits), 2L, info = lang)
  }
})

test_that("INC-22 (5): options of the cyclind method hide with OECD", {
  skip_if_not_installed("shiny")
  ui <- cyclind:::app_ui("en")
  only_cyclind <- c("filter", "aggregator", "transform", "extend",
                    "min_lead", "min_median_lead", "min_consistency",
                    "max_missed", "max_extra")
  for (id in only_cyclind) {
    expect_true("input.method == 'cyclind'" %in% display_conditions(ui, id),
                info = id)
  }
  for (id in c("method", "form", "seasonal", "oos_cutoffs", "alpha",
               "min_auc", "min_components", "max_iter")) {
    expect_false("input.method == 'cyclind'" %in% display_conditions(ui, id),
                 info = id)
  }
})

test_that("INC-22 (6): filter, aggregator, transform and rationale in id", {
  skip_if_not_installed("shiny")
  html <- as.character(cyclind:::app_ui("id"))
  keys <- c("flt_hp_double", "flt_hp", "flt_hp_smoother", "flt_cf",
            "flt_bk", "flt_hamilton", "agg_equal", "agg_equal_level",
            "agg_oecd", "agg_pca", "agg_cb", "agg_dfm", "tr_auto", "tr_log",
            "tr_none", "tr_test")
  for (k in keys) {
    expect_true(nzchar(cyclind:::app_text$id[[k]]), info = k)
    expect_true(nzchar(cyclind:::app_text$en[[k]]), info = k)
    expect_match(html, cyclind:::app_text$id[[k]], fixed = TRUE, info = k)
  }
  expect_false(grepl("Equal weights, plain mean", html, fixed = TRUE))
  res <- cli_build(sim_candidates(seed = 1))
  p <- cyclind:::phrase_translator("id")
  shown <- cyclind:::assessment_display(assessment(res)@table, p)
  expect_false(any(shown$rasional %in% names(cyclind:::oecd_rationales)))
  expect_true(all(shown$rasional %in% c("", unname(p(names(
    cyclind:::oecd_rationales))))))
})

test_that("INC-22 (7): the outlier-share warning shows once", {
  skip_if_not_installed("shiny")
  sim <- simulate_cycles(seed = 1)
  spikes <- seq(10, nrow(sim), by = 8)
  sim[spikes, "lead_6"] <- sim[spikes, "lead_6"] * 3
  f <- tempfile(fileext = ".csv")
  md <- format(seq(as.Date("1995-01-01"), by = "month",
                   length.out = nrow(sim)), "%Y-%m")
  utils::write.csv(data.frame(date = md, as.data.frame(sim)), f,
                   row.names = FALSE)
  shown <- capture_notifications(
    shiny::testServer(cyclind:::app_server("en"), {
      session$userData$data <- function() rv$data
      session$setInputs(file = as_upload(f, "spikes.csv"))
      run_all_steps(session, source = "upload", cutoffs = "2012")
      session$setInputs(run8 = 1)
      session$setInputs(cci_mode = "auto", run9 = 1)
      expect_false(is.null(rv$cci))
    })
  )
  hits <- grepl(cyclind:::app_text$en$warn_outlier_share, shown,
                fixed = TRUE)
  expect_equal(sum(hits), 1L)
})
