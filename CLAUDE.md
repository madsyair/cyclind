# cyclind — project rules for agents

R package for cyclical composite indicators (leading, coincident and
lagging; OECD method). Target: CRAN.

## Commands (run from the package root)

- Docs: `Rscript -e 'roxygen2::roxygenise()'`
- Tests: `Rscript -e 'devtools::test()'`
- Lint: `Rscript -e 'lintr::lint_package()'`
- Full check: `R CMD build . && R CMD check --as-cran cyclind_*.tar.gz`
- Calibration harness (slow, ~2 min per 60 reps):
  `Rscript data-raw/calibrate_collect.R && Rscript data-raw/calibrate_summarise.R`

## Development loop (one task = one issue)

1. Write the specification: inputs, outputs, reference formula.
2. Write a failing test first.
3. Implement the minimum to pass.
4. Run tests, lint, then the full check.
5. If something fails, go back to step 3; after five failed rounds, stop and
   ask the maintainer.
6. Commit only when the check is clean (0 errors, 0 warnings, notes
   explained) and coverage stays at or above 90 percent.

## Code rules

- S4 classes; new methods go in through the strategy classes
  (`CycleFilter` + `extract_cycle()`, `Dater` + `date_turning_points()`,
  `Aggregator` + `aggregate_components()`). Never add `if (class == ...)`
  branches in the core.
- Pure functions for computation; side effects only in show/plot methods.
- Errors through `cyclind_abort(message, class)`; classes start with
  `cyclind_error_`. Warnings through `cyclind_warn()`.
- Never call `set.seed()` inside package code except in `simulate_cycles()`
  with an explicit `seed`, which restores the session RNG.
- No new Imports without the maintainer's approval; optional packages go in
  Suggests and are used behind `requireNamespace()`.
- Style: tidyverse style guide, lines <= 80 characters.
- Examples must run in under 5 seconds; slow examples go in `\donttest{}`.
- Do not depend on BCDating (archived); refresh the golden file with
  `data-raw/make_fixtures.R` instead.

## Graphical interface

- `R/app.R` (UI, server, CSV reader), `R/app-text.R` (Indonesian and
  English text: every key in both languages), `R/app-plots.R` (base-graphics
  plots, also used outside the app).
- 'shiny' stays in Suggests; call it as `shiny::` only.
- The server must only call package functions; no statistics inside the app.
- Test the server with `shiny::testServer()` in `test-app.R`; the exported
  R script must reproduce the composite built in the app.
- After changing a plot, render it to PNG and look at it before committing.
- Browser tests (`test-app-e2e.R`) need Chrome; they are skipped on CRAN.
  Every key in `app_phrases_id` must translate a phrase used in a table or
  plot.

## Harness layers

| Layer | Where |
| --- | --- |
| Unit tests | `tests/testthat/test-*.R` |
| Snapshots of print methods | `tests/testthat/_snaps/` |
| Replication (mFilter, neverhpfilter, Coinprofile, BCDating golden file) | `test-filters.R`, `test-turning-points.R` |
| Simulation with known truth | `test-simulation-harness.R`, `data-raw/calibrate_*.R` |
| OECD options vs defaults | `test-oecd.R`; evidence in `data-raw/compare_oecd.R` -> `inst/calibration/oecd-*.csv` (rerun after `calibrate_collect.R`) |
| Selection bias | `test-selection-bias.R` |
| Quality scorecard | `test-quality.R` |
| Shiny server, CSV reader, plots | `test-app.R` |
| GUI end-to-end in headless Chrome (shinytest2) | `test-app-e2e.R`; on CI only in `gui-e2e.yaml` (`CYCLIND_E2E=true`) |
| CI on three OS | `.github/workflows/` |

When a default threshold changes, rerun the calibration harness and update
`inst/calibration/accuracy.csv`, the vignette and `?cli_spec`.
