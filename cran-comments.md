## Submission

This is a new submission (cyclind 0.2.0).

## Test environments

* Local: Ubuntu, R 4.3.3, `R CMD check --as-cran` on the tarball built
  from `git archive`.
* GitHub Actions: Ubuntu (R devel, release and oldrel-1), Windows (R
  release) and macOS (R release).

## R CMD check results

0 errors | 0 warnings | 3 notes

* checking CRAN incoming feasibility ... NOTE

  New submission.

  The README links to the GitHub Actions pages of the repository
  (`https://github.com/madsyair/cyclind/actions/workflows/*.yaml`), which
  returned 403 from the network of the local check machine. The pages
  exist and are public; GitHub refuses requests from that network.

* checking for future file timestamps ... NOTE

  "unable to verify current time": the local check machine cannot reach
  the time server. This does not come from the package.

* checking dependencies in R code ... NOTE

  The note holds only the line "Picked up JAVA_TOOL_OPTIONS: ...", printed
  by the Java runtime of the local check machine when 'RJDemetra'
  (Suggests) is loaded. It does not come from the package and does not
  appear on machines without that environment variable.

## Notes for CRAN

* The tests compare the turning points with those of 'BCDating', which was
  archived on 2026-05-06. Its results are stored as a golden file in
  `tests/testthat/fixtures/`, built once by `data-raw/make_fixtures.R`, so
  'BCDating' is not a dependency.
* Long Monte Carlo tests and the browser tests of the 'shiny' interface use
  `skip_on_cran()`; examples and tests use at most one core.
* 'RJDemetra' (Suggests) needs Java. It is used only for the optional TRAMO
  engine of `tramo_outliers()`, behind `requireNamespace()`; without it the
  package uses the X-13 regARIMA engine of 'seasonal', and the tests that
  need it are skipped.
* The vignettes build in a few seconds without Java or X-13: the steps that
  need TRAMO print results stored in `inst/vignette-results/`.
