## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | notes explained below

* New submission.

## Notes for CRAN

* The tests compare the turning points with those of 'BCDating', which was
  archived on 2026-05-06. Its results are stored as a golden file in
  `tests/testthat/fixtures/`, built once by `data-raw/make_fixtures.R`, so
  'BCDating' is not a dependency.
* Long Monte Carlo tests use `skip_on_cran()`; examples and tests use at most
  one core.
* 'RJDemetra' (Suggests) needs Java. It is used only for the optional TRAMO
  engine of `tramo_outliers()`, behind `requireNamespace()`; without it the
  package uses the X-13 regARIMA engine of 'seasonal', and the tests that
  need it are skipped.
