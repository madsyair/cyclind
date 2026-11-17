# Specification of the stored results of vignette("oecd-indonesia").
#
# Steps 4 to 9 of the vignette need TRAMO (RJDemetra, Java), so their output
# is computed by data-raw/oecd_idn_vignette.R and stored in
# inst/vignette-results/oecd-indonesia.rds. The stored file must
# * hold every element the vignette prints or quotes;
# * come from the data shipped in inst/extdata/oecd-idn (same MD5 sums);
# * agree with a fresh computation when RJDemetra is available (not on
#   CRAN: the computation takes several seconds and needs Java).

stored <- function() {
  path <- system.file("vignette-results", "oecd-indonesia.rds",
                      package = "cyclind")
  expect_true(nzchar(path))
  readRDS(path)
}

idn_files <- function() {
  dir <- system.file("extdata", "oecd-idn", package = "cyclind")
  file.path(dir, c("gdp.csv", "share_prices.csv", "call_money_rate.csv",
                   "rupiah_per_usd.csv", "consumer_confidence.csv"))
}

test_that("the stored vignette results hold what the vignette uses", {
  vr <- stored()
  expect_identical(vr$engine, "tramo")
  parts <- c("steps", "outliers", "tps", "assess", "build_warnings",
             "leaders", "cli4", "cor_oecd", "agreement", "quality",
             "quality_verdict", "presentation", "cycle", "index")
  expect_true(all(parts %in% names(vr$part_a)))
  expect_true(all(c(parts, "lead_cor") %in% names(vr$part_b)))
  expect_true(is.numeric(vr$part_a$cor_reference))
  expect_s3_class(vr$part_a$index, "ts")
  expect_identical(vr$part_a$agreement$statistic,
                   c("pearson", "spearman", "rmse"))
  expect_identical(attr(vr$part_a$agreement, "seed"), 1)
})

test_that("the stored vignette results come from the shipped data", {
  vr <- stored()
  prov <- vr$provenance
  files <- idn_files()
  md5 <- unname(tools::md5sum(files))
  items <- paste("md5 of", basename(files))
  expect_identical(prov$value[match(items, prov$item)], md5)
})

test_that("the stored vignette results agree with a fresh computation", {
  skip_on_cran()
  skip_if_not_installed("RJDemetra")
  vr <- stored()
  series <- read_cli_series(idn_files())
  x <- cli_align(series, type = c(gdp = "flow", call_money_rate = "average"),
                 reference = "gdp")
  meta <- data.frame(
    name = c("share_prices", "call_money_rate", "rupiah_per_usd",
             "consumer_confidence"),
    rationale = c("expectations", "prime_mover", "prime_mover",
                  "expectations"),
    sign = c(1, -1, -1, 1))
  cand <- cli_candidates(x, reference = "gdp", meta = meta)
  spec <- oecd_spec(engine = "tramo")
  cyc <- cli_cycles(cand, spec)
  a <- as.data.frame(cli_assess(cand, spec, cyc))
  expect_identical(a$class, vr$part_a$assess$class)
  expect_equal(a$median_lead, vr$part_a$assess$median_lead)
  rep <- outlier_report(cyc)
  expect_identical(rep$n_removed, vr$part_a$outliers$n_removed)
  expect_warning(res <- cli_build(cand, spec), "No candidate")
  cy <- cycles(res)
  cli4 <- cli_composite(cy, meta$name, spec, cy$gdp)
  dir <- system.file("extdata", "oecd-idn", package = "cyclind")
  oecd <- read_cli_data(file.path(dir, "oecd_cli.csv"))
  z <- stats::ts.intersect(cli4@cycle, oecd[, "cli_normalised"])
  z <- z[stats::complete.cases(z), ]
  expect_equal(stats::cor(z)[1, 2], vr$part_a$cor_oecd, tolerance = 1e-6)
})
