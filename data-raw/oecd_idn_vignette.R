# Precomputed results of vignette("oecd-indonesia").
#
# Steps 4 to 9 of the vignette run TRAMO (package RJDemetra, which needs
# Java) for the outliers, the log/level test and the forecasts. To keep the
# vignette fast and free of Java and X-13 when it is built, this script runs
# the code shown in the vignette and saves its printed output, the numbers
# quoted in the text and the series of the plots in
# inst/vignette-results/oecd-indonesia.rds. The vignette code and this
# script must stay identical; tests/testthat/test-vignette-results.R
# checks the stored file and, when RJDemetra is installed and the tests are
# not run on CRAN, recomputes the key numbers.
#
# Run from the package root after installing the package:
#   Rscript data-raw/oecd_idn_vignette.R
stopifnot(requireNamespace("RJDemetra", quietly = TRUE))
library(cyclind)

printed <- function(x) utils::capture.output(print(x))
common <- function(a, b) {
  z <- stats::ts.intersect(a, b)
  z[stats::complete.cases(z), , drop = FALSE]
}
crisis <- c(2020, 2021 + 11 / 12)

# Steps 1 to 3 (also run live in the vignette)
dir <- system.file("extdata", "oecd-idn", package = "cyclind")
files <- file.path(dir, c("gdp.csv", "share_prices.csv",
                          "call_money_rate.csv", "rupiah_per_usd.csv",
                          "consumer_confidence.csv"))
series <- read_cli_series(files)
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
oecd <- read_cli_data(file.path(dir, "oecd_cli.csv"))

run_part <- function(cand, spec, oecd_cycle) {
  out <- list()
  # Step 4. Cycles
  cyc <- cli_cycles(cand, spec)
  out$steps <- cyc$gdp@steps
  out$outliers <- outlier_report(cyc)
  # Step 5. Turning points
  tps <- cli_turning_points(cyc, spec)
  out$tps <- as.data.frame(tps$gdp)[, c("period", "type")]
  # Step 6. Classification
  a <- cli_assess(cand, spec, cyc)
  out$assess <- as.data.frame(a)[, c("name", "class", "median_lead",
                                     "n_matched", "extra", "reasons")]
  # Step 7. Composite
  res <- withCallingHandlers(cli_build(cand, spec), warning = function(w) {
    out$build_warnings <<- c(out$build_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  out$leaders <- leaders(res)
  cy <- cycles(res)
  cli4 <- cli_composite(cy, meta$name, spec, cy$gdp)
  out$cli4 <- printed(cli4)
  out$cor_oecd <- stats::cor(common(cli4@cycle, oecd_cycle))[1, 2]
  out$agreement <- cycle_agreement(cli4, oecd_cycle, exclude = crisis,
                                   reps = 499, seed = 1)
  # Step 8. Validation
  val <- cli_validate(cli4, cy$gdp, spec)
  out$quality <- printed(cli_quality(val, spec))
  out$quality_verdict <- cli_quality(val, spec)@verdict
  # Step 9. Presentation
  out$presentation <- cli_presentation(res, cli4)
  out$cycle <- cli4@cycle
  out$index <- cli4@index
  out$gdp_cycle <- cyc$gdp
  out
}

a <- run_part(cand, spec, oecd[, "cli_normalised"])
ref <- common(a$gdp_cycle@normalised, oecd[, "reference_normalised"])
a$cor_reference <- stats::cor(ref)[1, 2]
a$gdp_cycle <- NULL

# Part B: the growth rate cycle
meta_g <- meta
meta_g$unit <- c(NA, "percent", NA, NA)
cand_g <- cli_candidates(x, reference = "gdp", meta = meta_g)
spec_g <- oecd_spec(engine = "tramo", form = growth_form())
gy <- oecd[, "cli_growth_rate"]
b <- run_part(cand_g, spec_g, gy)
lead_cor <- function(k) {
  stats::cor(common(stats::lag(gy, -k), b$gdp_cycle@cycle))[1, 2]
}
b$lead_cor <- sapply(c(lead_0 = 0, lead_3 = 3, lead_6 = 6), lead_cor)
b$gdp_cycle <- NULL

results <- list(
  created = format(Sys.Date()),
  provenance = cyclind_provenance(cand, files = files),
  engine = "tramo",
  part_a = a,
  part_b = b
)
dir.create("inst/vignette-results", showWarnings = FALSE)
saveRDS(results, "inst/vignette-results/oecd-indonesia.rds", version = 2)
