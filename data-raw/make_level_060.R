# Golden file of the level form (tests/testthat/fixtures/level-0.6.0.rds):
# cycles and composite of a frozen development snapshot, so that the tests
# check that the default results do not change. The file is kept as it is;
# rerun this script from the package root only to refresh it on purpose:
#   Rscript data-raw/make_level_060.R tests/testthat/fixtures/level-0.6.0.rds
devtools::load_all(quiet = TRUE)
out <- commandArgs(TRUE)[1]
res <- lapply(c(12, 4), function(freq) {
  sim <- simulate_cycles(seed = 7, freq = freq)
  meta <- data.frame(name = colnames(sim)[-1], rationale = "early_stage")
  cand <- cli_candidates(sim, "reference", meta = meta)
  spec <- cli_spec(freq)
  b <- suppressWarnings(cli_build(cand, spec))
  list(freq = freq,
       cycles = lapply(b@cycles, function(cs) as.numeric(cs@normalised)),
       class = b@assessment@table$class,
       index = as.numeric(b@composite@index))
})
saveRDS(res, out, version = 2)
