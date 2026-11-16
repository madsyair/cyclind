#' Record what produced a result
#'
#' Lists the versions of cyclind, R, the X-13ARIMA-SEATS interface
#' \pkg{seasonal}, the X-13 binary (package \pkg{x13binary}) and
#' \pkg{RJDemetra}, and MD5 checksums of the input data and files, so that a
#' composite can be traced to the software and data that produced it. The
#' script exported by the interface ([cyclind_app()]) prints it.
#'
#' The checksum of `data` is computed from its values (written with 17
#' significant digits), names and time attributes, not from the bytes of
#' an R object, so it is the same on every platform and version of R. The
#' checksum of a file is that of its bytes.
#'
#' @param data Optional input: a [CandidateSet-class] (its data), a `ts`, a
#'   matrix or a data frame.
#' @param files Optional paths of input files.
#' @return A data frame with columns `item` and `value`.
#' @examples
#' sim <- simulate_cycles(seed = 1)
#' cyclind_provenance(sim)
#' @export
cyclind_provenance <- function(data = NULL, files = NULL) {
  out <- data.frame(
    item = c("cyclind", "R", "platform", "seasonal", "X-13 (x13binary)",
             "RJDemetra"),
    value = c(as.character(utils::packageVersion("cyclind")),
              R.version.string, R.version$platform,
              package_version_or("seasonal"),
              package_version_or("x13binary"),
              package_version_or("RJDemetra")),
    stringsAsFactors = FALSE
  )
  if (!is.null(data)) {
    out[nrow(out) + 1L, ] <- c("md5 of data", data_md5(data))
  }
  if (!is.null(files)) {
    if (!is.character(files) || !all(file.exists(files))) {
      cyclind_abort("`files` must be paths of existing files.",
                    "cyclind_error_argument")
    }
    md5 <- unname(as.character(tools::md5sum(files)))
    for (i in seq_along(files)) {
      out[nrow(out) + 1L, ] <- c(paste("md5 of", basename(files[i])), md5[i])
    }
  }
  out
}

package_version_or <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(utils::packageVersion(pkg))
  } else {
    "not installed"
  }
}

# MD5 of the values, names and time attributes of a data set.
data_md5 <- function(data) {
  if (is(data, "CandidateSet")) data <- data@data
  if (!(is.data.frame(data) || is.matrix(data) || stats::is.ts(data))) {
    cyclind_abort(paste("`data` must be a CandidateSet, a ts, a matrix or a",
                        "data frame."), "cyclind_error_argument")
  }
  tsp <- stats::tsp(data)
  m <- as.matrix(data)
  num <- if (is.numeric(m)) {
    formatC(as.numeric(m), digits = 17, format = "g")
  } else {
    as.character(m)
  }
  text <- c(paste(colnames(m), collapse = ","),
            paste(dim(m), collapse = "x"),
            if (!is.null(tsp)) formatC(tsp, digits = 17, format = "g"),
            num)
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(text, f, useBytes = TRUE)
  unname(as.character(tools::md5sum(f)))
}
