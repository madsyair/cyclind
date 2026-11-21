# Expert judgement in the graphical interface (ADR-014): an uploaded table
# of outlier decisions (step 3), an expert chronology of the reference
# turning points (step 5), the reason of a manual composite (step 7) and
# the log of decisions (step 10). The server only calls package functions.

# A small table read from an uploaded CSV file (decisions, chronology),
# with the separator detected as for data files.
read_expert_table <- function(path, need) {
  fmt <- csv_format(path)
  df <- utils::read.csv(path, sep = fmt$sep, dec = fmt$dec,
                        stringsAsFactors = FALSE, check.names = FALSE,
                        strip.white = TRUE, na.strings = c("", "NA"))
  names(df) <- tolower(trimws(names(df)))
  missing <- setdiff(need, names(df))
  if (length(missing) > 0L) {
    cyclind_abort(sprintf("The file needs the column(s) %s.",
                          paste(missing, collapse = ", ")),
                  "cyclind_error_argument")
  }
  df
}

# Code that rebuilds a small data frame of text and numbers.
table_code <- function(name, df) {
  cols <- vapply(names(df), function(nm) {
    v <- df[[nm]]
    if (is.factor(v)) v <- as.character(v)
    sprintf("  %s = %s", nm, deparse_value(v))
  }, character(1))
  c(sprintf("%s <- data.frame(", name),
    paste0(paste(cols, collapse = ",\n"), ","),
    "  stringsAsFactors = FALSE", ")")
}

# The columns of an outlier-decision table that the script keeps.
decision_table <- function(df) {
  keep <- intersect(c("series", "period", "time", "type", "action",
                      "reason", "who"), names(df))
  df <- df[, keep, drop = FALSE]
  check_outlier_decisions(df)
  df
}

# Periods typed as "2019-02, 2019-09" (NULL when empty).
parse_period_list <- function(text) {
  if (is.null(text) || !is.character(text) || length(text) != 1L ||
      !nzchar(trimws(text))) {
    return(NULL)
  }
  parts <- trimws(strsplit(text, "[,;]")[[1]])
  parts[nzchar(parts)]
}

# Turning points typed as "2008-05 peak; 2009-03 trough" (NULL when empty).
parse_tp_list <- function(text) {
  items <- parse_period_list(text)
  if (is.null(items)) return(NULL)
  parts <- strsplit(items, "[[:space:]]+")
  ok <- vapply(parts, length, integer(1)) == 2L
  if (!all(ok)) {
    cyclind_abort(paste("Write turning points to add as \"2008-05 peak;",
                        "2009-03 trough\"."), "cyclind_error_argument")
  }
  data.frame(period = vapply(parts, `[`, "", 1L),
             type = tolower(vapply(parts, `[`, "", 2L)),
             stringsAsFactors = FALSE)
}

# The expert chronology of step 5 and the code that rebuilds it: an
# uploaded chronology (data frame), or edits of the algorithm's chronology.
expert_chronology <- function(tp, table = NULL, drop = NULL, add = NULL,
                              reason = NULL, who = NULL) {
  reason <- one_text(reason)
  who <- one_text(who)
  if (!is.null(table)) {
    keep <- intersect(c("period", "time", "type", "reason", "who"),
                      names(table))
    table <- table[, keep, drop = FALSE]
    as_chronology(table)
    return(list(chronology = table,
                code = table_code("reference_tp", table)))
  }
  drop <- parse_period_list(drop)
  add <- parse_tp_list(add)
  chron <- edit_turning_points(tp, drop = drop, add = add,
                               reason = if (!is.na(reason)) reason,
                               who = if (!is.na(who)) who)
  args <- c(sprintf("tp_algorithm"),
            if (!is.null(drop)) sprintf("drop = %s", deparse_value(drop)),
            if (!is.null(add)) {
              sprintf("add = data.frame(period = %s, type = %s)",
                      deparse_value(add$period), deparse_value(add$type))
            },
            sprintf("reason = %s", deparse_value(reason)),
            if (!is.na(who)) sprintf("who = %s", deparse_value(who)))
  code <- c(
    "# the algorithm's chronology of the reference, edited by the expert",
    sprintf(paste0("tp_algorithm <- date_turning_points(\n  spec@dater, ",
                   "cli_cycles(cand, spec)[[%s]]@normalised,\n  name = %s)"),
            deparse_value(tp@name), deparse_value(tp@name)),
    sprintf("reference_tp <- edit_turning_points(\n  %s)",
            paste(args, collapse = ",\n  ")))
  list(chronology = chron, code = code)
}

# The log of expert decisions shown in step 10, with translated headers.
expert_log_app <- function(x, p) {
  lg <- expert_log(x)
  if (nrow(lg) == 0L) return(NULL)
  lg$status <- p(lg$status)
  lg$step <- p(lg$step)
  translate_names(lg[, c("step", "object", "decision", "status", "reason",
                         "who", "when")], p)
}

# Does the script carry expert decisions?
has_expert_state <- function(state) {
  !is.null(state$decisions) || !is.null(state$ref_tp_code) ||
    !is.na(one_text(state$manual_reason))
}

# Inputs of step 5 for the expert chronology of the reference.
expert_tp_ui <- function(t) {
  shiny::tagList(
    shiny::h4(t("expert_tp")),
    shiny::helpText(t("expert_tp_help")),
    shiny::fluidRow(
      shiny::column(6,
        shiny::textInput("tp_drop", t("tp_drop"), ""),
        shiny::textInput("tp_add", t("tp_add"), "")),
      shiny::column(6,
        shiny::textInput("tp_reason", t("tp_reason"), ""),
        shiny::textInput("tp_who", t("tp_who"), ""))
    ),
    shiny::fileInput("ref_tp_file", t("ref_tp_file"),
                     accept = c(".csv", ".txt", ".tsv")),
    shiny::actionButton("tp_apply", t("tp_apply"), class = "btn-primary"),
    shiny::actionButton("tp_reset", t("tp_reset")),
    shiny::tableOutput("tp_log")
  )
}
