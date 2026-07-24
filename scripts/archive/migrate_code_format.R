#!/usr/bin/env Rscript

# One-time migration: rename generate_* to make_* and add self-run footers.

source("scripts/self_contained_snippets.R")

studies_dir <- file.path(getwd(), "studies")
study_dirs <- list.dirs(studies_dir, recursive = FALSE, full.names = TRUE)

make_footer <- function(rep, make_name) {
  data_expr <- make_data_expr(rep$data)
  fmt_name <- format_function_name(rep)
  if (!is.null(fmt_name)) {
    return(c("", paste0(make_name, "(", data_expr, ") |> ", fmt_name, "()")))
  }
  c("", paste0(make_name, "(", data_expr, ")"))
}

for (study_path in study_dirs) {
  yml <- file.path(study_path, "replication.yml")
  if (!file.exists(yml)) next
  meta <- yaml::read_yaml(yml)
  study_dir <- normalizePath(study_path, winslash = "/", mustWork = FALSE)

  for (rep in meta$replications) {
    code_rel <- rep$code
    code_file <- file.path(study_path, code_rel)
    if (!file.exists(code_file)) next

    code_base <- tools::file_path_sans_ext(basename(code_file))
    make_name <- paste0("make_", gsub("[^a-zA-Z0-9_]", "_", code_base))
    gen_name <- if (identical(rep$type, "figure")) "generate_figure" else "generate_table"

    body <- readLines(code_file, warn = FALSE)
    if (any(grepl(paste0("^", make_name, "\\s*<-\\s*function"), body))) next
    if (grepl("^generate_(table|figure)\\s*<-\\s*function", body[[1]])) next

    body <- sub(
      paste0("^", gen_name, "\\s*<-\\s*function"),
      paste0(make_name, " <- function"),
      body
    )

    footer <- make_footer(rep, make_name)

    writeLines(c(body, footer), code_file)
    message("Updated: ", code_file)
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a
