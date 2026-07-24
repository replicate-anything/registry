#!/usr/bin/env Rscript

source("scripts/self_contained_snippets.R")

studies_dir <- file.path(getwd(), "studies")
study_dirs <- list.dirs(studies_dir, recursive = FALSE, full.names = TRUE)

for (study_path in study_dirs) {
  yml <- file.path(study_path, "replication.yml")
  if (!file.exists(yml)) next
  meta <- yaml::read_yaml(yml)

  for (rep in meta$replications) {
    code_file <- file.path(study_path, rep$code)
    if (!file.exists(code_file)) next

    code_base <- tools::file_path_sans_ext(basename(code_file))
    make_name <- paste0("make_", gsub("[^a-zA-Z0-9_]", "_", code_base))

    body <- readLines(code_file, warn = FALSE)
    if (!any(grepl(paste0("^", make_name, "\\s*<-\\s*function"), body))) next

    body <- strip_existing_preamble(body)
    body <- strip_existing_footer(body)

    deps <- collect_dependencies(meta, rep)
    header <- make_header_lines(meta, rep, study_path, code_file)
    libraries <- make_library_block(deps)
    run_line <- make_run_line(meta, rep, make_name)

    writeLines(c(header, libraries, body, "", run_line), code_file)
    message("Self-contained: ", code_file)
  }
}
