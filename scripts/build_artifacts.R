#!/usr/bin/env Rscript

# Build and validate replication artifacts for all papers in the registry.
# Usage:
#   Rscript scripts/build_artifacts.R
#   Rscript scripts/build_artifacts.R 10.1177_00491241211036161

args <- commandArgs(trailingOnly = TRUE)

local_pkg <- normalizePath(file.path(getwd(), "..", "replicateEverything"), winslash = "/", mustWork = FALSE)
if (dir.exists(local_pkg) && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(local_pkg, quiet = TRUE)
}

if (!requireNamespace("replicateEverything", quietly = TRUE)) {
  stop("Install replicateEverything before running this script.")
}

registry_root <- Sys.getenv("REGISTRY_ROOT", unset = normalizePath(file.path(getwd()), winslash = "/"))

options(replicateEverything.registry_root = registry_root)

papers_dir <- file.path(registry_root, "papers")
paper_folders <- if (length(args) > 0) {
  args
} else {
  list.dirs(papers_dir, recursive = FALSE, full.names = FALSE)
}

failures <- character(0)

for (folder in paper_folders) {
  yml_path <- file.path(papers_dir, folder, "replication.yml")
  if (!file.exists(yml_path)) {
    next
  }

  meta <- yaml::read_yaml(yml_path)
  doi <- meta$paper$doi
  doi <- replicateEverything::normalize_doi(doi)

  message("\n=== ", folder, " ===")

  local_index <- data.frame(
    folder = folder,
    doi = doi,
    title = meta$paper$title %||% "",
    journal = meta$paper$journal %||% "",
    year = meta$paper$year %||% NA,
    authors = paste(meta$paper$authors %||% "", collapse = ", "),
    repo = "replicate-anything/registry",
    stringsAsFactors = FALSE
  )

  options(replicateEverything.index = local_index)

  artifact_dir <- file.path(papers_dir, folder, "artifacts")
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

  manifest <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    folder = folder,
    doi = doi,
    replications = list()
  )

  for (rep in meta$replications) {
    rep_id <- rep$id
    message("Running ", rep_id, " ...")

    status <- tryCatch({
      result <- replicateEverything::render_replication(
        doi,
        rep_id,
        install_deps = TRUE
      )
      out <- replicateEverything::save_artifact(
        result,
        artifact_dir,
        doi = doi,
        folder = folder,
        install_deps = TRUE
      )
      out_file <- file.path(artifact_dir, basename(out))
      if (!file.exists(out_file)) {
        stop("Artifact file was not created: ", out_file)
      }
      replicateEverything::validate_artifact(doi, rep_id)
      list(
        status = "ok",
        artifact = file.path("artifacts", basename(out)),
        format = switch(
          tools::file_ext(out_file),
          html = "html",
          png = "ggplot",
          rds = "rds",
          result$format
        )
      )
    }, error = function(e) {
      failures <<- c(failures, paste0(folder, "/", rep_id, ": ", conditionMessage(e)))
      list(status = "error", message = conditionMessage(e))
    })

    manifest$replications[[rep_id]] <- status
  }

  manifest_path <- file.path(artifact_dir, "manifest.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
}

if (length(failures) > 0) {
  message("\nFailures:")
  cat(paste0(" - ", failures, collapse = "\n"), "\n")
  quit(status = 1)
}

message("\nAll replications completed successfully.")

`%||%` <- function(a, b) if (is.null(a)) b else a
