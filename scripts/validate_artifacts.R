#!/usr/bin/env Rscript

# Verify precomputed artifact files exist for every replication in the registry.
# Usage:
#   Rscript scripts/validate_artifacts.R
#   Rscript scripts/validate_artifacts.R papers/10.5555_cahw

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
  doi <- replicateEverything::normalize_doi(meta$paper$doi)

  if (replicateEverything::is_package_replication(meta)) {
    message("Checking ", folder, " (package-backed) ...")
    tryCatch(
      replicateEverything::validate_paper_artifacts(doi),
      error = function(e) {
        failures <<- c(failures, paste0(folder, ": ", conditionMessage(e)))
      }
    )
    next
  }

  message("Checking ", folder, " ...")

  manifest_path <- file.path(papers_dir, folder, "artifacts", "manifest.json")
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::read_json(manifest_path)
    for (rep_id in names(manifest$replications)) {
      entry <- manifest$replications[[rep_id]]
      if (!identical(entry$status, "ok")) {
        next
      }
      rel <- entry$artifact
      if (is.null(rel) || !nzchar(rel)) {
        next
      }
      full <- file.path(papers_dir, folder, rel)
      if (!file.exists(full)) {
        failures <- c(
          failures,
          paste0(folder, "/", rep_id, ": manifest ok but missing ", rel)
        )
      }
    }
  }

  tryCatch(
    replicateEverything::validate_paper_artifacts(doi),
    error = function(e) {
      failures <<- c(failures, paste0(folder, ": ", conditionMessage(e)))
    }
  )
}

if (length(failures) > 0) {
  message("\nMissing artifacts:")
  cat(paste0(" - ", failures, collapse = "\n"), "\n")
  message("\nRun: Rscript scripts/build_artifacts.R")
  quit(status = 1)
}

message("\nAll precomputed artifacts are present.")
