#!/usr/bin/env Rscript

# Verify precomputed artifact files exist for every replication in the registry.
# Usage:
#   Rscript scripts/validate_artifacts.R
#   Rscript scripts/validate_artifacts.R 10.5555_cahw

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

studies_dir <- file.path(registry_root, "studies")
study_folders <- if (length(args) > 0) {
  sub("^studies/", "", sub("^papers/", "", sub("\\.yml$", "", basename(args))))
} else {
  yml_files <- list.files(studies_dir, pattern = "\\.yml$", full.names = FALSE)
  sub("\\.yml$", "", yml_files)
}

failures <- character(0)

for (folder in study_folders) {
  yml_path <- file.path(studies_dir, paste0(folder, ".yml"))
  if (!file.exists(yml_path)) {
    yml_path <- file.path(studies_dir, folder, "replication.yml")
  }
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

  ctx <- replicateEverything::paper_context(doi, folder = folder)
  if (isTRUE(ctx$is_folder_study)) {
    message("Checking ", folder, " (folder-backed; study repo ", ctx$materials_repo, ") ...")
  } else {
    message("Checking ", folder, " ...")
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
  message("\nFor folder-backed studies, run build_study_artifacts() in the study repo.")
  quit(status = 1)
}

message("\nAll precomputed artifacts are present.")
