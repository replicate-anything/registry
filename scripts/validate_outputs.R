#!/usr/bin/env Rscript

# Verify precomputed artifact files exist for every replication in the registry.
# Usage:
#   Rscript scripts/validate_outputs.R
#   Rscript scripts/validate_outputs.R 10.5555_cahw

args <- commandArgs(trailingOnly = TRUE)

local_pkg <- normalizePath(file.path(getwd(), "..", "replicateEverything"), winslash = "/", mustWork = FALSE)
if (dir.exists(local_pkg) && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(local_pkg, quiet = TRUE)
}

if (!requireNamespace("replicateEverything", quietly = TRUE)) {
  stop("Install replicateEverything before running this script.")
}

registry_root <- Sys.getenv("REGISTRY_ROOT", unset = normalizePath(file.path(getwd()), winslash = "/"))

tryCatch(
  replicateEverything::validate_outputs(
    doi = "everywhere",
    what = "everything",
    registry_root = registry_root,
    folders = if (length(args) > 0) args else NULL
  ),
  error = function(e) {
    message(conditionMessage(e))
    quit(status = 1)
  }
)

message("\nAll precomputed artifacts are present.")
