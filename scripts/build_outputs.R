#!/usr/bin/env Rscript

# Build precomputed artifact files for every replication in the registry.
# Usage:
#   Rscript scripts/build_outputs.R
#   Rscript scripts/build_outputs.R 10.5555_cahw
#   ONLY_MISSING=1 Rscript scripts/build_outputs.R

args <- commandArgs(trailingOnly = TRUE)

local_pkg <- normalizePath(file.path(getwd(), "..", "replicateEverything"), winslash = "/", mustWork = FALSE)
if (dir.exists(local_pkg) && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(local_pkg, quiet = TRUE)
}

if (!requireNamespace("replicateEverything", quietly = TRUE)) {
  stop("Install replicateEverything before running this script.")
}

registry_root <- Sys.getenv("REGISTRY_ROOT", unset = normalizePath(file.path(getwd()), winslash = "/"))
only_missing <- identical(tolower(Sys.getenv("ONLY_MISSING", unset = "0")), "1")

tryCatch(
  replicateEverything::build_outputs(
    doi = "everywhere",
    what = "everything",
    registry_root = registry_root,
    folders = if (length(args) > 0) args else NULL,
    only_missing = only_missing
  ),
  error = function(e) {
    message(conditionMessage(e))
    quit(status = 1)
  }
)

message("\nArtifact build complete.")
