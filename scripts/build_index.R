#!/usr/bin/env Rscript
# Rebuild registry/index.csv from studies/*.yml stubs (no study-repo fetch).

args <- commandArgs(trailingOnly = TRUE)
registry_root <- if (length(args) >= 1L && nzchar(args[[1]])) {
  normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
} else {
  normalizePath(file.path(".."), winslash = "/", mustWork = FALSE)
}

if (!requireNamespace("replicateEverything", quietly = TRUE)) {
  pkg_root <- Sys.getenv("REPLICATE_EVERYTHING_PKG", unset = file.path(registry_root, "..", "replicateEverything"))
  if (dir.exists(pkg_root)) {
    devtools::load_all(pkg_root, quiet = TRUE)
  }
}

if (!exists("build_registry_index")) {
  stop("replicateEverything not available; install or set REPLICATE_EVERYTHING_PKG")
}

built <- replicateEverything::build_registry_index(registry_root)
message("Wrote ", built$index_path, " (", built$n, " studies)")
