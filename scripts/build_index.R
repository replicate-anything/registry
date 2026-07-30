#!/usr/bin/env Rscript
# Rebuild registry/index.csv from studies/*.yml stubs (no study-repo fetch).

args <- commandArgs(trailingOnly = TRUE)
registry_root <- if (length(args) >= 1L && nzchar(args[[1]])) {
  normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
} else {
  normalizePath(file.path(".."), winslash = "/", mustWork = FALSE)
}

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_file)) {
  dirname(normalizePath(sub("^--file=", "", script_file[[1]]), mustWork = FALSE))
} else {
  normalizePath(".", mustWork = FALSE)
}
standalone <- file.path(script_dir, "compile_index_standalone.R")

if (!requireNamespace("replicateEverything", quietly = TRUE)) {
  pkg_root <- Sys.getenv(
    "REPLICATE_EVERYTHING_PKG",
    unset = file.path(registry_root, "..", "replicateEverything")
  )
  if (dir.exists(pkg_root) && requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(pkg_root, quiet = TRUE)
  }
}

if (exists("build_registry_index", where = asNamespace("replicateEverything"), inherits = FALSE)) {
  built <- replicateEverything:::build_registry_index(registry_root)
  message("Wrote ", built$index_path, " (", built$n, " studies)")
} else if (file.exists(standalone)) {
  status <- system2("Rscript", c(standalone, registry_root), stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop("compile_index_standalone.R failed")
  }
} else {
  stop("replicateEverything not available; install or run compile_index_standalone.R")
}
