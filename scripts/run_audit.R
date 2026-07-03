# Run registry-wide audit_everything() and write audit_summary.json + audit_latest.rds.
#
# Usage (from registry repo root):
#   Rscript scripts/run_audit.R
#   REPLICATE_MONOREPO=/path/to/replicate_everything Rscript scripts/run_audit.R
#   Rscript scripts/run_audit.R 30   # patience in seconds

args <- commandArgs(trailingOnly = TRUE)
patience <- if (length(args) >= 1L) as.numeric(args[[1]]) else 20

registry_root <- normalizePath(
  file.path(dirname(sub("--file=", "", commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))])), ".."),
  winslash = "/",
  mustWork = TRUE
)

monorepo <- Sys.getenv("REPLICATE_MONOREPO", unset = "")
if (!nzchar(monorepo)) {
  parent <- normalizePath(file.path(registry_root, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(parent, "replicateEverything", "DESCRIPTION"))) {
    monorepo <- parent
  }
}
if (!nzchar(monorepo)) {
  stop(
    "Set REPLICATE_MONOREPO to your monorepo root (parent of registry/ and study repos).",
    call. = FALSE
  )
}

pkg_path <- file.path(monorepo, "replicateEverything")
if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
  stop("replicateEverything not found under ", monorepo, call. = FALSE)
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run the audit from a monorepo checkout.", call. = FALSE)
}
devtools::load_all(pkg_path, quiet = TRUE)
configure_local_monorepo(monorepo)
options(replicateEverything.registry_root = registry_root)

message("Registry: ", registry_root)
message("Monorepo: ", monorepo)
message("Patience: ", patience, "s per object")

audit <- audit_everything(
  patience = patience,
  verbose = TRUE,
  registry_root = registry_root
)

paths <- write_registry_audit_record(audit, registry_root)
print(audit)
message("Wrote ", paths$summary)
message("Wrote ", paths$rds)

vignette_rds <- file.path(monorepo, "replicateEverything", "inst", "vignette-data", "audit_latest.rds")
if (dir.exists(dirname(vignette_rds))) {
  file.copy(paths$rds, vignette_rds, overwrite = TRUE)
  message("Copied audit snapshot to ", vignette_rds)
}
