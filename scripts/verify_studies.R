root <- normalizePath(
  file.path(getwd(), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
pkg <- file.path(root, "replicateEverything")
registry <- file.path(root, "registry")
devtools::load_all(pkg, quiet = TRUE)

options(
  replicateEverything.registry_root = registry,
  replicateEverything.use_sibling_packages = TRUE,
  replicateEverything.study_folders_root = root
)

verify <- function(study, doi, what) {
  message("\n=== ", study, " ", what, " ===")
  invisible(suppressMessages(capture.output({
    obj <- replicateEverything::run_replication(doi, what)
  })))
  message("class: ", paste(class(obj), collapse = ","))
}

verify(
  "1038",
  "10.1038/s41591-021-01454-y",
  "fig_1"
)
verify(
  "cahw",
  "10.5555/cahw",
  "fig_1"
)

tryCatch(
  replicateEverything::build_study_outputs(
    file.path(root, "rep-10.1038-s41591-021-01454-y"),
    install_deps = TRUE,
    ids = c("fig_1", "fig_2", "fig_3", "fig_5", "fig_6", "tab_1", "tab_2"),
    registry_root = registry
  ),
  error = function(e) message("1038 partial build: ", conditionMessage(e))
)

tryCatch(
  replicateEverything::build_study_outputs(
    file.path(root, "rep-10.5555-cahw"),
    install_deps = TRUE,
    ids = c("fig_1", "fig_2"),
    registry_root = registry
  ),
  error = function(e) message("cahw partial build: ", conditionMessage(e))
)
