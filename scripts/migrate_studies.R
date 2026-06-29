root <- normalizePath(
  file.path(getwd(), "..", ".."),
  winslash = "/",
  mustWork = TRUE
)
pkg <- file.path(root, "replicateEverything")
registry <- file.path(root, "registry")
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(pkg, quiet = TRUE)
}

options(
  replicateEverything.registry_root = registry,
  replicateEverything.use_sibling_packages = TRUE,
  replicateEverything.study_folders_root = root
)

studies <- c(
  "rep-10.1038-s41591-021-01454-y",
  "rep-10.5555_cahw"
)

for (study in studies) {
  study_dir <- file.path(root, study)
  message("\n=== Building ", study, " ===")
  tryCatch(
    replicateEverything::build_study_artifacts(
      study_dir,
      install_deps = TRUE,
      registry_root = registry
    ),
    error = function(e) message("build warning: ", conditionMessage(e))
  )

  result <- replicateEverything::check_folder_replication(
    study_dir,
    full_replication = FALSE,
    registry_root = registry
  )
  failed <- result$checks[!result$checks$passed, , drop = FALSE]
  if (nrow(failed) > 0) {
    message("Check failures for ", study, ":")
    apply(failed, 1, function(r) {
      message("  ", r[["check"]], ": ", r[["message"]])
    })
  } else {
    replicateEverything::write_folder_registry_stub(study_dir)
    replicateEverything::sync_folder_paper(study_dir, registry_root = registry)
    message("Synced ", study)
  }

  doi <- yaml::read_yaml(file.path(study_dir, "replication.yml"))$paper$doi
  doi <- replicateEverything::normalize_doi(doi)
  invisible(suppressMessages(capture.output({
    rep <- replicateEverything::run_replication(doi, "fig_1")
  })))
  message("fig_1 class: ", paste(class(rep), collapse = ","))
}
