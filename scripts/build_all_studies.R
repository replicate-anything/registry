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

build_all <- function(study) {
  study_dir <- file.path(root, study)
  message("\n=== Full build: ", study, " ===")
  tryCatch(
    replicateEverything::build_study_outputs(
      study_dir,
      install_deps = TRUE,
      registry_root = registry
    ),
    error = function(e) message("BUILD ERROR: ", conditionMessage(e))
  )
}

build_all("rep-10.1038-s41591-021-01454-y")
build_all("rep-10.5555_cahw")
