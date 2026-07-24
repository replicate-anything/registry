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
  "rep-10.5555-cahw"
)

for (study in studies) {
  study_dir <- file.path(root, study)
  message("Writing stub for ", study)
  replicateEverything::write_folder_registry_stub(study_dir)
  replicateEverything::sync_study_to_registry(study_dir, registry_root = registry)
}

# Flatten any remaining legacy paper folders (stub-only dirs)
studies_dir <- file.path(registry, "studies")
legacy_dirs <- list.dirs(studies_dir, recursive = FALSE, full.names = TRUE)
for (legacy_dir in legacy_dirs) {
  folder <- basename(legacy_dir)
  legacy_yml <- file.path(legacy_dir, "replication.yml")
  flat_yml <- file.path(studies_dir, paste0(folder, ".yml"))
  if (file.exists(legacy_yml) && !file.exists(flat_yml)) {
    file.copy(legacy_yml, flat_yml, overwrite = TRUE)
  }
  if (dir.exists(legacy_dir)) {
    unlink(legacy_dir, recursive = TRUE)
  }
}

message("Registry studies/*.yml:")
print(list.files(studies_dir, pattern = "\\.yml$"))
