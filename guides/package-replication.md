# Package-backed studies in the registry

For studies maintained as **R packages**, the registry holds only a lightweight stub. All code, data, and display artifacts live in the study package repository.

## Registry stub file

```
studies/10.1371_journal.pone.0278337.yml
```

Example stub:

```yaml
paper:
  doi: https://doi.org/10.1371/journal.pone.0278337
  title: "..."
  package: rep1371journalpone0278337
  package_folder: rep-10.1371-journal.pone.0278337
  package_repo: replicate-anything/rep-10.1371-journal.pone.0278337
  package_ref: main
repo: replicate-anything/rep-10.1371-journal.pone.0278337
```

No `code/`, `data/`, or display outputs in the registry.

## Adding a package-backed paper

Contributor validates and bakes, maintainer registers, from the monorepo:

```r
library(replicateEverything)
options(replicateEverything.registry_root = "path/to/registry")

# Contributor: validate + bake artifacts
check_and_bake_study("path/to/rep_package", build_artifacts = TRUE)

# Maintainer: write/refresh the registry stub
sync_study_to_registry("path/to/rep_package")
# or: register_study("path/to/rep_package")
```

Full requirements: `vignette("package-replication-checklist", package = "replicateEverything")` or the [pkgdown article](https://replicate-anything.github.io/replicateEverything/articles/package-replication-checklist.html).

## Maintainer notes

- `scripts/build_outputs.R` builds package-backed papers directly (installs the study package, then `build_study_outputs()`).
- Artifacts are validated via the study package API (`load_artifact()`).
- Re-render `index.qmd` after updating `index.csv` if you publish the HTML catalog.
