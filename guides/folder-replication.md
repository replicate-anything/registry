# Folder-backed studies in the registry

For studies maintained as a **simple folder repository** (not an R package), the registry holds only a lightweight stub. All code, data, replications list, and display artifacts live in the study repository.

## Registry stub file

```
studies/10.1017S0003055403000534.yml
```

Example stub:

```yaml
paper:
  doi: https://doi.org/10.1017/S0003055403000534
  title: "..."
  materials: folder
  study_repo: replicate-anything/rep-10.1017-S0003055403000534
  study_folder: rep-10.1017-S0003055403000534
  study_ref: main
repo: replicate-anything/rep-10.1017-S0003055403000534
```

No `code/`, `data/`, or `artifacts/` in the registry.

## Study repository layout

```
replication.yml
data/
code/
artifacts/
tests/
  testthat/
```

Paths in `replication.yml` are relative to the study repo root.

## Build and validate (study repo)

From the study repository root:

```r
library(replicateEverything)
options(
  replicateEverything.registry_root = "../registry",
  replicateEverything.use_sibling_packages = TRUE
)

build_study_artifacts(".", install_deps = TRUE)
testthat::test_dir("tests/testthat")
prepare_folder_paper(".", build_artifacts = FALSE, registry_root = "../registry")
sync_folder_paper(".", registry_root = "../registry")
```

See `vignette("folder-replication-checklist", package = "replicateEverything")`.

## Maintainer notes

- `scripts/build_artifacts.R` **skips** folder-backed papers (artifacts are built in the study repo via `build_study_artifacts()`).
- `index.csv` `repo` column should name the study repository slug.
- For local monorepo development, clone the study repo as a sibling folder (e.g. `rep-10.1017-S0003055403000534`) and set `replicateEverything.use_sibling_packages = TRUE`.
