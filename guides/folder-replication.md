# Folder-backed studies in the registry

For studies maintained as a **simple folder repository** (not an R package), the registry holds only a lightweight stub. All code, data, replications list, and display outputs live in the study repository.

## Registry stub file

```
studies/10.1017_s0003055403000534.yml
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

No `code/`, `data/`, or `outputs/` in the registry.

## Study repository layout

```
replication.yml
data/
code/
outputs/
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

check_and_bake_study(".", build_artifacts = TRUE)   # contributor: validate + bake outputs/
testthat::test_dir("tests/testthat")
sync_study_to_registry(".", registry_root = "../registry")   # maintainer: write the stub
```

See `vignette("folder-replication-checklist", package = "replicateEverything")`.

## Maintainer notes

- `scripts/build_outputs.R` builds folder-backed papers only when a local checkout is available as a monorepo sibling (outputs are otherwise built in the study repo via `build_study_outputs()`).
- `index.csv` `repo` column should name the study repository slug.
- For local monorepo development, clone the study repo as a sibling folder (e.g. `rep-10.1017-S0003055403000534`) and set `replicateEverything.use_sibling_packages = TRUE`.
- **Substantive checks:** encourage submitters to add `tests/substantive/<step_id>.R` comparing replicated estimates to published benchmarks (see Fearon & Laitin `tab_1`). [check_replication()] and [audit_everything()] report and run these when present.
