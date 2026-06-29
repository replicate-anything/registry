# Package-backed studies in the registry

For studies maintained as **R packages**, the registry holds only a lightweight stub. All code, data, and display artifacts live in the study package repository.

## Registry folder (stub only)

```
papers/10.1371_journal.pone.0278337/
  replication.yml
```

Example stub:

```yaml
paper:
  doi: https://doi.org/10.1371/journal.pone.0278337
  title: "..."
  package: rep1371journalpone0278337
  package_folder: rep_10.1371_journal.pone.0278337
  package_repo: replicate-anything/rep_10.1371_journal.pone.0278337
  package_ref: main
repo: replicate-anything/rep_10.1371_journal.pone.0278337
```

No `code/`, `data/`, or `artifacts/` in the registry.

## Adding a package-backed paper

Use `replicateEverything::add_paper()` from the monorepo:

```r
library(replicateEverything)
options(replicateEverything.registry_root = "path/to/registry")

# Validate only
check_package_replication("path/to/rep_package", full_replication = FALSE)

# Validate + register
add_paper("path/to/rep_package", full_replication = FALSE)
```

Set `full_replication = TRUE` to also run every table and figure via `run_replication()`.

Full requirements: `vignette("package-replication-checklist", package = "replicateEverything")` or the [pkgdown article](https://replicate-anything.github.io/replicateEverything/articles/package-replication-checklist.html).

## Maintainer notes

- `scripts/build_artifacts.R` **skips** package-backed papers.
- Artifacts are validated via the study package API (`load_artifact()`).
- Re-render `index.qmd` after updating `index.csv` if you publish the HTML catalog.
