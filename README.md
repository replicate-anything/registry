# registry

Study metadata lives in `studies/<folder>.yml` stub files. Rebuild the machine-readable index with `Rscript scripts/build_index.R .` (or `replicateEverything::build_registry_index(".")`). Rendering `index.qmd` updates `index.html` and also writes a full `index.csv` (including `collections`, `maintainer_*`, `languages`) for `replicateEverything` and Shiny.

**Maintainer guide:** [guides/registry-management.md](guides/registry-management.md) — catalog updates, artifact checks, audit workflow, Shiny deploy.

**Registry audit:** `audit_everything.qmd` runs [`audit_everything()`](https://github.com/replicate-anything/replicateEverything) against every table and figure in the index. From this repo root:

```bash
quarto render audit_everything.qmd
# or: Rscript scripts/run_audit.R && quarto render audit_everything.qmd
```

This upserts `audit_jobs.csv` (doi × object × engine) and rebuilds derived `audit_summary.json` (Shiny health bar), `audit_latest.rds` (full portfolio snapshot), and `audit_everything.html`. Subset audits (`dois` / `collections`) no longer wipe other studies.

**Folder-backed** and **package-backed** studies keep only a lightweight stub in `studies/`. Materials live in the linked study repository or R package. See [guides/folder-replication.md](guides/folder-replication.md), [guides/package-replication.md](guides/package-replication.md). Contributors run `replicateEverything::check_and_bake_study()` against the study; maintainers write the stub with `sync_study_to_registry()` / `register_study()`.
