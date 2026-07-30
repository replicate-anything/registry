# Registry management

This guide lists the routine steps maintainers use to keep the [registry](https://github.com/replicate-anything/registry) consistent, audited, and visible in the Shiny app.

## Layout

| Path | Role |
|------|------|
| `studies/<folder>.yml` | Lightweight stub per study (DOI, title, repo link) |
| `index.qmd` | Renders the HTML catalog; also refreshes `index.csv` (full stub fields) |
| `index.csv` | Machine-readable index (`replicateEverything`, Shiny) — includes `collections`, `maintainer_*`, `languages` |
| `audit_everything.qmd` | Full replication health check + HTML report |
| `audit_jobs.csv` | Upsert store of per-job rows (doi × object × engine) |
| `audit_summary.json` | Derived portfolio counts + `progress` (Shiny health bar) |
| `audit_latest.rds` | Derived full `audit_everything` snapshot from the CSV |
| `guides/` | Contributor checklists |
| `scripts/` | Maintainer automation |

Folder-backed studies keep code, data, and display outputs in their own repos. Package-backed studies keep materials in an R package. The registry only holds stubs and generated indexes.

## Prerequisites

- R ≥ 4.1, [Quarto](https://quarto.org/) (for `index.qmd` and `audit_everything.qmd`)
- Latest [`replicateEverything`](https://github.com/replicate-anything/replicateEverything) from GitHub
- Local monorepo checkout with `registry/`, study repos, and (for Stata studies) Stata installed
- For a faithful audit: `configure_local_monorepo()` so runs use sibling study folders, not stale GitHub caches

```r
library(replicateEverything)
configure_local_monorepo("/path/to/replicate_everything")
options(replicateEverything.registry_root = "/path/to/replicate_everything/registry")
```

## Add or update a study

### Folder-backed

1. Create or update the study repo (`replication.yml`, `code/`, `data/`, `outputs/`).
2. Contributor: `check_and_bake_study(".")` (validates and optionally bakes `outputs/` via `build_study_outputs()`).
3. Add **substantive checks** (`tests/substantive/<step_id>.R`) where published benchmarks are available; see Fearon & Laitin `tab_1`.
4. Maintainer: `sync_study_to_registry(study_path)` (or `register_study(study_path)`) writes `studies/<folder>.yml` in this repo from the study's `replication.yml`. Studies never commit a local `registry/` handoff.
5. Re-render the catalog (below).

See [folder-replication.md](folder-replication.md).

### Package-backed

1. Maintain the study R package (`replication.yml`, `inst/replication.yml`, `inst/replication_code/`, display outputs via `build_study_outputs()`).
2. Contributor: `check_and_bake_study(".")`.
3. Maintainer: `sync_study_to_registry(study_path)` (or `register_study(study_path)`) writes the stub under `studies/`.

See [package-replication.md](package-replication.md).

## Refresh the catalog (`index.csv`)

After any stub change, rebuild `index.csv` from `studies/*.yml` (preferred):

```bash
cd registry
Rscript scripts/build_index.R .
# or: Rscript -e 'replicateEverything::build_registry_index(".")'
```

Optionally re-render the HTML catalog (also writes a full `index.csv` with
`collections`, `maintainer_*`, and `languages` — do not hand-edit a thin CSV):

```bash
quarto render index.qmd
```

Commit `index.csv`, `index.html`, and changed `studies/*.yml`.

## Check artifact consistency

Precomputed files referenced in `replication.yml` (for Shiny **Display**):

```bash
Rscript scripts/validate_outputs.R
```

Optional single-study check from R:

```r
validate_outputs("10.1017/S0003055403000534", what = "everything")
```

This does **not** run live replications; it only checks that declared artifact paths exist.

## Registry-wide audit (live replications)

The audit runs **every table and figure** in **each listed engine** (R and Stata where both exist). Each object gets a time limit (`patience`, default 20 seconds). Failures are recorded; the run continues.

As of replicateEverything **0.7.40**, each audit **upserts** into `audit_jobs.csv` and rebuilds derived `audit_summary.json` / `audit_latest.rds` from the **full** CSV. A one-DOI or one-collection run does **not** wipe other studies from the Shiny health bar.

### What gets saved

Rendering `audit_everything.qmd` (or `scripts/run_audit.R`) writes:

| File | Used by |
|------|---------|
| `audit_jobs.csv` | Canonical job store (upsert); source of truth for portfolio state |
| `audit_summary.json` | Shiny health bar (`load_registry_audit_summary()`; baked `progress`) |
| `audit_latest.rds` | Detailed results, Quarto `refresh:false` inputs (full CSV) |
| `audit_everything.html` | Human-readable report (from Quarto only) |

**Yes — re-rendering `audit_everything.qmd` is enough** to refresh the saved audit results and the HTML report, as long as you use a current `replicateEverything` build (≥ 0.7.40) and local study folders are available.

### Which helper to call

| Goal | Call |
|------|------|
| Run live replications (full or subset) | `audit_everything(...)` or Quarto / `scripts/run_audit.R` |
| Fill gaps from bake timings / prior RDS (no live runs) | `seed_registry_audit_jobs()` |
| Rebuild JSON/RDS after editing the CSV | `refresh_registry_audit_summary()` |

`audit_everything()` already upserts and refreshes the derived files; you do not need a separate summary rebuild after a normal audit.

### Recommended command (monorepo)

From the registry repo root:

```bash
# Option A: Quarto report + saved JSON/RDS + HTML (runs live audit)
quarto render audit_everything.qmd

# Option B: R audit, then HTML from saved RDS (no second live run)
Rscript scripts/run_audit.R
quarto render audit_everything.qmd -P refresh:false
```

Set the monorepo root if not using a sibling layout:

```bash
export REPLICATE_MONOREPO=/path/to/replicate_everything   # Linux/macOS
# PowerShell: $env:REPLICATE_MONOREPO = "C:/path/to/replicate_everything"
Rscript scripts/run_audit.R
```

### After a full audit

1. Commit `audit_jobs.csv`, `audit_summary.json`, `audit_latest.rds`, and `audit_everything.html` (+ `audit_everything_files/` if present).
2. Push **registry** (Shiny health bar reads `audit_summary.json` from GitHub when no local copy exists).
3. Optional — refresh the package vignette snapshot (avoids re-running audit on pkgdown); prefer the registry RDS (full portfolio), not a filtered in-memory return value:

```r
file.copy(
  "registry/audit_latest.rds",
  "replicateEverything/inst/vignette-data/audit_latest.rds",
  overwrite = TRUE
)
```

4. Optional — redeploy Shiny: `save_local_shiny()` in your server app directory. Health bar still reads baked `audit_summary.json` (local checkout or GitHub raw); no app code change required for the CSV store.

### Subset audit (one study)

Safe for the health bar: upserts that DOI’s jobs and rebuilds the **portfolio** summary from the full CSV.

```r
audit_everything(
  patience = 30,
  dois = "10.1596/1813-9450-10626",
  registry_root = normalizePath(".")
)
```

Or Quarto params: `dois: ["10.1596/1813-9450-10626"]`.

Note: with `refresh:true`, the Quarto HTML body shows **this run’s** return value (the subset). Disk JSON/RDS/CSV hold the full portfolio. Use `-P refresh:false` after any write to render HTML from the full RDS.

## Deploy Shiny

```r
library(replicateEverything)
setwd("/path/to/shiny_apps/replicate")  # server app directory
save_local_shiny()                       # writes app.R + www/ here
```

Create `local.R` once from `local.R.example` (registry path, Stata executable, etc.). `local.R` is never overwritten on update.

## Typical release checklist

1. Study repos updated and pushed.
2. `studies/<folder>.yml` stubs current.
3. Rebuild index: `Rscript scripts/build_index.R .` (and optionally `quarto render index.qmd` for HTML) → commit `index.csv` / `index.html`.
4. `Rscript scripts/validate_outputs.R`.
5. `quarto render audit_everything.qmd` → commit `audit_jobs.csv` + audit JSON/RDS/HTML (includes substantive checks when studies define `tests/substantive/`).
6. Copy `audit_latest.rds` to `replicateEverything/inst/vignette-data/` if publishing the package.
7. Push registry + study repos; update server Shiny copy if needed.

## CI

`.github/workflows/replications.yml` runs `scripts/build_outputs.R` (`build_outputs(doi = "everywhere", what = "everything")`), which builds package-backed studies directly (installing the study package) and any folder-backed studies checked out locally in the runner; it skips folder-backed studies without a local checkout. Folder-backed artifact builds otherwise happen in study repos. The full live audit is a maintainer job (Stata, data, patience).
