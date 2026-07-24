# Archived scripts

One-off / historical scripts kept for reference only. They predate the 0.7
hard-cut (`steps:`-only yaml, `check_and_bake_study()` / `register_study()` /
`sync_study_to_registry()`) and reference removed APIs or the legacy
`replications:` schema. Do not run them against the current package.

- `migrate_studies.R`, `flatten_registry_stubs.R`, `migrate_code_format.R` —
  one-time migrations (legacy `replications:` schema, `generate_*` ->
  `make_*` rename, flattening `studies/<folder>/replication.yml` stubs into
  `studies/<folder>.yml`).
- `build_all_studies.R`, `verify_studies.R` — ad hoc build/verify runs for
  two specific studies during onboarding; superseded by
  `scripts/build_outputs.R` / `scripts/validate_outputs.R`
  (`doi = "everywhere"`).
- `build_artifacts.R` — earlier registry-wide artifact builder against the
  legacy `replications:` schema and now-removed helpers
  (`render_replication()`, `save_artifact()`, `is_package_replication()`,
  `paper_context()`); superseded by `scripts/build_outputs.R`.
- `ensure_self_contained.R`, `self_contained_snippets.R`,
  `update_self_run_footers.R`, `standardize_script_footers.ps1` — tooling
  for the old "self-contained script with a `skip_self_run` footer"
  convention. Study code no longer needs interactive run footers; yaml
  (`steps:`) is the sole execution authority (see `rep-template/code/tab_1.R`).

Current, supported scripts live one level up in `scripts/`.
