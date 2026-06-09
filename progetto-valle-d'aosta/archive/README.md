# Archive

This folder holds **superseded, duplicate, or incidental files** that were moved out of the
working tree to keep the repository understandable. **Nothing here is required to reproduce the
analysis or rebuild the paper** — every active pipeline path was checked before moving these files.
If you need a file back, simply move it to its original location (noted below).

## Contents

### `latex-build-artifacts/`
Stray LaTeX build artifacts that had accumulated in the **repository root**. The real,
up-to-date build files live in [paper/](../paper). These root copies were leftovers from an
interrupted compilation.
- `paper_vda_tourism_energy.aux`, `.fls`, `.fdb_latexmk`, `.log`, `.out`
- `Rplots.pdf` (stray plot from an interactive R session)

### `logs-and-scratch/`
Session logs and scratch output that were never read by any script.
- `modelling_check_results.txt` (was at root)
- `def.txt`, `output.txt`, `output.log`, `full_output.txt`, `.Rhistory` (were in `Modelli/`)
- `phaseB_log.txt`, `Rplots.pdf` (were in `Modelli/Tail Risk/`)

### `duplicate-csv/`
Exact duplicates of EDA exports. The authoritative copies remain in [Eda/](../Eda).
- `eda_tourism_monthly_profile_by_municipality.csv`
- `eda_tourism_monthly_profile_prepost.csv`
- `eda_tourism_signature_by_municipality.csv`

### `legacy-scripts/`
One-off verification/demo scripts not part of the reproducible pipeline (not sourced anywhere).
- `check_modelling.R`, `check_modelling_to_file.R` (manual model sanity checks)
- `read_in_demo.R` (demographic read-in demo with a hard-coded local path)

### `legacy-figures/`
Old, informal exploratory plots (previously in `Figures/`). None are referenced by the paper.

## Folders that were removed (empty)
`Municipality_Plots/` and the root-level `report_assets/` were empty placeholders and were deleted.
They are recreated automatically by scripts if ever needed.
