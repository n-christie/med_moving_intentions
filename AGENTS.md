# Agent Instructions — med_moving_intentions

This file provides context for Claude Code and other AI agents working in this repository.

## Project overview

This is a research paper project analyzing medical professionals' moving intentions using survey data.

## Repository layout

| Path | Purpose |
|------|---------|
| `data/raw/` | Original survey exports — **read only, never modify** |
| `data/processed/` | Cleaned datasets produced by `scripts/01_clean/` |
| `data/codebooks/` | Variable definitions and survey instruments |
| `scripts/01_clean/` | Import and clean raw data |
| `scripts/02_analyze/` | Statistical analyses |
| `scripts/03_visualize/` | Figures and tables for the paper |
| `output/figures/` | Saved plots |
| `output/tables/` | Saved tables |
| `paper/` | Manuscript drafts |
| `docs/` | About/project page for co-authors |

## Conventions

- Language: **R**
- Use `tidyverse` for data wrangling, `ggplot2` for visualization, `tidymodels` or base R for modeling
- Use the base R pipe `|>` (not `%>%`)
- Scripts are numbered and should be run in order within each stage
- All figures saved to `output/figures/`, tables to `output/tables/`
- Raw data files must never be overwritten or deleted

## Key files

- `data/codebooks/` — check here first to understand variable names and survey structure
- `scripts/01_clean/` — understand the cleaning pipeline before analyzing data

## Notes for agents

- Ask before making changes to scripts in `scripts/02_analyze/` or `scripts/03_visualize/`
- Do not commit raw data files
- When adding new scripts, follow the existing naming convention (`NN_description.R`)
