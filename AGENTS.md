# Agent Instructions — med_moving_intentions

This file provides context for Claude Code and other AI agents working in this repository.

## Project overview

This is a longitudinal survey study examining whether self-reported moving intentions and housing-related factors at baseline predict actual relocation over a three-year period among older adults listed with an interest in relocation at housing companies, and assessing the extent to which intentions translate into realized moves.

**Population:** Older adults on housing company interest/waiting lists  
**Design:** Prospective longitudinal; three time points (T1 baseline, T2, T3)  
**Outcome:** `relocated` (binary, 0/1), `nr_reloc` (count), `reloc_date1/2/3` (dates)

## Research questions

| RQ | Question | Primary variables | Status |
|----|----------|-------------------|--------|
| 1 | To what extent do self-reported moving intentions at baseline predict actual relocation over a three-year period? | `VAR24_T1` → `relocated` | ✅ Complete |
| 2 | To what extent do housing-related factors at baseline predict relocation, independent of moving intentions? | `VAR13_*_T1`, `VAR14_T1`, `VAR16–18_T1` → `relocated` | ✅ Complete |
| 3 | Among those who intended to move at baseline, which factors are associated with not having relocated? | Subset: intenders; predictors = obstacles (`VAR30_T1`, `VAR31_*_T1`), housing | ✅ Complete |
| 4 | Among those who did not intend to move, which factors predict unexpected relocation? | Subset: non-intenders; predictors = housing factors, tenure | ✅ Complete |

## Key results summary

### RQ1
- Intentions strongly predict relocation: OR = 22.1 (< 1 year vs 2+ years), OR = 3.55 (1–2 years vs 2+ years)
- 71.4% of urgent intenders relocated; 10.4% of non-intenders relocated
- Nagelkerke R² = 0.29 after adjusting for age, sex, SRH

### RQ2
- Home satisfaction is the only housing factor independently associated with relocation (OR ≈ 0.76–0.86, p < 0.05)
- Housing suitability shows a suppression effect when entered with satisfaction (collinearity)
- Neighborhood cohesion not significant
- Housing factors add minimally beyond intentions (ΔNagelkerke R² = 0.008)

### RQ3 (intenders only, n ≈ 587 complete cases)
- Intention urgency remains dominant within intenders (OR = 5.8 for < 1 year vs 1–2 years)
- Older age associated with lower odds of follow-through (OR = 0.96 per year)
- Having dependents: OR = 0.18 (p = 0.035), but n = 19 endorsed, wide CI
- Housing factors and most obstacle types not significant in this subset

### RQ4 (non-intenders only, n = 1,221 complete cases; 130 relocated)
- Demographics (age, sex, SRH) not predictive of unexpected relocation
- Home satisfaction dominant predictor (OR = 0.50, 95% CI: 0.38–0.65, p < 0.001) — effect notably stronger than in full-sample models
- Renter vs owner: OR = 1.91 (95% CI: 1.18–3.01, p = 0.007) — renters without stated intentions are vulnerable to involuntary displacement
- Housing suitability positive association (OR ≈ 2.1) likely reflects suppression/collinearity with satisfaction, not a causal effect
- Housing factors as a block: LRT p < 0.001 vs demographics alone; ΔNagelkerke R² = 0.043

### Further analysis: Survival analysis
- Cox proportional hazards models fitted using exact relocation dates (`reloc_date1`) and `Date_T1`
- Censoring: `Date_T3` for T3 respondents; `2024-11-13` (study end) for 545 T3 non-respondents (administrative censoring — treat as a caveat)
- PH assumption violated for `intention_timeframe` (Schoenfeld p < 0.001); all other predictors satisfy PH
- Stratified Cox model (stratify on `intention_timeframe`) gives valid covariate HRs: home satisfaction HR = 0.84 (p = 0.005), other housing factors non-significant, consistent with logistic regression

### Further analysis: Latent profile analysis (LPA)
- Script: `scripts/02_analyze/06_LPA.R`; figure: `output/figures/LPA_profiles.png` (also in `docs/figures/`)
- Method: mclust EEI Gaussian mixture model on 6 z-standardized continuous indicators (intention urgency, home satisfaction, housing suitability, SRH, age, neighborhood cohesion); k = 2–6 evaluated; VVI did not converge
- Selected model: k = 5, BIC = -26,431, entropy = 0.785; analytic sample N = 1,799 (complete cases on all 6 indicators)
- Binary variables (homeownership, living alone) excluded from fitting; used as auxiliary descriptors after profile assignment
- Five profiles (reference = Profile 2, highest satisfaction / lowest relocation rate):

| Profile | n | % relocated | Label | Key features |
|---------|---|-------------|-------|--------------|
| 1 | 400 | 26% | Dissatisfied intenders | Moderate intention; below-average satisfaction (4.00) and suitability (4.47) |
| 2 | 574 | 11% | Satisfied older stayers (ref) | Low intention; max satisfaction; oldest (age 74.4); 87% owners |
| 3 | 104 | 72% | Eager movers | Max intention (3.0); max satisfaction; healthy; 86% owners |
| 4 | 134 | 47% | Environmentally pressured | Moderate intention; lowest satisfaction (2.19); lowest cohesion (1.87); 45% live alone; only 61% owners |
| 5 | 587 | 13% | Satisfied younger stayers | Lowest intention; max satisfaction; youngest (age 63.2); healthiest (SRH 4.09) |

- Logistic regression ORs vs Profile 2: Profile 1 OR = 2.79 (95% CI: 1.99–3.93, p < 0.001); Profile 3 OR = 20.2 (12.4–33.8, p < 0.001); Profile 4 OR = 6.95 (4.54–10.7, p < 0.001); Profile 5 OR = 1.18 (0.83–1.68, p = 0.35)
- Profile 4 is the theoretically central finding: person-environment mismatch (Lawton) drives relocation despite only moderate stated intention; high renter/living-alone proportions indicate structural vulnerability
- Profiles 2 and 5 are statistically indistinguishable in relocation odds but differ structurally (age 74 vs 63, different reasons for housing satisfaction)
- Full thorough write-up (rationale, method, profile descriptions, regression table, interpretation) is in `docs/index.qmd`

### Macro-economic context
- Study period (2021–2024) coincided with the Swedish Riksbank tightening cycle (0% → 4% benchmark rate, April 2022 – September 2023) and a ~20% peak-to-trough housing price collapse in 2022–2023
- Relocation timing plot shows a sharp drop in quarterly relocations from Q3 2022, recovering gradually in 2023–2024
- Tenure-stratified timing plot shows sustained suppression in owners (market lock-in) and a different volatile pattern in renters (possible inflation-driven displacement in Q3 2022 then sharp drop)
- These patterns are descriptive only; the survey has no time-varying market perception or financial capacity measures

**Key predictor groups at T1:**

| Variable(s) | Content | Used in |
|-------------|---------|---------|
| `VAR24_T1` | Expected timeframe to needing to move (primary intention measure) | RQ1–3 |
| `VAR13_1–4_T1` | Housing suitability (physical environment); composite `housing_suitability` | RQ2–4 |
| `VAR14_T1` | Home satisfaction; recoded as `home_satisfaction` | RQ2–4 |
| `VAR16–18_T1` | Neighbourhood social cohesion; composite `neighbourhood_cohesion` | RQ2–4 |
| `VAR30_T1` | Perceived obstacles to moving (any) | RQ3 |
| `VAR31_1–10_T1` | Specific obstacles (multi-select); recoded as `obs_*` | RQ3 |
| `VAR34_T1` | Self-rated health; recoded as `srh` (numeric 1–5) | All (covariate) |
| `VAR01_2_T1` | Home ownership (Yes/No); used as `owner` factor in RQ4 | RQ4, macro context |

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
| `docs/` | About/project page for co-authors (rendered to GitHub Pages) |
| `references/` | Reference papers and background literature |

## Conventions

- Language: **R**
- Use `tidyverse` for data wrangling, `ggplot2` for visualization, `tidymodels` or base R for modeling
- Use the base R pipe `|>` (not `%>%`)
- Scripts are numbered and should be run in order within each stage
- All figures saved to `output/figures/`, tables to `output/tables/`
- Figures used in the docs page are also copied to `docs/figures/`
- Raw data files must never be overwritten or deleted

## Pipeline order

Scripts must be run in this order:

| Step | Script | Run | Output |
|------|--------|-----|--------|
| 1 | `scripts/01_clean/00_translate_codebook.R` | **Once** — requires `DEEPL_API_KEY` in `~/.Renviron` | `data/codebooks/prosp_dict_en.rds` |
| 2 | `scripts/01_clean/01_import.R` | Each session — imports raw data, recodes NAs, parses dates, applies English labels | `data/processed/survey_clean.rds` |
| 3 | `scripts/01_clean/02_recode.R` | Each session — all deliberate variable recodings and composite creation; **every decision is documented here** | `data/processed/survey_analysis.rds` |
| 4 | `scripts/02_analyze/0N_RQN_*.R` | As needed — analysis scripts load `survey_analysis.rds` only | results / model objects |
| 5 | `scripts/03_visualize/0N_*.R` | As needed — visualization scripts load `survey_analysis.rds` only | figures in `output/figures/` and `docs/figures/` |

Analysis scripts must never perform their own data recodings. All transformations belong in `02_recode.R`.

## Analysis scripts

| Script | Purpose |
|--------|---------|
| `scripts/02_analyze/01_RQ1_intentions.R` | Logistic regression: intentions predicting relocation (M1 unadjusted, M2 adjusted) |
| `scripts/02_analyze/02_RQ2_housing.R` | Logistic regression: housing factors predicting relocation (M2–M6) |
| `scripts/02_analyze/03_RQ3_intenders.R` | Logistic regression: predictors of relocation among intenders (M1–M4) |
| `scripts/02_analyze/04_survival.R` | Cox PH models, PH assumption test, stratified Cox model |
| `scripts/02_analyze/05_RQ4_nonintenders.R` | Logistic regression: predictors of unexpected relocation among non-intenders (M1–M3) |
| `scripts/02_analyze/06_LPA.R` | Latent profile analysis: mclust EEI, k = 2–6 selection, profile descriptions, relocation regression |

## Visualization scripts

| Script | Output |
|--------|--------|
| `scripts/03_visualize/00_table1_descriptives.R` | Table 1 (descriptive stats by relocation status) — Word and HTML |
| `scripts/03_visualize/01_RQ1_forest_plot.R` | Forest plot: unadjusted vs adjusted ORs for RQ1 |
| `scripts/03_visualize/02_RQ2_forest_plot.R` | Forest plot: baseline vs full housing model for RQ2 |
| `scripts/03_visualize/03_RQ3_forest_plot.R` | Forest plot: baseline vs full model for RQ3 (intenders) |
| `scripts/03_visualize/04_descriptive_figures.R` | Bar charts: relocation rate by intention group and by age group |
| `scripts/03_visualize/05_RQ4_forest_plot.R` | Forest plot: demographics vs full model for RQ4 (non-intenders) |

## Key files

- `data/codebooks/prosp_dict.rds` — original Swedish variable/value labels
- `data/codebooks/prosp_dict_en.rds` — English-translated labels (generated by step 1)
- `data/processed/survey_clean.rds` — cleaned, labelled data (generated by step 2)
- `data/processed/survey_analysis.rds` — analysis-ready data with composites and recoded variables (generated by step 3)
- `scripts/01_clean/02_recode.R` — **read this before interpreting any variable in the analysis**
- `docs/index.qmd` — about page for co-authors; renders to GitHub Pages at https://n-christie.github.io/med_moving_intentions/

## Data notes

- `relocated` is binary (0 = No, 1 = Yes); 20.6% of participants relocated over the study period
- 3 rows have missing `LopNr_PersonNr` (participant ID) — treat with caution or exclude
- T1 uses 2-digit variable names (e.g. `VAR06_T1`); T2/T3 use 3-digit (e.g. `VAR006_T2`)
- Special NA codes in raw data: 99, 888, 9999 — already recoded to `NA` in `survey_clean.rds`
- `Date_T1`, `Date_T3`, `reloc_date1/2/3` are parsed Date columns available for survival analysis
- 545 participants are missing `Date_T3` (T3 non-respondents); administratively censored at `2024-11-13` in survival analysis
- `VAR25_T1` (subjective likelihood of moving within 2 years) has 68.6% missing — not used as a predictor

## Writing conventions

All written text (manuscripts, the about page, comments, documentation) must follow these conventions:

- **American spelling** — use "neighborhood" not "neighbourhood", "aging" not "ageing", "analyze" not "analyse", "behavior" not "behaviour", etc.
- **No hyphens as punctuation** — do not use em-dashes (—) or hyphens to set off phrases; use commas instead
- Hyphenated compound adjectives that are standard academic terms (e.g., "self-reported", "cross-sectional") are acceptable

## Current project state (last updated 2026-03-26)

All planned analyses are complete. The about page (`docs/index.qmd`) is fully up to date and live at https://n-christie.github.io/med_moving_intentions/.

| Analysis | Status | Script | Docs section |
|----------|--------|--------|--------------|
| RQ1: Intentions predict relocation | ✅ Complete | `02_analyze/01_RQ1_intentions.R` | ✅ In docs |
| RQ2: Housing factors | ✅ Complete | `02_analyze/02_RQ2_housing.R` | ✅ In docs |
| RQ3: Among intenders | ✅ Complete | `02_analyze/03_RQ3_intenders.R` | ✅ In docs |
| RQ4: Among non-intenders | ✅ Complete | `02_analyze/05_RQ4_nonintenders.R` | ✅ In docs |
| Survival analysis (Cox) | ✅ Complete | `02_analyze/04_survival.R` | ✅ In docs |
| Macro-economic context | ✅ Complete | (in docs narrative) | ✅ In docs |
| Latent profile analysis | ✅ Complete | `02_analyze/06_LPA.R` | ✅ In docs (thorough: rationale, method, fit table, profile table, regression, interpretation) |
| Manuscript drafting | 🔄 In progress | `paper/` | — |

**Next priority: Manuscript drafting.** The central narrative arc is documented in the conversation history and AGENTS.md key results. The LPA (Profile 4, environmentally pressured) is the theoretically novel contribution and should be foregrounded. Suggested journal targets include journals focused on aging, environments, and housing (e.g., *Ageing & Society*, *Housing Studies*, *The Gerontologist*).

## Notes for agents

- Ask before making changes to scripts in `scripts/02_analyze/` or `scripts/03_visualize/`
- Do not commit raw data files
- When adding new scripts, follow the existing naming convention (`NN_description.R`)
- All figures used in the docs page must also be copied to `docs/figures/` after saving to `output/figures/`
- The about page (`docs/index.qmd`) must be re-rendered and the resulting `docs/index.html` committed whenever content changes
