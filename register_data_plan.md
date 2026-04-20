# Research Plan: med_moving_intentions

## Paper Structure

### Paper 1 — Intentional Relocation (RQ1, RQ2, RQ3, Survival)
Scripts: `paper/paper1/analysis/`
- `01_RQ1.R` — Do moving intentions predict relocation? (binary logistic)
- `02_RQ2.R` — Do housing factors predict relocation independent of intentions?
- `03_RQ3.R` — Among intenders, what predicts blocked relocation?
- `04_survival.R` — Cox proportional hazards model for time-to-relocation

### Paper 2 — Reactive/Unexpected Relocation (RQ3 reframe, RQ4, LPA)
Scripts: `paper/paper2/analysis/`
- `01_RQ3_blocked.R` — Blocked intenders: macro-economic/structural framing
- `02_RQ4.R` — Among non-intenders, what predicts unexpected relocation?
- `03_LPA.R` — Latent profile analysis of baseline person-environment fit

### Shared Descriptive
Scripts: `paper/descriptive/`
- `00_cohort_tracking.R` — Full cohort attrition, Table 1, longitudinal change

## Output Conventions

All paper-specific outputs save relative to project root:

| Type | Location | Format |
|------|----------|--------|
| Figures | `paper/paperX/figures/` | PNG (300 dpi) |
| Tables  | `paper/paperX/tables/`  | CSV |
| Models  | `paper/paperX/models/`  | RDS |
| Descriptive tables | `paper/descriptive/tables/` | CSV |

## Primary Dataset
`data/processed/panel_merged.rds` (5,874 × 901; long format, 3 rows/person)
All analysis scripts filter to `wave == "T1"` for baseline cross-sectional analyses.
Survival script additionally joins T3 dates for censoring.

---

# Register Data Integration Plan

## Status: Pending — data preparation in progress

## Planned linkage variables

### Primary outcome validation
- Source: Location register (address coordinates)
- Variable: Address change between T1 (2021) and T3 (2024)
- Use: Validate/replace self-reported relocation outcome
- Expected variable name: `relocated_register` (binary 0/1)

### Attrition classification
- Source: Mortality register + T2/T3 response data
- Variables needed:
  - `died_before_t3` (binary) — distinguish death from dropout
  - `responded_t2` (binary)
  - `responded_t3` (binary)
- Use: Attrition analysis comparing completers vs dropouts vs deceased
- Figures to fill in later:
  - N died before T3: [PENDING]
  - N dropped out T2 only: [PENDING]
  - N dropped out T3 only: [PENDING]
  - N completed all waves: [PENDING]

### Health trajectory
- Source: NPR (inpatient/outpatient) + prescription register
- Variables: Comorbidity index (Charlson or Elixhauser),
  prescription count/categories
- Use: Supplement/replace self-rated health (currently non-significant)
- Time points needed: T1 year, T2 year, T3 year (annual snapshots)

### SES
- Source: SCB income register
- Variable: Disposable income (quintiles or continuous)
- Use: Replace self-reported financial stability (96% stable = useless)

### Objective housing accessibility
- Source: Building/apartment register
- Variables: Elevator (yes/no), building year, storey
- Use: Test objective vs perceived housing accessibility

---

## Integration notes

### Linkage key
- Linkage variable: `LopNr_PersonNr` (personal identity number)
- Note: 3 participants have missing `LopNr_PersonNr` — these cannot be linked
  and should be excluded from any register-linked analysis

### Current survey limitations the register data addresses

| Limitation | Register source | How it helps |
|------------|----------------|--------------|
| Self-reported relocation may be under-reported or imprecise | Location register | Objective address-change outcome |
| Non-response at T3 (n = 545) is treated as administrative censoring in survival analysis — may be informative if dropout is related to death | Mortality register | Separate true attrition from death; adjust censoring |
| Self-rated health non-significant (OR 1.09–1.14) — may lack sensitivity to detect health effects | NPR + prescriptions | Objective comorbidity/disease burden |
| Financial stability near-ceiling (95% report stable) — effectively uninformative as a predictor | SCB income | Continuous income measure with real variance |
| Self-reported housing suitability shows suppression/collinearity with satisfaction — may partly reflect response bias | Building register | Objective physical accessibility independent of perception |

### Analysis plan once register data are available

1. **Outcome validation**: Cross-tabulate `relocated` (survey) vs `relocated_register`
   (address change). Report agreement (kappa), identify discordant cases, and
   re-run primary models (RQ1–RQ4) substituting the register outcome. Report
   whether key findings hold.

2. **Attrition analysis**: Classify all T3 non-respondents as deceased, dropout,
   or administratively censored. Refit survival models with corrected censoring.
   Compare characteristics of completers, dropouts, and deceased to assess
   potential bias.

3. **Health trajectory models**: Add Charlson/Elixhauser comorbidity index to
   RQ2–RQ4 models in place of (or alongside) `srh`. Given that `srh` was
   non-significant in all current models, this tests whether the null health
   effect reflects measurement limitations or a true absence of association.

4. **SES analysis**: Add disposable income quintile to all models. Of particular
   interest in RQ4 (unexpected movers): whether low income predicts involuntary
   relocation among non-intenders, which would strengthen the market-constraint
   narrative alongside the tenure finding (renter OR = 1.91).

5. **Objective housing accessibility**: Replace or supplement `housing_suitability`
   (currently shows suppression effect with `home_satisfaction`) with objective
   building-level accessibility measures. This may resolve the collinearity issue
   and clarify whether the suppression is a perceptual artifact.

### Script naming convention (when register data arrive)
Follow the existing pipeline convention:

| Step | Script | Purpose |
|------|--------|---------|
| Clean | `scripts/01_clean/03_register_linkage.R` | Link survey data to register extracts; create `survey_register.rds` |
| Analyze | `scripts/02_analyze/06_register_validation.R` | Outcome validation and attrition analysis |
| Analyze | `scripts/02_analyze/07_register_models.R` | Re-run RQ1–RQ4 with register predictors/outcomes |
| Visualize | `scripts/03_visualize/06_register_figures.R` | Figures for register-linked results |

All register-linked analyses must load `survey_register.rds` only.
Raw register extracts go in `data/raw/` and must never be modified.
