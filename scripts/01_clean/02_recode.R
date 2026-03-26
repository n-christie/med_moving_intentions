library(tidyverse)
library(labelled)

# ── Overview ──────────────────────────────────────────────────────────────────
# This script applies all deliberate variable recodings and creates derived
# composite variables needed for analysis. Every decision is documented.
#
# Input:  data/processed/survey_clean.rds   (produced by 01_import.R)
# Output: data/processed/survey_analysis.rds
#
# Sections:
#   1. Data integrity fixes
#   2. Outcome variable
#   3. Key predictor: moving intentions
#   4. Demographics
#   5. Housing composites
#   6. Obstacle variables (filter-question handling)

df <- readRDS("data/processed/survey_clean.rds")

# ── 1. Data integrity fixes ───────────────────────────────────────────────────

# VAR18_T1 (neighbourhood trust): nominal 1–3 scale, but 3 rows have values 4
# and 5. These are almost certainly data entry errors — no valid response
# category exists beyond 3. Recoded to NA.
# (Affected rows: n = 3; the 3 non-integer values identified during data check)
n_var18_fixed <- sum(as.numeric(df$VAR18_T1) > 3, na.rm = TRUE)
df <- df |>
  mutate(VAR18_T1 = if_else(as.numeric(VAR18_T1) > 3, NA_real_, as.numeric(VAR18_T1)))
cat("VAR18_T1: recoded", n_var18_fixed, "stray values (>3) to NA\n")

# ── 2. Outcome variable ───────────────────────────────────────────────────────

# relocated: already 0/1 after import cleaning. Convert to labelled factor
# for use in models and tables.
df <- df |>
  mutate(
    relocated_f = factor(relocated, levels = c(0, 1), labels = c("No", "Yes"))
  )
var_label(df$relocated_f) <- "Relocated during study period"

# ── 3. Moving intentions ──────────────────────────────────────────────────────

# VAR24_T1: "Within what timeframe might you need to move to a new home?"
#   1 = Less than 1 year
#   2 = 1 to 2 years
#   3 = 2 years or more
#
# Reference category set to 3 (lowest intention) for RQ1/RQ2 full-sample models.
# For RQ3 (intenders only), reference is 2 (1–2 years) — see 03_RQ3_intenders.R.
df <- df |>
  mutate(
    intention_timeframe = factor(
      VAR24_T1,
      levels = c(3, 2, 1),
      labels = c("2+ years (ref)", "1\u20132 years", "< 1 year")
    )
  )
var_label(df$intention_timeframe) <- "Expected timeframe to move (VAR24_T1)"

# VAR24_T1 within-intenders version (reference = 1–2 years):
# Created here for consistency; used in RQ3 subset.
df <- df |>
  mutate(
    intention_level = factor(
      VAR24_T1,
      levels = c(2, 1),
      labels = c("1\u20132 years (ref)", "< 1 year")
    )
  )
var_label(df$intention_level) <- "Intention level among intenders (VAR24_T1, ref=1-2yr)"

# ── 4. Demographics and health ────────────────────────────────────────────────

df <- df |>
  mutate(
    age = as.numeric(Age_T1),
    sex = factor(Sex_T1, levels = c(1, 2), labels = c("Man", "Woman")),

    # VAR34_T1: "In general, would you say that your health is..."
    #   1 = Poor, 2 = Reasonably good, 3 = Good, 4 = Very good, 5 = Excellent
    # Treated as a continuous predictor (higher = better health).
    # Included in models as a covariate; poorer health motivates intention to move
    # but does not independently predict actual relocation after adjusting for
    # intentions (OR ≈ 1.07, p = .41 in preliminary analysis).
    srh = as.numeric(VAR34_T1)
  )
var_label(df$age) <- "Age at baseline (years)"
var_label(df$sex) <- "Sex"
var_label(df$srh) <- "Self-rated health (VAR34_T1; 1=Poor to 5=Excellent)"
cat("srh: missing =", sum(is.na(df$srh)), "\n")

# ── 5. Housing composites ─────────────────────────────────────────────────────

# 5a. Housing suitability — mean of VAR13_1–4 (scale: 1=not suitable, 5=suitable)
#
# Items:
#   VAR13_1: personal hygiene / getting dressed / restroom
#   VAR13_2: cooking / food preparation
#   VAR13_3: dishes / cleaning / tending plants
#   VAR13_4: laundry / clothing care
#
# Justification for composite: inter-item correlations r = 0.47–0.75 (checked
# in data exploration). All 4 items required — no partial scoring.
# Higher score = more suitable home = hypothesised lower odds of relocation.
df <- df |>
  mutate(
    housing_suitability = rowMeans(
      pick(VAR13_1_T1, VAR13_2_T1, VAR13_3_T1, VAR13_4_T1),
      na.rm = FALSE
    )
  )
var_label(df$housing_suitability) <- "Housing suitability composite (mean VAR13_1–4; 1–5)"
cat("housing_suitability: missing =", sum(is.na(df$housing_suitability)), "\n")

# 5b. Home satisfaction — VAR14_T1 (single item; 1=not at all, 5=yes absolutely)
# Used as a continuous predictor. No transformation applied.
df <- df |>
  mutate(home_satisfaction = as.numeric(VAR14_T1))
var_label(df$home_satisfaction) <- "Home satisfaction (VAR14_T1; 1–5)"

# 5c. Neighbourhood cohesion — mean of VAR16–18 (scale: 1=disagree, 3=agree)
#
# Items:
#   VAR16: "People in my neighbourhood know each other well"
#   VAR17: "People in my neighbourhood are willing to help one another"
#   VAR18: "The people in my neighbourhood are trustworthy"
#            (NB: 3 stray values recoded to NA in section 1 above)
#
# All three items use a 1–3 response scale. All 3 items required for composite.
df <- df |>
  mutate(
    neighbourhood_cohesion = rowMeans(
      pick(VAR16_T1, VAR17_T1, VAR18_T1),
      na.rm = FALSE
    )
  )
var_label(df$neighbourhood_cohesion) <- "Neighbourhood cohesion composite (mean VAR16–18; 1–3)"
cat("neighbourhood_cohesion: missing =", sum(is.na(df$neighbourhood_cohesion)), "\n")

# ── 6. Obstacle variables ─────────────────────────────────────────────────────

# VAR30_T1: "Are there any obstacles that might make it difficult to move?"
#   1 = Yes  |  2 = No, there are no obstacles
#
# VAR31_1–8_T1: Specific obstacles (multi-select; asked only to VAR30 = 1)
#   Because VAR31 items are filter questions, participants who said VAR30 = 2
#   (no obstacles) have NA on all VAR31 items — but the correct interpretation
#   is 0 (obstacle not present), not missing. We recode accordingly.
#
#   VAR30 = 1 (has obstacles) → VAR31 values used as observed (1=yes, 2=no → 0/1)
#   VAR30 = 2 (no obstacles)  → VAR31 items set to 0
#   VAR30 = NA                → VAR31 items remain NA
#
# Obstacle items included (VAR31_3 and VAR31_9 excluded):
#   VAR31_3: n = 20 endorsed — too sparse for modelling
#   VAR31_9: free-text "other" — not codeable

df <- df |>
  mutate(
    # Binary: any obstacle at all
    any_obstacle = case_when(
      VAR30_T1 == 1 ~ 1L,
      VAR30_T1 == 2 ~ 0L,
      TRUE          ~ NA_integer_
    ),

    # Individual obstacle items (recode VAR30=No → 0; VAR31=1 → 1; VAR31=2 → 0)
    across(
      all_of(paste0("VAR31_", c(1, 2, 4, 5, 6, 7, 8), "_T1")),
      \(x) case_when(
        VAR30_T1 == 2 ~ 0L,
        x == 1        ~ 1L,
        x == 2        ~ 0L,
        TRUE          ~ NA_integer_
      )
    )
  ) |>
  rename(
    obs_financial      = VAR31_1_T1,
    obs_supply         = VAR31_2_T1,
    obs_bulky          = VAR31_4_T1,
    obs_energy         = VAR31_5_T1,
    obs_own_health     = VAR31_6_T1,
    obs_partner_health = VAR31_7_T1,
    obs_dependents     = VAR31_8_T1
  )

var_label(df$any_obstacle)        <- "Any perceived obstacle to moving (VAR30_T1)"
var_label(df$obs_financial)       <- "Obstacle: Financial — moving too expensive (VAR31_1)"
var_label(df$obs_supply)          <- "Obstacle: Limited housing supply (VAR31_2)"
var_label(df$obs_bulky)           <- "Obstacle: Household goods too bulky (VAR31_4)"
var_label(df$obs_energy)          <- "Obstacle: Lack of energy to move (VAR31_5)"
var_label(df$obs_own_health)      <- "Obstacle: Own health impaired (VAR31_6)"
var_label(df$obs_partner_health)  <- "Obstacle: Partner health impaired (VAR31_7)"
var_label(df$obs_dependents)      <- "Obstacle: Has dependents (VAR31_8)"

cat("any_obstacle: missing =", sum(is.na(df$any_obstacle)), "\n")

# ── Save ──────────────────────────────────────────────────────────────────────
write_rds(df, "data/processed/survey_analysis.rds")
cat("\nSaved: data/processed/survey_analysis.rds\n")
cat("Dimensions:", nrow(df), "x", ncol(df), "\n")
