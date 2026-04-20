# =============================================================================
# 05_merge_register.R
# Merge LISA register data into survey panel
#
# Input 1: data/processed/survey_panel.rds  (5,874 × 756)
# Input 2: data/raw/lisa_out_anon.rds        (7,731 × 145)
# Output:  data/processed/panel_merged.rds
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# Step 1 — Load
# -----------------------------------------------------------------------------
panel <- readRDS("data/processed/survey_panel.rds")
lisa  <- readRDS("data/raw/lisa_out_anon.rds")

cat("Panel dimensions:", nrow(panel), "x", ncol(panel), "\n")
cat("LISA dimensions: ", nrow(lisa),  "x", ncol(lisa),  "\n")

# -----------------------------------------------------------------------------
# Step 2 — Align register to survey years only
# Register has 1,924 rows for 2023 — no survey wave counterpart; drop them.
# Keep only years matching survey waves: 2021, 2022, 2024.
# -----------------------------------------------------------------------------
lisa <- lisa |> filter(year %in% c(2021, 2022, 2024))
stopifnot(nrow(lisa) == 1951 + 1940 + 1916)  # = 5807
cat("LISA rows after filtering to survey years:", nrow(lisa), "\n")

# -----------------------------------------------------------------------------
# Step 3 — Rename key and tidy death variable
# -----------------------------------------------------------------------------
lisa <- lisa |>
  rename(
    LopNr_PersonNr = lopnr,
    death_date     = DODSDATN
  )

# -----------------------------------------------------------------------------
# Step 4 — Left join
# All 5,874 survey rows kept; register data added where available.
# 21 survey IDs with no register match → NAs in all register columns.
# -----------------------------------------------------------------------------
panel_merged <- panel |>
  left_join(lisa, by = c("LopNr_PersonNr", "year"))
stopifnot(nrow(panel_merged) == nrow(panel))  # 5,874 rows preserved
cat("Merged dimensions:", nrow(panel_merged), "x", ncol(panel_merged), "\n")

# -----------------------------------------------------------------------------
# Step 5 — Derive mortality flags
# died_in_study:    did this person die at any point during the study period?
# deceased_at_wave: had they died by the year of this specific wave?
# -----------------------------------------------------------------------------
panel_merged <- panel_merged |>
  mutate(
    died_in_study    = !is.na(death_date),
    deceased_at_wave = !is.na(death_year) & death_year <= year
  )

# -----------------------------------------------------------------------------
# Step 6 — Reorder key columns
# -----------------------------------------------------------------------------
panel_merged <- panel_merged |>
  select(LopNr_PersonNr, year, wave, Date,
         died_in_study, deceased_at_wave, death_date, death_year,
         everything())

# -----------------------------------------------------------------------------
# Step 7 — Save
# -----------------------------------------------------------------------------
write_rds(panel_merged, "data/processed/panel_merged.rds")
cat("Saved: data/processed/panel_merged.rds\n")
cat("Dimensions:", nrow(panel_merged), "x", ncol(panel_merged), "\n")

# =============================================================================
# Verification
# =============================================================================

# 1. Row count preserved
stopifnot(nrow(panel_merged) == 5874)
cat("\nRow count OK:", nrow(panel_merged), "\n")

# 2. Deceased count (30 unique individuals, visible as died_in_study == TRUE)
dec_summary <- panel_merged |>
  filter(died_in_study) |>
  distinct(LopNr_PersonNr) |>
  nrow()
cat("Unique deceased individuals:", dec_summary, "\n")
# Expected: 30

# 3. Survey-only IDs (no register match) → check register col is all-NA
no_match <- panel_merged |>
  filter(is.na(lone_ink)) |>
  distinct(LopNr_PersonNr) |>
  nrow()
cat("Survey IDs with no register data:", no_match, "\n")
# Expected: 21 (× 3 waves = 63 rows with NA register cols)

# 4. Wave × year counts unchanged
cat("\nWave × year distribution:\n")
print(panel_merged |> count(wave, year))

# 5. Spot-check: pick a known deceased person, verify death_date + flag
deceased_id <- panel_merged |>
  filter(died_in_study) |>
  pull(LopNr_PersonNr) |>
  first()
cat("\nSpot-check — deceased individual:", deceased_id, "\n")
print(panel_merged |>
  filter(LopNr_PersonNr == deceased_id) |>
  select(LopNr_PersonNr, wave, year, Date, death_date, death_year,
         died_in_study, deceased_at_wave))
