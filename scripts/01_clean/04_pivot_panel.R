# 04_pivot_panel.R
# Purpose: Convert wide survey_analysis.rds to long panel format (person x wave)
# Output:  data/processed/survey_panel.rds  (5874 rows x ~N cols)

library(tidyverse)
library(haven)

# ── Step 1: Load ──────────────────────────────────────────────────────────────

df <- readRDS("data/processed/survey_analysis.rds")
cat("Loaded: data/processed/survey_analysis.rds\n")
cat("Dimensions:", nrow(df), "x", ncol(df), "\n")

# ── Step 2: Drop 3 NA-ID rows ─────────────────────────────────────────────────
# 3 rows have no LopNr_PersonNr and are completely empty —
# useless for register merge (can't link without an ID)

df <- df |> filter(!is.na(LopNr_PersonNr))
stopifnot(nrow(df) == 1958)
cat("Dropped NA-ID rows. Remaining rows:", nrow(df), "\n")

# ── Step 3: Normalize T1 VAR naming (2-digit → 3-digit) ──────────────────────
# T1 uses 2-digit VAR numbers (e.g. VAR13_1_T1),
# T2/T3 use 3-digit (e.g. VAR013_1_T2).
# Pad the 161 affected T1 column names so pivot_longer can unify them.
#
# Pattern: columns matching ^VAR[0-9]{2}_(.+_)?T1$ get one leading zero added
# after "VAR". VAR085_T1 (already 3-digit) is NOT matched and left unchanged.
# The (.+_)? makes the sub-item suffix optional, catching both VAR24_T1 and
# VAR13_1_T1 (60 no-sub-item + 161 sub-item = 221 total T1 cols to pad).

names(df) <- ifelse(
  grepl("^VAR[0-9]{2}_(.+_)?T1$", names(df)),
  sub("^VAR([0-9]{2})", "VAR0\\1", names(df)),
  names(df)
)

# Spot-check
stopifnot("VAR013_1_T1" %in% names(df))   # was VAR13_1_T1
stopifnot("VAR024_T1"   %in% names(df))   # was VAR24_T1
cat("T1 VAR names normalized (2-digit → 3-digit padding applied)\n")

# ── Step 3b: Resolve character/numeric type conflicts ─────────────────────────
# pivot_longer errors when the same base variable is character in one wave and
# numeric in another (e.g. VAR031_10: free-text at T1/T2, coded at T3).
# Detect these dynamically and coerce all waves to character.
# haven_labelled vs numeric conflicts are handled gracefully by pivot as warnings.

wave_sfx     <- c("_T1", "_T2", "_T3")
base_names_all <- unique(sub("_(T1|T2|T3)$", "",
                             grep("_(T1|T2|T3)$", names(df), value = TRUE)))
for (base in base_names_all) {
  cols  <- intersect(paste0(base, wave_sfx), names(df))
  if (length(cols) < 2) next
  types <- vapply(cols, \(col) class(df[[col]])[1], character(1))
  # Coerce to character whenever any wave is character and not all are
  if ("character" %in% types && length(unique(types)) > 1) {
    df <- df |> mutate(across(all_of(cols), as.character))
  }
}
cat("Type-conflict resolution complete\n")

# ── Step 4: pivot_longer ──────────────────────────────────────────────────────

panel <- df |>
  pivot_longer(
    cols      = matches("_(T1|T2|T3)$"),
    names_to  = c(".value", "wave"),
    names_pattern = "^(.+)_(T[123])$"
  )

cat("Pivot complete. Dimensions:", nrow(panel), "x", ncol(panel), "\n")

# ── Step 5: Add `year` variable ───────────────────────────────────────────────

panel <- panel |>
  mutate(year = case_when(
    wave == "T1" ~ 2021L,
    wave == "T2" ~ 2022L,
    wave == "T3" ~ 2024L
  ))

# ── Step 6: Column ordering ───────────────────────────────────────────────────
# Move ID, year, wave to front for readability

panel <- panel |>
  select(LopNr_PersonNr, year, wave, Date, everything())

# ── Step 7: Save ──────────────────────────────────────────────────────────────

write_rds(panel, "data/processed/survey_panel.rds")
cat("Saved: data/processed/survey_panel.rds\n")
cat("Dimensions:", nrow(panel), "x", ncol(panel), "\n")

# ── Verification ──────────────────────────────────────────────────────────────

# 1. Row count
stopifnot(nrow(panel) == 1958 * 3)
cat("Row count OK:", nrow(panel), "\n")

# 2. Wave respondent counts (match validation script findings)
resp <- panel |>
  group_by(wave, year) |>
  summarise(
    n_total = n(),
    n_date  = sum(!is.na(Date)),
    .groups = "drop"
  )
print(resp)
# Expected: T1 ~1958, T2 ~1501, T3 ~1296 with non-NA Date

# 3. Year mapping check
stopifnot(all(panel$year[panel$wave == "T1"] == 2021))
stopifnot(all(panel$year[panel$wave == "T2"] == 2022))
stopifnot(all(panel$year[panel$wave == "T3"] == 2024))
cat("Year mapping OK\n")

# 4. Spot-check: pick the first person with all 3 waves present,
#    compare Age and VAR013_1 values to original wide df
sample_id <- panel |>
  group_by(LopNr_PersonNr) |>
  filter(n() == 3, sum(!is.na(Date)) == 3) |>
  pull(LopNr_PersonNr) |>
  first()

orig <- readRDS("data/processed/survey_analysis.rds") |>
  filter(LopNr_PersonNr == sample_id)

panel_person <- panel |> filter(LopNr_PersonNr == sample_id)

cat("\nSpot-check for LopNr_PersonNr =", sample_id, "\n")
cat("Age in panel:  T1=", panel_person$Age[panel_person$wave == "T1"],
    " T2=", panel_person$Age[panel_person$wave == "T2"],
    " T3=", panel_person$Age[panel_person$wave == "T3"], "\n")
cat("Age in wide:   T1=", orig$Age_T1,
    " T2=", orig$Age_T2,
    " T3=", orig$Age_T3, "\n")

# Use as.numeric() so class/attribute differences (e.g. haven_labelled vs numeric)
# don't cause false failures — we care about the values, not the wrapper type
stopifnot(
  as.numeric(panel_person$Age[panel_person$wave == "T1"]) == as.numeric(orig$Age_T1),
  as.numeric(panel_person$Age[panel_person$wave == "T2"]) == as.numeric(orig$Age_T2),
  as.numeric(panel_person$Age[panel_person$wave == "T3"]) == as.numeric(orig$Age_T3)
)
cat("Spot-check PASSED\n")

# 5. No-suffix vars repeated correctly (e.g. relocated same on all 3 rows)
stopifnot(
  panel |>
    group_by(LopNr_PersonNr) |>
    summarise(n_distinct_reloc = n_distinct(relocated, na.rm = FALSE)) |>
    pull(n_distinct_reloc) |>
    max() == 1
)
cat("Constant variable check (relocated) PASSED\n")
