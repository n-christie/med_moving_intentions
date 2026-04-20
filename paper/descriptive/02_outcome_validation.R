# =============================================================================
# 02_outcome_validation.R
# Outcome validation: classify T3 non-responders using register data.
# For individuals who did not complete T3, determine probable reason using:
#   - death_date (deceased)
#   - Date_avreg (de-registered / emigrated)
#   - lan/kommun changes between T1 and T3 (probable move)
#
# Input:  data/processed/panel_merged.rds
# Output: paper/descriptive/tables/t3_nonresponder_classification.csv
# =============================================================================

library(tidyverse)

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")

# ── T3 non-responders ─────────────────────────────────────────────────────────
non_resp_t3 <- panel |>
  filter(wave == "T3", is.na(Date)) |>
  select(LopNr_PersonNr, death_date, Date_avreg, lan, kommun)

cat("T3 non-responders:", nrow(non_resp_t3), "\n")

# ── T1 geographic reference for same individuals ──────────────────────────────
t1_geo <- panel |>
  filter(wave == "T1") |>
  select(LopNr_PersonNr, lan_t1 = lan, kommun_t1 = kommun)

# ── Classify non-responders ───────────────────────────────────────────────────
non_resp_classified <- non_resp_t3 |>
  left_join(t1_geo, by = "LopNr_PersonNr") |>
  mutate(
    status = case_when(
      !is.na(death_date)                            ~ "Deceased during study",
      !is.na(Date_avreg)                            ~ "De-registered (emigrated)",
      !is.na(lan) & lan != lan_t1                   ~ "County change (probable move)",
      !is.na(kommun) & kommun != kommun_t1          ~ "Municipality change (probable move)",
      !is.na(lan)                                   ~ "No address change (likely no move)",
      TRUE                                          ~ "No register match"
    )
  )

# ── Summary table ─────────────────────────────────────────────────────────────
summary_tab <- non_resp_classified |>
  count(status) |>
  mutate(
    pct = round(n / sum(n) * 100, 1)
  ) |>
  arrange(desc(n))

cat("\nT3 non-responder classification:\n")
print(summary_tab)

# ── Save ──────────────────────────────────────────────────────────────────────
write_csv(summary_tab, "paper/descriptive/tables/t3_nonresponder_classification.csv")
cat("\nSaved: paper/descriptive/tables/t3_nonresponder_classification.csv\n")

# ── Additional detail: probable movers ───────────────────────────────────────
probable_movers <- non_resp_classified |>
  filter(status %in% c("County change (probable move)",
                        "Municipality change (probable move)"))

cat("\nProbable movers among T3 non-responders:", nrow(probable_movers), "\n")
cat("These individuals likely relocated but did not complete T3.\n")
cat("Their survey-based relocated status may be 0 (missing follow-up).\n")
