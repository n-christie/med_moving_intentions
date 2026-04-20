library(tidyverse)

# ── Overview ──────────────────────────────────────────────────────────────────
# Cohort tracking: longitudinal descriptives for the full sample across
# T1 (2021) → T2 (2022) → T3 (2024).
#
# Sections:
#   1. Response and attrition by wave
#   2. Baseline characteristics (Table 1)
#   3. Longitudinal change in key measures (completers)
#   4. Register data summary (LISA linkage at T1)
#
# Outputs:
#   tables/cohort_response_by_wave.csv
#   tables/cohort_deaths_by_interval.csv
#   tables/cohort_baseline_continuous.csv
#   tables/cohort_baseline_sex.csv
#   tables/cohort_baseline_relocation.csv
#   tables/cohort_baseline_intention.csv
#   tables/cohort_stratified_by_relocation.csv
#   tables/cohort_stratified_by_t3.csv
#   tables/cohort_completers_intention_by_wave.csv
#   tables/cohort_completers_srh_by_wave.csv

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")

cat("Loaded panel_merged.rds:", nrow(panel), "rows x", ncol(panel), "cols\n")
cat("Unique individuals:", n_distinct(panel$LopNr_PersonNr), "\n\n")

# ── Section 1: Response and attrition ─────────────────────────────────────────
cat("════════════════════════════════════════════════════════════\n")
cat("SECTION 1: Response and attrition\n")
cat("════════════════════════════════════════════════════════════\n\n")

wave_response <- panel |>
  group_by(wave, year) |>
  summarise(
    n_total       = n(),
    n_responded   = sum(!is.na(Date)),
    pct_responded = round(n_responded / n_total * 100, 1),
    .groups = "drop"
  )
cat("Response by wave:\n")
print(wave_response)

deaths <- panel |>
  group_by(LopNr_PersonNr) |>
  summarise(died = any(died_in_study, na.rm = TRUE), .groups = "drop")
n_died <- sum(deaths$died, na.rm = TRUE)
cat("\nDeaths during study (died_in_study):", n_died, "unique individuals\n\n")

deaths_detail <- panel |>
  filter(!is.na(death_date)) |>
  distinct(LopNr_PersonNr, death_date, death_year) |>
  mutate(
    interval = case_when(
      death_year <= 2021 ~ "Before or at T1",
      death_year == 2022 ~ "T1\u2192T2 (2022)",
      death_year == 2023 ~ "T2\u2192T3 (2023)",
      death_year >= 2024 ~ "At/after T3 (2024+)",
      TRUE ~ "Unknown"
    )
  )
cat("Deaths by study interval:\n")
deaths_by_interval <- deaths_detail |> count(interval)
print(deaths_by_interval)

t3_dat <- panel |> filter(wave == "T3")
cat("\nT3 attrition:\n")
cat("  Responded:      ", sum(!is.na(t3_dat$Date)), "\n")
cat("  Admin censored: ", sum(is.na(t3_dat$Date)), "\n\n")

# ── Section 2: Baseline characteristics ──────────────────────────────────────
cat("════════════════════════════════════════════════════════════\n")
cat("SECTION 2: Baseline characteristics (T1)\n")
cat("════════════════════════════════════════════════════════════\n\n")

t1 <- panel |> filter(wave == "T1")

t3_responded_ids <- panel |>
  filter(wave == "T3", !is.na(Date)) |>
  pull(LopNr_PersonNr)

t1 <- t1 |>
  mutate(t3_participated = LopNr_PersonNr %in% t3_responded_ids)

cat("T1 n:", nrow(t1), "| T3 participated:", sum(t1$t3_participated),
    "| Not:", sum(!t1$t3_participated), "\n\n")

summ_cont <- function(x, label) {
  tibble(variable = label, n = sum(!is.na(x)),
         mean = round(mean(x, na.rm = TRUE), 1),
         sd   = round(sd(x, na.rm = TRUE), 1),
         median = round(median(x, na.rm = TRUE), 1),
         min  = round(min(x, na.rm = TRUE), 1),
         max  = round(max(x, na.rm = TRUE), 1))
}

cat("=== Continuous variables — full T1 sample ===\n")
baseline_cont <- bind_rows(
  summ_cont(t1$age,                    "Age"),
  summ_cont(t1$srh,                    "Self-rated health (1\u20135)"),
  summ_cont(t1$housing_suitability,    "Housing suitability"),
  summ_cont(t1$home_satisfaction,      "Home satisfaction"),
  summ_cont(t1$neighbourhood_cohesion, "Neighbourhood cohesion")
)
print(baseline_cont)

cat("\n=== Sex ===\n")
baseline_sex <- t1 |>
  count(sex) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(baseline_sex)

cat("\n=== Relocation status ===\n")
baseline_reloc <- t1 |>
  count(relocated_f) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(baseline_reloc)

cat("\n=== Intention timeframe ===\n")
baseline_intention <- t1 |>
  count(intention_timeframe) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(baseline_intention)

cat("\n=== Continuous variables stratified by relocation status ===\n")
strat_by_reloc <- t1 |>
  filter(!is.na(relocated_f)) |>
  group_by(relocated_f) |>
  summarise(
    n                 = n(),
    age_mean          = round(mean(age, na.rm = TRUE), 1),
    age_sd            = round(sd(age, na.rm = TRUE), 1),
    srh_mean          = round(mean(srh, na.rm = TRUE), 2),
    srh_sd            = round(sd(srh, na.rm = TRUE), 2),
    suitability_mean  = round(mean(housing_suitability, na.rm = TRUE), 2),
    suitability_sd    = round(sd(housing_suitability, na.rm = TRUE), 2),
    satisfaction_mean = round(mean(home_satisfaction, na.rm = TRUE), 2),
    satisfaction_sd   = round(sd(home_satisfaction, na.rm = TRUE), 2),
    cohesion_mean     = round(mean(neighbourhood_cohesion, na.rm = TRUE), 2),
    cohesion_sd       = round(sd(neighbourhood_cohesion, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(strat_by_reloc)

cat("\n=== Continuous variables stratified by T3 participation ===\n")
strat_by_t3 <- t1 |>
  group_by(t3_participated) |>
  summarise(
    n                 = n(),
    age_mean          = round(mean(age, na.rm = TRUE), 1),
    age_sd            = round(sd(age, na.rm = TRUE), 1),
    srh_mean          = round(mean(srh, na.rm = TRUE), 2),
    srh_sd            = round(sd(srh, na.rm = TRUE), 2),
    suitability_mean  = round(mean(housing_suitability, na.rm = TRUE), 2),
    suitability_sd    = round(sd(housing_suitability, na.rm = TRUE), 2),
    satisfaction_mean = round(mean(home_satisfaction, na.rm = TRUE), 2),
    satisfaction_sd   = round(sd(home_satisfaction, na.rm = TRUE), 2),
    cohesion_mean     = round(mean(neighbourhood_cohesion, na.rm = TRUE), 2),
    cohesion_sd       = round(sd(neighbourhood_cohesion, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(strat_by_t3)

# ── Section 3: Longitudinal change (completers) ───────────────────────────────
cat("════════════════════════════════════════════════════════════\n")
cat("SECTION 3: Longitudinal change (completers)\n")
cat("════════════════════════════════════════════════════════════\n\n")

completer_ids <- panel |>
  group_by(LopNr_PersonNr) |>
  summarise(n_responded = sum(!is.na(Date)), .groups = "drop") |>
  filter(n_responded == 3) |>
  pull(LopNr_PersonNr)

cat("Completers (3 waves):", length(completer_ids), "\n\n")

completers <- panel |> filter(LopNr_PersonNr %in% completer_ids)

cat("=== Intention (VAR024) by wave — completers ===\n")
completers_intention <- completers |>
  mutate(VAR024_num = as.numeric(VAR024)) |>
  group_by(wave) |>
  summarise(
    n        = sum(!is.na(VAR024_num)),
    mean     = round(mean(VAR024_num, na.rm = TRUE), 2),
    sd       = round(sd(VAR024_num, na.rm = TRUE), 2),
    pct_1or2 = round(mean(VAR024_num %in% c(1, 2), na.rm = TRUE) * 100, 1),
    pct_3    = round(mean(VAR024_num == 3, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(completers_intention)

cat("\n=== Self-rated health (SRH) by wave — completers ===\n")
completers_srh <- completers |>
  group_by(wave) |>
  summarise(
    n    = sum(!is.na(srh)),
    mean = round(mean(srh, na.rm = TRUE), 2),
    sd   = round(sd(srh, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(completers_srh)

# ── Section 4: Register summary ───────────────────────────────────────────────
cat("════════════════════════════════════════════════════════════\n")
cat("SECTION 4: Register data summary (LISA linkage)\n")
cat("════════════════════════════════════════════════════════════\n\n")

has_disp_ink  <- "disp_ink_ke"  %in% names(panel)
has_syss_stat <- "syss_stat19"  %in% names(panel)

if (!has_disp_ink && !has_syss_stat) {
  cat("Note: Register variables (disp_ink_ke, syss_stat19) not found.\n")
  cat("      Run 05_merge_register.R to link LISA data.\n\n")
  cat("Available register-like columns:\n")
  grep("ink|disp|income|syss|empl|LISA", names(panel),
       value = TRUE, ignore.case = TRUE) |> print()
} else {
  t1_reg <- panel |> filter(wave == "T1")

  if (has_disp_ink) {
    n_with_register <- sum(!is.na(t1_reg$disp_ink_ke))
    cat("Register coverage:", n_with_register, "of", nrow(t1_reg),
        "(", round(n_with_register / nrow(t1_reg) * 100, 1), "%)\n\n")
    disp_ink_summary <- t1_reg |>
      summarise(n = sum(!is.na(disp_ink_ke)),
                mean = round(mean(disp_ink_ke, na.rm = TRUE), 0),
                sd   = round(sd(disp_ink_ke, na.rm = TRUE), 0),
                median = round(median(disp_ink_ke, na.rm = TRUE), 0),
                p25  = round(quantile(disp_ink_ke, .25, na.rm = TRUE), 0),
                p75  = round(quantile(disp_ink_ke, .75, na.rm = TRUE), 0))
    cat("=== Disposable income (disp_ink_ke) at T1 ===\n")
    print(disp_ink_summary)
    cat("\n")
  }
  if (has_syss_stat) {
    cat("=== Employment status (syss_stat19) at T1 ===\n")
    t1_reg |>
      count(syss_stat19) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      arrange(desc(n)) |>
      print()
    cat("\n")
  }
}

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(wave_response,          "paper/descriptive/tables/cohort_response_by_wave.csv")
write_csv(deaths_by_interval,     "paper/descriptive/tables/cohort_deaths_by_interval.csv")
write_csv(baseline_cont,          "paper/descriptive/tables/cohort_baseline_continuous.csv")
write_csv(baseline_sex,           "paper/descriptive/tables/cohort_baseline_sex.csv")
write_csv(baseline_reloc,         "paper/descriptive/tables/cohort_baseline_relocation.csv")
write_csv(baseline_intention,     "paper/descriptive/tables/cohort_baseline_intention.csv")
write_csv(strat_by_reloc,         "paper/descriptive/tables/cohort_stratified_by_relocation.csv")
write_csv(strat_by_t3,            "paper/descriptive/tables/cohort_stratified_by_t3.csv")
write_csv(completers_intention,   "paper/descriptive/tables/cohort_completers_intention_by_wave.csv")
write_csv(completers_srh,         "paper/descriptive/tables/cohort_completers_srh_by_wave.csv")

cat("Saved all tables to paper/descriptive/tables/\n")
cat("Done.\n")
