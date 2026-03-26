library(tidyverse)
library(survival)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Further analysis: Cox proportional hazards model for time-to-relocation.
#
# Complements the binary logistic regression (RQ1–RQ3) by using the exact
# relocation dates and correctly accounting for:
#   (a) varying follow-up time (T1 surveys span March–December 2021)
#   (b) right-censoring of participants who had not relocated by T3
#
# Censoring rules:
#   Relocated          → end_date = reloc_date1 (exact relocation date)
#   Not relocated, T3 responded  → end_date = Date_T3 (last known contact)
#   Not relocated, T3 missing    → end_date = 2024-11-13 (administrative
#                                   censoring at last T3 response date)
#
# NOTE: 571 non-responders at T3 are assigned administrative censoring.
# If non-response is associated with relocation or health, this introduces
# informative censoring. Results should be interpreted with this caveat.
#
# Models:
#   Cox M1 — intention_timeframe (unadjusted)
#   Cox M2 — intention_timeframe + age + sex + srh
#   Cox M3 — Cox M2 + housing_suitability + home_satisfaction +
#             neighbourhood_cohesion
#
# Variable derivations: see scripts/01_clean/02_recode.R

# ── Load data ─────────────────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

study_end <- as.Date("2024-11-13")

surv_dat <- df |>
  filter(!is.na(relocated_f), !is.na(Date_T1), !is.na(intention_timeframe),
         !is.na(age), !is.na(sex), !is.na(srh)) |>
  mutate(
    end_date    = case_when(
      relocated == 1  ~ reloc_date1,
      !is.na(Date_T3) ~ Date_T3,
      TRUE            ~ study_end
    ),
    time_months = as.numeric(end_date - Date_T1) / 30.44,
    event       = as.integer(relocated == 1)
  ) |>
  filter(time_months > 0)

cat("=== Survival analysis sample ===\n")
cat("n:", nrow(surv_dat), "\n")
cat("Events (relocated):", sum(surv_dat$event), "\n")
cat("Censored:", sum(surv_dat$event == 0), "\n")
cat("  of which T3 responded:", sum(surv_dat$event == 0 & !is.na(surv_dat$Date_T3)), "\n")
cat("  of which admin censored:", sum(surv_dat$event == 0 & is.na(surv_dat$Date_T3)), "\n")
cat("Median follow-up (months):", round(median(surv_dat$time_months), 1), "\n\n")

# ── Fit Cox models ────────────────────────────────────────────────────────────
cox_m1 <- coxph(Surv(time_months, event) ~ intention_timeframe,
                data = surv_dat)

cox_m2 <- coxph(Surv(time_months, event) ~ intention_timeframe + age + sex + srh,
                data = surv_dat)

# M3 needs complete housing cases
surv_dat_m3 <- surv_dat |>
  filter(complete.cases(pick(housing_suitability, home_satisfaction,
                             neighbourhood_cohesion)))

cox_m3 <- coxph(Surv(time_months, event) ~ intention_timeframe + age + sex + srh +
                  housing_suitability + home_satisfaction + neighbourhood_cohesion,
                data = surv_dat_m3)

# ── Print results ─────────────────────────────────────────────────────────────
print_cox <- function(model, label) {
  cat("\n===", label, "===\n")
  model |>
    tidy(conf.int = TRUE, exponentiate = TRUE) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
           p.value = round(p.value, 3)) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    print()
  cat("Concordance:", round(summary(model)$concordance[1], 3), "\n")
}

print_cox(cox_m1, "Cox M1: Unadjusted")
print_cox(cox_m2, "Cox M2: Adjusted (age, sex, SRH)")
print_cox(cox_m3, "Cox M3: + Housing factors")

# ── Proportional hazards test (Schoenfeld residuals) ──────────────────────────
cat("\n=== PH assumption test — Cox M2 ===\n")
ph_test <- cox.zph(cox_m2)
print(ph_test)

cat("\n=== PH assumption test — Cox M3 ===\n")
print(cox.zph(cox_m3))

# ── Stratified Cox model (handles PH violation for intention_timeframe) ────────
# Because intention_timeframe violates the PH assumption, we stratify on it.
# This removes the HR for intention (which is shown by the KM curves instead)
# but gives valid, unbiased HRs for all other covariates.
cat("\n=== Stratified Cox model (strata = intention_timeframe) ===\n")
cox_strat <- coxph(
  Surv(time_months, event) ~ strata(intention_timeframe) +
    age + sex + srh +
    housing_suitability + home_satisfaction + neighbourhood_cohesion,
  data = surv_dat_m3
)

cox_strat |>
  tidy(conf.int = TRUE, exponentiate = TRUE) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
         p.value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  print()

cat("Concordance:", round(summary(cox_strat)$concordance[1], 3), "\n")
cat("\nPH test — stratified model:\n")
print(cox.zph(cox_strat))
