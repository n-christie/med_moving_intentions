library(tidyverse)
library(survival)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Cox proportional hazards model for time-to-relocation.
#
# Censoring rules:
#   Relocated            → end_date = reloc_date1
#   Not relocated, T3    → end_date = Date (T3 row)
#   Not relocated, no T3 → end_date = 2024-11-13 (administrative censoring)
#
# Models:
#   Cox M1 — intention_timeframe (unadjusted)
#   Cox M2 — intention_timeframe + age + sex + srh
#   Cox M3 — Cox M2 + housing factors
#   Cox stratified — strata(intention_timeframe) + covariates [primary model]
#
# Outputs:
#   figures/survival_cox_forest.png
#   tables/survival_sample.csv
#   tables/survival_cox_m1.csv, _m2.csv, _m3.csv, _stratified.csv
#   tables/survival_ph_test.csv
#   models/survival_cox_m1.rds … cox_strat.rds

# ── Load data ─────────────────────────────────────────────────────────────────
panel     <- readRDS("data/processed/panel_merged.rds")
study_end <- as.Date("2024-11-13")

t1 <- panel |> filter(wave == "T1")

t3_dates <- panel |>
  filter(wave == "T3") |>
  select(LopNr_PersonNr, Date_T3 = Date)

surv_dat <- t1 |>
  left_join(t3_dates, by = "LopNr_PersonNr") |>
  filter(!is.na(relocated_f), !is.na(Date), !is.na(intention_timeframe),
         !is.na(age), !is.na(sex), !is.na(srh)) |>
  mutate(
    end_date    = case_when(
      relocated == 1  ~ reloc_date1,
      !is.na(Date_T3) ~ Date_T3,
      TRUE            ~ study_end
    ),
    time_months = as.numeric(end_date - Date) / 30.44,
    event       = as.integer(relocated == 1)
  ) |>
  filter(time_months > 0)

cat("=== Survival analysis sample ===\n")
surv_sample <- tibble(
  n                = nrow(surv_dat),
  n_events         = sum(surv_dat$event),
  n_censored       = sum(surv_dat$event == 0),
  n_t3_responded   = sum(surv_dat$event == 0 & !is.na(surv_dat$Date_T3)),
  n_admin_censored = sum(surv_dat$event == 0 & is.na(surv_dat$Date_T3)),
  median_followup  = round(median(surv_dat$time_months), 1)
)
print(surv_sample)

# ── Fit Cox models ────────────────────────────────────────────────────────────
cox_m1 <- coxph(Surv(time_months, event) ~ intention_timeframe,
                data = surv_dat)

cox_m2 <- coxph(Surv(time_months, event) ~ intention_timeframe + age + sex + srh,
                data = surv_dat)

surv_dat_m3 <- surv_dat |>
  filter(complete.cases(pick(housing_suitability, home_satisfaction,
                             neighbourhood_cohesion)))

cox_m3 <- coxph(Surv(time_months, event) ~ intention_timeframe + age + sex + srh +
                  housing_suitability + home_satisfaction + neighbourhood_cohesion,
                data = surv_dat_m3)

cox_strat <- coxph(
  Surv(time_months, event) ~ strata(intention_timeframe) +
    age + sex + srh +
    housing_suitability + home_satisfaction + neighbourhood_cohesion,
  data = surv_dat_m3
)

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

cat("\n=== PH assumption test — Cox M2 ===\n")
ph_m2 <- cox.zph(cox_m2)
print(ph_m2)

cat("\n=== PH assumption test — Cox M3 ===\n")
ph_m3 <- cox.zph(cox_m3)
print(ph_m3)

cat("\n=== Stratified Cox model (strata = intention_timeframe) ===\n")
cox_strat |>
  tidy(conf.int = TRUE, exponentiate = TRUE) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
         p.value = round(p.value, 3)) |>
  select(term, estimate, conf.low, conf.high, p.value) |>
  print()
cat("Concordance:", round(summary(cox_strat)$concordance[1], 3), "\n")

cat("\nPH test — stratified model:\n")
ph_strat <- cox.zph(cox_strat)
print(ph_strat)

# ── Save outputs ──────────────────────────────────────────────────────────────
extract_cox <- function(model, nm) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
           p.value = round(p.value, 4),
           concordance = round(summary(model)$concordance[1], 3),
           model = nm)
}

cox_m1_tbl    <- extract_cox(cox_m1,    "Cox_M1")
cox_m2_tbl    <- extract_cox(cox_m2,    "Cox_M2")
cox_m3_tbl    <- extract_cox(cox_m3,    "Cox_M3")
cox_strat_tbl <- extract_cox(cox_strat, "Cox_stratified")

ph_tbl <- bind_rows(
  as.data.frame(ph_m2$table)  |> rownames_to_column("term") |> mutate(model = "M2"),
  as.data.frame(ph_strat$table) |> rownames_to_column("term") |> mutate(model = "stratified")
)

write_csv(surv_sample,    "paper/paper1/tables/survival_sample.csv")
write_csv(cox_m1_tbl,     "paper/paper1/tables/survival_cox_m1.csv")
write_csv(cox_m2_tbl,     "paper/paper1/tables/survival_cox_m2.csv")
write_csv(cox_m3_tbl,     "paper/paper1/tables/survival_cox_m3.csv")
write_csv(cox_strat_tbl,  "paper/paper1/tables/survival_cox_stratified.csv")
write_csv(ph_tbl,         "paper/paper1/tables/survival_ph_test.csv")

write_rds(cox_m1,   "paper/paper1/models/survival_cox_m1.rds")
write_rds(cox_m2,   "paper/paper1/models/survival_cox_m2.rds")
write_rds(cox_m3,   "paper/paper1/models/survival_cox_m3.rds")
write_rds(cox_strat,"paper/paper1/models/survival_cox_stratified.rds")

# Forest plot — stratified Cox (primary model)
forest_dat <- tidy(cox_strat, conf.int = TRUE, exponentiate = TRUE) |>
  mutate(
    term = recode(term,
      "age"                   = "Age (per year)",
      "sexWoman"              = "Sex: Woman",
      "srh"                   = "Self-rated health",
      "housing_suitability"   = "Housing suitability",
      "home_satisfaction"     = "Home satisfaction",
      "neighbourhood_cohesion"= "Neighbourhood cohesion"
    ),
    term = factor(term, levels = rev(c(
      "Age (per year)", "Sex: Woman", "Self-rated health",
      "Housing suitability", "Home satisfaction", "Neighbourhood cohesion"
    ))),
    sig = p.value < 0.05
  )

p_forest <- ggplot(forest_dat,
       aes(x = estimate, y = term, colour = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.7) +
  geom_point(size = 3) +
  scale_x_log10() +
  scale_colour_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#999999"),
                      labels  = c("TRUE" = "p < .05", "FALSE" = "p ≥ .05")) +
  labs(x = "Hazard ratio (log scale)", y = NULL, colour = NULL,
       title   = "Survival: Stratified Cox model (strata = intention timeframe)",
       caption = "Intention timeframe handled via stratification; HR not shown.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper1/figures/survival_cox_forest.png", p_forest,
       width = 7, height = 4, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper1/\n")
