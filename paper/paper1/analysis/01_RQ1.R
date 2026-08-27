library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ1: To what extent do self-reported moving intentions at baseline predict
#      actual relocation over a three-year period?
#
# Outcome:    relocated_f (factor, No/Yes)
# Predictor:  intention_4cat (VAR024 x VAR021_1; ref = "No stated intention")
#             see scripts/01_clean/02_recode.R section 3 for the full
#             construction rationale (this is NOT a skip-logic pair — both
#             source items were asked of the full T1 sample).
# Covariates: age, sex, srh (self-rated health)
#
# Main models (full sample, 4-level intention predictor):
#   M0 — null (intercept only)
#   M1 — intention_4cat only (unadjusted)
#   M2 — intention_4cat + age + sex + srh (adjusted)
#
# Sequential decomposition (reviewer-requested reframing): separates the
# "any intention vs none" claim from the "urgency among intenders" claim so
# the headline result is not read as tautological.
#   Step A (any intention, full sample):
#     A1 — has_intention only (unadjusted)
#     A2 — has_intention + age + sex + srh (adjusted)
#   Step B (urgency, among has_intention == "Yes" only):
#     B1 — intention_timeframe_among_intenders only (unadjusted)
#     B2 — intention_timeframe_among_intenders + age + sex + srh (adjusted)
#
# Outputs:
#   figures/RQ1_forest.png                      (main 4-level model)
#   figures/RQ1_sequential_forest.png           (Step A + Step B)
#   tables/RQ1_relocation_by_intention.csv
#   tables/RQ1_m1_coefficients.csv
#   tables/RQ1_m2_coefficients.csv
#   tables/RQ1_model_fit.csv
#   tables/RQ1_stepA_coefficients.csv
#   tables/RQ1_stepB_coefficients.csv
#   tables/RQ1_sequential_model_fit.csv
#   models/RQ1_m0.rds, RQ1_m1.rds, RQ1_m2.rds, RQ1_a1.rds, RQ1_a2.rds, RQ1_b1.rds, RQ1_b2.rds

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

# ── Analysis sample (main 4-level model) ───────────────────────────────────────
dat_m <- df |>
  filter(!is.na(relocated_f), !is.na(intention_4cat), !is.na(age), !is.na(sex), !is.na(srh))

cat("Analysis sample (main model):\n")
cat("  Total n:        ", nrow(dat_m), "\n")
cat("  Relocated (Yes):", sum(dat_m$relocated_f == "Yes"), "\n")
cat("  Relocated (%):  ", round(mean(dat_m$relocated_f == "Yes") * 100, 1), "\n\n")

cat("Relocation rate by intention_4cat:\n")
reloc_by_intention <- dat_m |>
  group_by(intention_4cat) |>
  summarise(
    n           = n(),
    n_relocated = sum(relocated_f == "Yes"),
    pct         = round(n_relocated / n * 100, 1),
    .groups = "drop"
  )
print(reloc_by_intention)

# ── Fit main models (4-level, ref = No stated intention) ───────────────────────
m0 <- glm(relocated_f ~ 1,                                   data = dat_m, family = binomial)
m1 <- glm(relocated_f ~ intention_4cat,                      data = dat_m, family = binomial)
m2 <- glm(relocated_f ~ intention_4cat + age + sex + srh,    data = dat_m, family = binomial)

# ── Fit sequential models ───────────────────────────────────────────────────────
# Step A: any stated intention vs none (full sample, same rows as dat_m).
a1 <- glm(relocated_f ~ has_intention,                       data = dat_m, family = binomial)
a2 <- glm(relocated_f ~ has_intention + age + sex + srh,     data = dat_m, family = binomial)

# Step B: urgency among those with any stated intention only.
dat_intenders <- dat_m |> filter(has_intention == "Yes", !is.na(intention_timeframe_among_intenders))
b0 <- glm(relocated_f ~ 1,                                                    data = dat_intenders, family = binomial)
b1 <- glm(relocated_f ~ intention_timeframe_among_intenders,                  data = dat_intenders, family = binomial)
b2 <- glm(relocated_f ~ intention_timeframe_among_intenders + age + sex + srh, data = dat_intenders, family = binomial)

cat("\nStep B analysis sample (any stated intention only):\n")
cat("  Total n:        ", nrow(dat_intenders), "\n")
cat("  Relocated (Yes):", sum(dat_intenders$relocated_f == "Yes"), "\n\n")

# ── Results ───────────────────────────────────────────────────────────────────
print_model <- function(model, label) {
  cat("\n===", label, "===\n")
  model |>
    tidy(conf.int = TRUE, exponentiate = TRUE) |>
    filter(!str_detect(term, "Intercept")) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
           p.value = round(p.value, 3)) |>
    print()
}

print_model(m1, "M1: Unadjusted (4-level intention, ref = no stated intention)")
print_model(m2, "M2: Adjusted (4-level intention + age + sex + srh)")
print_model(a1, "A1: Step A unadjusted (any intention vs none)")
print_model(a2, "A2: Step A adjusted (any intention vs none + age + sex + srh)")
print_model(b1, "B1: Step B unadjusted (urgency among intenders only)")
print_model(b2, "B2: Step B adjusted (urgency among intenders only + age + sex + srh)")

# ── Model fit ─────────────────────────────────────────────────────────────────
nagelkerke <- function(model, null) {
  n <- nobs(model); l0 <- logLik(null); lm <- logLik(model)
  round((1 - exp((2/n) * (l0 - lm))) / (1 - exp(2 * l0 / n)), 3)
}

cat("\n=== Main model fit (4-level intention) ===\n")
model_fit <- tibble(
  model         = c("M0 (null)", "M1 (intention_4cat)", "M2 (+ age + sex + srh)"),
  n             = c(nobs(m0), nobs(m1), nobs(m2)),
  AIC           = round(c(AIC(m0), AIC(m1), AIC(m2)), 1),
  nagelkerke_r2 = c(NA, nagelkerke(m1, m0), nagelkerke(m2, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1))
print(model_fit)

cat("\n=== Sequential model fit (Step A: any intention; Step B: urgency among intenders) ===\n")
sequential_fit <- tibble(
  step          = c("A1 (any intention)", "A2 (any intention, adjusted)",
                     "B1 (urgency | intends)", "B2 (urgency | intends, adjusted)"),
  n             = c(nobs(a1), nobs(a2), nobs(b1), nobs(b2)),
  AIC           = round(c(AIC(a1), AIC(a2), AIC(b1), AIC(b2)), 1),
  nagelkerke_r2 = c(nagelkerke(a1, glm(relocated_f ~ 1, data = model.frame(a1), family = binomial)),
                     nagelkerke(a2, glm(relocated_f ~ 1, data = model.frame(a2), family = binomial)),
                     nagelkerke(b1, b0),
                     nagelkerke(b2, b0))
)
print(sequential_fit)

# ── Save outputs ──────────────────────────────────────────────────────────────
# Tables — main 4-level model
m1_coef <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4), model = "M1")

m2_coef <- tidy(m2, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4), model = "M2")

write_csv(reloc_by_intention,            "paper/paper1/tables/RQ1_relocation_by_intention.csv")
write_csv(m1_coef,                       "paper/paper1/tables/RQ1_m1_coefficients.csv")
write_csv(m2_coef,                       "paper/paper1/tables/RQ1_m2_coefficients.csv")
write_csv(model_fit,                     "paper/paper1/tables/RQ1_model_fit.csv")

# Tables — sequential decomposition
stepA_coef <- bind_rows(
  tidy(a1, conf.int = TRUE, exponentiate = TRUE) |> mutate(step = "A1"),
  tidy(a2, conf.int = TRUE, exponentiate = TRUE) |> mutate(step = "A2")
) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4))

stepB_coef <- bind_rows(
  tidy(b1, conf.int = TRUE, exponentiate = TRUE) |> mutate(step = "B1"),
  tidy(b2, conf.int = TRUE, exponentiate = TRUE) |> mutate(step = "B2")
) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4))

write_csv(stepA_coef,       "paper/paper1/tables/RQ1_stepA_coefficients.csv")
write_csv(stepB_coef,       "paper/paper1/tables/RQ1_stepB_coefficients.csv")
write_csv(sequential_fit,   "paper/paper1/tables/RQ1_sequential_model_fit.csv")

# ── SES-adjusted robustness check (item 10) ─────────────────────────────────────
# Does the intention effect survive adjustment for register-measured SES?
# Same three register covariates as the A8 attrition model and the shared
# register appendix (docs/index.qmd): disposable income, marital status,
# employment status. M2 is refit on the SES-complete subsample so the
# comparison is apples-to-apples (same N), not confounded by the covariate's
# own missingness pattern.
dat_ses <- dat_m |>
  mutate(
    civil_f = case_when(
      civil == "G"  ~ "Married",
      civil == "S"  ~ "Cohabiting",
      civil == "OG" ~ "Single",
      civil == "Ä"  ~ "Widowed",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Married", "Cohabiting", "Single", "Widowed")),
    syss_f = case_when(
      syss_stat19 == 1 ~ "Employed",
      syss_stat19 == 6 ~ "Retired",
      !is.na(syss_stat19) ~ "Other",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Employed", "Retired", "Other"))
  ) |>
  filter(!is.na(disp_ink_ke), !is.na(civil_f), !is.na(syss_f))

cat("\nSES-adjusted robustness sample n:", nrow(dat_ses), "\n")

m2_common <- glm(relocated_f ~ intention_4cat + age + sex + srh, data = dat_ses, family = binomial)
m2_ses    <- glm(relocated_f ~ intention_4cat + age + sex + srh + disp_ink_ke + civil_f + syss_f,
                  data = dat_ses, family = binomial)

ses_comparison <- bind_rows(
  tidy(m2_common, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M2 (survey-only)"),
  tidy(m2_ses,    conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M2 + register SES")
) |>
  filter(str_detect(term, "intention_4cat")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4))

cat("\n=== SES-adjusted robustness: does the intention effect survive? ===\n")
print(ses_comparison)

ses_full_coef <- tidy(m2_ses, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4))

write_csv(ses_comparison, "paper/paper1/tables/RQ1_ses_adjusted_comparison.csv")
write_csv(ses_full_coef,  "paper/paper1/tables/RQ1_ses_adjusted_full.csv")
write_rds(m2_ses,         "paper/paper1/models/RQ1_m2_ses.rds")

# Models
write_rds(m0, "paper/paper1/models/RQ1_m0.rds")
write_rds(m1, "paper/paper1/models/RQ1_m1.rds")
write_rds(m2, "paper/paper1/models/RQ1_m2.rds")
write_rds(a1, "paper/paper1/models/RQ1_a1.rds")
write_rds(a2, "paper/paper1/models/RQ1_a2.rds")
write_rds(b1, "paper/paper1/models/RQ1_b1.rds")
write_rds(b2, "paper/paper1/models/RQ1_b2.rds")

# ── Forest plot: main 4-level model ─────────────────────────────────────────────
forest_dat <- bind_rows(
  tidy(m1, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M1: Unadjusted"),
  tidy(m2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M2: Adjusted")
) |>
  filter(str_detect(term, "intention_4cat")) |>
  mutate(
    term  = str_remove(term, "intention_4cat"),
    term  = factor(term, levels = c("2+ years, intends", "1–2 years", "< 1 year")),
    model = factor(model, levels = c("M1: Unadjusted", "M2: Adjusted"))
  )

p_forest <- ggplot(forest_dat,
       aes(x = estimate, y = term, colour = model, shape = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 30, 50),
                labels  = c("1", "2", "5", "10", "20", "30", "50")) +
  scale_colour_manual(values = c("M1: Unadjusted" = "#555555",
                                 "M2: Adjusted"   = "#0072B2")) +
  scale_shape_manual(values  = c("M1: Unadjusted" = 16,
                                 "M2: Adjusted"   = 17)) +
  labs(x = "Odds ratio (log scale)",
       y = "Expected timeframe to move\n(Reference: no stated intention)",
       colour = NULL, shape = NULL,
       title   = "RQ1: Moving intentions predicting relocation",
       caption = "Adjusted model includes age, sex, and self-rated health.\nReference category separates \"no stated intention\" from \"2+ years, intends.\"") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper1/figures/RQ1_forest.png", p_forest,
       width = 7, height = 4, dpi = 300)

# ── Forest plot: sequential decomposition (Step A + Step B) ────────────────────
seq_forest_dat <- bind_rows(
  tidy(a1, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Unadjusted", step = "Step A: any intention (ref: none)"),
  tidy(a2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Adjusted",   step = "Step A: any intention (ref: none)"),
  tidy(b1, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Unadjusted", step = "Step B: urgency among intenders (ref: 2+ years)"),
  tidy(b2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Adjusted",   step = "Step B: urgency among intenders (ref: 2+ years)")
) |>
  filter(str_detect(term, "has_intention|intention_timeframe_among_intenders")) |>
  mutate(
    term  = str_remove(term, "has_intentionYes"),
    term  = str_remove(term, "intention_timeframe_among_intenders"),
    term  = if_else(term == "", "Any intention (vs none)", term),
    term  = factor(term, levels = c("Any intention (vs none)", "1–2 years", "< 1 year")),
    model = factor(model, levels = c("Unadjusted", "Adjusted")),
    step  = factor(step, levels = c("Step A: any intention (ref: none)",
                                     "Step B: urgency among intenders (ref: 2+ years)"))
  )

p_seq_forest <- ggplot(seq_forest_dat,
       aes(x = estimate, y = term, colour = model, shape = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 30, 50),
                labels  = c("1", "2", "5", "10", "20", "30", "50")) +
  scale_colour_manual(values = c("Unadjusted" = "#555555", "Adjusted" = "#0072B2")) +
  scale_shape_manual(values  = c("Unadjusted" = 16, "Adjusted" = 17)) +
  facet_grid(step ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Odds ratio (log scale)", y = NULL,
       colour = NULL, shape = NULL,
       title   = "RQ1 sequential decomposition: any intention vs. urgency",
       caption = "Step A: does any stated intention predict relocation? Step B: among those with\nany intention, does urgency (timeframe) further predict relocation? Adjusted models\ninclude age, sex, and self-rated health.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement    = "outside",
        strip.text.y.left  = element_text(angle = 0, hjust = 0))

ggsave("paper/paper1/figures/RQ1_sequential_forest.png", p_seq_forest,
       width = 7.5, height = 4.5, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper1/\n")
