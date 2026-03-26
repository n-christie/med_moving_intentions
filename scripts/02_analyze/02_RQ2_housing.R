library(tidyverse)
library(labelled)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ2: To what extent do housing-related factors at baseline predict actual
#      relocation, independent of moving intentions?
#
# Outcome:    relocated_f (factor, No/Yes)
# Models build on the RQ1 adjusted baseline (M2):
#   M2 — intentions + age + sex                        [baseline, from RQ1]
#   M3 — M2 + housing suitability
#   M4 — M2 + home satisfaction
#   M5 — M2 + neighbourhood cohesion
#   M6 — M2 + all housing factors (full model)
#
# Variable derivations: see scripts/01_clean/02_recode.R

# ── Load data ─────────────────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

dat_m <- df |>
  filter(
    !is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex),
    complete.cases(pick(housing_suitability, home_satisfaction, neighbourhood_cohesion))
  )

cat("Model sample n:", nrow(dat_m), "\n\n")

# ── Housing variable descriptives ─────────────────────────────────────────────
cat("=== Housing variable descriptives ===\n")
dat_m |>
  select(housing_suitability, home_satisfaction, neighbourhood_cohesion) |>
  summarise(across(everything(), list(
    mean = \(x) round(mean(x, na.rm = TRUE), 2),
    sd   = \(x) round(sd(x, na.rm = TRUE), 2)
  ))) |>
  pivot_longer(everything(), names_to = c("var", ".value"), names_sep = "_(?=[^_]+$)") |>
  print()

# ── Fit models ────────────────────────────────────────────────────────────────
m2 <- glm(relocated_f ~ intention_timeframe + age + sex,
          data = dat_m, family = binomial)
m3 <- glm(relocated_f ~ intention_timeframe + age + sex + housing_suitability,
          data = dat_m, family = binomial)
m4 <- glm(relocated_f ~ intention_timeframe + age + sex + home_satisfaction,
          data = dat_m, family = binomial)
m5 <- glm(relocated_f ~ intention_timeframe + age + sex + neighbourhood_cohesion,
          data = dat_m, family = binomial)
m6 <- glm(relocated_f ~ intention_timeframe + age + sex +
            housing_suitability + home_satisfaction + neighbourhood_cohesion,
          data = dat_m, family = binomial)

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

print_model(m3, "M3: Baseline + housing suitability")
print_model(m4, "M4: Baseline + home satisfaction")
print_model(m5, "M5: Baseline + neighbourhood cohesion")
print_model(m6, "M6: Full housing model")

# ── Model fit ─────────────────────────────────────────────────────────────────
nagelkerke <- function(model, null) {
  n <- nobs(model); l0 <- logLik(null); lm <- logLik(model)
  round((1 - exp((2/n) * (l0 - lm))) / (1 - exp(2 * l0 / n)), 3)
}

m0 <- glm(relocated_f ~ 1, data = dat_m, family = binomial)

cat("\n=== Model fit comparison ===\n")
tibble(
  model = c("M2 (baseline)", "M3 (+suitability)", "M4 (+satisfaction)",
            "M5 (+cohesion)", "M6 (full housing)"),
  AIC           = round(c(AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6)), 1),
  nagelkerke_r2 = c(nagelkerke(m2, m0), nagelkerke(m3, m0), nagelkerke(m4, m0),
                    nagelkerke(m5, m0), nagelkerke(m6, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1)) |>
  print()

cat("\n=== Likelihood ratio tests vs M2 baseline ===\n")
list(M3 = m3, M4 = m4, M5 = m5, M6 = m6) |>
  imap_dfr(\(mod, nm) {
    lrt <- anova(m2, mod, test = "LRT")
    tibble(model = nm, delta_df = lrt$Df[2],
           delta_dev = round(lrt$Deviance[2], 2),
           p_value   = round(lrt$`Pr(>Chi)`[2], 3))
  }) |>
  print()
