library(tidyverse)
library(labelled)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ1: To what extent do self-reported moving intentions at baseline predict
#      actual relocation over a three-year period?
#
# Outcome:    relocated_f (factor, No/Yes)
# Predictor:  intention_timeframe (VAR24_T1; ref = "2+ years")
# Covariates: age, sex, srh (self-rated health)
#
# Models:
#   M0 — null (intercept only)
#   M1 — intention_timeframe only (unadjusted)
#   M2 — intention_timeframe + age + sex + srh (adjusted)
#
# Variable derivations: see scripts/01_clean/02_recode.R

# ── Load data ─────────────────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

# ── Analysis sample ───────────────────────────────────────────────────────────
dat_m <- df |>
  filter(!is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex), !is.na(srh))

cat("Analysis sample:\n")
cat("  Total n:        ", nrow(dat_m), "\n")
cat("  Relocated (Yes):", sum(dat_m$relocated_f == "Yes"), "\n")
cat("  Relocated (%):  ", round(mean(dat_m$relocated_f == "Yes") * 100, 1), "\n\n")

cat("Relocation rate by intention timeframe:\n")
dat_m |>
  group_by(intention_timeframe) |>
  summarise(
    n           = n(),
    n_relocated = sum(relocated_f == "Yes"),
    pct         = round(n_relocated / n * 100, 1),
    .groups = "drop"
  ) |>
  print()

# ── Fit models ────────────────────────────────────────────────────────────────
m0 <- glm(relocated_f ~ 1,                                       data = dat_m, family = binomial)
m1 <- glm(relocated_f ~ intention_timeframe,                      data = dat_m, family = binomial)
m2 <- glm(relocated_f ~ intention_timeframe + age + sex + srh,    data = dat_m, family = binomial)

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

print_model(m1, "M1: Unadjusted (intentions only)")
print_model(m2, "M2: Adjusted (intentions + age + sex)")

# ── Model fit ─────────────────────────────────────────────────────────────────
nagelkerke <- function(model, null) {
  n <- nobs(model); l0 <- logLik(null); lm <- logLik(model)
  round((1 - exp((2/n) * (l0 - lm))) / (1 - exp(2 * l0 / n)), 3)
}

cat("\n=== Model fit ===\n")
tibble(
  model         = c("M0 (null)", "M1 (intentions)", "M2 (+ age + sex + srh)"),
  AIC           = round(c(AIC(m0), AIC(m1), AIC(m2)), 1),
  nagelkerke_r2 = c(NA, nagelkerke(m1, m0), nagelkerke(m2, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1)) |>
  print()
