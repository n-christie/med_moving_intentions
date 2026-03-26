library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ4: Among those who did not intend to move at baseline (VAR24_T1 = 3),
#      which factors predict unexpected relocation?
#
# Outcome:  relocated_f (factor, No/Yes)
# Subset:   VAR24_T1 == 3 ("2 years or more") — non-intenders
# N:        1,286 total; 134 relocated (10.4%)
#
# Models:
#   M1 — age + sex + srh                          [demographic baseline]
#   M2 — M1 + housing_suitability + home_satisfaction + neighbourhood_cohesion
#   M3 — M2 + housing tenure (owner vs renter)
#
# Note: obstacle variables (VAR30/31) are theoretically less interpretable in
# this subset — non-intenders may not have engaged with the obstacle question
# in the same way as intenders. Not included as primary predictors.
#
# Variable derivations: see scripts/01_clean/02_recode.R

# ── Load data and subset ──────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

dat <- df |>
  filter(!is.na(relocated_f), as.numeric(VAR24_T1) == 3) |>
  mutate(
    owner = factor(
      case_when(
        as.numeric(VAR01_2_T1) == 1 ~ "Owner",
        as.numeric(VAR01_2_T1) == 2 ~ "Renter",
        TRUE ~ NA_character_
      ),
      levels = c("Owner", "Renter")
    )
  )

cat("Non-intender sample:\n")
cat("  Total n:        ", nrow(dat), "\n")
cat("  Relocated (Yes):", sum(dat$relocated_f == "Yes"), "\n")
cat("  Relocated (%):  ", round(mean(dat$relocated_f == "Yes") * 100, 1), "\n\n")

# ── Analysis sample (complete cases across all models) ────────────────────────
dat_m <- dat |>
  filter(
    !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(housing_suitability, home_satisfaction,
                        neighbourhood_cohesion)),
    !is.na(owner)
  )

cat("Complete-case model sample n:", nrow(dat_m),
    "(relocated:", sum(dat_m$relocated_f == "Yes"), ")\n\n")

# ── Descriptives: key variables by relocation status ─────────────────────────
cat("=== Continuous predictors by relocation status ===\n")
dat_m |>
  group_by(relocated_f) |>
  summarise(
    across(c(age, srh, housing_suitability, home_satisfaction,
             neighbourhood_cohesion),
           list(mean = \(x) round(mean(x), 2),
                sd   = \(x) round(sd(x), 2)))
  ) |>
  pivot_longer(-relocated_f,
               names_to  = c("var", ".value"),
               names_sep = "_(?=[^_]+$)") |>
  pivot_wider(names_from = relocated_f,
              values_from = c(mean, sd)) |>
  print()

cat("\n=== Tenure by relocation status ===\n")
dat_m |>
  count(owner, relocated_f) |>
  group_by(owner) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  filter(relocated_f == "Yes") |>
  print()

# ── Fit models ────────────────────────────────────────────────────────────────
m0 <- glm(relocated_f ~ 1, data = dat_m, family = binomial)
m1 <- glm(relocated_f ~ age + sex + srh, data = dat_m, family = binomial)
m2 <- glm(relocated_f ~ age + sex + srh +
            housing_suitability + home_satisfaction + neighbourhood_cohesion,
          data = dat_m, family = binomial)
m3 <- glm(relocated_f ~ age + sex + srh +
            housing_suitability + home_satisfaction + neighbourhood_cohesion +
            owner,
          data = dat_m, family = binomial)

# ── Results ───────────────────────────────────────────────────────────────────
print_model <- function(model, label) {
  cat("\n===", label, "===\n")
  model |>
    tidy(conf.int = TRUE, exponentiate = TRUE) |>
    filter(!str_detect(term, "Intercept")) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
           p.value = round(p.value, 3)) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    print()
}

print_model(m1, "M1: Demographics only")
print_model(m2, "M2: + Housing factors")
print_model(m3, "M3: + Tenure")

# ── Model fit ─────────────────────────────────────────────────────────────────
nagelkerke <- function(model, null) {
  n <- nobs(model); l0 <- logLik(null); lm <- logLik(model)
  round((1 - exp((2/n) * (l0 - lm))) / (1 - exp(2 * l0 / n)), 3)
}

cat("\n=== Model fit ===\n")
tibble(
  model = c("M0 (null)", "M1 (demographics)", "M2 (+housing)", "M3 (+tenure)"),
  AIC   = round(c(AIC(m0), AIC(m1), AIC(m2), AIC(m3)), 1),
  nagelkerke_r2 = c(NA, nagelkerke(m1, m0), nagelkerke(m2, m0),
                    nagelkerke(m3, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1)) |>
  print()

cat("\n=== Likelihood ratio tests vs M1 ===\n")
list(M2 = m2, M3 = m3) |>
  imap_dfr(\(mod, nm) {
    lrt <- anova(m1, mod, test = "LRT")
    tibble(model = nm, delta_df = lrt$Df[2],
           delta_dev = round(lrt$Deviance[2], 2),
           p_value   = round(lrt$`Pr(>Chi)`[2], 3))
  }) |>
  print()
