library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ2: To what extent do housing-related factors at baseline predict actual
#      relocation, independent of moving intentions?
#
# Outcome:    relocated_f (factor, No/Yes)
# Models build on the RQ1 adjusted baseline (M2):
#   M2 — intentions + age + sex + srh                  [baseline, from RQ1]
#   M3 — M2 + housing suitability
#   M4 — M2 + home satisfaction
#   M5 — M2 + neighbourhood cohesion
#   M6 — M2 + all housing factors (full model)
#
# Outputs:
#   figures/RQ2_forest.png
#   tables/RQ2_housing_descriptives.csv
#   tables/RQ2_all_coefficients.csv
#   tables/RQ2_model_fit.csv
#   tables/RQ2_lrt.csv
#   models/RQ2_m2.rds … RQ2_m6.rds

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

dat_m <- df |>
  filter(
    !is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(housing_suitability, home_satisfaction, neighbourhood_cohesion))
  )

cat("Model sample n:", nrow(dat_m), "\n\n")

# ── Housing variable descriptives ─────────────────────────────────────────────
cat("=== Housing variable descriptives ===\n")
housing_desc <- dat_m |>
  select(housing_suitability, home_satisfaction, neighbourhood_cohesion) |>
  summarise(across(everything(), list(
    mean = \(x) round(mean(x, na.rm = TRUE), 2),
    sd   = \(x) round(sd(x, na.rm = TRUE), 2)
  ))) |>
  pivot_longer(everything(), names_to = c("var", ".value"), names_sep = "_(?=[^_]+$)")
print(housing_desc)

# ── Fit models ────────────────────────────────────────────────────────────────
m2 <- glm(relocated_f ~ intention_timeframe + age + sex + srh,
          data = dat_m, family = binomial)
m3 <- glm(relocated_f ~ intention_timeframe + age + sex + srh + housing_suitability,
          data = dat_m, family = binomial)
m4 <- glm(relocated_f ~ intention_timeframe + age + sex + srh + home_satisfaction,
          data = dat_m, family = binomial)
m5 <- glm(relocated_f ~ intention_timeframe + age + sex + srh + neighbourhood_cohesion,
          data = dat_m, family = binomial)
m6 <- glm(relocated_f ~ intention_timeframe + age + sex + srh +
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
model_fit <- tibble(
  model = c("M2 (baseline)", "M3 (+suitability)", "M4 (+satisfaction)",
            "M5 (+cohesion)", "M6 (full housing)"),
  AIC           = round(c(AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6)), 1),
  nagelkerke_r2 = c(nagelkerke(m2, m0), nagelkerke(m3, m0), nagelkerke(m4, m0),
                    nagelkerke(m5, m0), nagelkerke(m6, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1))
print(model_fit)

cat("\n=== Likelihood ratio tests vs M2 baseline ===\n")
lrt <- list(M3 = m3, M4 = m4, M5 = m5, M6 = m6) |>
  imap_dfr(\(mod, nm) {
    l <- anova(m2, mod, test = "LRT")
    tibble(model = nm, delta_df = l$Df[2],
           delta_dev = round(l$Deviance[2], 2),
           p_value   = round(l$`Pr(>Chi)`[2], 3))
  })
print(lrt)

# ── Save outputs ──────────────────────────────────────────────────────────────
# All model coefficients combined
all_coef <- list(M2 = m2, M3 = m3, M4 = m4, M5 = m5, M6 = m6) |>
  imap_dfr(\(mod, nm) {
    tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
      filter(!str_detect(term, "Intercept")) |>
      mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
             p.value = round(p.value, 4), model = nm)
  })

write_csv(housing_desc, "paper/paper1/tables/RQ2_housing_descriptives.csv")
write_csv(all_coef,     "paper/paper1/tables/RQ2_all_coefficients.csv")
write_csv(model_fit,    "paper/paper1/tables/RQ2_model_fit.csv")
write_csv(lrt,          "paper/paper1/tables/RQ2_lrt.csv")

write_rds(m2, "paper/paper1/models/RQ2_m2.rds")
write_rds(m3, "paper/paper1/models/RQ2_m3.rds")
write_rds(m4, "paper/paper1/models/RQ2_m4.rds")
write_rds(m5, "paper/paper1/models/RQ2_m5.rds")
write_rds(m6, "paper/paper1/models/RQ2_m6.rds")

# Forest plot — M6 full housing model
forest_dat <- tidy(m6, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term = recode(term,
      "intention_timeframe1\u20132 years" = "Intention: 1\u20132 years",
      "intention_timeframe< 1 year"       = "Intention: < 1 year",
      "age"                               = "Age (per year)",
      "sexWoman"                          = "Sex: Woman",
      "srh"                               = "Self-rated health",
      "housing_suitability"               = "Housing suitability",
      "home_satisfaction"                 = "Home satisfaction",
      "neighbourhood_cohesion"            = "Neighbourhood cohesion"
    ),
    term = factor(term, levels = rev(c(
      "Intention: 1\u20132 years", "Intention: < 1 year",
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
  labs(x = "Odds ratio (log scale)", y = NULL, colour = NULL,
       title   = "RQ2: Housing factors predicting relocation (M6, full model)",
       caption = "Reference: 2+ years intention, Male, adjusted for age, sex, SRH.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper1/figures/RQ2_forest.png", p_forest,
       width = 7, height = 5, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper1/\n")
