library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ1: To what extent do self-reported moving intentions at baseline predict
#      actual relocation over a three-year period?
#
# Outcome:    relocated_f (factor, No/Yes)
# Predictor:  intention_timeframe (VAR024; ref = "2+ years")
# Covariates: age, sex, srh (self-rated health)
#
# Models:
#   M0 — null (intercept only)
#   M1 — intention_timeframe only (unadjusted)
#   M2 — intention_timeframe + age + sex + srh (adjusted)
#
# Outputs:
#   figures/RQ1_forest.png
#   tables/RQ1_relocation_by_intention.csv
#   tables/RQ1_m1_coefficients.csv
#   tables/RQ1_m2_coefficients.csv
#   tables/RQ1_model_fit.csv
#   models/RQ1_m0.rds, RQ1_m1.rds, RQ1_m2.rds

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

# ── Analysis sample ───────────────────────────────────────────────────────────
dat_m <- df |>
  filter(!is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex), !is.na(srh))

cat("Analysis sample:\n")
cat("  Total n:        ", nrow(dat_m), "\n")
cat("  Relocated (Yes):", sum(dat_m$relocated_f == "Yes"), "\n")
cat("  Relocated (%):  ", round(mean(dat_m$relocated_f == "Yes") * 100, 1), "\n\n")

cat("Relocation rate by intention timeframe:\n")
reloc_by_intention <- dat_m |>
  group_by(intention_timeframe) |>
  summarise(
    n           = n(),
    n_relocated = sum(relocated_f == "Yes"),
    pct         = round(n_relocated / n * 100, 1),
    .groups = "drop"
  )
print(reloc_by_intention)

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
model_fit <- tibble(
  model         = c("M0 (null)", "M1 (intentions)", "M2 (+ age + sex + srh)"),
  AIC           = round(c(AIC(m0), AIC(m1), AIC(m2)), 1),
  nagelkerke_r2 = c(NA, nagelkerke(m1, m0), nagelkerke(m2, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1))
print(model_fit)

# ── Save outputs ──────────────────────────────────────────────────────────────
# Tables
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

# Models
write_rds(m0, "paper/paper1/models/RQ1_m0.rds")
write_rds(m1, "paper/paper1/models/RQ1_m1.rds")
write_rds(m2, "paper/paper1/models/RQ1_m2.rds")

# Forest plot
forest_dat <- bind_rows(
  tidy(m1, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M1: Unadjusted"),
  tidy(m2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M2: Adjusted")
) |>
  filter(str_detect(term, "intention_timeframe")) |>
  mutate(
    term  = str_remove(term, "intention_timeframe"),
    term  = factor(term, levels = c("1\u20132 years", "< 1 year")),
    model = factor(model, levels = c("M1: Unadjusted", "M2: Adjusted"))
  )

p_forest <- ggplot(forest_dat,
       aes(x = estimate, y = term, colour = model, shape = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 30),
                labels  = c("1", "2", "5", "10", "20", "30")) +
  scale_colour_manual(values = c("M1: Unadjusted" = "#555555",
                                 "M2: Adjusted"   = "#0072B2")) +
  scale_shape_manual(values  = c("M1: Unadjusted" = 16,
                                 "M2: Adjusted"   = 17)) +
  labs(x = "Odds ratio (log scale)",
       y = "Expected timeframe to move\n(Reference: 2+ years)",
       colour = NULL, shape = NULL,
       title   = "RQ1: Moving intentions predicting relocation",
       caption = "Adjusted model includes age, sex, and self-rated health.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper1/figures/RQ1_forest.png", p_forest,
       width = 7, height = 4, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper1/\n")
