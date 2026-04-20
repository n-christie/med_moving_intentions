library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ3: Among those who intended to move at baseline, which factors are
#      associated with NOT having relocated after three years?
#
# Subset:  Intenders — VAR024 %in% c(1, 2) (expecting to move within 2 years)
# Outcome: relocated_f (1 = Yes, 0 = No). ORs < 1 = reduced odds of relocation.
#
# Models (all fitted on common complete cases):
#   M1 — age + sex + intention_level (< 1 yr vs 1–2 yrs)
#   M2 — M1 + housing factors
#   M3 — M1 + obstacles
#   M4 — M1 + housing + obstacles (full model)
#
# Outputs:
#   figures/RQ3_forest.png
#   tables/RQ3_intender_sample.csv
#   tables/RQ3_obstacle_prevalence.csv
#   tables/RQ3_all_coefficients.csv
#   tables/RQ3_model_fit.csv
#   tables/RQ3_lrt.csv
#   models/RQ3_m1.rds … RQ3_m4.rds

# ── Load data and subset to intenders ─────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

dat <- df |>
  filter(VAR024 %in% c(1, 2), !is.na(relocated_f))

cat("=== Intender subset ===\n")
cat("Total n:        ", nrow(dat), "\n")
cat("Relocated (Yes):", sum(dat$relocated_f == "Yes"), "\n")
cat("Not relocated:  ", sum(dat$relocated_f == "No"), "\n\n")

cat("Relocation rate by intention level:\n")
intender_sample <- dat |>
  group_by(intention_level) |>
  summarise(n = n(), relocated = sum(relocated_f == "Yes"),
            pct = round(relocated / n * 100, 1), .groups = "drop")
print(intender_sample)

cat("\nObstacle prevalence among intenders:\n")
obstacle_prev <- dat |>
  summarise(
    `Any obstacle (VAR30)` = sum(any_obstacle == 1, na.rm = TRUE),
    Financial              = sum(obs_financial == 1, na.rm = TRUE),
    `Limited supply`       = sum(obs_supply == 1, na.rm = TRUE),
    `No energy`            = sum(obs_energy == 1, na.rm = TRUE),
    `Own health`           = sum(obs_own_health == 1, na.rm = TRUE),
    `Partner health`       = sum(obs_partner_health == 1, na.rm = TRUE),
    Dependents             = sum(obs_dependents == 1, na.rm = TRUE),
    `Bulky goods`          = sum(obs_bulky == 1, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "obstacle", values_to = "n_endorsed") |>
  mutate(pct = round(n_endorsed / nrow(dat) * 100, 1))
print(obstacle_prev)

# ── Fit models on common complete cases ───────────────────────────────────────
dat_m <- dat |>
  filter(
    !is.na(intention_level), !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(
      housing_suitability, home_satisfaction, neighbourhood_cohesion,
      any_obstacle, obs_financial, obs_supply, obs_energy,
      obs_own_health, obs_partner_health, obs_dependents, obs_bulky
    ))
  )

cat("\nModel sample n:", nrow(dat_m),
    "(relocated:", sum(dat_m$relocated_f == "Yes"), ")\n")

m1_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh,
  data = dat_m, family = binomial
)
m2_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh +
    housing_suitability + home_satisfaction + neighbourhood_cohesion,
  data = dat_m, family = binomial
)
m3_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh +
    any_obstacle + obs_financial + obs_supply + obs_energy +
    obs_own_health + obs_partner_health + obs_dependents + obs_bulky,
  data = dat_m, family = binomial
)
m4_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh +
    housing_suitability + home_satisfaction + neighbourhood_cohesion +
    any_obstacle + obs_financial + obs_supply + obs_energy +
    obs_own_health + obs_partner_health + obs_dependents + obs_bulky,
  data = dat_m, family = binomial
)

# ── Results ───────────────────────────────────────────────────────────────────
term_recode <- c(
  "intention_level< 1 year"    = "Intention: < 1 year",
  "age"                        = "Age (per year)",
  "sexWoman"                   = "Sex: Woman",
  "srh"                        = "Self-rated health (1\u20135)",
  "housing_suitability"        = "Housing suitability",
  "home_satisfaction"          = "Home satisfaction",
  "neighbourhood_cohesion"     = "Neighbourhood cohesion",
  "any_obstacle"               = "Any obstacle (VAR30)",
  "obs_financial"              = "Obstacle: Financial",
  "obs_supply"                 = "Obstacle: Limited supply",
  "obs_energy"                 = "Obstacle: No energy",
  "obs_own_health"             = "Obstacle: Own health",
  "obs_partner_health"         = "Obstacle: Partner health",
  "obs_dependents"             = "Obstacle: Dependents",
  "obs_bulky"                  = "Obstacle: Bulky goods"
)

print_model <- function(model, label) {
  cat("\n===", label, "===\n")
  model |>
    tidy(conf.int = TRUE, exponentiate = TRUE) |>
    filter(!str_detect(term, "Intercept")) |>
    mutate(
      term = str_replace_all(term, term_recode),
      across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
      p.value = round(p.value, 3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.1  ~ ".",
        TRUE ~ ""
      )
    ) |>
    select(term, estimate, conf.low, conf.high, p.value, sig) |>
    print(n = 20)
}

print_model(m1_rq3, "M1: Demographics + intention level")
print_model(m2_rq3, "M2: M1 + housing factors")
print_model(m3_rq3, "M3: M1 + obstacles")
print_model(m4_rq3, "M4: Full model (housing + obstacles)")

# ── Model fit ─────────────────────────────────────────────────────────────────
nagelkerke <- function(model, null) {
  n <- nobs(model); l0 <- logLik(null); lm <- logLik(model)
  round((1 - exp((2/n) * (l0 - lm))) / (1 - exp(2 * l0 / n)), 3)
}

m0_rq3 <- glm(relocated_f ~ 1, data = dat_m, family = binomial)

cat("\n=== Model fit comparison ===\n")
model_fit <- tibble(
  model = c("M1 (demo + intention)", "M2 (+housing)", "M3 (+obstacles)", "M4 (full)"),
  AIC   = round(c(AIC(m1_rq3), AIC(m2_rq3), AIC(m3_rq3), AIC(m4_rq3)), 1),
  nagelkerke_r2 = c(nagelkerke(m1_rq3, m0_rq3), nagelkerke(m2_rq3, m0_rq3),
                    nagelkerke(m3_rq3, m0_rq3), nagelkerke(m4_rq3, m0_rq3))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1))
print(model_fit)

cat("\n=== Likelihood ratio tests vs M1 ===\n")
lrt <- list(M2 = m2_rq3, M3 = m3_rq3, M4 = m4_rq3) |>
  imap_dfr(\(mod, nm) {
    l <- anova(m1_rq3, mod, test = "LRT")
    tibble(model = nm, delta_df = l$Df[2],
           delta_dev = round(l$Deviance[2], 2),
           p_value   = round(l$`Pr(>Chi)`[2], 3))
  })
print(lrt)

# ── Save outputs ──────────────────────────────────────────────────────────────
all_coef <- list(M1 = m1_rq3, M2 = m2_rq3, M3 = m3_rq3, M4 = m4_rq3) |>
  imap_dfr(\(mod, nm) {
    tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
      filter(!str_detect(term, "Intercept")) |>
      mutate(term = str_replace_all(term, term_recode),
             across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
             p.value = round(p.value, 4), model = nm)
  })

write_csv(intender_sample, "paper/paper1/tables/RQ3_intender_sample.csv")
write_csv(obstacle_prev,   "paper/paper1/tables/RQ3_obstacle_prevalence.csv")
write_csv(all_coef,        "paper/paper1/tables/RQ3_all_coefficients.csv")
write_csv(model_fit,       "paper/paper1/tables/RQ3_model_fit.csv")
write_csv(lrt,             "paper/paper1/tables/RQ3_lrt.csv")

write_rds(m1_rq3, "paper/paper1/models/RQ3_m1.rds")
write_rds(m2_rq3, "paper/paper1/models/RQ3_m2.rds")
write_rds(m3_rq3, "paper/paper1/models/RQ3_m3.rds")
write_rds(m4_rq3, "paper/paper1/models/RQ3_m4.rds")

# Forest plot — M3 (obstacles model; best AIC)
forest_dat <- tidy(m3_rq3, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term = str_replace_all(term, term_recode),
    term = factor(term, levels = rev(unique(term))),
    sig  = p.value < 0.05
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
       title   = "RQ3: Predictors of relocation among intenders (M3)",
       caption = "Intenders only (VAR024 = 1 or 2). Reference: 1\u20132 years intention.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper1/figures/RQ3_forest.png", p_forest,
       width = 7, height = 6, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper1/\n")
