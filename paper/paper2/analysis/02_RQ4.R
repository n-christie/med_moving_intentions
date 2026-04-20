library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# RQ4: Among those who did not intend to move at baseline (VAR024 == 3),
#      which factors predict unexpected relocation?
#
# Outcome:  relocated_f (factor, No/Yes)
# Subset:   VAR024 == 3 — non-intenders (n = 1,286; 10.4% relocated)
#
# Models:
#   M1 — age + sex + srh
#   M2 — M1 + housing_suitability + home_satisfaction + neighbourhood_cohesion
#   M3 — M2 + housing tenure (owner vs renter)
#
# Outputs:
#   figures/RQ4_forest.png
#   tables/RQ4_nonintender_sample.csv
#   tables/RQ4_predictors_by_relocation.csv
#   tables/RQ4_tenure_by_relocation.csv
#   tables/RQ4_all_coefficients.csv
#   tables/RQ4_model_fit.csv
#   tables/RQ4_lrt.csv
#   models/RQ4_m0.rds … RQ4_m3.rds

# ── Load data and subset ──────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

dat <- df |>
  filter(!is.na(relocated_f), as.numeric(VAR024) == 3) |>
  mutate(
    owner = factor(
      case_when(
        as.numeric(VAR001_2) == 1 ~ "Owner",
        as.numeric(VAR001_2) == 2 ~ "Renter",
        TRUE ~ NA_character_
      ),
      levels = c("Owner", "Renter")
    )
  )

cat("Non-intender sample:\n")
cat("  Total n:        ", nrow(dat), "\n")
cat("  Relocated (Yes):", sum(dat$relocated_f == "Yes"), "\n")
cat("  Relocated (%):  ", round(mean(dat$relocated_f == "Yes") * 100, 1), "\n\n")

# ── Analysis sample ───────────────────────────────────────────────────────────
dat_m <- dat |>
  filter(
    !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(housing_suitability, home_satisfaction,
                        neighbourhood_cohesion)),
    !is.na(owner)
  )

cat("Complete-case model sample n:", nrow(dat_m),
    "(relocated:", sum(dat_m$relocated_f == "Yes"), ")\n\n")

# ── Descriptives by relocation status ────────────────────────────────────────
cat("=== Continuous predictors by relocation status ===\n")
pred_by_reloc <- dat_m |>
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
              values_from = c(mean, sd))
print(pred_by_reloc)

cat("\n=== Tenure by relocation status ===\n")
tenure_by_reloc <- dat_m |>
  count(owner, relocated_f) |>
  group_by(owner) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(tenure_by_reloc)

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
model_fit <- tibble(
  model = c("M0 (null)", "M1 (demographics)", "M2 (+housing)", "M3 (+tenure)"),
  AIC   = round(c(AIC(m0), AIC(m1), AIC(m2), AIC(m3)), 1),
  nagelkerke_r2 = c(NA, nagelkerke(m1, m0), nagelkerke(m2, m0),
                    nagelkerke(m3, m0))
) |>
  mutate(delta_aic = round(AIC - AIC[1], 1))
print(model_fit)

cat("\n=== Likelihood ratio tests vs M1 ===\n")
lrt <- list(M2 = m2, M3 = m3) |>
  imap_dfr(\(mod, nm) {
    l <- anova(m1, mod, test = "LRT")
    tibble(model = nm, delta_df = l$Df[2],
           delta_dev = round(l$Deviance[2], 2),
           p_value   = round(l$`Pr(>Chi)`[2], 3))
  })
print(lrt)

# ── Save outputs ──────────────────────────────────────────────────────────────
nonintender_sample <- tibble(
  n_total    = nrow(dat),
  n_relocated = sum(dat$relocated_f == "Yes"),
  pct_relocated = round(mean(dat$relocated_f == "Yes") * 100, 1),
  n_model    = nrow(dat_m),
  n_model_relocated = sum(dat_m$relocated_f == "Yes")
)

all_coef <- list(M1 = m1, M2 = m2, M3 = m3) |>
  imap_dfr(\(mod, nm) {
    tidy(mod, conf.int = TRUE, exponentiate = TRUE) |>
      filter(!str_detect(term, "Intercept")) |>
      mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
             p.value = round(p.value, 4), model = nm)
  })

write_csv(nonintender_sample, "paper/paper2/tables/RQ4_nonintender_sample.csv")
write_csv(pred_by_reloc,      "paper/paper2/tables/RQ4_predictors_by_relocation.csv")
write_csv(tenure_by_reloc,    "paper/paper2/tables/RQ4_tenure_by_relocation.csv")
write_csv(all_coef,           "paper/paper2/tables/RQ4_all_coefficients.csv")
write_csv(model_fit,          "paper/paper2/tables/RQ4_model_fit.csv")
write_csv(lrt,                "paper/paper2/tables/RQ4_lrt.csv")

write_rds(m0, "paper/paper2/models/RQ4_m0.rds")
write_rds(m1, "paper/paper2/models/RQ4_m1.rds")
write_rds(m2, "paper/paper2/models/RQ4_m2.rds")
write_rds(m3, "paper/paper2/models/RQ4_m3.rds")

# Forest plot — M3 (full model)
forest_dat <- tidy(m3, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term = recode(term,
      "age"                    = "Age (per year)",
      "sexWoman"               = "Sex: Woman",
      "srh"                    = "Self-rated health",
      "housing_suitability"    = "Housing suitability",
      "home_satisfaction"      = "Home satisfaction",
      "neighbourhood_cohesion" = "Neighbourhood cohesion",
      "ownerRenter"            = "Tenure: Renter"
    ),
    term = factor(term, levels = rev(c(
      "Age (per year)", "Sex: Woman", "Self-rated health",
      "Housing suitability", "Home satisfaction",
      "Neighbourhood cohesion", "Tenure: Renter"
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
       title   = "RQ4: Predictors of unexpected relocation among non-intenders (M3)",
       caption = "Non-intenders only (VAR024 = 3). Reference: Owner-occupier.") +
  theme_bw(base_size = 12) +
  theme(legend.position    = "bottom",
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("paper/paper2/figures/RQ4_forest.png", p_forest,
       width = 7, height = 5, dpi = 300)

cat("\nSaved tables, models, and figures to paper/paper2/\n")
