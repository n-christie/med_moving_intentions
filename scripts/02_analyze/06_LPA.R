library(tidyverse)
library(tidyLPA)

# ── Overview ──────────────────────────────────────────────────────────────────
# Latent Profile Analysis (LPA) of baseline person-environment characteristics.
#
# Theoretical frame: Lawton's person-environment fit model. Profiles are
# defined by the combination of moving intentions and housing environment
# factors. The hypothesis is that person-environment mismatch (low
# satisfaction, low suitability regardless of intention) constitutes a
# distinct profile associated with unexpected/reactive relocation.
#
# Indicators (continuous; z-standardised before fitting):
#   intention_num          — urgency of moving intention (3=urgent, 1=low)
#   home_satisfaction      — satisfaction with current home (1–5)
#   housing_suitability    — perceived physical suitability (1–5)
#   srh                    — self-rated health (1–5)
#   age                    — age in years
#   neighbourhood_cohesion — neighbourhood social cohesion (1–3)
#
# Auxiliary variables (examined after profile assignment, not LPA indicators):
#   lives_alone  — single-person household (binary)
#   owner_num    — homeowner (binary; 0 = renter)
#   relocated_f  — actual relocation outcome
#
# Binary variables are excluded from the Gaussian mixture model because
# Bernoulli indicators require a different model class. Their distributions
# across profiles are examined descriptively and as covariates.
#
# Model selection: BIC (lower = better) and entropy (higher = better, max 1).
# Fit models with k = 2 to 6 profiles.
#
# Variable derivations: see scripts/01_clean/02_recode.R

# ── Load and prepare data ─────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

lpa_dat <- df |>
  filter(!is.na(relocated_f)) |>
  mutate(
    # Higher score = more urgent intention
    intention_num = 4 - as.numeric(VAR24_T1),
    lives_alone   = if_else(as.numeric(VAR11_1_T1) == 1, 1L, 0L),
    owner_num     = if_else(as.numeric(VAR01_2_T1) == 1, 1L, 0L)
  ) |>
  filter(
    complete.cases(pick(intention_num, home_satisfaction, housing_suitability,
                        srh, age, neighbourhood_cohesion, lives_alone, owner_num))
  )

cat("LPA sample n:", nrow(lpa_dat),
    "\nRelocated:", sum(lpa_dat$relocated_f == "Yes"), "\n\n")

# Standardise indicators for comparability across scales
indicators <- c("intention_num", "home_satisfaction", "housing_suitability",
                 "srh", "age", "neighbourhood_cohesion")

lpa_scaled <- lpa_dat |>
  mutate(across(all_of(indicators), scale))

# ── Fit LPA models: k = 2 to 6 ───────────────────────────────────────────────
# Model 1: equal variances, zero covariances (most parsimonious)
# Model 3: varying variances, zero covariances (allows profiles to differ
#           in spread, more flexible)
cat("Fitting LPA models...\n")

models_m1 <- lpa_scaled |>
  select(all_of(indicators)) |>
  estimate_profiles(2:6, models = 1)

models_m3 <- lpa_scaled |>
  select(all_of(indicators)) |>
  estimate_profiles(2:6, models = 3)

# ── Model fit comparison ───────────────────────────────────────────────────────
cat("\n=== Model fit: equal variances (Model 1) ===\n")
get_fit(models_m1) |>
  select(Model, Classes, AIC, BIC, Entropy, n_min, n_max) |>
  mutate(across(c(AIC, BIC), \(x) round(x, 1)),
         Entropy = round(Entropy, 3)) |>
  print()

cat("\n=== Model fit: varying variances (Model 3) ===\n")
get_fit(models_m3) |>
  select(Model, Classes, AIC, BIC, Entropy, n_min, n_max) |>
  mutate(across(c(AIC, BIC), \(x) round(x, 1)),
         Entropy = round(Entropy, 3)) |>
  print()

# ── Select best model ─────────────────────────────────────────────────────────
# Update k and model type here based on fit statistics above.
# Rule of thumb: lowest BIC with entropy > 0.80 and smallest class n > 50.
best_k     <- 5    # update after reviewing fit table
best_model <- 1    # update if Model 3 fits substantially better

cat("\n--- Fitting selected model: k =", best_k,
    ", model type =", best_model, "---\n")

fit_selected <- lpa_scaled |>
  select(all_of(indicators)) |>
  estimate_profiles(best_k, models = best_model)

# ── Extract profile assignments ───────────────────────────────────────────────
profiles <- get_data(fit_selected) |>
  select(Class) |>
  bind_cols(lpa_dat |> select(
    all_of(indicators), lives_alone, owner_num, relocated_f, age
  )) |>
  mutate(Class = factor(Class))

cat("\n=== Profile sizes ===\n")
profiles |> count(Class) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |> print()

# ── Profile means (unstandardised) ────────────────────────────────────────────
cat("\n=== Profile means (original scale) ===\n")
profiles |>
  group_by(Class) |>
  summarise(
    n               = n(),
    intention       = round(mean(intention_num), 2),
    satisfaction    = round(mean(home_satisfaction), 2),
    suitability     = round(mean(housing_suitability), 2),
    health          = round(mean(srh), 2),
    age             = round(mean(age), 1),
    cohesion        = round(mean(neighbourhood_cohesion), 2),
    pct_alone       = round(mean(lives_alone) * 100, 1),
    pct_owner       = round(mean(owner_num) * 100, 1),
    pct_relocated   = round(mean(relocated_f == "Yes") * 100, 1)
  ) |>
  arrange(desc(intention)) |>
  print()

# ── Relocation rates by profile ────────────────────────────────────────────────
cat("\n=== Relocation rate by profile ===\n")
profiles |>
  group_by(Class) |>
  summarise(
    n           = n(),
    n_relocated = sum(relocated_f == "Yes"),
    pct         = round(n_relocated / n * 100, 1)
  ) |>
  arrange(desc(pct)) |>
  print()

# ── Logistic regression: profile membership predicting relocation ─────────────
# Use profile with highest satisfaction / lowest relocation rate as reference.
# Update ref_class after reviewing the profile means table above.
ref_class <- levels(profiles$Class)[1]   # update as needed

cat("\n=== Logistic regression: profile → relocation ===\n")
m_lpa <- glm(
  relocated_f ~ relevel(Class, ref = ref_class),
  data = profiles, family = binomial
)

broom::tidy(m_lpa, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term  = str_remove(term, "relevel\\(Class, ref = ref_class\\)"),
    across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
    p.value = round(p.value, 3)
  ) |>
  rename(profile = term) |>
  print()

# ── Profile plot ──────────────────────────────────────────────────────────────
plot_dat <- profiles |>
  group_by(Class) |>
  summarise(
    n            = n(),
    pct_reloc    = round(mean(relocated_f == "Yes") * 100, 1),
    across(all_of(indicators), mean)
  ) |>
  pivot_longer(all_of(indicators), names_to = "indicator", values_to = "mean") |>
  mutate(
    indicator = recode(indicator,
      "intention_num"          = "Intention urgency",
      "home_satisfaction"      = "Home satisfaction",
      "housing_suitability"    = "Housing suitability",
      "srh"                    = "Self-rated health",
      "age"                    = "Age",
      "neighbourhood_cohesion" = "Neighbourhood cohesion"
    ),
    label = paste0("Profile ", Class, "\nn = ", n,
                   " (", pct_reloc, "% relocated)")
  )

p_lpa <- ggplot(plot_dat,
       aes(x = indicator, y = mean, colour = Class, group = Class)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ label, nrow = 1) +
  labs(
    x      = NULL,
    y      = "Mean (original scale)",
    colour = "Profile",
    title  = "Latent profile means by class"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 35, hjust = 1),
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )

ggsave("output/figures/LPA_profiles.png", p_lpa,
       width = max(8, best_k * 2), height = 5, dpi = 300)
file.copy("output/figures/LPA_profiles.png",
          "docs/figures/LPA_profiles.png", overwrite = TRUE)
cat("\nSaved: output/figures/LPA_profiles.png\n")
p_lpa
