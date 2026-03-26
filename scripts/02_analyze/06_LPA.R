library(tidyverse)
library(mclust)

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
    intention_num = 4 - haven::zap_labels(VAR24_T1),
    lives_alone   = if_else(haven::zap_labels(VAR11_1_T1) == 1, 1L, 0L),
    owner_num     = if_else(haven::zap_labels(VAR01_2_T1) == 1, 1L, 0L)
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
  mutate(across(all_of(indicators), \(x) as.numeric(scale(x))))

# ── Fit LPA models: k = 2 to 6 (mclust EEI / VVI) ───────────────────────────
# EEI: equal variance, axis-aligned (simplest; equivalent to tidyLPA model 1)
# VVI: varying variance, axis-aligned (more flexible; equivalent to model 3)
# Both exclude covariances between indicators (axis-aligned).
cat("Fitting LPA models (EEI, k = 2–6)...\n")
X <- lpa_scaled |> select(all_of(indicators)) |> as.matrix()

fit_summary <- map_dfr(2:6, \(k) {
  eei <- Mclust(X, G = k, modelNames = "EEI", verbose = FALSE)
  vvi <- Mclust(X, G = k, modelNames = "VVI", verbose = FALSE)
  bind_rows(
    tibble(model = "EEI", k = k,
           BIC     = if (!is.null(eei)) round(eei$bic, 1)     else NA,
           loglik  = if (!is.null(eei)) round(eei$loglik, 1)  else NA,
           entropy = if (!is.null(eei)) {
             probs <- eei$z; round(1 + sum(probs * log(probs + 1e-10)) /
                                     (nrow(probs) * log(k)), 3) } else NA),
    tibble(model = "VVI", k = k,
           BIC     = if (!is.null(vvi)) round(vvi$bic, 1)     else NA,
           loglik  = if (!is.null(vvi)) round(vvi$loglik, 1)  else NA,
           entropy = if (!is.null(vvi)) {
             probs <- vvi$z; round(1 + sum(probs * log(probs + 1e-10)) /
                                     (nrow(probs) * log(k)), 3) } else NA)
  )
})

cat("\n=== Model fit comparison (higher BIC = better in mclust) ===\n")
print(fit_summary)

# ── Select best model ─────────────────────────────────────────────────────────
# Choose k and model with highest BIC and entropy > 0.70, min class n > 50.
# Update after reviewing the table above.
best_k     <- 5
best_mname <- "EEI"

cat("\n--- Fitting selected model: k =", best_k, best_mname, "---\n")
fit_selected <- Mclust(X, G = best_k, modelNames = best_mname, verbose = FALSE)
cat("BIC:", round(fit_selected$bic, 1), "\n")

# ── Extract profile assignments ───────────────────────────────────────────────
profiles <- lpa_dat |>
  mutate(Class = factor(fit_selected$classification)) |>
  select(Class, all_of(indicators), lives_alone, owner_num, relocated_f)

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
# Set reference to profile with highest satisfaction / lowest relocation rate
# after reviewing the means table. Update ref_class as needed.
ref_class <- profiles |>
  group_by(Class) |>
  summarise(sat = mean(home_satisfaction), reloc = mean(relocated_f == "Yes")) |>
  slice_max(sat, n = 1) |>
  pull(Class) |>
  as.character()

cat("Reference class (highest satisfaction):", ref_class, "\n")

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
