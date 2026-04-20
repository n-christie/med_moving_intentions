library(tidyverse)
library(mclust)

# ── Overview ──────────────────────────────────────────────────────────────────
# Latent Profile Analysis (LPA) of baseline person-environment characteristics.
#
# Theoretical frame: Lawton's person-environment fit model.
#
# Indicators (continuous; z-standardised before fitting):
#   intention_num, home_satisfaction, housing_suitability,
#   srh, age, neighbourhood_cohesion
#
# Auxiliary variables (not LPA indicators):
#   lives_alone, owner_num, relocated_f
#
# Outputs:
#   figures/LPA_profiles.png
#   tables/LPA_model_fit.csv
#   tables/LPA_profile_sizes.csv
#   tables/LPA_profile_means.csv
#   tables/LPA_relocation_by_profile.csv
#   tables/LPA_regression.csv
#   models/LPA_fit_selected.rds

# ── Load and prepare data ─────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")
df    <- panel |> filter(wave == "T1")

lpa_dat <- df |>
  filter(!is.na(relocated_f)) |>
  mutate(
    intention_num = 4 - haven::zap_labels(VAR024),
    lives_alone   = if_else(haven::zap_labels(VAR011_1) == 1, 1L, 0L),
    owner_num     = if_else(haven::zap_labels(VAR001_2) == 1, 1L, 0L)
  ) |>
  filter(
    complete.cases(pick(intention_num, home_satisfaction, housing_suitability,
                        srh, age, neighbourhood_cohesion, lives_alone, owner_num))
  )

cat("LPA sample n:", nrow(lpa_dat),
    "\nRelocated:", sum(lpa_dat$relocated_f == "Yes"), "\n\n")

indicators <- c("intention_num", "home_satisfaction", "housing_suitability",
                 "srh", "age", "neighbourhood_cohesion")

lpa_scaled <- lpa_dat |>
  mutate(across(all_of(indicators), \(x) as.numeric(scale(x))))

# ── Fit LPA models: k = 2 to 6 ───────────────────────────────────────────────
cat("Fitting LPA models (EEI, k = 2\u20136)...\n")
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
profile_sizes <- profiles |>
  count(Class) |>
  mutate(pct = round(n / sum(n) * 100, 1))
print(profile_sizes)

cat("\n=== Profile means (original scale) ===\n")
profile_means <- profiles |>
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
  arrange(desc(intention))
print(profile_means)

cat("\n=== Relocation rate by profile ===\n")
reloc_by_profile <- profiles |>
  group_by(Class) |>
  summarise(
    n           = n(),
    n_relocated = sum(relocated_f == "Yes"),
    pct         = round(n_relocated / n * 100, 1)
  ) |>
  arrange(desc(pct))
print(reloc_by_profile)

# ── Logistic regression: profile → relocation ─────────────────────────────────
ref_class <- profiles |>
  group_by(Class) |>
  summarise(sat = mean(home_satisfaction), reloc = mean(relocated_f == "Yes")) |>
  slice_max(sat, n = 1, with_ties = FALSE) |>
  pull(Class) |>
  as.character()

cat("Reference class (highest satisfaction):", ref_class, "\n")

cat("\n=== Logistic regression: profile → relocation ===\n")
m_lpa <- glm(
  relocated_f ~ relevel(Class, ref = ref_class),
  data = profiles, family = binomial
)

lpa_regression <- broom::tidy(m_lpa, conf.int = TRUE, exponentiate = TRUE) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term  = str_remove(term, "relevel\\(Class, ref = ref_class\\)"),
    across(c(estimate, conf.low, conf.high), \(x) round(x, 2)),
    p.value = round(p.value, 3)
  ) |>
  rename(profile = term)
print(lpa_regression)

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
  labs(x = NULL, y = "Mean (original scale)", colour = "Profile",
       title = "Latent profile means by class") +
  theme_bw(base_size = 11) +
  theme(axis.text.x      = element_text(angle = 35, hjust = 1),
        legend.position  = "none",
        panel.grid.minor = element_blank())

ggsave("paper/paper2/figures/LPA_profiles.png", p_lpa,
       width = max(8, best_k * 2), height = 5, dpi = 300)

# ── Save outputs ──────────────────────────────────────────────────────────────
write_csv(fit_summary,       "paper/paper2/tables/LPA_model_fit.csv")
write_csv(profile_sizes,     "paper/paper2/tables/LPA_profile_sizes.csv")
write_csv(profile_means,     "paper/paper2/tables/LPA_profile_means.csv")
write_csv(reloc_by_profile,  "paper/paper2/tables/LPA_relocation_by_profile.csv")
write_csv(lpa_regression,    "paper/paper2/tables/LPA_regression.csv")

write_rds(fit_selected, "paper/paper2/models/LPA_fit_selected.rds")

cat("\nSaved tables, model, and figure to paper/paper2/\n")
