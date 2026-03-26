library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Forest plot for RQ3: Among intenders (VAR24_T1 %in% c(1, 2)), which factors
# are associated with not having relocated?
#
# Compares M1 (demographics + intention level) vs M4 (full model: + housing +
# obstacles) to show the independent contribution of each predictor block.

# ── Load data and subset to intenders ─────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

dat_m <- df |>
  filter(
    !is.na(intention_level),
    !is.na(relocated_f),
    !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(
      housing_suitability, home_satisfaction, neighbourhood_cohesion,
      any_obstacle, obs_financial, obs_supply, obs_energy,
      obs_own_health, obs_partner_health, obs_dependents, obs_bulky
    ))
  )

cat("Forest plot sample: n =", nrow(dat_m),
    "(relocated:", sum(dat_m$relocated_f == "Yes"), ")\n")

# ── Refit models on common complete cases ─────────────────────────────────────
m1_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh,
  data = dat_m, family = binomial
)

m4_rq3 <- glm(
  relocated_f ~ intention_level + age + sex + srh +
    housing_suitability + home_satisfaction + neighbourhood_cohesion +
    any_obstacle + obs_financial + obs_supply + obs_energy +
    obs_own_health + obs_partner_health + obs_dependents + obs_bulky,
  data = dat_m, family = binomial
)

# ── Term labels and ordering ───────────────────────────────────────────────────
term_labels <- c(
  "intention_level< 1 year"  = "Intention: < 1 year",
  "age"                      = "Age (per year)",
  "sexWoman"                 = "Sex: Woman",
  "srh"                      = "Self-rated health (1\u20135)",
  "housing_suitability"      = "Housing suitability",
  "home_satisfaction"        = "Home satisfaction",
  "neighbourhood_cohesion"   = "Neighborhood cohesion",
  "any_obstacle"             = "Any obstacle",
  "obs_financial"            = "Financial",
  "obs_supply"               = "Limited supply",
  "obs_energy"               = "No energy to move",
  "obs_own_health"           = "Own health",
  "obs_partner_health"       = "Partner health",
  "obs_dependents"           = "Dependents",
  "obs_bulky"                = "Bulky goods"
)

term_order <- c(
  "Intention: < 1 year",
  "Age (per year)", "Sex: Woman", "Self-rated health (1\u20135)",
  "Housing suitability", "Home satisfaction", "Neighborhood cohesion",
  "Any obstacle",
  "Financial", "Limited supply", "No energy to move",
  "Own health", "Partner health", "Dependents", "Bulky goods"
)

section_labels <- c(
  "Intention: < 1 year"          = "Moving intentions",
  "Age (per year)"               = "Demographics",
  "Sex: Woman"                   = "Demographics",
  "Self-rated health (1\u20135)" = "Demographics",
  "Housing suitability"   = "Housing factors",
  "Home satisfaction"     = "Housing factors",
  "Neighborhood cohesion" = "Housing factors",
  "Any obstacle"          = "Obstacles",
  "Financial"             = "Obstacles",
  "Limited supply"        = "Obstacles",
  "No energy to move"     = "Obstacles",
  "Own health"            = "Obstacles",
  "Partner health"        = "Obstacles",
  "Dependents"            = "Obstacles",
  "Bulky goods"           = "Obstacles"
)

# ── Extract and combine results ────────────────────────────────────────────────
results <- bind_rows(
  tidy(m1_rq3, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M1: Baseline"),
  tidy(m4_rq3, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M4: Full model")
) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term    = recode(term, !!!term_labels),
    term    = factor(term, levels = rev(term_order)),
    model   = factor(model, levels = c("M1: Baseline", "M4: Full model")),
    section = recode(as.character(term), !!!section_labels),
    section = factor(section, levels = c("Moving intentions", "Demographics",
                                         "Housing factors", "Obstacles"))
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(results, aes(x = estimate, y = term, colour = model, shape = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_linerange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.5),
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  scale_x_log10(
    breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20),
    labels = c("0.1", "0.25", "0.5", "1", "2", "5", "10", "20")
  ) +
  scale_colour_manual(values = c("M1: Baseline" = "#555555", "M4: Full model" = "#0072B2")) +
  scale_shape_manual(values  = c("M1: Baseline" = 16,        "M4: Full model" = 17)) +
  facet_grid(section ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    x       = "Odds ratio (log scale)",
    y       = NULL,
    colour  = NULL,
    shape   = NULL,
    title   = "Predictors of relocation among intenders",
    caption = paste(
      paste0("Subset: participants expecting to move within 2 years (n = ", nrow(dat_m), ")."),
      "Reference categories: intention 1\u20132 years, Man.",
      "Housing suitability and satisfaction scaled 1\u20135; neighborhood cohesion 1\u20133.",
      "Obstacle items coded 0/1; any obstacle = VAR30 (yes/no).",
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.placement    = "outside",
    strip.background   = element_rect(fill = "grey92", colour = NA),
    strip.text.y.left  = element_text(angle = 0, hjust = 1, face = "bold"),
    plot.caption       = element_text(hjust = 0, size = 9, colour = "grey40")
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("output/figures/RQ3_forest_plot.png", plot = p, width = 8, height = 8, dpi = 300)
ggsave("output/figures/RQ3_forest_plot.pdf", plot = p, width = 8, height = 8)
cat("Saved: output/figures/RQ3_forest_plot.png/.pdf\n")
p
