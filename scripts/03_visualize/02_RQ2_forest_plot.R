library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Forest plot for RQ2: comparing M2 (baseline: intentions + demographics) and
# M6 (full housing model) to show the independent contribution of housing factors.

# ── Load data and refit models ────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

dat_m <- df |>
  filter(
    !is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(housing_suitability, home_satisfaction, neighbourhood_cohesion))
  )

m2 <- glm(relocated_f ~ intention_timeframe + age + sex + srh,
          data = dat_m, family = binomial)

m6 <- glm(relocated_f ~ intention_timeframe + age + sex + srh +
            housing_suitability + home_satisfaction + neighbourhood_cohesion,
          data = dat_m, family = binomial)

# ── Extract and label results ─────────────────────────────────────────────────
term_labels <- c(
  "intention_timeframe1\u20132 years" = "Intention: 1\u20132 years",
  "intention_timeframe< 1 year"       = "Intention: < 1 year",
  "age"                               = "Age (per year)",
  "sexWoman"                          = "Sex: Woman",
  "srh"                               = "Self-rated health (1\u20135)",
  "housing_suitability"               = "Housing suitability",
  "home_satisfaction"                 = "Home satisfaction",
  "neighbourhood_cohesion"            = "Neighbourhood cohesion"
)

term_order <- c(
  "Neighbourhood cohesion", "Home satisfaction", "Housing suitability",
  "Self-rated health (1\u20135)", "Sex: Woman", "Age (per year)",
  "Intention: 1\u20132 years", "Intention: < 1 year"
)

# Section dividers for the plot
section_labels <- c(
  "Intention: 1\u20132 years"    = "Moving intentions",
  "Intention: < 1 year"          = "Moving intentions",
  "Age (per year)"               = "Demographics",
  "Sex: Woman"                   = "Demographics",
  "Self-rated health (1\u20135)" = "Demographics",
  "Housing suitability"          = "Housing factors",
  "Home satisfaction"            = "Housing factors",
  "Neighbourhood cohesion"       = "Housing factors"
)

results <- bind_rows(
  tidy(m2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M2: Baseline"),
  tidy(m6, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M6: + Housing factors")
) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term  = recode(term, !!!term_labels),
    term  = factor(term, levels = term_order),
    model = factor(model, levels = c("M2: Baseline", "M6: + Housing factors")),
    section = recode(as.character(term), !!!section_labels)
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
    breaks = c(0.5, 1, 2, 5, 10, 20, 30),
    labels = c("0.5", "1", "2", "5", "10", "20", "30")
  ) +
  scale_colour_manual(values = c("M2: Baseline" = "#555555", "M6: + Housing factors" = "#0072B2")) +
  scale_shape_manual(values  = c("M2: Baseline" = 16,        "M6: + Housing factors" = 17)) +
  # Facet by predictor group
  facet_grid(section ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    x       = "Odds ratio (log scale)",
    y       = NULL,
    colour  = NULL,
    shape   = NULL,
    title   = "Predictors of relocation: intentions and housing factors",
    caption = paste(
      "Reference categories: intention 2+ years, Man.",
      "Housing suitability and satisfaction scaled 1\u20135; neighbourhood cohesion 1\u20133.",
      paste0("n = ", nrow(dat_m), ". All models adjust for age, sex, and self-rated health."),
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position      = "bottom",
    panel.grid.minor     = element_blank(),
    panel.grid.major.y   = element_blank(),
    strip.placement      = "outside",
    strip.background     = element_rect(fill = "grey92", colour = NA),
    strip.text.y.left    = element_text(angle = 0, hjust = 1, face = "bold"),
    plot.caption         = element_text(hjust = 0, size = 9, colour = "grey40")
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("output/figures/RQ2_forest_plot.png", plot = p, width = 8, height = 6, dpi = 300)
ggsave("output/figures/RQ2_forest_plot.pdf", plot = p, width = 8, height = 6)
cat("Saved: output/figures/RQ2_forest_plot.png/.pdf\n")
p