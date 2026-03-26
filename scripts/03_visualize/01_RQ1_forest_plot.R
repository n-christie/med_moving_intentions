library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Forest plot of ORs from RQ1 logistic regression models:
#   M1: relocated ~ intention_timeframe (unadjusted)
#   M2: relocated ~ intention_timeframe + age + sex (adjusted)
# Source models fitted in scripts/02_analyze/01_RQ1_intentions.R

# ── Load data and refit models ────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

dat_m <- df |>
  filter(!is.na(relocated_f), !is.na(intention_timeframe), !is.na(age), !is.na(sex), !is.na(srh))

m1 <- glm(relocated_f ~ intention_timeframe,                   data = dat_m, family = binomial)
m2 <- glm(relocated_f ~ intention_timeframe + age + sex + srh, data = dat_m, family = binomial)

# ── Extract results ───────────────────────────────────────────────────────────
results <- bind_rows(
  tidy(m1, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Unadjusted"),
  tidy(m2, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Adjusted (age, sex, health)")
) |>
  filter(str_detect(term, "intention_timeframe")) |>
  mutate(
    term  = str_remove(term, "intention_timeframe"),
    term  = factor(term,  levels = c("1\u20132 years", "< 1 year")),
    model = factor(model, levels = c("Unadjusted", "Adjusted (age, sex, health)"))
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
    breaks = c(1, 2, 5, 10, 20, 30),
    labels = c("1", "2", "5", "10", "20", "30")
  ) +
  scale_colour_manual(values = c("Unadjusted" = "#555555", "Adjusted (age, sex, health)" = "#0072B2")) +
  scale_shape_manual(values  = c("Unadjusted" = 16,        "Adjusted (age, sex, health)" = 17)) +
  labs(
    x       = "Odds ratio (log scale)",
    y       = "Expected timeframe to move\n(Reference: 2+ years)",
    colour  = NULL,
    shape   = NULL,
    title   = "Moving intentions at baseline predicting relocation",
    caption = "Reference category: expecting to move in 2+ years.\nAdjusted model includes age, sex, and self-rated health."
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  "output/figures/RQ1_forest_plot.png",
  plot   = p,
  width  = 7,
  height = 4,
  dpi    = 300
)

ggsave(
  "output/figures/RQ1_forest_plot.pdf",
  plot   = p,
  width  = 7,
  height = 4
)

cat("Saved: output/figures/RQ1_forest_plot.png/.pdf\n")
p
