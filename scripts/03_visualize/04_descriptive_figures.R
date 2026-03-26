library(tidyverse)

# ── Overview ──────────────────────────────────────────────────────────────────
# Descriptive figures for the about page and manuscript
#   Fig 1: Relocation rate by intention timeframe (RQ1 context)
#   Fig 2: Relocation rate by age group and sex (sample context)

df <- readRDS("data/processed/survey_analysis.rds")
dat <- df |> filter(!is.na(relocated_f), !is.na(intention_timeframe))

# ── Figure 1: Relocation rate by intention timeframe ──────────────────────────
rate_by_intention <- dat |>
  group_by(intention_timeframe) |>
  summarise(
    n         = n(),
    relocated = sum(relocated_f == "Yes"),
    pct       = relocated / n * 100,
    .groups   = "drop"
  )

p1 <- ggplot(rate_by_intention,
             aes(x = intention_timeframe, y = pct)) +
  geom_col(fill = "#0072B2", width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%\n(n = ", n, ")")),
    vjust = -0.4, size = 3.5
  ) +
  scale_y_continuous(limits = c(0, 85), labels = \(x) paste0(x, "%")) +
  labs(
    x     = "Expected timeframe to move (baseline)",
    y     = "Relocated within 3 years (%)",
    title = "Relocation rates by baseline moving intention"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

ggsave("docs/figures/fig_relocation_by_intention.png",
       plot = p1, width = 7, height = 5, dpi = 300)
cat("Saved: fig_relocation_by_intention.png\n")

# ── Figure 2: Relocation rate by age group ────────────────────────────────────
rate_by_age <- dat |>
  mutate(age_group = cut(age,
                         breaks = c(54, 60, 65, 70, 75, 80, Inf),
                         labels = c("55–60", "61–65", "66–70",
                                    "71–75", "76–80", "81+"),
                         right  = TRUE)) |>
  filter(!is.na(age_group)) |>
  group_by(age_group) |>
  summarise(
    n         = n(),
    relocated = sum(relocated_f == "Yes"),
    pct       = relocated / n * 100,
    .groups   = "drop"
  )

p2 <- ggplot(rate_by_age, aes(x = age_group, y = pct)) +
  geom_col(fill = "#E69F00", width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%\n(n = ", n, ")")),
    vjust = -0.4, size = 3.5
  ) +
  scale_y_continuous(limits = c(0, 35), labels = \(x) paste0(x, "%")) +
  labs(
    x     = "Age group at baseline",
    y     = "Relocated within 3 years (%)",
    title = "Relocation rates by age group"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

ggsave("docs/figures/fig_relocation_by_age.png",
       plot = p2, width = 7, height = 5, dpi = 300)
cat("Saved: fig_relocation_by_age.png\n")

p1
p2
