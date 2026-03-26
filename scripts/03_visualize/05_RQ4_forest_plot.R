library(tidyverse)
library(broom)

# ── Overview ──────────────────────────────────────────────────────────────────
# Forest plot for RQ4: Among non-intenders (VAR24_T1 = 3), which factors
# predict unexpected relocation?
#
# Compares M1 (demographics only) vs M3 (full: + housing + tenure).

df <- readRDS("data/processed/survey_analysis.rds")

dat_m <- df |>
  filter(!is.na(relocated_f), as.numeric(VAR24_T1) == 3) |>
  mutate(
    owner = factor(
      case_when(
        as.numeric(VAR01_2_T1) == 1 ~ "Owner",
        as.numeric(VAR01_2_T1) == 2 ~ "Renter",
        TRUE ~ NA_character_
      ),
      levels = c("Owner", "Renter")
    )
  ) |>
  filter(
    !is.na(age), !is.na(sex), !is.na(srh),
    complete.cases(pick(housing_suitability, home_satisfaction,
                        neighbourhood_cohesion)),
    !is.na(owner)
  )

cat("Forest plot sample: n =", nrow(dat_m),
    "(relocated:", sum(dat_m$relocated_f == "Yes"), ")\n")

# ── Refit models ──────────────────────────────────────────────────────────────
m1_rq4 <- glm(relocated_f ~ age + sex + srh,
               data = dat_m, family = binomial)

m3_rq4 <- glm(relocated_f ~ age + sex + srh +
                 housing_suitability + home_satisfaction +
                 neighbourhood_cohesion + owner,
               data = dat_m, family = binomial)

# ── Term labels and ordering ───────────────────────────────────────────────────
term_labels <- c(
  "age"                    = "Age (per year)",
  "sexWoman"               = "Sex: Woman",
  "srh"                    = "Self-rated health (1\u20135)",
  "housing_suitability"    = "Housing suitability",
  "home_satisfaction"      = "Home satisfaction",
  "neighbourhood_cohesion" = "Neighborhood cohesion",
  "ownerRenter"            = "Tenure: Renter"
)

term_order <- rev(c(
  "Age (per year)", "Sex: Woman", "Self-rated health (1\u20135)",
  "Housing suitability", "Home satisfaction", "Neighborhood cohesion",
  "Tenure: Renter"
))

section_labels <- c(
  "Age (per year)"               = "Demographics",
  "Sex: Woman"                   = "Demographics",
  "Self-rated health (1\u20135)" = "Demographics",
  "Housing suitability"          = "Housing factors",
  "Home satisfaction"            = "Housing factors",
  "Neighborhood cohesion"        = "Housing factors",
  "Tenure: Renter"               = "Housing tenure"
)

# ── Extract and combine results ────────────────────────────────────────────────
results <- bind_rows(
  tidy(m1_rq4, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M1: Demographics"),
  tidy(m3_rq4, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "M3: Full model")
) |>
  filter(!str_detect(term, "Intercept")) |>
  mutate(
    term    = recode(term, !!!term_labels),
    term    = factor(term, levels = term_order),
    model   = factor(model, levels = c("M1: Demographics", "M3: Full model")),
    section = recode(as.character(term), !!!section_labels),
    section = factor(section, levels = c("Demographics", "Housing factors",
                                         "Housing tenure"))
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(results, aes(x = estimate, y = term,
                          colour = model, shape = model)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_linerange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.5),
    linewidth = 0.7
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4),
    labels = c("0.25", "0.5", "1", "2", "4")
  ) +
  scale_colour_manual(
    values = c("M1: Demographics" = "#555555", "M3: Full model" = "#0072B2")
  ) +
  scale_shape_manual(
    values = c("M1: Demographics" = 16, "M3: Full model" = 17)
  ) +
  facet_grid(section ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    x       = "Odds ratio (log scale)",
    y       = NULL,
    colour  = NULL,
    shape   = NULL,
    title   = "Predictors of unexpected relocation among non-intenders",
    caption = paste(
      paste0("Subset: participants expecting to move in 2+ years at baseline (n = ",
             nrow(dat_m), ")."),
      "Reference categories: Man, Owner.",
      "Housing suitability and satisfaction scaled 1\u20135; neighborhood cohesion 1\u20133.",
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

ggsave("output/figures/RQ4_forest_plot.png", p, width = 7, height = 6, dpi = 300)
ggsave("output/figures/RQ4_forest_plot.pdf", p, width = 7, height = 6)
file.copy("output/figures/RQ4_forest_plot.png",
          "docs/figures/RQ4_forest_plot.png", overwrite = TRUE)
cat("Saved: RQ4_forest_plot.png/.pdf\n")
p
