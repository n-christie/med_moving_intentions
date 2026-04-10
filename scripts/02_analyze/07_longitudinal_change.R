library(tidyverse)

# ── Load data ─────────────────────────────────────────────────────────────────
survey_analysis <- readRDS("data/processed/survey_analysis.rds")
dict            <- readRDS("data/codebooks/prosp_dict_en.rds")

# ── 1. Build column index with normalized base names ──────────────────────────
# T1 uses 2-digit VAR numbers (e.g. VAR24_T1); T2/T3 use 3-digit (VAR024_T2).
# Pad T1 2-digit numbers to 3-digit so names align across waves.

cols_index <- tibble(col = names(survey_analysis)) |>
  mutate(
    timepoint = str_extract(col, "T[123]$"),
    base      = str_remove(col, "_T[123]$"),
    base_norm = str_replace(base, "^VAR(\\d{2})(_.+|$)", "VAR0\\1\\2")
  ) |>
  filter(!is.na(timepoint))

# Variables appearing at 2+ time points
repeated_bases <- cols_index |>
  group_by(base_norm) |>
  filter(n_distinct(timepoint) > 1) |>
  ungroup()

# ── 2. Compute per-wave means for repeated numeric variables ──────────────────
numeric_cols <- survey_analysis |> select(where(is.numeric)) |> names()

col_lookup <- repeated_bases |>
  filter(col %in% numeric_cols) |>
  select(base_norm, timepoint, col)

wave_stats <- col_lookup |>
  group_by(base_norm) |>
  group_modify(~{
    map_dfr(seq_len(nrow(.x)), function(i) {
      vals <- survey_analysis[[.x$col[i]]]
      tibble(
        timepoint = .x$timepoint[i],
        mean      = mean(vals, na.rm = TRUE),
        sd        = sd(vals, na.rm = TRUE),
        min_val   = min(vals, na.rm = TRUE),
        max_val   = max(vals, na.rm = TRUE),
        n         = sum(!is.na(vals))
      )
    })
  }) |>
  ungroup()

# ── 3. Filter to variables present at all 3 waves with consistent scales ──────
# Scale consistency check: ratio of largest to smallest observed range < 3.
# Catches variables that were recoded differently across waves.

change_wide <- wave_stats |>
  pivot_wider(names_from = timepoint,
              values_from = c(mean, sd, min_val, max_val, n)) |>
  filter(!is.na(mean_T1), !is.na(mean_T2), !is.na(mean_T3)) |>
  mutate(
    range_T1    = max_val_T1 - min_val_T1,
    range_T2    = max_val_T2 - min_val_T2,
    range_T3    = max_val_T3 - min_val_T3,
    scale_ratio = pmax(range_T1, range_T2, range_T3) /
                  pmin(range_T1, range_T2, range_T3),
    # Standardized change (Cohen's d relative to T1 SD)
    d_t1_t2 = (mean_T2 - mean_T1) / sd_T1,
    d_t1_t3 = (mean_T3 - mean_T1) / sd_T1
  ) |>
  filter(scale_ratio < 3, !is.na(scale_ratio))

# ── 4. Verify same question across waves using codebook labels ────────────────
# Note: T1 2-digit VAR numbers don't always correspond to the same question as
# T2/T3 3-digit numbers. Three offset blocks were identified:
#   Block A (VAR002-011): T2 = T1 + 1  — one question inserted before this block
#   Block B (VAR013-031): T2 = T1      — key study variables, correctly aligned
#   Block C (VAR033-045): T2 = T1 - 1  — VAR32_T1 dropped from T2 onward
# ADL/mobility items (VAR048-057) moved to end of T2/T3 survey.

t1_labels <- dict |>
  filter(str_detect(var_name, "_T1$")) |>
  mutate(base_norm = str_replace(str_remove(var_name, "_T1$"),
                                 "^VAR(\\d{2})(_.+|$)", "VAR0\\1\\2")) |>
  select(base_norm, label_t1 = var_label_en)

t2_labels <- dict |>
  filter(str_detect(var_name, "_T2$")) |>
  mutate(base_norm = str_remove(var_name, "_T2$")) |>
  select(base_norm, label_t2 = var_label_en)

label_compare <- t1_labels |>
  inner_join(t2_labels, by = "base_norm") |>
  filter(!is.na(label_t1), !is.na(label_t2)) |>
  mutate(
    same = str_sub(str_to_lower(label_t1), 1, 30) ==
           str_sub(str_to_lower(label_t2), 1, 30)
  )

verified_same <- label_compare |> filter(same) |> pull(base_norm)

# Keep verified VAR items + all non-VAR constructs (composites, scales)
analysis_vars <- change_wide |>
  filter(
    base_norm %in% verified_same | !str_detect(base_norm, "^VAR"),
    base_norm != "Age",
    # Remove residual scale artifacts: cap effect size at ±1.5 SD
    abs(d_t1_t3) < 1.5,
    abs(d_t1_t2) < 1.5,
    abs(d_t1_t3) > 0.05   # exclude near-zero change
  ) |>
  mutate(
    domain = case_when(
      str_detect(base_norm, "^MOH|^moh_") ~ "Meaning of Home (MOH)",
      str_detect(base_norm, "^HCQ")        ~ "Health/Coping (HCQ)",
      str_detect(base_norm, "mojlighet|mangd|formaga|vilja") ~ "Activity & Participation",
      str_detect(base_norm, "^ls_|^totalscore|^opportunity") ~ "Life Satisfaction",
      str_detect(base_norm, "^VAR")        ~ "Survey items (VAR)",
      TRUE                                 ~ "Other"
    )
  )

cat("Variables included in longitudinal change analysis:", nrow(analysis_vars), "\n")
analysis_vars |> count(domain, sort = TRUE) |> print()

# ── 5. Inspect: ranked table of all variables by |T1→T3 change| ───────────────
# Join T1 labels for readability
change_table <- analysis_vars |>
  left_join(t1_labels |> distinct(base_norm, .keep_all = TRUE), by = "base_norm") |>
  arrange(desc(abs(d_t1_t3))) |>
  select(domain, base_norm, label_t1, mean_T1, mean_T2, mean_T3, d_t1_t2, d_t1_t3)

print(change_table, n = 40)

# ── 6. Figure: slope plot by domain ──────────────────────────────────────────
top5_per_domain <- analysis_vars |>
  group_by(domain) |>
  slice_max(abs(d_t1_t3), n = 5) |>
  ungroup()

slope_df <- top5_per_domain |>
  select(base_norm, domain, d_t1_t3, mean_T1, mean_T2, mean_T3, sd_T1) |>
  mutate(
    z_T1 = 0,
    z_T2 = (mean_T2 - mean_T1) / sd_T1,
    z_T3 = (mean_T3 - mean_T1) / sd_T1
  ) |>
  pivot_longer(cols = c(z_T1, z_T2, z_T3),
               names_to = "wave", values_to = "z",
               names_prefix = "z_") |>
  mutate(
    wave_label = factor(wave, levels = c("T1", "T2", "T3"),
                        labels = c("T1\nBaseline", "T2\nYear 1", "T3\nYear 3"))
  )

p_change <- ggplot(slope_df,
                   aes(x = wave_label, y = z, group = base_norm, color = d_t1_t3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 2.2) +
  scale_color_gradient2(
    low = "#d6604d", mid = "grey75", high = "#4393c3",
    midpoint = 0, limits = c(-1.1, 1.1),
    name = "T1\u2192T3\nchange (d)"
  ) +
  facet_wrap(~domain, ncol = 3) +
  labs(
    title = "Longitudinal change in repeated measures (T1 to T3)",
    subtitle = "Top 5 variables per domain; y-axis = SD units from T1 mean",
    x = NULL, y = "Standardized change from T1 (d)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p_change)

ggsave("output/figures/longitudinal_change_verified.png",
       plot = p_change, width = 9, height = 6.5, dpi = 300)

cat("Saved: output/figures/longitudinal_change_verified.png\n")
