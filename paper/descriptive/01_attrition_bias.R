# =============================================================================
# 01_attrition_bias.R
# Attrition bias analysis: compare T3 responders vs. non-responders on survey
# and register variables at T1 baseline.
#
# Input:  data/processed/panel_merged.rds
# Output: paper/descriptive/tables/attrition_bias_table.csv
#         paper/descriptive/tables/attrition_bias_logistic.csv
# =============================================================================

library(tidyverse)
library(gtsummary)

# ── Load data ─────────────────────────────────────────────────────────────────
panel <- readRDS("data/processed/panel_merged.rds")

# ── Recode register variables ─────────────────────────────────────────────────
panel <- panel |> mutate(
  civil_f = case_when(
    civil == "G"  ~ "Married",
    civil == "S"  ~ "Cohabiting",
    civil == "OG" ~ "Single",
    civil == "Ä"  ~ "Widowed",
    !is.na(civil) ~ "Other",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Married", "Cohabiting", "Single", "Widowed", "Other")),

  syss_f = case_when(
    syss_stat19 == 1 ~ "Employed",
    syss_stat19 == 6 ~ "Retired",
    !is.na(syss_stat19) ~ "Other",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Employed", "Retired", "Other"))
)

# ── Identify T3 responders ────────────────────────────────────────────────────
t3_responders <- panel |>
  filter(wave == "T3", !is.na(Date)) |>
  pull(LopNr_PersonNr)

cat("T3 responders:", length(t3_responders), "\n")

# ── T1 baseline with attrition flag ──────────────────────────────────────────
df <- panel |>
  filter(wave == "T1") |>
  mutate(t3_responded = factor(
    if_else(LopNr_PersonNr %in% t3_responders, "Responded", "Non-responder"),
    levels = c("Non-responder", "Responded")
  ))

cat("T1 rows:", nrow(df), "\n")
cat("T3 responders at T1:", sum(df$t3_responded == "Responded"), "\n")
cat("T3 non-responders at T1:", sum(df$t3_responded == "Non-responder"), "\n")

# ── Build comparison table ────────────────────────────────────────────────────
tbl_attr <- tbl_summary(
  df,
  by = t3_responded,
  include = c(age, sex, srh, intention_timeframe, housing_suitability,
              disp_ink_ke, civil_f, syss_f),
  label = list(
    age                 ~ "Age (years)",
    sex                 ~ "Sex",
    srh                 ~ "Self-rated health (1–5)",
    intention_timeframe ~ "Expected timeframe to move",
    housing_suitability ~ "Housing suitability (1–5)",
    disp_ink_ke         ~ "Disposable household income (SEK/month)",
    civil_f             ~ "Marital status (register)",
    syss_f              ~ "Employment status (register)"
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1)),
  missing = "ifany",
  missing_text = "Missing"
) |>
  add_overall(last = FALSE) |>
  add_p(
    test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
    pvalue_fun = \(x) style_pvalue(x, digits = 3)
  ) |>
  bold_labels() |>
  bold_p(t = 0.05)

# ── Save as CSV ───────────────────────────────────────────────────────────────
tbl_attr |>
  as_tibble() |>
  write_csv("paper/descriptive/tables/attrition_bias_table.csv")

cat("Saved: paper/descriptive/tables/attrition_bias_table.csv\n")

# ── Logistic model: T3 response ~ register variables ─────────────────────────
dat_logistic <- df |>
  filter(!is.na(disp_ink_ke), !is.na(civil_f), !is.na(syss_f),
         !is.na(age), !is.na(sex), !is.na(srh))

m_attr <- glm(
  t3_responded ~ age + sex + srh + disp_ink_ke + civil_f + syss_f,
  data    = dat_logistic |> mutate(t3_responded = as.integer(t3_responded == "Responded")),
  family  = binomial
)

cat("\nLogistic model: T3 response ~ register variables\n")
print(broom::tidy(m_attr, exponentiate = TRUE, conf.int = TRUE))

broom::tidy(m_attr, exponentiate = TRUE, conf.int = TRUE) |>
  write_csv("paper/descriptive/tables/attrition_bias_logistic.csv")

cat("Saved: paper/descriptive/tables/attrition_bias_logistic.csv\n")
