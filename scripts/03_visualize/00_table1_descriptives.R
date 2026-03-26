library(tidyverse)
library(labelled)
library(gtsummary)
library(flextable)

# ── Overview ──────────────────────────────────────────────────────────────────
# Table 1: Baseline characteristics of the analysis sample stratified by
# relocation status (not relocated / relocated).
#
# Continuous variables: mean (SD), compared with t-test
# Categorical variables: n (%), compared with chi-squared test
#
# Output:
#   output/tables/table1_descriptives.docx  (Word, for manuscript)
#   output/tables/table1_descriptives.html  (HTML, for review)

# ── Load data ─────────────────────────────────────────────────────────────────
df <- readRDS("data/processed/survey_analysis.rds")

# ── Prepare table variables ───────────────────────────────────────────────────
tab_dat <- df |>
  filter(!is.na(relocated_f)) |>
  mutate(
    # Collapse sparse housing type categories (n < 10 each) into "Other"
    housing_type = case_when(
      VAR01_1_T1 == 1 ~ "Rental apartment",
      VAR01_1_T1 == 2 ~ "Condominium",
      VAR01_1_T1 == 3 ~ "Detached house/villa",
      VAR01_1_T1 == 4 ~ "Townhouse/semi-detached",
      VAR01_1_T1 %in% c(5, 6, 7) ~ "Other",
      TRUE ~ NA_character_
    ) |>
      factor(levels = c("Rental apartment", "Condominium",
                        "Detached house/villa", "Townhouse/semi-detached", "Other")),

    home_ownership = factor(
      case_when(VAR01_2_T1 == 1 ~ "Yes", VAR01_2_T1 == 2 ~ "No", TRUE ~ NA_character_),
      levels = c("Yes", "No")
    ),

    any_obstacle_f = factor(
      case_when(any_obstacle == 1 ~ "Yes", any_obstacle == 0 ~ "No", TRUE ~ NA_character_),
      levels = c("Yes", "No")
    )
  )

# ── Compute p-values (t-test / chi-squared) ───────────────────────────────────
test_vars <- list(
  continuous   = c("age", "housing_suitability", "home_satisfaction", "neighbourhood_cohesion"),
  categorical  = c("sex", "housing_type", "home_ownership", "intention_timeframe", "any_obstacle_f")
)

pvals <- bind_rows(
  map_dfr(test_vars$continuous, \(v) {
    d <- tab_dat |> filter(!is.na(.data[[v]]), !is.na(relocated_f))
    tibble(variable = v, p_value = t.test(d[[v]] ~ d$relocated_f)$p.value)
  }),
  map_dfr(test_vars$categorical, \(v) {
    d <- tab_dat |> filter(!is.na(.data[[v]]))
    tibble(variable = v, p_value = chisq.test(table(d[[v]], d$relocated_f))$p.value)
  })
) |>
  mutate(p_fmt = case_when(
    p_value < 0.001 ~ "<0.001",
    TRUE            ~ formatC(p_value, digits = 3, format = "f")
  ))

# ── Build table ───────────────────────────────────────────────────────────────
tbl1 <- tab_dat |>
  select(
    relocated_f,
    age, sex,
    housing_type, home_ownership,
    intention_timeframe,
    housing_suitability, home_satisfaction, neighbourhood_cohesion,
    any_obstacle_f
  ) |>
  tbl_summary(
    by = relocated_f,
    label = list(
      age                    ~ "Age, years",
      sex                    ~ "Sex",
      housing_type           ~ "Housing type",
      home_ownership         ~ "Owns home",
      intention_timeframe    ~ "Expected timeframe to move",
      housing_suitability    ~ "Housing suitability (1\u20135)",
      home_satisfaction      ~ "Home satisfaction (1\u20135)",
      neighbourhood_cohesion ~ "Neighbourhood cohesion (1\u20133)",
      any_obstacle_f         ~ "Any perceived obstacle to moving"
    ),
    type = list(
      housing_suitability    ~ "continuous",
      home_satisfaction      ~ "continuous",
      neighbourhood_cohesion ~ "continuous"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  modify_header(
    label  ~ "**Characteristic**",
    stat_0 ~ "**Overall**\nN = {N}",
    stat_1 ~ "**Not relocated**\nn = {n}",
    stat_2 ~ "**Relocated**\nn = {n}"
  ) |>
  modify_table_body(
    ~ .x |>
      left_join(select(pvals, variable, p_fmt), by = "variable") |>
      mutate(p_fmt = if_else(row_type == "label", p_fmt, NA_character_))
  ) |>
  modify_column_alignment(columns = p_fmt, align = "right") |>
  modify_header(p_fmt ~ "**p-value**") |>
  modify_footnote(
    stat_0 ~ "Mean (SD) for continuous variables; n (%) for categorical variables",
    p_fmt  ~ "t-test for continuous variables; chi-squared for categorical variables"
  ) |>
  modify_caption(
    "**Table 1.** Baseline characteristics of the analysis sample stratified by relocation status"
  )

# ── Save ──────────────────────────────────────────────────────────────────────
tbl1 |>
  as_flex_table() |>
  save_as_docx(path = "output/tables/table1_descriptives.docx")

tbl1 |>
  as_gt() |>
  gt::gtsave("output/tables/table1_descriptives.html")

cat("Saved: output/tables/table1_descriptives.docx\n")
cat("Saved: output/tables/table1_descriptives.html\n")
