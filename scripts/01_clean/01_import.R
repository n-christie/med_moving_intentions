library(tidyverse)
library(labelled)

# Prerequisite: run 00_translate_codebook.R once to generate prosp_dict_en.rds

# ── Import raw data ───────────────────────────────────────────────────────────

raw <- read_tsv(
  "data/Giedre_Lev_prospective.txt",
  show_col_types = FALSE
)

# ── Recode special values to NA ───────────────────────────────────────────────
# 99   = not applicable / skip
# 888  = not applicable (sub-items)
# 9999 = not asked / filter question
NA_CODES <- c(99, 888, 9999)

df <- raw |>
  mutate(across(where(is.numeric), \(x) ifelse(x %in% NA_CODES, NA, x))) |>
  # relocated should be 0/1 only; stray non-integer values indicate data errors
  mutate(relocated = if_else(relocated %in% c(0, 1), relocated, NA_real_))

# ── Parse date columns ────────────────────────────────────────────────────────
date_cols <- c("reloc_date1", "reloc_date2", "reloc_date3",
               "Date_avreg", "Date_T1", "Date_T2", "Date_T3")

df <- df |>
  mutate(across(any_of(date_cols), \(x) mdy(x)))

# ── Apply English variable and value labels ───────────────────────────────────
# Labels translated from Swedish via DeepL; source: data/codebooks/prosp_dict_en.rds
dict_raw <- readRDS("data/codebooks/prosp_dict.rds")
dict_en  <- readRDS("data/codebooks/prosp_dict_en.rds")

var_labels_map <- dict_en |>
  filter(!is.na(var_label_en), var_name %in% names(df)) |>
  select(var_name, var_label_en) |>
  deframe()

var_label(df) <- as.list(var_labels_map)

val_entries <- dict_en |>
  filter(var_name %in% names(df),
         map_lgl(value_labels_en, \(x) length(x) > 0))

for (i in seq_len(nrow(val_entries))) {
  vn       <- val_entries$var_name[i]
  orig_num <- dict_raw[[vn]]$value_labels
  en_names <- val_entries$value_labels_en[[i]]

  if (!is.null(orig_num) && is.numeric(orig_num) && length(orig_num) == length(en_names)) {
    val_labels(df[[vn]]) <- set_names(as.numeric(orig_num), en_names)
  }
}

# ── Validation ────────────────────────────────────────────────────────────────
stopifnot(nrow(df) == nrow(raw))

cat("Rows:", nrow(df), "\n")
cat("Cols:", ncol(df), "\n")
cat("Labelled cols:", sum(map_lgl(df, \(x) !is.null(var_label(x)))), "\n")
cat("Missing values in key columns:\n")
df |>
  select(LopNr_PersonNr, T1, T2, T3, relocated, Sex_T1, Age_T1) |>
  summarise(across(everything(), \(x) sum(is.na(x)))) |>
  print()

# ── Save ──────────────────────────────────────────────────────────────────────
write_rds(df, "data/processed/survey_clean.rds")
cat("Saved to data/processed/survey_clean.rds\n")
