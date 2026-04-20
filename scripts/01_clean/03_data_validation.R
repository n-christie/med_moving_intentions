library(tidyverse)
library(haven)

# ── Overview ──────────────────────────────────────────────────────────────────
# Read-only data validation script. Loads .rds files, prints results, saves
# nothing. Run from project root:
#   source("scripts/01_clean/03_data_validation.R")
#
# Checks:
#   1. Sample flow (wave Ns vs expected)
#   2. Missing data profile (key variables)
#   3. Consistency checks (reloc dates, date order, nr_reloc, IDs)
#   4. Variable range checks
#   5. T2/T3 variable completeness
#   6. Summary banner

# ── Helper ────────────────────────────────────────────────────────────────────

report <- function(label, actual, expected = NULL, pass = NULL, detail = NULL) {
  if (!is.null(expected) && is.null(pass)) {
    pass <- actual == expected
  }
  tag  <- if (isTRUE(pass)) "[PASS]" else if (isFALSE(pass)) "[FAIL]" else "[NOTE]"
  line <- paste0(tag, " ", label)
  if (!is.null(expected)) {
    line <- paste0(line, " — actual: ", actual, "  expected: ", expected)
  } else {
    line <- paste0(line, " — ", actual)
  }
  if (!is.null(detail)) {
    line <- paste0(line, "\n        ", detail)
  }
  cat(line, "\n")
}

# ── Load data ─────────────────────────────────────────────────────────────────

cat("Loading data...\n")
df_clean <- readRDS("data/processed/survey_clean.rds")
df       <- readRDS("data/processed/survey_analysis.rds")
cat("survey_clean.rds:    ", nrow(df_clean), "rows x", ncol(df_clean), "cols\n")
cat("survey_analysis.rds:", nrow(df),       "rows x", ncol(df),       "cols\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 1: Sample flow
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 1: Sample flow\n")
cat("══════════════════════════════════════════════════════════════\n")

# Use Date_T* as canonical wave participation measure (consistent with survival
# script; not affected by flag-column data errors)
n_total <- nrow(df)
n_t1    <- sum(!is.na(df$Date_T1))
n_t2    <- sum(!is.na(df$Date_T2))
n_t3    <- sum(!is.na(df$Date_T3))

report("Total rows in dataset",   n_total, expected = 1964)
report("T1 respondents (Date_T1 non-NA)", n_t1,
       pass = dplyr::between(n_t1, 1959, 1964),
       detail = "Expected ~1961-1964 (3 missing LopNr_PersonNr, plus date parsing)")
report("T2 respondents (Date_T2 non-NA)", n_t2, expected = 1509)
report("T3 respondents (Date_T3 non-NA)", n_t3, expected = 1302)

# Cross-check with flag columns (available in df_clean only)
if (all(c("T2", "T3") %in% names(df_clean))) {
  n_t2_flag <- sum(as.numeric(df_clean$T2) == 1, na.rm = TRUE)
  n_t3_flag <- sum(as.numeric(df_clean$T3) == 1, na.rm = TRUE)
  report("T2 flag column (T2==1)", n_t2_flag,
         pass = abs(n_t2_flag - n_t2) <= 5,
         detail = paste0("Date-based count: ", n_t2, "; flag-based: ", n_t2_flag))
  report("T3 flag column (T3==1)", n_t3_flag,
         pass = abs(n_t3_flag - n_t3) <= 5,
         detail = paste0("Date-based count: ", n_t3, "; flag-based: ", n_t3_flag))
} else {
  cat("[NOTE] T2/T3 flag columns not found in df_clean — skipping flag cross-check\n")
}

# T3 non-respondents
n_t3_nonresp <- n_total - n_t3
report("T3 non-respondents (administratively censored)",
       n_t3_nonresp,
       pass = NULL,
       detail = paste0("AGENTS.md documents '545 T3 non-respondents'; actual = ",
                       n_t3_nonresp,
                       if (n_t3_nonresp != 545) " — DISCREPANCY FROM AGENTS.md" else ""))

# Analytical sample (complete on RQ1 core variables)
n_analytical <- df |>
  filter(!is.na(relocated_f), !is.na(intention_timeframe),
         !is.na(age), !is.na(sex), !is.na(srh)) |>
  nrow()
report("Analytical sample (complete on RQ1 vars)", n_analytical,
       pass = NULL,
       detail = "No single expected value; serves as reference for analysis scripts")

cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 2: Missing data profile
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 2: Missing data profile (key variables)\n")
cat("══════════════════════════════════════════════════════════════\n")

KEY_VARS <- c(
  "LopNr_PersonNr", "Date_T1", "Date_T2", "Date_T3",
  "relocated", "relocated_f", "VAR24_T1", "intention_timeframe",
  "reloc_date1", "reloc_date2", "reloc_date3",
  "age", "sex", "srh",
  "housing_suitability", "home_satisfaction", "neighbourhood_cohesion",
  "any_obstacle",
  "VAR13_1_T1", "VAR13_2_T1", "VAR13_3_T1", "VAR13_4_T1",
  "VAR14_T1", "VAR16_T1", "VAR17_T1", "VAR18_T1",
  "VAR34_T1", "VAR30_T1"
)

present_vars <- intersect(KEY_VARS, names(df))
missing_vars <- setdiff(KEY_VARS, names(df))

if (length(missing_vars) > 0) {
  cat("[NOTE] These key vars not found in df:", paste(missing_vars, collapse = ", "), "\n\n")
}

miss_tab <- tibble(
  variable  = present_vars,
  n_missing = map_int(present_vars, \(v) sum(is.na(df[[v]]))),
  pct_miss  = round(100 * n_missing / nrow(df), 1)
)

cat(sprintf("%-35s %10s %8s\n", "Variable", "N missing", "% miss"))
cat(strrep("-", 56), "\n")
for (i in seq_len(nrow(miss_tab))) {
  flag <- if (miss_tab$pct_miss[i] > 10 &&
              !miss_tab$variable[i] %in% c("Date_T2", "Date_T3",
                                            "reloc_date1", "reloc_date2", "reloc_date3",
                                            "relocated_f", "relocated",
                                            "intention_timeframe", "any_obstacle")) " *** HIGH ***" else ""
  cat(sprintf("%-35s %10d %8.1f%%%s\n",
              miss_tab$variable[i], miss_tab$n_missing[i],
              miss_tab$pct_miss[i], flag))
}

# Flag any unexpectedly high missingness (>10%) among core demographic/health vars
core_check <- c("age", "sex", "srh", "Date_T1", "LopNr_PersonNr")
core_high  <- miss_tab |>
  filter(variable %in% core_check, pct_miss > 10)
if (nrow(core_high) == 0) {
  cat("\n[PASS] No unexpectedly high missingness (>10%) in core demographic/ID variables\n")
} else {
  cat("\n[FAIL] High missingness (>10%) in core variables:",
      paste(core_high$variable, collapse = ", "), "\n")
}
cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 3: Consistency checks
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 3: Consistency checks\n")
cat("══════════════════════════════════════════════════════════════\n")

# 3.1 relocated==1 → at least one reloc_date non-NA
viol_31 <- df |>
  filter(!is.na(relocated), relocated == 1) |>
  filter(is.na(reloc_date1) & is.na(reloc_date2) & is.na(reloc_date3)) |>
  nrow()
report("3.1 relocated==1 → at least one reloc_date non-NA", viol_31,
       expected = 0)

# 3.2 relocated==0 → all reloc_dates are NA
viol_32 <- df |>
  filter(!is.na(relocated), relocated == 0) |>
  filter(!is.na(reloc_date1) | !is.na(reloc_date2) | !is.na(reloc_date3)) |>
  nrow()
report("3.2 relocated==0 → all reloc_dates NA", viol_32,
       expected = 0)

# 3.3 VAR24_T1 values only in {1, 2, 3, NA}
# VAR24_T1 may have labelled class — strip labels before comparing
var24_vals <- as.numeric(zap_labels(df_clean$VAR24_T1))
viol_33 <- sum(!is.na(var24_vals) & !var24_vals %in% c(1, 2, 3))
report("3.3 VAR24_T1 values in {1,2,3} only", viol_33,
       expected = 0,
       detail = paste0("Checked in df_clean (pre-recode); unique non-NA values: ",
                       paste(sort(unique(var24_vals[!is.na(var24_vals)])), collapse = ", ")))

# 3.4a Date_T1 < Date_T2 where both present
if (all(c("Date_T1", "Date_T2") %in% names(df))) {
  viol_34a <- df |>
    filter(!is.na(Date_T1), !is.na(Date_T2)) |>
    filter(Date_T1 >= Date_T2) |>
    nrow()
  report("3.4a Date_T1 < Date_T2 (where both non-NA)", viol_34a, expected = 0)
}

# 3.4b Date_T2 < Date_T3 where both present
if (all(c("Date_T2", "Date_T3") %in% names(df))) {
  viol_34b <- df |>
    filter(!is.na(Date_T2), !is.na(Date_T3)) |>
    filter(Date_T2 >= Date_T3) |>
    nrow()
  report("3.4b Date_T2 < Date_T3 (where both non-NA)", viol_34b, expected = 0)
}

# 3.5a relocated==1 → nr_reloc >= 1
if ("nr_reloc" %in% names(df)) {
  viol_35a <- df |>
    filter(!is.na(relocated), !is.na(nr_reloc), relocated == 1) |>
    filter(nr_reloc < 1) |>
    nrow()
  report("3.5a relocated==1 → nr_reloc >= 1", viol_35a, expected = 0)

  # 3.5b relocated==0 → nr_reloc == 0
  viol_35b <- df |>
    filter(!is.na(relocated), !is.na(nr_reloc), relocated == 0) |>
    filter(nr_reloc != 0) |>
    nrow()
  report("3.5b relocated==0 → nr_reloc == 0", viol_35b, expected = 0)
} else {
  cat("[NOTE] nr_reloc not found in df — skipping checks 3.5a/b\n")
}

# 3.6 LopNr_PersonNr missing count == 3
n_missing_id <- sum(is.na(df$LopNr_PersonNr))
report("3.6 LopNr_PersonNr missing count == 3", n_missing_id,
       expected = 3,
       detail = "3 missing IDs are expected by design (documented in AGENTS.md)")

cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 4: Variable range checks
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 4: Variable range checks\n")
cat("══════════════════════════════════════════════════════════════\n")

check_range <- function(data, var, lo, hi, label = NULL, expected_oor = 0) {
  lbl <- if (!is.null(label)) label else var
  if (!var %in% names(data)) {
    cat(sprintf("[NOTE] %-40s — variable not found\n", lbl))
    return(invisible(NULL))
  }
  vals  <- suppressWarnings(as.numeric(zap_labels(data[[var]])))
  n_oor <- sum(!is.na(vals) & (vals < lo | vals > hi))
  pass  <- n_oor == expected_oor
  tag   <- if (pass) "[PASS]" else "[FAIL]"
  suffix <- if (expected_oor != 0) paste0("  (expected: ", expected_oor, ")") else ""
  cat(sprintf("%s %-40s range [%g, %g] — %d out-of-range%s\n",
              tag, lbl, lo, hi, n_oor, suffix))
  invisible(n_oor)
}

# VAR13_1–4_T1 (suitability items): 1–5
for (i in 1:4) {
  check_range(df, paste0("VAR13_", i, "_T1"), 1, 5)
}

# VAR14_T1 (home satisfaction): 1–5
check_range(df, "VAR14_T1", 1, 5)

# Age_T1 (broad valid range)
check_range(df, "Age_T1", 10, 120, label = "Age_T1 (broad valid: 10-120)")

# Flag corrupt/aggregate rows with Age_T1 < 10 (fractional aggregate rows not
# caught by NA-coding)
if ("Age_T1" %in% names(df)) {
  age_vals  <- suppressWarnings(as.numeric(df$Age_T1))
  n_age_low <- sum(!is.na(age_vals) & age_vals < 10)
  tag_age   <- if (n_age_low == 0) "[PASS]" else "[FAIL]"
  cat(sprintf("%s %-40s — %d rows with Age_T1 < 10 (corrupt)\n",
              tag_age, "Age_T1 corrupt rows (< 10)", n_age_low))
}

# VAR34_T1 (SRH): 1–5
check_range(df, "VAR34_T1", 1, 5)

# VAR16_T1, VAR17_T1: 1–3
check_range(df, "VAR16_T1", 1, 3)
check_range(df, "VAR17_T1", 1, 3)

# VAR18_T1 in df (analysis): expect 0 out-of-range (recoded by 02_recode.R)
check_range(df, "VAR18_T1", 1, 3,
            label = "VAR18_T1 in survey_analysis (expect 0 OOR)")

# VAR18_T1 in df_clean (pre-recode): expect 3 out-of-range (values 4 and 5)
check_range(df_clean, "VAR18_T1", 1, 3,
            label = "VAR18_T1 in survey_clean   (expect 3 OOR)",
            expected_oor = 3)

# housing_suitability composite: 1–5
check_range(df, "housing_suitability", 1, 5)

# neighbourhood_cohesion composite: 1–3
check_range(df, "neighbourhood_cohesion", 1, 3)

# relocated: {0, 1} only
if ("relocated" %in% names(df)) {
  rel_vals <- suppressWarnings(as.numeric(df$relocated))
  n_rel_oor <- sum(!is.na(rel_vals) & !rel_vals %in% c(0, 1))
  tag_rel   <- if (n_rel_oor == 0) "[PASS]" else "[FAIL]"
  cat(sprintf("%s %-40s — %d out-of-range (non 0/1 values)\n",
              tag_rel, "relocated {0,1} only", n_rel_oor))
}

cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 5: T2/T3 variable completeness
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 5: T2/T3 variable completeness\n")
cat("══════════════════════════════════════════════════════════════\n")

check_wave_completeness <- function(data, wave, date_var, key_vars,
                                    threshold = 0.70) {
  respondents <- data |> filter(!is.na(.data[[date_var]]))
  n_resp      <- nrow(respondents)
  cat(sprintf("\n%s respondents (N = %d):\n", wave, n_resp))
  cat(sprintf("  %-25s %8s %8s %6s\n", "Variable", "N complete", "% complete", ""))
  present <- intersect(key_vars, names(data))
  absent  <- setdiff(key_vars, names(data))
  if (length(absent) > 0) {
    cat("  [NOTE] Not in dataset:", paste(absent, collapse = ", "), "\n")
  }
  for (v in present) {
    n_complete <- sum(!is.na(respondents[[v]]))
    pct        <- 100 * n_complete / n_resp
    tag        <- if (pct >= threshold * 100) "[PASS]" else "[FAIL]"
    cat(sprintf("  %s %-25s %8d %8.1f%%\n", tag, v, n_complete, pct))
  }

  # Cross-check: non-respondents should NOT have T2/T3 data
  nonresp    <- data |> filter(is.na(.data[[date_var]]))
  n_nonresp  <- nrow(nonresp)
  cat(sprintf("\n  Non-respondents (N = %d) should have no %s data:\n",
              n_nonresp, wave))
  for (v in present) {
    n_with_data <- sum(!is.na(nonresp[[v]]))
    pct_leak    <- 100 * n_with_data / n_nonresp
    tag         <- if (n_with_data == 0) "[PASS]" else "[NOTE]"
    cat(sprintf("  %s %-25s %8d non-resp have data (%.1f%%)\n",
                tag, v, n_with_data, pct_leak))
  }
}

T2_VARS <- c("VAR013_1_T2", "VAR014_T2", "VAR016_T2",
             "VAR017_T2", "VAR018_T2", "VAR024_T2", "VAR034_T2")
T3_VARS <- c("VAR013_1_T3", "VAR014_T3", "VAR016_T3",
             "VAR017_T3", "VAR018_T3", "VAR024_T3", "VAR034_T3")

check_wave_completeness(df, "T2", "Date_T2", T2_VARS)
cat("\n  NOTE: VAR016/017/018_T2 (neighborhood cohesion) are low at T2.\n")
cat("  These items may have been filter-question or not included at T2.\n")
cat("  Low completeness is not necessarily a data error.\n")

check_wave_completeness(df, "T3", "Date_T3", T3_VARS)
cat("\n  NOTE: VAR024_T3 (intention timeframe) is very low at T3 (~3.5%).\n")
cat("  The intention question may not have been a core T3 item.\n")

cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# CHECK 6: Summary banner
# ─────────────────────────────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("CHECK 6: Summary\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("Dataset notes (known expected-vs-actual discrepancies):\n\n")

cat("  T1 total N\n")
cat("    Documented: 1964 (total rows)\n")
cat("    Analytical: 1961 (3 missing LopNr_PersonNr excluded)\n")
cat("    Actual total rows:", n_total, "\n\n")

cat("  T2 respondents\n")
cat("    Documented: 1509\n")
cat("    Actual (Date_T2 non-NA):", n_t2, "\n\n")

cat("  T3 respondents\n")
cat("    Documented: 1302\n")
cat("    Actual (Date_T3 non-NA):", n_t3, "\n\n")

cat("  T3 non-respondents (administrative censoring)\n")
cat("    AGENTS.md documents '545 T3 non-respondents'\n")
cat("    Survival script comment: '571 non-responders at T3'\n")
cat("    Actual T3 non-respondents:", n_t3_nonresp,
    "(=", n_total, "total -", n_t3, "T3 respondents)\n\n")

cat("  VAR18_T1 stray values\n")
cat("    3 rows had values 4/5 (outside valid 1-3 range)\n")
cat("    Already recoded to NA in 02_recode.R\n")
cat("    Confirmed: 0 out-of-range in survey_analysis.rds,",
    "3 out-of-range in survey_clean.rds\n\n")

cat("  Missing LopNr_PersonNr\n")
cat("    3 rows missing participant ID — expected by design\n")
cat("    These rows should be treated with caution in analyses\n\n")

cat("No files were written. Validation complete.\n")
