library(tidyverse)
library(httr2)

# ── Overview ──────────────────────────────────────────────────────────────────
# This script translates the Swedish variable and value labels in
# data/codebooks/prosp_dict.rds into English using the DeepL API, and saves
# the result as data/codebooks/prosp_dict_en.rds and prosp_dict_en.csv.
#
# Run this script ONCE (or whenever the source dictionary changes).
# 01_import.R depends on prosp_dict_en.rds being present.
#
# Requires: DEEPL_API_KEY set in ~/.Renviron (free or paid tier both work).
#   To set: usethis::edit_r_environ() then add DEEPL_API_KEY=your-key-here

# ── Check API key ─────────────────────────────────────────────────────────────
key <- Sys.getenv("DEEPL_API_KEY")
if (nchar(key) == 0) stop("DEEPL_API_KEY not found. Add it to ~/.Renviron.")

# Free-tier keys end in ":fx" and use a different API URL
url <- if (endsWith(key, ":fx")) {
  "https://api-free.deepl.com/v2/translate"
} else {
  "https://api.deepl.com/v2/translate"
}

# ── Load source dictionary ────────────────────────────────────────────────────
dict_raw <- readRDS("data/codebooks/prosp_dict.rds")

# ── Flatten to tidy tibble (skip Personnummer) ────────────────────────────────
dict_df <- imap_dfr(dict_raw, function(entry, var_name) {
  if (var_name == "Personnummer") return(NULL)
  tibble(
    var_name,
    var_label_sv    = entry$var_label %||% NA_character_,
    value_labels_sv = list(
      if (!is.null(entry$value_labels) && length(entry$value_labels) > 0)
        names(entry$value_labels)
      else
        character(0)
    )
  )
})

# ── Collect unique strings ────────────────────────────────────────────────────
unique_var_labels <- dict_df |>
  filter(!is.na(var_label_sv)) |>
  distinct(var_label_sv) |>
  pull(var_label_sv)

unique_val_labels <- dict_df |>
  pull(value_labels_sv) |>
  unlist() |>
  unique() |>
  discard(\(x) is.na(x) | x == "")

cat("Unique var labels to translate:", length(unique_var_labels), "\n")
cat("Unique value labels to translate:", length(unique_val_labels), "\n")

# ── Translation helper ────────────────────────────────────────────────────────
translate_batch <- function(texts, key, url) {
  resp <- request(url) |>
    req_headers(Authorization = paste("DeepL-Auth-Key", key)) |>
    req_body_json(list(text = as.list(texts), target_lang = "EN")) |>
    req_perform() |>
    resp_body_json()
  map_chr(resp$translations, "text")
}

translate_all <- function(texts, key, url, batch_size = 50) {
  batches <- split(texts, ceiling(seq_along(texts) / batch_size))
  results <- character(length(texts))
  for (b in seq_along(batches)) {
    idx <- ((b - 1) * batch_size + 1):min(b * batch_size, length(texts))
    results[idx] <- translate_batch(batches[[b]], key, url)
    Sys.sleep(0.3)
  }
  results
}

# ── Translate ─────────────────────────────────────────────────────────────────
cat("Translating var labels...\n")
var_label_en <- translate_all(unique_var_labels, key, url)

cat("Translating value labels...\n")
val_label_en <- translate_all(unique_val_labels, key, url)

# ── Build lookup tables ───────────────────────────────────────────────────────
var_label_lookup <- set_names(var_label_en, unique_var_labels)
val_label_lookup <- set_names(val_label_en, unique_val_labels)

# Manual fixes: short tokens that DeepL mistranslates without context
val_label_lookup["Ja"]  <- "Yes"
val_label_lookup["Nej"] <- "No"

# ── Reconstruct English codebook ─────────────────────────────────────────────
dict_en <- dict_df |>
  mutate(
    var_label_en = var_label_lookup[var_label_sv],
    # Preserve T1/T2/T3 as codes, not words
    var_label_en = case_when(
      var_label_sv %in% c("T1", "T2", "T3") ~ var_label_sv,
      TRUE ~ var_label_en
    ),
    value_labels_en = map(value_labels_sv, \(vals) {
      if (length(vals) == 0) return(character(0))
      val_label_lookup[vals]
    })
  )

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(dict_en, "data/codebooks/prosp_dict_en.rds")

dict_en |>
  mutate(
    value_labels_sv = map_chr(value_labels_sv, \(x) paste(x, collapse = " | ")),
    value_labels_en = map_chr(value_labels_en, \(x) paste(x, collapse = " | "))
  ) |>
  write_csv("data/codebooks/prosp_dict_en.csv")

cat("Saved: data/codebooks/prosp_dict_en.rds\n")
cat("Saved: data/codebooks/prosp_dict_en.csv\n")
