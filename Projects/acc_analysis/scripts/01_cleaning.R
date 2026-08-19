# =============================================================================
# 01_cleaning.R
# ACC CLM Dodoma — HIV Service Satisfaction Survey
# -----------------------------------------------------------------------------
# Loads all 12 Excel files, applies the May-18 exclusion, identifies Likert
# rating columns by content, recodes responses to integers 1–5, preserves sex
# and a respondent_id for downstream stratification and maoni–PCA linkage,
# separates opinion (maoni) text, and writes cleaned CSVs + combined RDS.
# =============================================================================

source("/Users/matemba/Documents/Claude/Projects/acc_analysis/scripts/00_setup.R")


# ── Core per-file processing function ─────────────────────────────────────────
process_file <- function(filepath, group_label) {
  message("\n── ", group_label, " ──────────────────────────────────────────")

  # Read everything as text; .name_repair = "unique" appends ...N to duplicates
  # (e.g. repeated "Maoni/maelezo" → "Maoni/maelezo", "Maoni/maelezo...23", …)
  raw <- readxl::read_excel(filepath, col_types = "text", .name_repair = "unique")
  message("  Raw: ", nrow(raw), " rows × ", ncol(raw), " cols")

  # ── Step 1a: Exclude May 18 records ─────────────────────────────────────────
  if ("Tarehe" %in% names(raw)) {
    raw <- raw %>%
      mutate(Tarehe_date = purrr::map_vec(Tarehe, parse_excel_date)) %>%
      filter(is.na(Tarehe_date) | Tarehe_date != EXCLUDE_DATE) %>%
      select(-Tarehe_date)
  }
  message("  After May-18 exclusion: ", nrow(raw), " rows")

  # ── Step 2: Classify every column ────────────────────────────────────────────
  meta_name_patterns <- paste(c(
    "^Tarehe$", "^Halmashauri", "^Mkusanyaji", "^Kituo\\s+cha\\s+Afya\\s*-",
    "^Kata\\s*-",  "^UTAYARI",    "^Mkoa$",      "^Kata$",
    "^Kituo\\s+cha\\s+afya$", "^Mhojiwa$", "^Aina\\s+ya\\s+Mhojiwa$",
    "^Wadhifa", "^Idadi", "^Kundi\\s+linalohojiwa$",
    "^Umri", "^_", "^__version__", "^meta/"
  ), collapse = "|")

  section_pattern <- "^Je\\s+unaridhika"
  maoni_pattern   <- "^Maoni"
  # Sex columns — kept separately so they can join with ratings
  sex_pattern     <- "^Jinsi[a]?$"

  col_class <- purrr::map_chr(names(raw), function(cn) {
    if (str_detect(cn, regex(sex_pattern,         ignore_case = TRUE))) return("sex")
    if (str_detect(cn, regex(meta_name_patterns,  ignore_case = TRUE))) return("metadata")
    if (str_detect(cn, regex(section_pattern,     ignore_case = TRUE))) return("section_header")
    if (str_detect(cn, regex(maoni_pattern,        ignore_case = TRUE))) return("maoni")
    if (is_likert_col(raw[[cn]]))                                        return("rating")
    return("other")
  })

  rating_cols  <- names(raw)[col_class == "rating"]
  maoni_cols   <- names(raw)[col_class == "maoni"]
  sex_cols     <- names(raw)[col_class == "sex"]
  meta_cols    <- names(raw)[col_class %in% c("metadata", "other")]

  message("  Columns → rating: ", length(rating_cols),
          " | maoni: ",  length(maoni_cols),
          " | sex: ",    length(sex_cols),
          " | meta: ",   length(meta_cols))

  # ── Step 3: Recode Likert + rescue trailing free-text into paired maoni ──────
  all_col_names  <- names(raw)
  maoni_position <- which(col_class == "maoni")
  rating_pairing <- purrr::map(
    which(col_class == "rating"),
    function(ri) {
      nxt <- maoni_position[maoni_position > ri]
      if (length(nxt) > 0) nxt[1] else NA_integer_
    }
  )
  names(rating_pairing) <- rating_cols

  df <- as.data.frame(raw, stringsAsFactors = FALSE)

  for (rc in rating_cols) {
    paired_idx  <- rating_pairing[[rc]]
    orig_vals   <- df[[rc]]

    # If a rating cell contains "ridhika" PLUS extra text, move surplus to maoni
    if (!is.na(paired_idx)) {
      mc_name   <- all_col_names[paired_idx]
      has_extra <- !is.na(orig_vals) &
        str_detect(as.character(orig_vals), regex("ridhika", ignore_case = TRUE)) &
        str_length(trimws(as.character(orig_vals))) > 25
      df[[mc_name]] <- ifelse(
        has_extra & (is.na(df[[mc_name]]) | nchar(trimws(df[[mc_name]])) == 0),
        orig_vals, df[[mc_name]]
      )
    }
    df[[rc]] <- recode_likert(orig_vals)
  }

  # ── Step 4: Build output data frames ─────────────────────────────────────────

  # Respondent ID: sequential within group, used to link maoni ↔ PCA label
  respondent_id <- paste0(group_label, "_", seq_len(nrow(df)))

  # 4a — Ratings (includes sex and respondent_id for stratification / linkage)
  sex_val <- if (length(sex_cols) > 0) {
    # Standardise sex labels: Male/Female in English
    raw_sex <- trimws(df[[sex_cols[1]]])
    dplyr::case_when(
      str_detect(raw_sex, regex("^(M|Me|Mme|Kiume|Male)",   ignore_case = TRUE)) ~ "Male",
      str_detect(raw_sex, regex("^(F|Ke|Kike|Female|Wanawake)", ignore_case = TRUE)) ~ "Female",
      TRUE ~ NA_character_
    )
  } else {
    rep(NA_character_, nrow(df))
  }

  ratings_df <- df[, rating_cols, drop = FALSE] %>%
    mutate(across(everything(), as.integer)) %>%
    mutate(
      respondent_id = respondent_id,
      group         = group_label,
      sex           = sex_val,
      .before       = 1
    )

  # 4b — Maoni (renamed to maoni_01, maoni_02 … to avoid duplicates)
  if (length(maoni_cols) > 0) {
    maoni_df <- df[, maoni_cols, drop = FALSE]
    names(maoni_df) <- sprintf("maoni_%02d", seq_along(maoni_cols))
    attr(maoni_df, "original_names") <- maoni_cols
    maoni_df <- maoni_df %>%
      mutate(respondent_id = respondent_id, group = group_label, .before = 1)
  } else {
    maoni_df <- data.frame(respondent_id = respondent_id,
                           group = group_label, stringsAsFactors = FALSE)
  }

  # 4c — Metadata
  meta_df <- df[, intersect(meta_cols, names(df)), drop = FALSE] %>%
    mutate(respondent_id = respondent_id, group = group_label, .before = 1)

  # ── Step 5: Missingness report ────────────────────────────────────────────────
  if (length(rating_cols) > 0) {
    item_cols <- setdiff(names(ratings_df), c("respondent_id", "group", "sex"))
    miss_pct  <- colMeans(is.na(ratings_df[, item_cols, drop = FALSE])) * 100
    miss_df   <- data.frame(
      question    = names(miss_pct),
      pct_missing = round(miss_pct, 1),
      flag        = ifelse(miss_pct > 50, "HIGH (>50%)", "OK"),
      stringsAsFactors = FALSE
    )
    n_high <- sum(miss_df$flag != "OK")
    if (n_high > 0)
      message("  ⚠ ", n_high, " questions >50% missing (excluded from PCA)")
    else
      message("  ✓ No questions with >50% missing")

    write.csv(miss_df,
              file.path(CLEAN_DIR, paste0(group_label, "_missingness.csv")),
              row.names = FALSE)

    high_miss_cols <- miss_df$question[miss_df$flag != "OK"]
  } else {
    high_miss_cols <- character(0)
  }

  list(
    ratings        = ratings_df,
    maoni          = maoni_df,
    metadata       = meta_df,
    high_miss_cols = high_miss_cols
  )
}


# ── Run over all 12 files ──────────────────────────────────────────────────────
xlsx_files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE)
results    <- list()

for (f in xlsx_files) {
  fname <- basename(f)
  label <- GROUP_LABELS[fname]
  if (is.na(label)) { message("Skipping: ", fname); next }
  results[[label]] <- process_file(f, label)
}

# ── Summary ────────────────────────────────────────────────────────────────────
message("\n── Summary ────────────────────────────────────────────────────────────")
for (lbl in names(results)) {
  r      <- results[[lbl]]
  n_rows  <- nrow(r$ratings)
  n_items <- ncol(r$ratings) - 3   # minus respondent_id, group, sex
  has_sex <- any(!is.na(r$ratings$sex))
  message(sprintf("  %-35s  %3d rows  |  %2d items  |  sex: %s",
                  lbl, n_rows, n_items, if (has_sex) "YES" else "no"))
}

# ── Write cleaned CSVs ─────────────────────────────────────────────────────────
for (lbl in names(results)) {
  write.csv(results[[lbl]]$ratings,
            file.path(CLEAN_DIR, paste0(lbl, "_ratings.csv")), row.names = FALSE)
  write.csv(results[[lbl]]$maoni,
            file.path(CLEAN_DIR, paste0(lbl, "_maoni.csv")),   row.names = FALSE)
}

saveRDS(results, file.path(OUTPUT_DIR, "cleaned_results.rds"))
message("\n✓ 01_cleaning.R complete — RDS saved to ", OUTPUT_DIR)
