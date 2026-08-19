# =============================================================================
# 03_frequency_tables.R
# ACC CLM Dodoma — HIV Service Satisfaction Survey
# -----------------------------------------------------------------------------
# Produces satisfaction frequency tables focused on the PCA-derived binary
# classification (Satisfied / Not Satisfied) as the primary lens, with the
# raw 5-level distribution as a supplementary reference.
#
# Sex stratification is applied automatically to groups that:
#   (a) have a sex column, AND
#   (b) have ≥ SEX_STRAT_MIN respondents of each sex
#
# Per-group sheet layout (one sheet per group):
#   SECTION 1 — Binary Satisfaction Table  [PRIMARY]
#               % Satisfied / Not Satisfied per question, sorted by % Satisfied
#   SECTION 2 — Sex-Stratified Binary Table  [where applicable]
#               Side-by-side Male vs Female % Satisfied per question
#   SECTION 3 — Raw 5-Level Likert Table  [supplementary reference]
#
# Plus:
#   Sheet "Overview" — one-row-per-group summary across all groups
#
# Run AFTER 01_cleaning.R and 02_pca.R.
# =============================================================================

source("/Users/matemba/Documents/Claude/Projects/acc_analysis/scripts/00_setup.R")

results <- readRDS(file.path(OUTPUT_DIR, "cleaned_results.rds"))

SEX_STRAT_MIN <- 5   # minimum N per sex to include stratified table


# ── Styling helpers ────────────────────────────────────────────────────────────
mk_style <- function(fill, font = "white", bold = TRUE, halign = "center") {
  createStyle(fgFill = fill, fontColour = font,
              textDecoration = if (bold) "bold" else "normal",
              halign = halign, wrapText = TRUE)
}

st_primary  <- mk_style("#1a3a5c")          # dark navy  — binary table header
st_female   <- mk_style("#7b2d8b")          # purple     — female stratif.
st_male     <- mk_style("#1a6e3b")          # dark green — male stratif.
st_raw      <- mk_style("#4d4d4d")          # dark grey  — 5-level header
st_section  <- mk_style("#d9e8f5", font = "#1a3a5c", bold = TRUE, halign = "left")
st_stripe   <- createStyle(fgFill = "#f4f8fc")
st_sat_good <- createStyle(fgFill = "#d4edda")   # light green for high satisfaction
st_sat_warn <- createStyle(fgFill = "#fff3cd")   # amber for mid satisfaction
st_sat_bad  <- createStyle(fgFill = "#f8d7da")   # light red for low satisfaction
st_bold_l   <- createStyle(textDecoration = "bold", halign = "left")
st_bold_c   <- createStyle(textDecoration = "bold", halign = "center")
st_pct      <- createStyle(numFmt = "0.0", halign = "center")


# ── Helper: colour-code cells by % satisfied ──────────────────────────────────
colour_satisfaction <- function(wb, sheet, pct_col_idx, data_start_row, n_rows) {
  for (i in seq_len(n_rows)) {
    # pct_col_idx is the column number of "Pct_Satisfied"
    # We need to read the cell value — use a workaround via the data frame passed in
  }
}

# ── Binary table builder ───────────────────────────────────────────────────────
build_binary_table <- function(ratings_df, item_cols) {
  purrr::map_dfr(item_cols, function(qcol) {
    vals    <- as.integer(ratings_df[[qcol]])
    n_valid <- sum(!is.na(vals))
    binary  <- ifelse(is.na(vals), NA_integer_, ifelse(vals <= 2L, 0L, 1L))
    n_sat   <- sum(binary == 1, na.rm = TRUE)
    n_not   <- sum(binary == 0, na.rm = TRUE)
    data.frame(
      Question         = qcol,
      N                = n_valid,
      N_Satisfied      = n_sat,
      Pct_Satisfied    = ifelse(n_valid > 0, round(n_sat / n_valid * 100, 1), NA_real_),
      N_NotSatisfied   = n_not,
      Pct_NotSatisfied = ifelse(n_valid > 0, round(n_not / n_valid * 100, 1), NA_real_),
      stringsAsFactors = FALSE
    )
  }) %>%
    arrange(desc(Pct_Satisfied))
}

# ── 5-level table builder ──────────────────────────────────────────────────────
build_5level_table <- function(ratings_df, item_cols) {
  purrr::map_dfr(item_cols, function(qcol) {
    vals    <- as.integer(ratings_df[[qcol]])
    n_valid <- sum(!is.na(vals))
    counts  <- table(factor(vals, levels = 1:5))
    pcts    <- if (n_valid > 0) round(as.numeric(counts) / n_valid * 100, 1) else rep(NA, 5)
    data.frame(
      Question           = qcol,
      N                  = n_valid,
      `1_NotSat_n`       = as.integer(counts[1]),
      `1_NotSat_pct`     = pcts[1],
      `2_SomewhatSat_n`  = as.integer(counts[2]),
      `2_SomewhatSat_pct`= pcts[2],
      `3_Satisfied_n`    = as.integer(counts[3]),
      `3_Satisfied_pct`  = pcts[3],
      `4_HighSat_n`      = as.integer(counts[4]),
      `4_HighSat_pct`    = pcts[4],
      `5_VerySat_n`      = as.integer(counts[5]),
      `5_VerySat_pct`    = pcts[5],
      N_Missing          = sum(is.na(vals)),
      check.names        = FALSE,
      stringsAsFactors   = FALSE
    )
  })
}

# ── Write a table block into the workbook ─────────────────────────────────────
write_block <- function(wb, sheet, df, start_row, header_style,
                        col_widths = NULL) {
  writeData(wb, sheet, df, startRow = start_row, rowNames = FALSE)
  n_cols <- ncol(df)
  # Header row style
  addStyle(wb, sheet, header_style,
           rows = start_row, cols = 1:n_cols, gridExpand = TRUE)
  # Stripe alternate data rows
  for (r in seq(start_row + 1, start_row + nrow(df))) {
    if ((r - start_row) %% 2 == 0)
      addStyle(wb, sheet, st_stripe, rows = r, cols = 1:n_cols, gridExpand = TRUE)
  }
  if (!is.null(col_widths))
    setColWidths(wb, sheet, cols = seq_along(col_widths), widths = col_widths)

  start_row + nrow(df) + 2   # return next available row
}


# ── Build workbook ─────────────────────────────────────────────────────────────
wb <- createWorkbook()

# Collect summary rows as we loop
overview_rows <- list()


for (lbl in names(results)) {

  rat      <- results[[lbl]]$ratings
  hi_miss  <- results[[lbl]]$high_miss_cols
  display  <- GROUP_DISPLAY[lbl]

  # Item columns only (no respondent_id, group, sex, pca_label*)
  item_cols <- setdiff(names(rat),
                       c("respondent_id", "group", "sex",
                         "pca_label", "pca_label_lv", hi_miss))
  if (length(item_cols) == 0) {
    message("Skipping ", lbl, " — no item columns")
    next
  }

  n_total <- nrow(rat)
  sheet   <- substr(lbl, 1, 31)
  addWorksheet(wb, sheet)

  cur_row <- 1

  # ── Group title ──────────────────────────────────────────────────────────────
  writeData(wb, sheet,
            data.frame(x = paste0(display, "   (N = ", n_total, ")")),
            startRow = cur_row, colNames = FALSE)
  addStyle(wb, sheet,
           createStyle(textDecoration = "bold", fontSize = 13, fontColour = "#1a3a5c"),
           rows = cur_row, cols = 1)
  cur_row <- cur_row + 2


  # ════════════════════════════════════════════════════════════════════════════
  # SECTION 1 — Binary Satisfaction (PRIMARY)
  # ════════════════════════════════════════════════════════════════════════════
  writeData(wb, sheet,
            data.frame(x = "SECTION 1 — Satisfaction by Question  (Satisfied = ratings 3–5 | Not Satisfied = ratings 1–2)"),
            startRow = cur_row, colNames = FALSE)
  addStyle(wb, sheet, st_section, rows = cur_row, cols = 1)
  cur_row <- cur_row + 1

  bin_tbl <- build_binary_table(rat, item_cols)

  # Colour-code Pct_Satisfied column values in the data frame for conditional fill
  bin_display <- bin_tbl %>%
    mutate(
      Question = str_trunc(Question, 80),
      Pct_Satisfied    = paste0(Pct_Satisfied, "%"),
      Pct_NotSatisfied = paste0(Pct_NotSatisfied, "%")
    )

  writeData(wb, sheet, bin_display, startRow = cur_row, rowNames = FALSE)
  addStyle(wb, sheet, st_primary,
           rows = cur_row, cols = 1:ncol(bin_display), gridExpand = TRUE)

  # Conditional fill on Pct_Satisfied column (col 4 = Pct_Satisfied)
  pct_vals <- bin_tbl$Pct_Satisfied
  for (i in seq_along(pct_vals)) {
    if (!is.na(pct_vals[i])) {
      fill_st <- if (pct_vals[i] >= 75) st_sat_good else
                 if (pct_vals[i] >= 50) st_sat_warn  else st_sat_bad
      addStyle(wb, sheet, fill_st,
               rows = cur_row + i, cols = 4, gridExpand = FALSE)
    }
  }

  # Stripe
  for (r in seq(cur_row + 1, cur_row + nrow(bin_display))) {
    if ((r - cur_row) %% 2 == 0)
      addStyle(wb, sheet, st_stripe, rows = r,
               cols = c(1,2,3,5,6), gridExpand = TRUE)
  }

  setColWidths(wb, sheet, cols = 1:6, widths = c(75, 8, 15, 17, 15, 18))
  cur_row <- cur_row + nrow(bin_display) + 3


  # ════════════════════════════════════════════════════════════════════════════
  # SECTION 2 — Sex-Stratified Binary  (if applicable)
  # ════════════════════════════════════════════════════════════════════════════
  has_sex    <- "sex" %in% names(rat) && any(!is.na(rat$sex))
  sex_counts <- if (has_sex) table(rat$sex[!is.na(rat$sex)]) else NULL
  do_strat   <- has_sex &&
    !lbl %in% c("PBFW", "FGD_Community") &&   # PBFW all-female; FGD group resp.
    all(c("Male","Female") %in% names(sex_counts)) &&
    sex_counts["Male"] >= SEX_STRAT_MIN &&
    sex_counts["Female"] >= SEX_STRAT_MIN

  if (do_strat) {
    n_m <- sex_counts["Male"]
    n_f <- sex_counts["Female"]

    writeData(wb, sheet,
              data.frame(x = paste0(
                "SECTION 2 — Satisfaction Stratified by Sex",
                "   (Male n = ", n_m, " | Female n = ", n_f, ")")),
              startRow = cur_row, colNames = FALSE)
    addStyle(wb, sheet, st_section, rows = cur_row, cols = 1)
    cur_row <- cur_row + 1

    male_tbl   <- build_binary_table(rat %>% filter(sex == "Male"),   item_cols)
    female_tbl <- build_binary_table(rat %>% filter(sex == "Female"), item_cols)

    strat_tbl <- male_tbl %>%
      select(Question,
             Male_N           = N,
             Male_Pct_Sat     = Pct_Satisfied,
             Male_Pct_NotSat  = Pct_NotSatisfied) %>%
      left_join(
        female_tbl %>%
          select(Question,
                 Female_N          = N,
                 Female_Pct_Sat    = Pct_Satisfied,
                 Female_Pct_NotSat = Pct_NotSatisfied),
        by = "Question"
      ) %>%
      mutate(
        Diff_Pct_Sat = round(Male_Pct_Sat - Female_Pct_Sat, 1),
        Question = str_trunc(Question, 80)
      ) %>%
      arrange(desc((Male_Pct_Sat + Female_Pct_Sat) / 2))

    # Write with split male/female headers
    # Header row 1: Male block / Female block
    male_header <- data.frame(
      Question = "Question",
      Male_N = paste0("Male (n=",n_m,")"), Male_Pct_Sat = "", Male_Pct_NotSat = "",
      Female_N = paste0("Female (n=",n_f,")"), Female_Pct_Sat = "", Female_Pct_NotSat = "",
      Diff = "Diff (M-F)"
    )
    writeData(wb, sheet, strat_tbl, startRow = cur_row, rowNames = FALSE)
    addStyle(wb, sheet, st_male,
             rows = cur_row, cols = 1:4, gridExpand = TRUE)
    addStyle(wb, sheet, st_female,
             rows = cur_row, cols = 5:7, gridExpand = TRUE)
    addStyle(wb, sheet, st_primary,
             rows = cur_row, cols = 8, gridExpand = TRUE)

    # Conditional fill on Male_Pct_Sat (col 3) and Female_Pct_Sat (col 6)
    for (i in seq_len(nrow(strat_tbl))) {
      for (col_idx in c(3, 6)) {
        val <- if (col_idx == 3) strat_tbl$Male_Pct_Sat[i] else strat_tbl$Female_Pct_Sat[i]
        if (!is.na(val)) {
          fill_st <- if (val >= 75) st_sat_good else
                     if (val >= 50) st_sat_warn  else st_sat_bad
          addStyle(wb, sheet, fill_st, rows = cur_row + i, cols = col_idx)
        }
      }
    }

    for (r in seq(cur_row + 1, cur_row + nrow(strat_tbl))) {
      if ((r - cur_row) %% 2 == 0)
        addStyle(wb, sheet, st_stripe, rows = r,
                 cols = c(1,2,4,5,7,8), gridExpand = TRUE)
    }
    setColWidths(wb, sheet, cols = 1:8,
                 widths = c(75, 8, 15, 17, 8, 15, 17, 12))
    cur_row <- cur_row + nrow(strat_tbl) + 3

    message("  ✓ Sex stratification written (M=", n_m, " F=", n_f, ")")
  } else if (has_sex && lbl == "PBFW") {
    writeData(wb, sheet,
              data.frame(x = "SECTION 2 — Sex Stratification: Not applicable (PBFW group is all-female by definition)"),
              startRow = cur_row, colNames = FALSE)
    addStyle(wb, sheet, st_section, rows = cur_row, cols = 1)
    cur_row <- cur_row + 3
  }


  # ════════════════════════════════════════════════════════════════════════════
  # SECTION 3 — Raw 5-Level Likert (supplementary reference)
  # ════════════════════════════════════════════════════════════════════════════
  writeData(wb, sheet,
            data.frame(x = "SECTION 3 — Supplementary: Raw 5-Level Likert Distribution"),
            startRow = cur_row, colNames = FALSE)
  addStyle(wb, sheet, st_section, rows = cur_row, cols = 1)
  cur_row <- cur_row + 1

  raw5_tbl <- build_5level_table(rat, item_cols) %>%
    mutate(Question = str_trunc(Question, 80))

  writeData(wb, sheet, raw5_tbl, startRow = cur_row, rowNames = FALSE)
  addStyle(wb, sheet, st_raw,
           rows = cur_row, cols = 1:ncol(raw5_tbl), gridExpand = TRUE)
  for (r in seq(cur_row + 1, cur_row + nrow(raw5_tbl))) {
    if ((r - cur_row) %% 2 == 0)
      addStyle(wb, sheet, st_stripe, rows = r,
               cols = 1:ncol(raw5_tbl), gridExpand = TRUE)
  }
  setColWidths(wb, sheet, cols = 1:ncol(raw5_tbl),
               widths = c(75, 7, rep(9, ncol(raw5_tbl) - 3), 9))
  cur_row <- cur_row + nrow(raw5_tbl) + 3

  message("  ✓ Sheet written: ", sheet)


  # ── Collect summary row ──────────────────────────────────────────────────────
  all_vals   <- unlist(rat[, item_cols], use.names = FALSE)
  all_vals   <- all_vals[!is.na(all_vals)]
  grand_mean <- round(mean(as.numeric(unlist(rat[, item_cols])), na.rm = TRUE), 2)
  pct_sat    <- round(mean(all_vals >= 3) * 100, 1)
  pct_pca    <- if ("pca_label" %in% names(rat))
    round(mean(rat$pca_label == 1, na.rm = TRUE) * 100, 1) else NA_real_

  q_means  <- colMeans(rat[, item_cols, drop = FALSE] %>%
                         mutate(across(everything(), as.numeric)),
                       na.rm = TRUE)
  q_sorted <- sort(q_means, decreasing = TRUE)
  top3     <- paste(str_trunc(names(q_sorted)[1:min(3, length(q_sorted))], 50),
                    collapse = " | ")
  bot3     <- paste(str_trunc(rev(names(q_sorted))[1:min(3, length(q_sorted))], 50),
                    collapse = " | ")

  overview_rows[[lbl]] <- data.frame(
    Group                   = display,
    N_respondents           = n_total,
    N_rating_items          = length(item_cols),
    Mean_score_1to5         = grand_mean,
    Pct_Satisfied_collapsed = paste0(pct_sat, "%"),
    Pct_Satisfied_PCA       = ifelse(is.na(pct_pca), "N/A", paste0(pct_pca, "%")),
    Sex_stratified          = ifelse(do_strat, "Yes", "No"),
    Top_3_highest_rated     = top3,
    Top_3_lowest_rated      = bot3,
    stringsAsFactors        = FALSE
  )
}


# ── Overview sheet ─────────────────────────────────────────────────────────────
addWorksheet(wb, "Overview")
overview_df <- bind_rows(overview_rows)

writeData(wb, "Overview",
          data.frame(x = "ACC CLM Dodoma — HIV Service Satisfaction Survey: Cross-Group Overview"),
          startRow = 1, colNames = FALSE)
addStyle(wb, "Overview",
         createStyle(textDecoration = "bold", fontSize = 13, fontColour = "#1a3a5c"),
         rows = 1, cols = 1)

writeData(wb, "Overview", overview_df, startRow = 3)
addStyle(wb, "Overview", st_primary, rows = 3,
         cols = 1:ncol(overview_df), gridExpand = TRUE)

# Colour-code Pct_Satisfied_collapsed column (col 5)
pct_clean <- as.numeric(gsub("%", "", overview_df$Pct_Satisfied_collapsed))
for (i in seq_along(pct_clean)) {
  if (!is.na(pct_clean[i])) {
    fill_st <- if (pct_clean[i] >= 75) st_sat_good else
               if (pct_clean[i] >= 50) st_sat_warn  else st_sat_bad
    addStyle(wb, "Overview", fill_st, rows = 3 + i, cols = 5)
  }
}

for (r in seq(4, 3 + nrow(overview_df))) {
  if (r %% 2 == 0)
    addStyle(wb, "Overview", st_stripe, rows = r,
             cols = 1:ncol(overview_df), gridExpand = TRUE)
}
setColWidths(wb, "Overview", cols = 1:ncol(overview_df),
             widths = c(38, 14, 14, 16, 22, 20, 16, 60, 60))

# Move Overview to the front
worksheetOrder(wb) <- c(
  which(names(wb) == "Overview"),
  which(names(wb) != "Overview")
)

saveWorkbook(wb, file.path(OUTPUT_DIR, "frequency_tables.xlsx"), overwrite = TRUE)
message("\n✓ frequency_tables.xlsx written to ", OUTPUT_DIR)
message("✓ 03_frequency_tables.R complete.")
