# =============================================================================
# 02_pca.R
# ACC CLM Dodoma — HIV Service Satisfaction Survey
# -----------------------------------------------------------------------------
# Runs PCA per respondent group on the cleaned Likert ratings.
# The first principal component (PC1) captures overall satisfaction variance.
# PC1 scores are dichotomised at the median to produce a two-level binary
# label: 1 = Satisfied, 0 = Not Satisfied.
#
# Groups with fewer than PCA_MIN_N (5) complete observations fall back to a
# mean-score threshold (≥ 3.0 = Satisfied) because PCA is unreliable at that
# sample size; this is clearly flagged in the output.
#
# Outputs
#   • pca_results.xlsx  — PC1 loadings, binary labels, and variance explained
#   • pca_plots/        — scree plot + PC1 score distribution per group
#   • cleaned_results.rds updated with pca_label column in each ratings frame
# =============================================================================

source("/Users/matemba/Documents/Claude/Projects/acc_analysis/scripts/00_setup.R")

# Load cleaned data produced by 01_cleaning.R
results <- readRDS(file.path(OUTPUT_DIR, "cleaned_results.rds"))


# ── PCA function for one group ─────────────────────────────────────────────────
run_pca_group <- function(group_label, result_entry) {

  ratings <- result_entry$ratings
  hi_miss  <- result_entry$high_miss_cols   # questions flagged >50% missing

  # Select only numeric rating columns (drop 'group', drop high-miss items)
  item_cols <- setdiff(names(ratings), c("group", hi_miss))
  item_mat  <- ratings[, item_cols, drop = FALSE] %>%
    mutate(across(everything(), as.numeric))

  # Rows with at least one valid rating
  complete_rows <- which(rowSums(!is.na(item_mat)) > 0)
  n_complete    <- length(complete_rows)
  n_items       <- ncol(item_mat)

  message("\n── ", group_label, " (n = ", n_complete, ", items = ", n_items, ")")

  # ── Fallback: too few rows for stable PCA ────────────────────────────────────
  if (n_complete < PCA_MIN_N) {
    message("  ⚠ n < ", PCA_MIN_N, " — using mean-score threshold (≥ 3.0 = Satisfied)")

    row_means <- rowMeans(item_mat, na.rm = TRUE)
    pca_label <- ifelse(is.na(row_means), NA_integer_,
                        ifelse(row_means >= 3.0, 1L, 0L))
    label_src <- "mean_threshold"

    return(list(
      group        = group_label,
      label_source = label_src,
      pca_label    = pca_label,
      pc1_scores   = rep(NA_real_, nrow(ratings)),
      var_explained= NA_real_,
      loadings     = NULL,
      n_pca        = n_complete
    ))
  }

  # ── Column-wise mean imputation (for PCA only; originals untouched) ──────────
  item_imputed <- item_mat
  for (col in names(item_imputed)) {
    col_mean <- mean(item_imputed[[col]], na.rm = TRUE)
    item_imputed[[col]][is.na(item_imputed[[col]])] <- col_mean
  }

  # Drop columns that are still all-NA after imputation (constant columns break PCA)
  item_imputed <- item_imputed[, apply(item_imputed, 2, sd) > 0, drop = FALSE]

  if (ncol(item_imputed) < 2) {
    message("  ⚠ Fewer than 2 usable items after variance filtering — mean fallback")
    row_means <- rowMeans(item_mat, na.rm = TRUE)
    pca_label <- ifelse(is.na(row_means), NA_integer_,
                        ifelse(row_means >= 3.0, 1L, 0L))
    return(list(
      group        = group_label,
      label_source = "mean_threshold",
      pca_label    = pca_label,
      pc1_scores   = rep(NA_real_, nrow(ratings)),
      var_explained= NA_real_,
      loadings     = NULL,
      n_pca        = n_complete
    ))
  }

  # ── Run PCA ───────────────────────────────────────────────────────────────────
  pca <- prcomp(item_imputed, scale. = TRUE, center = TRUE)

  var_exp   <- summary(pca)$importance[2, 1]  # proportion of variance by PC1
  loadings  <- pca$rotation[, 1, drop = FALSE]
  pc1_raw   <- pca$x[, 1]

  # Ensure PC1 is oriented so that HIGHER score = MORE satisfied.
  # If the median loading is negative, flip the sign.
  if (median(loadings) < 0) {
    pc1_raw   <- -pc1_raw
    loadings  <- -loadings
    message("  → PC1 sign flipped (median loading was negative)")
  }

  message(sprintf("  PC1 explains %.1f%% of variance", var_exp * 100))

  # ── Dichotomise PC1 at its median ─────────────────────────────────────────────
  # Median split: respondents above the median are classified as Satisfied (1),
  # those at or below as Not Satisfied (0).
  pc1_median <- median(pc1_raw, na.rm = TRUE)
  pca_label  <- ifelse(pc1_raw > pc1_median, 1L, 0L)

  n_satisfied    <- sum(pca_label == 1, na.rm = TRUE)
  n_notsatisfied <- sum(pca_label == 0, na.rm = TRUE)
  message(sprintf("  Binary split (median = %.3f): Satisfied = %d | Not Satisfied = %d",
                  pc1_median, n_satisfied, n_notsatisfied))

  # ── Scree plot ────────────────────────────────────────────────────────────────
  p_scree <- fviz_eig(
    pca, addlabels = TRUE, ylim = c(0, 100),
    title = paste0("Scree Plot — ", GROUP_DISPLAY[group_label])
  ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))

  ggsave(
    filename = file.path(PLOT_DIR, paste0(group_label, "_scree.png")),
    plot     = p_scree,
    width    = 7, height = 4, dpi = 150
  )

  # ── PC1 score distribution ────────────────────────────────────────────────────
  score_df <- data.frame(
    pc1   = pc1_raw,
    level = factor(ifelse(pca_label == 1, "Satisfied", "Not Satisfied"),
                   levels = c("Not Satisfied", "Satisfied"))
  )

  p_dist <- ggplot(score_df, aes(x = pc1, fill = level)) +
    geom_histogram(bins = max(5, n_complete %/% 3), colour = "white", alpha = 0.85) +
    geom_vline(xintercept = pc1_median, linetype = "dashed", colour = "grey30") +
    scale_fill_manual(values = BINARY_COLOURS) +
    labs(
      title    = paste0("PC1 Score Distribution — ", GROUP_DISPLAY[group_label]),
      subtitle = sprintf("Median cut-point = %.3f  |  PC1 explains %.1f%% of variance",
                         pc1_median, var_exp * 100),
      x = "PC1 Score", y = "Count", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "top")

  ggsave(
    filename = file.path(PLOT_DIR, paste0(group_label, "_pc1_dist.png")),
    plot     = p_dist,
    width    = 7, height = 4, dpi = 150
  )

  list(
    group        = group_label,
    label_source = "PCA_median_split",
    pca_label    = pca_label,
    pc1_scores   = pc1_raw,
    var_explained= round(var_exp * 100, 1),
    loadings     = data.frame(
      item    = rownames(loadings),
      loading = round(as.numeric(loadings), 4),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(loading)),
    n_pca        = n_complete
  )
}


# ── Run PCA for every group ───────────────────────────────────────────────────
pca_outputs <- list()
for (lbl in names(results)) {
  pca_outputs[[lbl]] <- run_pca_group(lbl, results[[lbl]])
}


# ── Attach pca_label back into the ratings data frames ────────────────────────
for (lbl in names(results)) {
  pca_lab <- pca_outputs[[lbl]]$pca_label
  # pca_label length matches nrow(ratings); NAs for rows with no valid response
  results[[lbl]]$ratings$pca_label    <- as.integer(pca_lab)
  results[[lbl]]$ratings$pca_label_lv <- ifelse(
    is.na(pca_lab), NA_character_,
    ifelse(pca_lab == 1, "Satisfied", "Not Satisfied")
  )
}

# Save updated results
saveRDS(results, file.path(OUTPUT_DIR, "cleaned_results.rds"))
message("\n✓ PCA labels attached and RDS re-saved.")


# ── Build the Excel output ────────────────────────────────────────────────────
wb <- createWorkbook()

# ── Sheet 1: Summary — one row per group ─────────────────────────────────────
addWorksheet(wb, "Summary")

summary_rows <- purrr::map_dfr(names(pca_outputs), function(lbl) {
  po <- pca_outputs[[lbl]]
  data.frame(
    Group          = GROUP_DISPLAY[lbl],
    N_analysed     = po$n_pca,
    Method         = po$label_source,
    PC1_var_expl   = ifelse(is.na(po$var_explained), "N/A (fallback)",
                            paste0(po$var_explained, "%")),
    N_Satisfied    = sum(po$pca_label == 1, na.rm = TRUE),
    N_NotSatisfied = sum(po$pca_label == 0, na.rm = TRUE),
    Pct_Satisfied  = ifelse(
      po$n_pca > 0,
      paste0(round(mean(po$pca_label == 1, na.rm = TRUE) * 100, 1), "%"),
      "N/A"
    ),
    stringsAsFactors = FALSE
  )
})

writeData(wb, "Summary", summary_rows)

# Style header row
header_style <- createStyle(textDecoration = "bold", fgFill = "#4575b4",
                             fontColour = "white", halign = "center")
addStyle(wb, "Summary", header_style, rows = 1, cols = 1:ncol(summary_rows),
         gridExpand = TRUE)
setColWidths(wb, "Summary", cols = 1:ncol(summary_rows),
             widths = c(35, 12, 22, 15, 14, 16, 15))

# ── Sheets 2-N: PC1 loadings per group ───────────────────────────────────────
for (lbl in names(pca_outputs)) {
  po <- pca_outputs[[lbl]]
  if (is.null(po$loadings)) next

  sheet_name <- substr(paste0("Load_", lbl), 1, 31)  # Excel 31-char limit
  addWorksheet(wb, sheet_name)

  load_df <- po$loadings %>%
    mutate(
      Direction = ifelse(loading > 0, "↑ Drives satisfaction", "↓ Reduces satisfaction"),
      Group     = GROUP_DISPLAY[lbl]
    )

  writeData(wb, sheet_name, load_df)
  addStyle(wb, sheet_name, header_style,
           rows = 1, cols = 1:ncol(load_df), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = 1:ncol(load_df), widths = c(55, 10, 25, 35))
}

saveWorkbook(wb, file.path(OUTPUT_DIR, "pca_results.xlsx"), overwrite = TRUE)
message("✓ pca_results.xlsx written to ", OUTPUT_DIR)
message("✓ Scree plots and PC1 distributions saved to ", PLOT_DIR)
message("\n✓ 02_pca.R complete.")
