# =============================================================================
# 04_maoni_rubric.R
# ACC CLM Dodoma — HIV Service Satisfaction Survey
# -----------------------------------------------------------------------------
# Extracts all free-text "Maoni/maelezo" responses, links each response to its
# respondent's PCA satisfaction label, codes 8 themes AND a positive/negative
# sentiment layer, then produces:
#
#   maoni_rubric.xlsx   — coded table + per-group theme breakdown showing what
#                          SATISFIED vs NOT-SATISFIED respondents say per theme
#   maoni_summary.docx  — analytical narrative: for each theme, an interpretation
#                          of the public's sentiment (what is working, what is
#                          not, representative voices from each side)
#
# Run AFTER 01_cleaning.R and 02_pca.R.
# =============================================================================

source("/Users/matemba/Documents/Claude/Projects/acc_analysis/scripts/00_setup.R")

results <- readRDS(file.path(OUTPUT_DIR, "cleaned_results.rds"))


# ── 1. Thematic rubric (8 themes) ─────────────────────────────────────────────
THEMES <- list(
  A_ServiceQuality = c(
    "wakarimu", "heshima", "tabia", "mtazamo", "lugha", "watoa\\s*huduma",
    "ninaheshimika", "kusikiliza", "kuheshimu", "huruma", "uaminifu",
    "vizuri", "ubora", "ufanisi", "huduma\\s+nzuri", "wanasikiliza",
    "wanashughulika", "wanatuambia", "wanatuhusu"
  ),
  B_AccessAvailability = c(
    "upatikanaji", "uwepo", "zinapatikana", "ipo", "vifaa", "dawa\\s+(zi)?po",
    "kondomu", "PrEP", "PEP", "ARV", "dawa\\s+za", "vituo", "kupatikana",
    "kufika", "umbali", "mbali", "karibu", "hawapati", "hazipo", "hakuna\\s+dawa",
    "upungufu\\s+wa\\s+dawa", "vifaa\\s+havipo", "hazitoshi"
  ),
  C_WaitingTimeFacilityFlow = c(
    "muda\\s+wa\\s+kusubiria", "kusubiria", "foleni", "muda\\s+mrefu",
    "haraka", "saa\\s+nyingi", "miadi", "ratiba", "mtiririko",
    "kuchelewa", "majibu.*yanachelewa", "majibu.*hayaji", "hawafiki\\s+kwa\\s+wakati",
    "muda\\s+wa\\s+miadi", "miadi\\s+inazingatiwa"
  ),
  D_StigmaDiscrimination = c(
    "unyanyapaa", "ubaguzi", "aibu", "kutengwa", "kudharauliwa",
    "heshima\\s+ya", "nyanyapaa", "haki\\s+sawa", "kutokuwa\\s+na\\s+ubaguzi",
    "kukubalika", "wanakubaliana", "hawanyanyapai", "unyanyapaa\\s+umepungua",
    "bado\\s+kuna\\s+unyanyapaa"
  ),
  E_EconomicLogistical = c(
    "gharama", "nauli", "fedha", "pesa", "usafiri", "gari", "pikipiki",
    "umbali\\s+wa", "mbali\\s+sana", "njia\\s+mbaya", "barabara",
    "kiuchumi", "uchumi", "chakula", "hawana\\s+fedha", "ada",
    "hawana\\s+uwezo", "ugumu\\s+wa\\s+kiuchumi"
  ),
  F_TreatmentAdherence = c(
    "ARV", "dawa\\s+za\\s+kuzuia", "matumizi\\s+ya\\s+dawa", "tiba",
    "ushauri\\s+wa\\s+dawa", "matumizi\\s+ya\\s+tiba", "kufuata\\s+tiba",
    "ufuasi", "kutumia\\s+dawa", "dawa\\s+hazipo", "PEP", "PrEP",
    "DBS", "viral\\s+load", "wingi\\s+wa\\s+virusi", "kipimo",
    "ushauri\\s+nasaha", "kukumbushwa", "dawa\\s+zinapatikana"
  ),
  G_CommunitySupportFamily = c(
    "familia", "wenza", "ndugu", "jamaa", "msaada\\s+wa\\s+jamii",
    "jamii", "CHW", "mama\\s+vinara", "waelimishaji\\s+rika",
    "vikundi", "kikundi", "kushirikiana", "mzazi", "wazazi", "walezi",
    "ushiriki\\s+wa\\s+familia", "msaada\\s+unaotolewa", "wana\\s+msaada"
  ),
  H_SuggestionsRecommendations = c(
    "inapaswa", "waboreshe", "kuboreshwa", "mapendekezo", "pendekezo",
    "inabidi", "lazima", "serikali\\s+i", "wahakikishe",
    "waongeze", "waajiri", "wapeleke", "tunapendekeza", "tunahitaji",
    "hitaji", "ombi", "tunaomba", "tunapenda",
    "kuongeza\\s+idadi", "watoa\\s+huduma\\s+zaidi", "waboreshe\\s+huduma"
  )
)

THEME_LABELS <- c(
  A_ServiceQuality          = "A – Service Quality & Provider Behaviour",
  B_AccessAvailability      = "B – Access & Availability of Services",
  C_WaitingTimeFacilityFlow = "C – Waiting Time & Facility Flow",
  D_StigmaDiscrimination    = "D – Stigma & Discrimination",
  E_EconomicLogistical      = "E – Economic & Logistical Barriers",
  F_TreatmentAdherence      = "F – Treatment & Medication Adherence",
  G_CommunitySupportFamily  = "G – Community & Family Support",
  H_SuggestionsRecommendations = "H – Suggestions & Recommendations"
)

# ── 2. Sentiment lexicon ───────────────────────────────────────────────────────
# POSITIVE: words expressing satisfaction, appreciation, or praise
POSITIVE_WORDS <- c(
  "nzuri", "vizuri", "ridhika", "inasaidia", "inafanya\\s+kazi", "ufanisi",
  "nimefurahi", "napenda", "wakarimu", "wanasaidia", "imeboreshwa",
  "tunashukuru", "wapongezwa", "inafaa", "inakidhi", "inatosha",
  "wanasikiliza", "wanajali", "wanahusu", "mazuri", "pongezi",
  "salama", "mzuri", "mzuri\\s+sana", "hakuna\\s+tatizo",
  "zinapatikana\\s+kwa\\s+wakati", "zinapatikana\\s+kwa\\s+wingi"
)

# NEGATIVE: words expressing dissatisfaction, concern, or criticism
NEGATIVE_WORDS <- c(
  "mbaya", "tatizo", "changamoto", "inabidi", "inapaswa", "waboreshe",
  "hawana", "ukosefu", "hawafanyi", "upungufu", "kikwazo", "shida",
  "hakuna", "wachache", "kuchelewa", "hawafiki", "vigumu", "ugumu",
  "sijaridhika", "hatujaridhika", "siridhiki", "hakuridhika",
  "hawajafunzwa", "hawaelewi", "wanakataa", "wanapuuza",
  "hazitoshi", "hazipo", "hawapati", "bado\\s+kuna", "inabidi\\s+iboreshe",
  "inabidi\\s+wongeze", "tunahitaji\\s+zaidi", "waongeze"
)

classify_sentiment <- function(text_vec) {
  txt <- str_to_lower(as.character(text_vec))
  pos_pat <- paste(POSITIVE_WORDS, collapse = "|")
  neg_pat <- paste(NEGATIVE_WORDS, collapse = "|")

  pos_hits <- str_count(txt, regex(pos_pat, ignore_case = TRUE))
  neg_hits <- str_count(txt, regex(neg_pat, ignore_case = TRUE))

  dplyr::case_when(
    pos_hits > 0 & neg_hits == 0 ~ "Positive",
    neg_hits > 0 & pos_hits == 0 ~ "Negative",
    pos_hits > 0 & neg_hits > 0  ~ "Mixed",
    TRUE                          ~ "Neutral"
  )
}


# ── 3. Collect all maoni and link to PCA labels ───────────────────────────────
all_maoni <- purrr::map_dfr(names(results), function(lbl) {
  maoni_df  <- results[[lbl]]$maoni
  ratings_df <- results[[lbl]]$ratings

  maoni_cols <- setdiff(names(maoni_df), c("respondent_id", "group"))
  if (length(maoni_cols) == 0) return(NULL)

  # Pull PCA labels (if available) keyed by respondent_id
  pca_lookup <- ratings_df %>%
    select(respondent_id,
           pca_label    = any_of("pca_label"),
           pca_label_lv = any_of("pca_label_lv"))

  maoni_long <- maoni_df %>%
    pivot_longer(cols = all_of(maoni_cols),
                 names_to  = "maoni_ref",
                 values_to = "maoni_text") %>%
    filter(!is.na(maoni_text) & nchar(trimws(maoni_text)) > 3) %>%
    left_join(pca_lookup, by = "respondent_id")

  # Ensure pca_label_lv exists even if column was absent
  if (!"pca_label_lv" %in% names(maoni_long))
    maoni_long$pca_label_lv <- NA_character_

  maoni_long
})

message("Total non-empty maoni responses: ", nrow(all_maoni))


# ── 4. Apply theme rubric and sentiment ───────────────────────────────────────
apply_rubric <- function(text_vec) {
  txt <- str_to_lower(as.character(text_vec))
  purrr::map_dfc(names(THEMES), function(tn) {
    pat  <- paste(THEMES[[tn]], collapse = "|")
    flag <- as.integer(str_detect(txt, regex(pat, ignore_case = TRUE)))
    tibble(!!tn := flag)
  })
}

theme_flags <- apply_rubric(all_maoni$maoni_text)
theme_flags$U_Unclassified <- as.integer(rowSums(theme_flags) == 0)

coded_maoni <- bind_cols(all_maoni, theme_flags) %>%
  mutate(
    sentiment  = classify_sentiment(maoni_text),
    group_disp = GROUP_DISPLAY[group]
  )

# Verify theme counts are non-zero
message("Theme totals:")
print(sort(colSums(theme_flags[, names(THEMES), drop = FALSE]), decreasing = TRUE))
message("Unclassified: ", sum(theme_flags$U_Unclassified))


# ── 5. Build maoni_rubric.xlsx ────────────────────────────────────────────────
wb2 <- createWorkbook()

hdr_st  <- createStyle(textDecoration = "bold", fgFill = "#1a3a5c",
                       fontColour = "white", halign = "center", wrapText = TRUE)
stripe2 <- createStyle(fgFill = "#f0f4fc")
pos_st  <- createStyle(fgFill = "#d4edda")   # green  — positive sentiment
neg_st  <- createStyle(fgFill = "#f8d7da")   # red    — negative sentiment
mix_st  <- createStyle(fgFill = "#fff3cd")   # amber  — mixed
neu_st  <- createStyle(fgFill = "#e2e3e5")   # grey   — neutral

# Sheet 1: Full coded response table
addWorksheet(wb2, "All_Responses")
out_tbl <- coded_maoni %>%
  select(Group = group_disp, maoni_ref, maoni_text, sentiment,
         PCA_level = pca_label_lv,
         all_of(names(THEMES)), U_Unclassified)

writeData(wb2, "All_Responses", out_tbl)
addStyle(wb2, "All_Responses", hdr_st,
         rows = 1, cols = 1:ncol(out_tbl), gridExpand = TRUE)

# Colour-code sentiment column (col 4)
sent_map <- c(Positive = pos_st, Negative = neg_st, Mixed = mix_st, Neutral = neu_st)
for (i in seq_len(nrow(out_tbl))) {
  s <- out_tbl$sentiment[i]
  if (!is.na(s) && s %in% names(sent_map))
    addStyle(wb2, "All_Responses", sent_map[[s]], rows = i + 1, cols = 4)
}
# Stripe
for (r in seq(2, nrow(out_tbl) + 1)) {
  if (r %% 2 == 0)
    addStyle(wb2, "All_Responses", stripe2, rows = r,
             cols = c(1,2,3,5), gridExpand = TRUE)
}
setColWidths(wb2, "All_Responses", cols = 1:ncol(out_tbl),
             widths = c(35, 10, 80, 12, 16, rep(5, length(THEMES) + 1)))


# Sheet 2: Theme breakdown — satisfied vs not satisfied respondents
addWorksheet(wb2, "Theme_by_Satisfaction")

breakdown_rows <- purrr::map_dfr(names(THEMES), function(tn) {
  theme_sub <- coded_maoni %>% filter(.data[[tn]] == 1)

  n_total   <- nrow(theme_sub)
  n_pos_sent <- sum(theme_sub$sentiment == "Positive", na.rm = TRUE)
  n_neg_sent <- sum(theme_sub$sentiment %in% c("Negative","Mixed"), na.rm = TRUE)

  pca_sub <- theme_sub %>% filter(!is.na(pca_label_lv))
  n_sat_pca  <- sum(pca_sub$pca_label_lv == "Satisfied",     na.rm = TRUE)
  n_not_pca  <- sum(pca_sub$pca_label_lv == "Not Satisfied", na.rm = TRUE)

  data.frame(
    Theme                 = THEME_LABELS[tn],
    Total_responses       = n_total,
    Pct_Positive_sentiment= ifelse(n_total>0, round(n_pos_sent/n_total*100,1), NA),
    Pct_Negative_Mixed    = ifelse(n_total>0, round(n_neg_sent/n_total*100,1), NA),
    From_Satisfied_PCA    = n_sat_pca,
    From_NotSatisfied_PCA = n_not_pca,
    Pct_from_Satisfied    = ifelse((n_sat_pca+n_not_pca)>0,
                                   round(n_sat_pca/(n_sat_pca+n_not_pca)*100,1), NA),
    stringsAsFactors      = FALSE
  )
})

# Per-group breakdown
grp_breakdown <- purrr::map_dfr(names(THEMES), function(tn) {
  coded_maoni %>%
    filter(.data[[tn]] == 1) %>%
    count(group_disp, sentiment, name = "n") %>%
    mutate(theme = THEME_LABELS[tn])
}) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  rename(Group = group_disp, Theme = theme) %>%
  mutate(Total = rowSums(select(., any_of(c("Positive","Negative","Mixed","Neutral"))),
                         na.rm = TRUE)) %>%
  arrange(Theme, desc(Total))

writeData(wb2, "Theme_by_Satisfaction",
          data.frame(x = "Summary: Theme Responses by Sentiment and PCA Satisfaction Group"),
          startRow = 1, colNames = FALSE)
addStyle(wb2, "Theme_by_Satisfaction",
         createStyle(textDecoration = "bold", fontSize = 12, fontColour = "#1a3a5c"),
         rows = 1, cols = 1)
writeData(wb2, "Theme_by_Satisfaction", breakdown_rows, startRow = 3)
addStyle(wb2, "Theme_by_Satisfaction", hdr_st,
         rows = 3, cols = 1:ncol(breakdown_rows), gridExpand = TRUE)
for (r in seq(4, 3 + nrow(breakdown_rows))) {
  if (r %% 2 == 0)
    addStyle(wb2, "Theme_by_Satisfaction", stripe2, rows = r,
             cols = 1:ncol(breakdown_rows), gridExpand = TRUE)
}
setColWidths(wb2, "Theme_by_Satisfaction",
             cols = 1:ncol(breakdown_rows),
             widths = c(40, 14, 22, 22, 20, 22, 22))

start_grp <- nrow(breakdown_rows) + 6
writeData(wb2, "Theme_by_Satisfaction",
          data.frame(x = "Per-Group Theme Breakdown by Sentiment"),
          startRow = start_grp - 1, colNames = FALSE)
writeData(wb2, "Theme_by_Satisfaction", grp_breakdown, startRow = start_grp)
addStyle(wb2, "Theme_by_Satisfaction", hdr_st,
         rows = start_grp, cols = 1:ncol(grp_breakdown), gridExpand = TRUE)

saveWorkbook(wb2, file.path(OUTPUT_DIR, "maoni_rubric.xlsx"), overwrite = TRUE)
message("✓ maoni_rubric.xlsx written.")


# ── 6. Narrative Word document ────────────────────────────────────────────────
doc <- read_docx()

# ── Title page ────────────────────────────────────────────────────────────────
doc <- doc %>%
  body_add_par("Public Opinion Analysis — HIV Services, Dodoma Region",
               style = "heading 1") %>%
  body_add_par(paste0(
    "This document presents a thematic and sentiment analysis of free-text opinions ",
    "(maoni) collected across 12 respondent groups in the ACC CLM Dodoma HIV Service ",
    "Satisfaction Survey. A total of ", nrow(coded_maoni), " substantive responses ",
    "were analysed. Each response has been coded to one or more of eight themes and ",
    "classified by sentiment (Positive, Negative, Mixed, or Neutral) to show not just ",
    "what the public talks about, but whether they are satisfied or dissatisfied with ",
    "that aspect of HIV services."
  ), style = "Normal") %>%
  body_add_par("Data from 2026-05-18 was excluded from all analysis.", style = "Normal") %>%
  body_add_par("", style = "Normal")

# Sentinel helper: pick representative quotes
pick_quotes <- function(df, sentiment_filter = NULL, n = 3) {
  sub <- df
  if (!is.null(sentiment_filter))
    sub <- sub %>% filter(sentiment %in% sentiment_filter)
  sub %>%
    filter(nchar(trimws(maoni_text)) > 20) %>%
    distinct(maoni_text, .keep_all = TRUE) %>%
    slice_max(order_by = nchar(maoni_text), n = n, with_ties = FALSE) %>%
    pull(maoni_text) %>%
    str_squish()
}

# ── One section per theme ──────────────────────────────────────────────────────
for (tn in names(THEMES)) {
  label      <- THEME_LABELS[tn]
  theme_data <- coded_maoni %>% filter(.data[[tn]] == 1)
  n_total    <- nrow(theme_data)

  if (n_total == 0) {
    doc <- doc %>%
      body_add_par(label, style = "heading 2") %>%
      body_add_par("No responses coded under this theme.", style = "Normal") %>%
      body_add_par("", style = "Normal")
    next
  }

  # Sentiment breakdown
  sent_tbl  <- theme_data %>% count(sentiment) %>%
    mutate(pct = round(n / sum(n) * 100))
  n_pos  <- sent_tbl$n[sent_tbl$sentiment == "Positive"] %>% { if (length(.) == 0) 0L else . }
  n_neg  <- sent_tbl$n[sent_tbl$sentiment %in% c("Negative","Mixed")] %>% sum()
  pct_pos <- round(n_pos / n_total * 100)
  pct_neg <- round(n_neg / n_total * 100)

  # PCA group split
  pca_data   <- theme_data %>% filter(!is.na(pca_label_lv))
  n_sat_pca  <- sum(pca_data$pca_label_lv == "Satisfied",     na.rm = TRUE)
  n_not_pca  <- sum(pca_data$pca_label_lv == "Not Satisfied", na.rm = TRUE)

  # Overall verdict for the theme
  verdict <- if (pct_pos >= 60) {
    "Overall, respondents express a predominantly POSITIVE outlook on this aspect of HIV services."
  } else if (pct_neg >= 60) {
    "Overall, respondents express a predominantly NEGATIVE or CRITICAL outlook on this aspect, indicating it is an area requiring improvement."
  } else {
    "Responses on this theme are MIXED — there is recognition of progress but also significant areas of concern."
  }

  # ── Write section ────────────────────────────────────────────────────────────
  doc <- doc %>%
    body_add_par(label, style = "heading 2") %>%
    body_add_par(
      paste0("Total responses: ", n_total,
             " | Positive: ", pct_pos, "% | Negative/Critical: ", pct_neg, "%",
             " | From Satisfied respondents (PCA): ", n_sat_pca,
             " | From Not-Satisfied respondents (PCA): ", n_not_pca),
      style = "Normal"
    ) %>%
    body_add_par(verdict, style = "Normal") %>%
    body_add_par("", style = "Normal")

  # What is working (positive quotes)
  pos_quotes <- pick_quotes(theme_data, c("Positive"), n = 3)
  if (length(pos_quotes) > 0) {
    doc <- doc %>%
      body_add_par("What the public appreciates:", style = "heading 3")
    for (q in pos_quotes)
      doc <- doc %>%
        body_add_par(paste0("“", q, "”"), style = "Normal")
    doc <- doc %>% body_add_par("", style = "Normal")
  }

  # What is not working (negative/mixed quotes)
  neg_quotes <- pick_quotes(theme_data, c("Negative","Mixed"), n = 3)
  if (length(neg_quotes) > 0) {
    doc <- doc %>%
      body_add_par("Concerns and challenges raised:", style = "heading 3")
    for (q in neg_quotes)
      doc <- doc %>%
        body_add_par(paste0("“", q, "”"), style = "Normal")
    doc <- doc %>% body_add_par("", style = "Normal")
  }

  # Per-group breakdown table
  grp_tbl <- theme_data %>%
    group_by(Group = group_disp) %>%
    summarise(
      Total         = n(),
      Positive      = sum(sentiment == "Positive", na.rm = TRUE),
      Negative_Mixed= sum(sentiment %in% c("Negative","Mixed"), na.rm = TRUE),
      Pct_Positive  = paste0(round(Positive  / Total * 100), "%"),
      .groups = "drop"
    ) %>%
    arrange(desc(Pct_Positive))

  if (nrow(grp_tbl) > 0) {
    ft <- flextable(grp_tbl) %>%
      theme_vanilla() %>%
      bold(part = "header") %>%
      bg(bg = "#1a3a5c", part = "header") %>%
      color(color = "white", part = "header") %>%
      set_table_properties(width = 1, layout = "autofit") %>%
      bg(i = ~ as.numeric(gsub("%","",Pct_Positive)) >= 60,
         j = "Pct_Positive", bg = "#d4edda") %>%
      bg(i = ~ as.numeric(gsub("%","",Pct_Positive)) < 40,
         j = "Pct_Positive", bg = "#f8d7da")

    doc <- doc %>%
      body_add_par("Response breakdown by respondent group:", style = "heading 3") %>%
      body_add_flextable(ft) %>%
      body_add_par("", style = "Normal")
  }

  doc <- doc %>% body_add_par("", style = "Normal")
}

# ── Unclassified section ──────────────────────────────────────────────────────
n_unclass <- sum(coded_maoni$U_Unclassified == 1)
doc <- doc %>%
  body_add_par("U – Unclassified Responses", style = "heading 2") %>%
  body_add_par(
    paste0(n_unclass, " responses did not match any keyword in the rubric. ",
           "These include very short affirmations, incomplete sentences, or novel ",
           "themes not yet represented in the keyword dictionary. They are available ",
           "for manual review in the 'All_Responses' sheet of maoni_rubric.xlsx."),
    style = "Normal"
  )

print(doc, target = file.path(OUTPUT_DIR, "maoni_summary.docx"))
message("✓ maoni_summary.docx written.")
message("\n✓ 04_maoni_rubric.R complete.")
