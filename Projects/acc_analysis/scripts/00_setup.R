# =============================================================================
# 00_setup.R
# ACC CLM Dodoma — HIV Service Satisfaction Survey
# -----------------------------------------------------------------------------
# Loads required packages, defines all shared paths and constants, and
# provides helper functions used by every downstream script.
# Source this file at the top of scripts 01–04.
# =============================================================================

# ── 1. Packages ────────────────────────────────────────────────────────────────
required_packages <- c(
  "readxl",      # reading .xlsx files
  "dplyr",       # data manipulation
  "tidyr",       # pivoting / reshaping
  "stringr",     # string operations & regex
  "purrr",       # functional programming (map, walk)
  "ggplot2",     # base plotting
  "factoextra",  # PCA scree plots and biplots
  "openxlsx",    # multi-sheet Excel output
  "officer",     # Word document creation
  "flextable"    # formatted tables for Word / HTML
)

missing_pkgs <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  message("Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))


# ── 2. Paths ───────────────────────────────────────────────────────────────────
DATA_DIR   <- "/Users/matemba/Library/CloudStorage/OneDrive-Personal/acc_clm_dodoma/data"
OUTPUT_DIR <- "/Users/matemba/Documents/Claude/Projects/acc_analysis"
CLEAN_DIR  <- file.path(OUTPUT_DIR, "cleaned_data")
PLOT_DIR   <- file.path(OUTPUT_DIR, "pca_plots")

# Create subdirectories if they don't yet exist
for (d in c(CLEAN_DIR, PLOT_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}


# ── 3. Constants ───────────────────────────────────────────────────────────────
EXCLUDE_DATE <- as.Date("2026-05-18")   # Data from this date is not viable

# Short, filesystem-safe label for each source file.
# Used as identifiers throughout all downstream scripts.
GROUP_LABELS <- c(
  "Data - Adolescents and Youth.xlsx"                           = "Adolescents_Youth",
  "Data - Caregivers of HIV-Positive Children.xlsx"            = "Caregivers_CLHIV",
  "Data - Community Health Workers (CHWs).xlsx"                = "CHWs",
  "Data - FGD Makundi (Community Groups).xlsx"                 = "FGD_Community",
  "Data - HIV General Community.xlsx"                          = "HIV_Community",
  "Data - Key Informants.xlsx"                                 = "Key_Informants",
  "Data - Kiongozi wa Konga (Community Elders).xlsx"           = "Community_Elders",
  "Data - PBFW (Pregnant and Breastfeeding Women).xlsx"        = "PBFW",
  "Data - Religious Leaders.xlsx"                              = "Religious_Leaders",
  "Data - Wakunga wa Jadi (Traditional Birth Attendants).xlsx" = "Traditional_Birth_Attendants",
  "Data - Walimu (Teachers).xlsx"                              = "Teachers",
  "Data - Watoa Huduma (Healthcare Providers).xlsx"            = "Healthcare_Providers"
)

# Human-readable display names (used in report titles / Excel sheet headers)
GROUP_DISPLAY <- c(
  "Adolescents_Youth"             = "Adolescents & Youth",
  "Caregivers_CLHIV"              = "Caregivers of HIV-Positive Children",
  "CHWs"                          = "Community Health Workers (CHWs)",
  "FGD_Community"                 = "FGD – Community Groups",
  "HIV_Community"                 = "HIV General Community",
  "Key_Informants"                = "Key Informants",
  "Community_Elders"              = "Community Elders (Kiongozi wa Konga)",
  "PBFW"                          = "Pregnant & Breastfeeding Women (PBFW)",
  "Religious_Leaders"             = "Religious Leaders",
  "Traditional_Birth_Attendants"  = "Traditional Birth Attendants (Wakunga wa Jadi)",
  "Teachers"                      = "Teachers (Walimu)",
  "Healthcare_Providers"          = "Healthcare Providers (Watoa Huduma)"
)

# Minimum number of rows required to run PCA (groups below this threshold
# receive a mean-score-based binary label instead).
PCA_MIN_N <- 5


# ── 4. Likert recoding helper ──────────────────────────────────────────────────
# Converts any recognised Swahili satisfaction phrase to an integer 1–5.
#
# Logic:
#   • Checks are ordered from most specific to least specific so that compound
#     phrases (e.g. "Nimeridhika sana") are caught before the base "ridhika".
#   • Responses that START with a valid phrase but contain trailing free text
#     (e.g. "Nimeridhika kiasi kwasababu...") are still recoded via the leading
#     phrase; the full original text is preserved in the paired maoni column
#     by script 01_cleaning.R.
#   • Unrecognised text → NA  (logged separately in the missingness report).
recode_likert <- function(x) {
  x <- trimws(as.character(x))

  dplyr::case_when(
    # ── 5: Very Satisfied ─────────────────────────────────────────────────────
    str_detect(x, regex("^(Nime|Tume|Tuna)ridhika\\s+sana",     ignore_case = TRUE)) ~ 5L,
    str_detect(x, regex("^Tumeridhika\\s+San\\b",                ignore_case = TRUE)) ~ 5L,  # FGD typo
    str_detect(x, regex("^Naridhika\\s+kabisa",                  ignore_case = TRUE)) ~ 5L,  # "completely satisfied"

    # ── 4: Highly Satisfied ───────────────────────────────────────────────────
    str_detect(x, regex("^(Nime|Tume|Na)ridhika\\s+zaidi",      ignore_case = TRUE)) ~ 4L,

    # ── 2: Somewhat Satisfied (checked before base "3") ──────────────────────
    str_detect(x, regex("^(Nime|Tume|Na|Ame|Wame)ridhika\\s+kiasi", ignore_case = TRUE)) ~ 2L,
    str_detect(x, regex("^Huduma\\s+nzuri\\s+kiasi",             ignore_case = TRUE)) ~ 2L,

    # ── 1: Not Satisfied ──────────────────────────────────────────────────────
    str_detect(x, regex("^Sijaridhika\\s+kabisa",                ignore_case = TRUE)) ~ 1L,
    str_detect(x, regex("^Hatujaridhika",                        ignore_case = TRUE)) ~ 1L,
    str_detect(x, regex("^Siridhiki|^Hakuridhika",               ignore_case = TRUE)) ~ 1L,

    # ── 3: Satisfied (base — contains "ridhika" without a modifier above) ────
    str_detect(x, regex("ridhika",                               ignore_case = TRUE)) ~ 3L,

    # ── Unrecognised ─────────────────────────────────────────────────────────
    TRUE ~ NA_integer_
  )
}


# ── 5. Column-type detection helpers ──────────────────────────────────────────

# Returns TRUE if ≥25% of a column's non-blank values look like Likert phrases.
# Used in script 01 to identify rating columns by their content rather than name.
is_likert_col <- function(col_values) {
  non_null <- col_values[!is.na(col_values) & nchar(trimws(as.character(col_values))) > 0]
  if (length(non_null) < 1) return(FALSE)
  pct <- mean(str_detect(
    as.character(non_null),
    regex("ridhika|Sijaridhika|Hatujaridhika", ignore_case = TRUE)
  ))
  pct >= 0.25
}

# Converts an Excel date serial (stored as character after col_types = "text")
# to an R Date object.  Falls back to direct character parsing if the value
# is already in YYYY-MM-DD format.
parse_excel_date <- function(x) {
  serial <- suppressWarnings(as.numeric(x))
  if (!is.na(serial)) {
    as.Date(serial, origin = "1899-12-30")
  } else {
    suppressWarnings(as.Date(x))
  }
}


# ── 6. Colour palette for plots ───────────────────────────────────────────────
LIKERT_COLOURS <- c(
  "1 – Not Satisfied"        = "#d73027",
  "2 – Somewhat Satisfied"   = "#fc8d59",
  "3 – Satisfied"            = "#fee090",
  "4 – Highly Satisfied"     = "#91bfdb",
  "5 – Very Satisfied"       = "#4575b4"
)

BINARY_COLOURS <- c(
  "Not Satisfied" = "#d73027",
  "Satisfied"     = "#4575b4"
)


message("✓ 00_setup.R loaded — packages, paths, and helpers are ready.")
