# ACC CLM Dodoma — Analysis Plan
**Project:** HIV Service Satisfaction Survey, Dodoma Region  
**Datasets:** 12 respondent groups | **Date:** 2026-05-26

---

## 1. Overview of the Data

### Respondent Groups and Sample Sizes

| File | Group | Sheet | Rows | Rating Qs | Maoni Cols |
|------|-------|-------|------|-----------|------------|
| Data - Adolescents and Youth | Vijana/Balehe | vijana_balehe | 38 | 16 | 31 |
| Data - Caregivers of HIV-Positive Children | Walezi wa Watoto | walezi_wa_watoto_wenye_vvu | 62 | 8 | 16 |
| Data - Community Health Workers (CHWs) | CHWs | chws | 6 | 15 | 15 |
| Data - FGD Makundi (Community Groups) | Makundi ya Jamii | fgd_makundi | 19 | 64 | 63 |
| Data - HIV General Community | Jamii kwa Ujumla | hiv_jamii_kwa_ujumla | 69 | 18 | 16 |
| Data - Key Informants | Wahojiwa Wakuu | key_informant | 23 | 22 | 20 |
| Data - Kiongozi wa Konga (Community Elders) | Viongozi wa Konga | kiongozi_wa_konga | 2 | 17 | 18 |
| Data - PBFW (Pregnant and Breastfeeding Women) | Mama Wajawazito | mama_wajawazito_wanaonyonyesha | 95 | 53 | 106 |
| Data - Religious Leaders | Viongozi wa Dini | viongozi_wa_dini | 3 | 12 | 12 |
| Data - Wakunga wa Jadi (Traditional Birth Attendants) | Wakunga wa Jadi | wakunga_wa_jadi | 4 | 18 | 18 |
| Data - Walimu (Teachers) | Walimu | walimu | 4 | 11 | 9 |
| Data - Watoa Huduma (Healthcare Providers) | Watoa Huduma | watoa_huduma | 28 | 60 | 93 |

**Total respondents after exclusion: 311 (42 May 18 records dropped — see Step 1a below)**

### Likert Scale Values Found in the Data

The survey uses a 5-point satisfaction scale expressed in two grammatical forms — singular first-person ("Ni-") and plural first-person ("Tu-") — which refer to the same scale:

| Level | Singular Form | Plural Form | English |
|-------|--------------|-------------|---------|
| 5 | Nimeridhika sana | Tumeridhika sana | Very Satisfied |
| 4 | Nimeridhika zaidi | Tumeridhika zaidi | Highly Satisfied |
| 3 | Nimeridhika | Tumeridhika | Satisfied |
| 2 | Nimeridhika kiasi | Tumeridhika kiasi | Somewhat Satisfied |
| 1 | Sijaridhika kabisa | Hatujaridhika hata Kidogo | Not Satisfied At All |

> **Note:** Some respondents wrote free-text explanations directly in the rating column (e.g., "Nimeridhika kiasi kwasababu Wana unyanyapaa"). These will be parsed and recoded.

### Column Types in Each File
Each file has three types of columns:
1. **Metadata** — Date, data collector, ward, facility, respondent ID, submission info
2. **Rating columns** — Substantive Likert questions (the ones we will analyse)
3. **Maoni/maelezo columns** — Free-text opinion fields paired with each rating question

---

## 2. Data Cleaning Plan

### Step 1 — Load and Normalise All Files
- Load all 12 `.xlsx` files using `readxl` in R
- Tag each row with its source group (e.g., `group = "Adolescents and Youth"`)
- Separate metadata columns, rating columns, and maoni columns into distinct data frames per file

### Step 1a — Exclude May 18 Records *(correction)*
Data collected on **2026-05-18 is not viable and must be excluded** before any analysis. Filter out all rows where `Tarehe == as.Date("2026-05-18")`. Impact by group:

| Group | Original N | Dropped | Retained |
|-------|-----------|---------|---------|
| Adolescents and Youth | 38 | 6 | **32** |
| Caregivers of HIV-Positive Children | 62 | 9 | **53** |
| PBFW (Pregnant and Breastfeeding Women) | 95 | 23 | **72** |
| Watoa Huduma (Healthcare Providers) | 28 | 4 | **24** |
| All other groups | 130 | 0 | **130** |
| **Total** | **353** | **42** | **311** |

### Step 2 — Identify Rating vs. Maoni Columns
- Rating columns: substantive Swahili questions (not starting with "Maoni/maelezo", not metadata)
- Maoni columns: all columns whose header is "Maoni/maelezo" — pair each with its preceding rating question by positional order using column index
- In files where "Maoni/maelezo" appears without a unique header, rename them sequentially as `maoni_Q1`, `maoni_Q2`, etc. using `make.unique()` in R

### Step 3 — Recode the Likert Scale to Numeric
Map all recognised text responses to integers 1–5:

```
Nimeridhika sana / Tumeridhika sana  →  5
Nimeridhika zaidi / Tumeridhika zaidi →  4
Nimeridhika / Tumeridhika            →  3
Nimeridhika kiasi / Tumeridhika kiasi →  2
Sijaridhika kabisa / Hatujaridhika hata Kidogo → 1
```

For responses that begin with a valid level phrase but then continue with free text (e.g., "Nimeridhika kiasi kwasababu..."), extract the leading level phrase and recode; move the trailing explanation into the paired maoni column if it is empty.

### Step 4 — Handle Remaining Anomalies
- **Fully free-text responses in rating columns** (responses that do not start with any recognised level phrase): flag as `NaN` and log them separately for manual review
- **Blank / null rating cells**: retain as `NaN` — do not impute before PCA; use pairwise complete observations
- **Duplicate column names** (e.g., *Watoa Huduma* has duplicate stigma-domain questions): deduplicate by appending `_a` / `_b` suffixes and note which set belongs to which thematic domain
- **Section-header rows** (rows with "Je unaridhika au kutokuridhika na?" as a value): these are sub-section labels that appear in some files as data rows — drop them

### Step 5 — Build a Single Per-Group Cleaned DataFrame
Each group gets one tidy DataFrame with:
- One row per respondent
- One column per rating question (numeric 1–5)
- Paired maoni text in separate columns
- Metadata columns (ward, facility, district) retained for subgroup analysis

### Step 6 — Quality Check
- Print a missingness report per group and per question (% missing)
- Flag any questions with >50% missing as "low coverage" — exclude from PCA but keep in frequency tables with a note
- Confirm that all numeric values fall in {1, 2, 3, 4, 5, NaN}

---

## 3. PCA Analysis — Collapsing to Two Levels

### Rationale
PCA on the item-level Likert scores will surface a dominant "overall satisfaction" axis (PC1). Respondents scoring above the midpoint on PC1 are broadly satisfied; those below are not. This reduces the 5-level scale to a binary **Satisfied / Not Satisfied** classification without discarding inter-item covariance information.

### Steps

**3.1 Prepare the item matrix**  
For each group: extract all numeric rating columns, drop rows that are all-NaN, mean-impute remaining NaN values per column (only for the PCA step — original values are retained for frequency tables).

**3.2 Standardise**  
Z-score each column (mean = 0, SD = 1) before PCA so that questions with wider variance do not dominate.

**3.3 Run PCA**  
- Use `prcomp()` in R with `scale. = TRUE` (handles standardisation internally)
- Report variance explained by each component via `summary(pca)` and a scree plot using `ggplot2`
- PC1 is expected to capture the bulk of variance (overall satisfaction factor)

**3.4 Dichotomise PC1 → Two Levels**  
- Compute each respondent's PC1 score
- Split at **PC1 = 0** (the centre of the standardised axis):
  - PC1 ≥ 0 → **Level 1: Satisfied**
  - PC1 < 0 → **Level 0: Not Satisfied**
- If the median PC1 differs substantially from 0 (skewed data), use the **median** as the cut-point instead, and report both options
- Add the binary label as a new column `pca_level` in the cleaned DataFrame

**3.5 Validate the Dichotomy**  
- Cross-tabulate `pca_level` against the raw mean score per respondent — confirm directionality
- Report the loading of each question on PC1 (item contribution table)
- Run the same PCA per group (separately) so that the two-level classification reflects satisfaction within that group's context

**3.6 Output per group:**
- Scree plot (PNG)
- PC1 loadings table (which questions drive satisfaction vs. dissatisfaction)
- Distribution of `pca_level` (count and %)

---

## 4. Frequency Tables

Two sets of frequency tables will be produced for each group:

### 4A — Raw 5-Level Likert Frequency Table
For each group × each question:

| Question | Not Satisfied (1) | Somewhat Satisfied (2) | Satisfied (3) | Highly Satisfied (4) | Very Satisfied (5) | N | % Missing |
|----------|-------------------|------------------------|---------------|----------------------|--------------------|---|-----------|
| Q1: ... | n (%) | n (%) | n (%) | n (%) | n (%) | n | x% |
| Q2: ... | ... | ... | ... | ... | ... | ... | ... |

Also produce a **combined table across all groups** using the group-tagged master DataFrame.

### 4B — PCA Binary-Level Frequency Table
For each group × each question:

| Question | Not Satisfied (0) | Satisfied (1) | N |
|----------|-------------------|---------------|---|
| Q1: ... | n (%) | n (%) | n |

### 4C — Group-Level Summary Table
One row per group showing:
- Mean overall satisfaction score (1–5)
- % Satisfied (PCA level = 1)
- N respondents
- Top 3 highest-rated questions
- Top 3 lowest-rated questions

### Output format
All frequency tables will be written to a single multi-sheet Excel workbook (`frequency_tables.xlsx`), one sheet per group, plus a summary sheet.

---

## 5. Maoni (Opinion) Rubric

### Objective
Summarise the free-text "Maoni/maelezo" responses to capture what the public thinks and wants regarding HIV services in Dodoma.

### Approach: Thematic Rubric Coding

A rubric with **8 thematic categories** will be applied to every maoni response. Each response will be coded to one or more categories. The rubric is as follows:

| Code | Theme | Description | Example Swahili keywords |
|------|-------|-------------|--------------------------|
| A | **Service Quality & Provider Behaviour** | How health workers treat clients; attitude, respect, language | *wakarimu, heshima, lugha, mtazamo* |
| B | **Access & Availability** | Whether services are present, reachable, or stocked | *upatikanaji, umbali, dawa, vifaa* |
| C | **Waiting Time & Facility Flow** | Queue length, clinic schedule, appointment management | *muda, kusubiria, miadi, foleni* |
| D | **Stigma & Discrimination** | Discrimination at facility or community level | *unyanyapaa, ubaguzi, aibu, wazi* |
| E | **Economic & Logistical Barriers** | Cost, transport, loss of income due to clinic visits | *gharama, nauli, fedha, usafiri* |
| F | **Treatment & Medication Adherence** | ARVs, PEP/PrEP, adherence support, counselling | *dawa, ARV, matumizi, ushauri* |
| G | **Community & Family Support** | Peer support, family acceptance, CHW engagement | *familia, wenza, msaada, jamii* |
| H | **Suggestions & Recommendations** | What respondents want to see improved | *inapaswa, watarajie, mapendekezo, waboreshe* |

### Processing Method
1. Extract all maoni text columns from all 12 files into one flat table with columns: `group`, `question_ref`, `maoni_text`
2. Apply keyword-based auto-coding using a named list of Swahili keyword vectors per theme in R (`stringr::str_detect`)
3. For each response, flag all matching themes (multi-label)
4. Responses that match no keyword → flagged as `U` (Unclassified) for manual review
5. Produce a **rubric summary table** showing:
   - Frequency of each theme per group
   - Top 5 most common verbatim phrases per theme (after stopword removal)
   - A one-paragraph narrative synthesis per theme across all groups

### Output
- `maoni_rubric.xlsx` — coded maoni table with one row per response, columns for each theme flag
- `maoni_summary.docx` — narrative summary document: one section per theme, with representative quotes and key takeaways per respondent group

---

## 6. Deliverables

| # | Deliverable | Format | Description |
|---|-------------|--------|-------------|
| 1 | `scripts/00_setup.R` | R script | Package loading and path configuration |
| 2 | `scripts/01_cleaning.R` | R script | Load all 12 files, exclude May 18, recode Likert 1–5, separate maoni |
| 3 | `scripts/02_pca.R` | R script | PCA per group, PC1 dichotomisation to 2 levels, loadings output |
| 4 | `scripts/03_frequency_tables.R` | R script | 5-level + 2-level frequency tables → `frequency_tables.xlsx` |
| 5 | `scripts/04_maoni_rubric.R` | R script | Keyword rubric coding → `maoni_rubric.xlsx` + `maoni_summary.docx` |
| 6 | `cleaned_data/` | Folder of 12 CSV files | One cleaned CSV per group (written by `01_cleaning.R`) |
| 7 | `frequency_tables.xlsx` | Excel workbook | 5-level + 2-level freq tables, one sheet per group + summary |
| 8 | `pca_results.xlsx` | Excel workbook | PC1 loadings, binary labels, scree data per group |
| 9 | `pca_plots/` | Folder of PNGs | Scree plot + PC1 score distribution per group |
| 10 | `maoni_rubric.xlsx` | Excel workbook | Coded maoni responses with theme flags |
| 11 | `maoni_summary.docx` | Word document | Narrative synthesis of public opinion by theme |

---

## 7. Implementation Stack

- **R** (run in RStudio) with the following packages:
  - `readxl` — loading `.xlsx` files
  - `dplyr`, `tidyr`, `stringr` — data wrangling and Likert recoding
  - `FactoMineR` / `prcomp` — PCA
  - `factoextra` — scree plots and PCA visualisation
  - `ggplot2` — all plots
  - `openxlsx` — writing multi-sheet Excel workbooks
  - `officer` + `flextable` — Word document output
- All analysis delivered as **R scripts** (`scripts/` folder), sourced in order:
  1. `00_setup.R` — package loading and file path configuration
  2. `01_cleaning.R` — load, filter May 18, recode Likert, separate maoni
  3. `02_pca.R` — PCA per group, dichotomise to 2 levels, save outputs
  4. `03_frequency_tables.R` — 5-level and 2-level freq tables → Excel
  5. `04_maoni_rubric.R` — keyword coding, summary table → Excel + Word

---

## 8. Key Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Small sample sizes in some groups (2–6 rows) | PCA will be run on these groups but results flagged as illustrative only; frequency tables remain valid |
| Free text in rating columns | Keyword extraction captures the lead phrase; remainder moved to maoni — logged for review |
| Inconsistent Likert phrasing across groups | Unified mapping dictionary covers all observed variants |
| High missingness in some questions | Questions >50% missing excluded from PCA; reported separately in frequency tables |
| Maoni in Swahili | Keyword rubric is built in Swahili; representative quotes will be provided in both Swahili and English translation |
