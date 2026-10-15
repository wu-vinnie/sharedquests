# ============================================================
# Fullerton College (FC) Report — Setup
# Template: report (shared across html + pdf formats)
# Package:  fctools
#
# Loaded via source("setup.R") from index.qmd's setup chunk.
# Anything that would otherwise clutter the top of the .qmd
# (package loads, theme/font registration, options) goes here.
# ============================================================

library(tidyverse)
library(plotly)
library(fctools)

# ------------------------------------------------------------
# Joint FC + IVC palette — mirrors the variables in template/cair.css
# ------------------------------------------------------------

cair_colors <- c(
  stinger_blue = "#0F406B",  # FC, primary
  hornet_yellow = "#FFD600", # FC, primary (fills only, not lines/text)
  ivc_silver = "#C7CACE",    # IVC, primary (neutral, gridlines, "other")
  bright_blue = "#0092D1",   # IVC
  rustic = "#E6605B",        # FC
  green = "#9DC136",         # IVC (fills only)
  magenta = "#C13A84",       # IVC
  elderberry = "#1B0D42",    # FC
  sky = "#375E9E",           # FC
  blue_dust = "#7C98B3",     # IVC
  light_blue = "#C0D9E8",    # IVC
  lemon_zest = "#F5F3BF"     # FC
)

# Categorical series order: alternates schools, max hue separation
cair_series <- unname(cair_colors[c(
  "stinger_blue", "hornet_yellow", "bright_blue",
  "rustic", "green", "magenta"
)])

# Two-group comparisons (e.g. FC vs. IVC)
cair_pair <- c(FC = "#0F406B", IVC = "#0092D1")

cair_text <- "#222222"  # matches body text in cair.css



# ============================================================
# #RealCollegeCA (Version 2 example)
# Only what make_demographic_plot() needs. Trimmed from the full
# report setup: no fonts, flextable, heatmaps, or demographic bars.
# fc_plotly_theme() comes from fctools.
# ============================================================

library(readxl)

# Quarto renders from scripts/, so the data folder is one level up
df <- read_xlsx(file.path("data", "RealCollegeCA_clean_23+25_v2.xlsx"))

# adding lgbtqia2s+ (Any_LGBT / Non_LGBT are predictors in label_map)
lgbt_vars <- grep("^LGBT_", names(df), value = TRUE)

df <- df %>%
  mutate(
    Any_LGBT = if_any(all_of(lgbt_vars), ~ .x == 1),
    Non_LGBT = if_else(Any_LGBT, 0, 1) # explicit comparison group
  )

# ---- Statewide 2025 benchmarks (manual) ----
# The % ordering is: Food insecurity, Housing insecurity, then Homeless
statewide_2025 <- tribble(
  ~College                  , ~year                   , ~Predictor              , ~PredictorValue ,
  ~Food_Insecurity_total    , ~Food_Insecurity_n_1    , ~Food_Insecurity_pct    ,
  ~Housing_Insecurity_total , ~Housing_Insecurity_n_1 , ~Housing_Insecurity_pct ,
  ~Homeless_Any_total       , ~Homeless_Any_n_1       , ~Homeless_Any_pct       ,
  ~Any_Insecurity_total     , ~Any_Insecurity_n_1     , ~Any_Insecurity_pct     ,

  "State"                   ,                    2025 , "Gender_Male"           ,               1 , NA , NA , 45 , NA , NA , 55 , NA , NA , 23 , NA , NA , NA ,
  "State"                   ,                    2025 , "Gender_Female"         ,               1 , NA , NA , 47 , NA , NA , 60 , NA , NA , 18 , NA , NA , NA ,
  "State"                   ,                    2025 , "Gender_NonBinary"      ,               1 , NA , NA , 52 , NA , NA , 57 , NA , NA , 25 , NA , NA , NA ,
  "State"                   ,                    2025 , "Gender_Trans"          ,               1 , NA , NA , 53 , NA , NA , 58 , NA , NA , 26 , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Bisexual"         ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Gay"              ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Lesbian"          ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Pansexual"        ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Queer"            ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_SameGenderLoving" ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "LGBT_Questioning"      ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_Hispanic"         ,               1 , NA , NA , 49 , NA , NA , 62 , NA , NA , 18 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_Black"            ,               1 , NA , NA , 63 , NA , NA , 75 , NA , NA , 34 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_AIAA_Native"      ,               1 , NA , NA , 62 , NA , NA , 72 , NA , NA , 30 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_EAsian"           ,               1 , NA , NA , 33 , NA , NA , 41 , NA , NA , 16 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_SAsian"           ,               1 , NA , NA , 38 , NA , NA , 46 , NA , NA , 16 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_SEAsian"          ,               1 , NA , NA , 41 , NA , NA , 47 , NA , NA , 15 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_OAsian"           ,               1 , NA , NA , 47 , NA , NA , 56 , NA , NA , 16 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_MENA"             ,               1 , NA , NA , 42 , NA , NA , 56 , NA , NA , 21 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_PINH"             ,               1 , NA , NA , 53 , NA , NA , 64 , NA , NA , 29 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_White"            ,               1 , NA , NA , 41 , NA , NA , 52 , NA , NA , 19 , NA , NA , NA ,
  "State"                   ,                    2025 , "Race_Other"            ,               1 , NA , NA , 57 , NA , NA , 70 , NA , NA , 31 , NA , NA , NA ,
  "State"                   ,                    2025 , "FullTime"              ,               1 , NA , NA , 47 , NA , NA , 57 , NA , NA , 21 , NA , NA , NA ,
  "State"                   ,                    2025 , "PartTime"              ,               1 , NA , NA , 45 , NA , NA , 59 , NA , NA , 19 , NA , NA , NA ,
  "State"                   ,                    2025 , "Parent"                ,               1 , NA , NA , 56 , NA , NA , 71 , NA , NA , 20 , NA , NA , NA ,
  "State"                   ,                    2025 , "SingleParent"          ,               1 , NA , NA , 71 , NA , NA , 87 , NA , NA , 30 , NA , NA , NA ,
  "State"                   ,                    2025 , "ProvideCare"           ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "FirstGen"              ,               1 , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA , NA ,
  "State"                   ,                    2025 , "Foster"                ,               1 , NA , NA , 73 , NA , NA , 82 , NA , NA , 47 , NA , NA , NA ,
  "State"                   ,                    2025 , "Convicted"             ,               1 , NA , NA , 71 , NA , NA , 85 , NA , NA , 53 , NA , NA , NA ,
  "State"                   ,                    2025 , "Veteran"               ,               1 , NA , NA , 47 , NA , NA , 63 , NA , NA , 29 , NA , NA , NA ,
  "State"                   ,                    2025 , "Grades_A"              ,               1 , NA , NA , 42 , NA , NA , 54 , NA , NA , 17 , NA , NA , NA ,
  "State"                   ,                    2025 , "Grades_B"              ,               1 , NA , NA , 49 , NA , NA , 60 , NA , NA , 20 , NA , NA , NA ,
  "State"                   ,                    2025 , "Grades_C"              ,               1 , NA , NA , 56 , NA , NA , 67 , NA , NA , 26 , NA , NA , NA ,
  "State"                   ,                    2025 , "Grades_DF"             ,               1 , NA , NA , 62 , NA , NA , 72 , NA , NA , 32 , NA , NA , NA ,
  "State"                   ,                    2025 , "Grades_W"              ,               1 , NA , NA , 58 , NA , NA , 73 , NA , NA , 37 , NA , NA , NA
)

# Statewide OVERALL row (known %s)
statewide_overall_2025 <- tibble::tibble(
  College = "State",
  year = "2025",
  Predictor = "Overall",
  PredictorValue = 1,
  Food_Insecurity_total = NA_real_,
  Food_Insecurity_n_1 = NA_real_,
  Food_Insecurity_pct = 46,
  Housing_Insecurity_total = NA_real_,
  Housing_Insecurity_n_1 = NA_real_,
  Housing_Insecurity_pct = 58,
  Homeless_Any_total = NA_real_,
  Homeless_Any_n_1 = NA_real_,
  Homeless_Any_pct = 20,
  Any_Insecurity_total = NA_real_,
  Any_Insecurity_n_1 = NA_real_,
  Any_Insecurity_pct = 67
)

statewide_2025 <- statewide_2025 %>%
  mutate(year = as.character(year)) %>%
  bind_rows(statewide_overall_2025)

# ---- Aliases: display labels for predictor codes ----
label_map <- c(
  "Race_SEAsian" = "Southeast Asian",
  "Race_SAsian" = "South Asian",
  "Race_EAsian" = "East Asian",
  "Race_AIAA_Native" = "American Indian / Alaska Native",
  "Race_PINH" = "Pacific Islander / Native Hawaiian",
  "Race_MENA" = "Middle Eastern / North African",
  "Race_OAsian" = "Other Asian",
  "Race_Hispanic" = "Hispanic / Latine",
  "Race_Black" = "Black / African American",
  "Race_White" = "White",
  "Race_Other" = "Other Race / Ethnicity",

  "Gender_NonBinary" = "Non-binary / Gender-fluid",
  "Gender_Trans" = "Transgender",
  "Gender_Male" = "Male",
  "Gender_Female" = "Female",

  "FullTime" = "Full-time",
  "PartTime" = "Part-time",

  "LGBT_Bisexual" = "Bisexual",
  "LGBT_Gay" = "Gay",
  "LGBT_Lesbian" = "Lesbian",
  "LGBT_Pansexual" = "Pansexual",
  "LGBT_Queer" = "Queer",
  "LGBT_SameGenderLoving" = "Same-gender loving",
  "LGBT_Questioning" = "Questioning / Unsure",
  "Any_LGBT" = "LGBTQIA2S+",
  "Non_LGBT" = "Non-LGBTQIA2S+",

  "Parent" = "Parent / Guardian",
  "SingleParent" = "Single Parent",
  "ProvideCare" = "Provides Care for Family Member",
  "FirstGen" = "First-generation College Student",
  "Foster" = "Foster Youth / Former Foster Care",
  "Convicted" = "Justice-impacted / Formerly Incarcerated",
  "Veteran" = "Veteran",
  "Any_Disability" = "Any Disability",

  "Grades_A" = "A",
  "Grades_B" = "B",
  "Grades_C" = "C",
  "Grades_DF" = "D/F",
  "Grades_W" = "Withdrew"
)

# ---- summary_wide: % with each outcome, by predictor ----
predictors <- names(label_map)

outcomes <- c(
  "Food_Insecurity",
  "Housing_Insecurity",
  "Homeless_Any",
  "Any_Insecurity"
)

summary_tables <- lapply(outcomes, function(outcome) {
  df %>%
    select(College, year, all_of(predictors), all_of(outcome)) %>%
    pivot_longer(
      cols = all_of(predictors),
      names_to = "Predictor",
      values_to = "PredictorValue"
    ) %>%
    group_by(
      College,
      year,
      Outcome = outcome,
      Predictor,
      PredictorValue,
      !!sym(outcome)
    ) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!is.na(PredictorValue)) %>%
    group_by(College, year, Predictor, PredictorValue, Outcome) %>%
    summarise(
      total = sum(count[!is.na(!!sym(outcome))]), # non-missing responses
      n_1 = sum(count[!!sym(outcome) == 1], na.rm = TRUE), # outcome = 1
      pct = round(n_1 / total * 100, 0),
      .groups = "drop"
    )
}) %>%
  bind_rows() %>%
  filter(PredictorValue == 1) %>% # keep "yes" responses only
  distinct(College, year, Predictor, PredictorValue, Outcome, .keep_all = TRUE)

summary_wide <- summary_tables %>%
  pivot_wider(
    names_from = Outcome,
    values_from = c(total, n_1, pct),
    names_glue = "{Outcome}_{.value}"
  ) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(statewide_2025)

# Overall % for the outcomes
overall_rows <- df %>%
  dplyr::group_by(College, year) %>%
  dplyr::summarise(
    Food_Insecurity_total = sum(!is.na(Food_Insecurity)),
    Food_Insecurity_n_1 = sum(Food_Insecurity == 1, na.rm = TRUE),
    Food_Insecurity_pct = round(100 * Food_Insecurity_n_1 / Food_Insecurity_total, 0),

    Housing_Insecurity_total = sum(!is.na(Housing_Insecurity)),
    Housing_Insecurity_n_1 = sum(Housing_Insecurity == 1, na.rm = TRUE),
    Housing_Insecurity_pct = round(100 * Housing_Insecurity_n_1 / Housing_Insecurity_total, 0),

    Homeless_Any_total = sum(!is.na(Homeless_Any)),
    Homeless_Any_n_1 = sum(Homeless_Any == 1, na.rm = TRUE),
    Homeless_Any_pct = round(100 * Homeless_Any_n_1 / Homeless_Any_total, 0),

    Any_Insecurity_total = sum(!is.na(Any_Insecurity)),
    Any_Insecurity_n_1 = sum(Any_Insecurity == 1, na.rm = TRUE),
    Any_Insecurity_pct = round(100 * Any_Insecurity_n_1 / Any_Insecurity_total, 0),

    .groups = "drop"
  ) %>%
  dplyr::mutate(Predictor = "Overall", PredictorValue = 1, year = as.character(year))

summary_wide <- dplyr::bind_rows(summary_wide, overall_rows) %>%
  arrange(Predictor, year, College)


## making the disaggregation bar charts

make_demographic_plot <- function(
  summary_wide,
  outcome,
  predictor_prefix = NULL,
  predictors = NULL
) {
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(plotly)
  library(glue)
  library(stringr)
  library(rlang)

  # --- Helper to safely handle NA text (prevents "NA" showing up)
  safe_ifelse <- function(cond, yes, no = "") {
    out <- ifelse(cond, yes, no)
    out[is.na(out)] <- ""
    out
  }

  # --- Build variable names
  outcome_label <- case_when(
    tolower(outcome) == "food_insecurity" ~ "Food Insecurity",
    tolower(outcome) == "housing_insecurity" ~ "Housing Insecurity",
    tolower(outcome) == "homeless_any" ~ "Homelessness",
    tolower(outcome) == "any_insecurity" ~ "At Least One Insecurity",
    TRUE ~ outcome
  )

  pct_col <- paste0(outcome, "_pct")
  total_col <- paste0(outcome, "_total")
  n1_col <- paste0(outcome, "_n_1")

  # --- Filter and prep base data
  df <- summary_wide %>%
    mutate(Predictor_chr = as.character(Predictor)) %>%
    filter(
      if (!is.null(predictor_prefix)) {
        str_starts(Predictor_chr, predictor_prefix)
      } else if (!is.null(predictors)) {
        Predictor_chr %in% predictors
      } else {
        TRUE
      }
    ) %>%
    select(-Predictor_chr) %>%
    select(College, year, Predictor, all_of(c(total_col, n1_col, pct_col))) %>%
    mutate(
      PredictorValue = Predictor,
      Source = ifelse(College == "State", "State 2025", paste0("FC ", year)),
      PredictorValue = as.character(PredictorValue)
    )

  # ---- Optional manual labels override ----
  if (!is.null(predictors)) {
    manual_labels <- c(
      "Parent" = "Parent / Guardian",
      "SingleParent" = "Single Parent",
      "ProvideCare" = "Provides Care for Family Member",
      "FirstGen" = "First-generation College Student",
      "Foster" = "Foster Youth / Former Foster Care",
      "Convicted" = "Justice-impacted / Formerly Incarcerated",
      "Veteran" = "Veteran",
      "Any_Disability" = "Any Disability",
      "FullTime" = "Full-time",
      "PartTime" = "Part-time"
    )

    df <- df %>%
      mutate(
        PredictorValue = recode(PredictorValue, !!!manual_labels),
        PredictorLabel = recode(PredictorValue, !!!manual_labels)
      )
  }

  # --- Ensure numeric
  df[[pct_col]] <- as.numeric(df[[pct_col]])
  df[[total_col]] <- as.numeric(df[[total_col]])

  # --- Clean and rename PredictorValue labels
  if (!is.null(predictor_prefix)) {
    df <- df %>%
      mutate(
        PredictorValue = recode(
          PredictorValue,
          "Race_AIAA_Native" = "American Indian / Alaska Native",
          "Race_Black" = "Black / African American",
          "Race_EAsian" = "East Asian",
          "Race_Hispanic" = "Hispanic / Latine",
          "Race_MENA" = "Middle Eastern / North African",
          "Race_OAsian" = "Other Asian",
          "Race_Other" = "Other Race / Ethnicity",
          "Race_PINH" = "Pacific Islander / Native Hawaiian",
          "Race_SAsian" = "South Asian",
          "Race_SEAsian" = "Southeast Asian",
          "Race_White" = "White",
          "Gender_Trans" = "Transgender",
          "Gender_NonBinary" = "Non-Binary",
          "Gender_Female" = "Female",
          "Gender_Male" = "Male",
          "LGBT_Bisexual" = "Bisexual",
          "LGBT_Gay" = "Gay",
          "LGBT_Lesbian" = "Lesbian",
          "LGBT_Pansexual" = "Pansexual",
          "LGBT_Queer" = "Queer",
          "LGBT_SameGenderLoving" = "Same-gender loving",
          "LGBT_Questioning" = "Questioning / Unsure",
          "FullTime" = "Full-time",
          "PartTime" = "Part-time",
          "Parent" = "Parent / Guardian",
          "SingleParent" = "Single Parent",
          "ProvideCare" = "Provides Care for Family Member",
          "FirstGen" = "First-generation College Student",
          "Foster" = "Foster Youth / Former Foster Care",
          "Convicted" = "Justice-impacted / Formerly Incarcerated",
          "Veteran" = "Veteran",
          "Any_Disability" = "Any Disability",
          .default = PredictorValue
        )
      )
  }

  # --- Establish FC 2025 order
  order_fc25 <- df %>%
    filter(College == "Fullerton College", year == 2025) %>%
    arrange(desc(.data[[pct_col]])) %>%
    pull(PredictorValue)

  # --- Prepare wrapped labels
  df <- df %>%
    mutate(
      PredictorValue = factor(
        PredictorValue,
        levels = order_fc25,
        ordered = TRUE
      ),
      PredictorLabel = ifelse(
        nchar(as.character(PredictorValue)) > 18,
        gsub(" / ", " /<br>", as.character(PredictorValue)),
        as.character(PredictorValue)
      )
    )

  # --- Preserve totals per College/year
  df_totals <- df %>%
    mutate(total_numeric = as.numeric(.data[[total_col]])) %>%
    select(PredictorValue, College, year, total_numeric)

  # --- Collapse duplicates and compute differences
  df_wide <- df %>%
    group_by(PredictorValue, Source) %>%
    summarise(
      !!pct_col := mean(.data[[pct_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = Source, values_from = all_of(pct_col)) %>%
    mutate(
      diff_23_25 = round(.data[["FC 2025"]] - .data[["FC 2023"]], 1),
      diff_25_state = round(.data[["FC 2025"]] - .data[["State 2025"]], 1),
      diff_23_state = round(.data[["FC 2023"]] - .data[["State 2025"]], 1)
    )

  # --- Join back
  df <- df %>%
    left_join(df_wide, by = "PredictorValue") %>%
    left_join(df_totals, by = c("PredictorValue", "College", "year"))

  # --- Small-sample flags (<25 respondents)
  df <- df %>%
    group_by(PredictorValue) %>%
    mutate(
      small_n_2023_group = any(
        College == "Fullerton College" & year == 2023 & total_numeric < 25,
        na.rm = TRUE
      ),
      small_n_2025_group = any(
        College == "Fullerton College" & year == 2025 & total_numeric < 25,
        na.rm = TRUE
      ),
      small_n_flag = case_when(
        College == "Fullerton College" &
          year == 2023 &
          total_numeric < 25 ~ "2023",
        College == "Fullerton College" &
          year == 2025 &
          total_numeric < 25 ~ "2025",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    mutate(
      PredictorLabel = ifelse(
        small_n_2023_group | small_n_2025_group,
        paste0(PredictorLabel, "*"),
        PredictorLabel
      )
    )

  # --- Hover text with small-N warning (now fully NA-safe)
  df <- df %>%
    mutate(
      hovertext = case_when(
        Source == "FC 2023" ~ paste0(
          "<b><span style='color:#FFD600;'>",
          PredictorValue,
          " (",
          Source,
          ")</span></b><br>",
          outcome_label,
          ": ",
          safe_ifelse(!is.na(.data[[pct_col]]), .data[[pct_col]]),
          "%<br>",
          "vs. FC 2025: ",
          safe_ifelse(
            !is.na(diff_23_25),
            paste0(ifelse(diff_23_25 > 0, "+", ""), diff_23_25, " pts")
          ),
          safe_ifelse(
            !is.na(diff_23_state),
            paste0(
              "<br>vs. State 2025: ",
              ifelse(diff_23_state > 0, "+", ""),
              diff_23_state,
              " pts"
            )
          ),
          safe_ifelse(
            small_n_flag == "2023",
            "<br><b><span style='color:#E64B35;'>⚠️ Small sample size (&lt;25 respondents)</span></b>"
          )
        ),

        Source == "FC 2025" ~ paste0(
          "<b><span style='color:#FFD600;'>",
          PredictorValue,
          " (",
          Source,
          ")</span></b><br>",
          outcome_label,
          ": ",
          safe_ifelse(!is.na(.data[[pct_col]]), .data[[pct_col]]),
          "%<br>",
          "vs. FC 2023: ",
          safe_ifelse(
            !is.na(diff_23_25),
            paste0(ifelse(-diff_23_25 > 0, "+", ""), -diff_23_25, " pts")
          ),
          safe_ifelse(
            !is.na(diff_25_state),
            paste0(
              "<br>vs. State 2025: ",
              ifelse(diff_25_state > 0, "+", ""),
              diff_25_state,
              " pts"
            )
          ),
          safe_ifelse(
            small_n_flag == "2025",
            "<br><b><span style='color:#E64B35;'>⚠️ Small sample size (&lt;25 respondents)</span></b>"
          )
        ),

        Source == "State 2025" ~ paste0(
          "<b><span style='color:#FFD600;'>",
          PredictorValue,
          " (",
          Source,
          ")</span></b><br>",
          outcome_label,
          ": ",
          safe_ifelse(!is.na(.data[[pct_col]]), .data[[pct_col]]),
          "%"
        )
      )
    )

  # --- Re-assert consistent ordering for both value and label
  df <- df %>%
    mutate(
      PredictorValue = factor(
        PredictorValue,
        levels = order_fc25,
        ordered = TRUE
      )
    ) %>%
    arrange(PredictorValue) %>%
    group_by(PredictorValue) %>%
    mutate(PredictorLabel = dplyr::first(PredictorLabel)) %>%
    ungroup()

  tick_map <- df %>%
    select(PredictorValue, PredictorLabel) %>%
    distinct() %>%
    arrange(factor(PredictorValue, levels = order_fc25))

  # --- Plotly chart
  plot <- plot_ly(
    data = df %>% filter(Source %in% c("FC 2023", "FC 2025", "State 2025")),
    x = ~PredictorValue,
    y = ~ .data[[pct_col]],
    color = ~Source,
    colors = c(
      "FC 2023" = "#375E9E",
      "FC 2025" = "#0F406B",
      "State 2025" = "#FFD600"
    ),
    type = "bar",
    text = ~ paste0(.data[[pct_col]], "%"),
    textfont = list(color = "#333333"),
    textposition = "outside",
    hovertext = ~hovertext,
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(
        title = "",
        tickangle = -45,
        tickmode = "array",
        tickvals = tick_map$PredictorValue,
        ticktext = tick_map$PredictorLabel,
        categoryorder = "array",
        categoryarray = order_fc25
      ),
      yaxis = list(
        title = paste0("% of Students Experiencing ", outcome_label),
        range = c(0, max(df[[pct_col]], na.rm = TRUE) * 1.15)
      ),
      barmode = "group",
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.10,
        xanchor = "center",
        yanchor = "bottom"
      ),
      annotations = list(
        list(
          text = "* Fewer than 25 respondents",
          xref = "paper",
          yref = "paper",
          x = 1,
          y = 1.03,
          showarrow = FALSE,
          font = list(size = 12, color = "#555555"),
          xanchor = "right",
          yanchor = "top"
        ),
        list(
          text = "Tip: Click on the legend or demographic to filter. Click again to return.",
          xref = "paper",
          yref = "paper",
          x = 1,
          y = 0.98,
          showarrow = FALSE,
          font = list(size = 12, color = "#555555"),
          xanchor = "right",
          yanchor = "top"
        )
      )
    ) %>%
    fc_plotly_theme() %>%
    htmlwidgets::onRender(
      "
      function(el, x) {
        const gd = document.getElementById(el.id);
        const allCats = [...new Set(x.data[0].x)];
        let active = null;
        gd.on('plotly_click', function(e) {
          if (!e.points || e.points.length === 0) return;
          const clickedCat = e.points[0].x;
          const current = active;
          if (current === clickedCat) {
            Plotly.relayout(gd, {'xaxis.range': null, 'xaxis.autorange': true});
            active = null;
          } else {
            const index = allCats.indexOf(clickedCat);
            const pad = 0.5;
            Plotly.relayout(gd, {'xaxis.range': [index - pad, index + pad]});
            active = clickedCat;
          }
        });
      }
    "
    ) %>%
    config(responsive = TRUE)

  return(plot)
}
