# ============================================================
# Fullerton College (FC) ggplot Theme + Brand Colors
# Author: Vinnie Wu
# Purpose: Consistent visual identity for FC reports
# ============================================================

# ---- Libraries ----
library(tidyverse) # Core data wrangling & plotting: dplyr, ggplot2, tidyr, readr, purrr, forcats, stringr
library(plotly) # Interactive charts
library(here) # File paths that work across systems
library(readxl) # Excel import
library(writexl) # Excel export
library(haven) # SPSS/SAS/Stata import
library(glue) # Flexible text interpolation
library(flextable) # Publication-quality tables
library(ggtext) # Rich text in ggplot (HTML/Markdown)
library(ragg) # High-quality raster rendering for plots
library(extrafont) # Font management (e.g., Century Gothic)
library(bookdown)
library(htmltools)
library(quarto)

loadfonts(device = "win", quiet = TRUE)

# ---- Fullerton College Color Palette ----
fc_colors <- c(
  "Blue" = "#0F406B",
  "Yellow" = "#FFD600",
  "Sky" = "#375E9E",
  "Creek" = "#5DCCD4",
  "Breeze" = "#BAEEED",
  "LemonZest" = "#F5F3BF",
  "Rustic" = "#E6605B",
  "Elderberry" = "#1B0D42"
)

# Neutral tone for text, lines, etc.
fc_blue <- "#0F406B"
fc_gray <- "#333333"

# ---- FC ggplot Theme ----
theme_fc <- function(base_size = 12, base_family = "Century Gothic") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(family = base_family, color = fc_gray),
      plot.title = element_text(
        face = "bold",
        color = fc_colors["Blue"],
        size = rel(1.2),
        hjust = 0.5
      ),
      plot.subtitle = element_text(color = fc_gray, hjust = 0.5),
      axis.title = element_text(color = fc_gray, face = "bold"),
      axis.text = element_text(color = fc_gray),
      panel.grid.major = element_line(color = "#DDDDDD"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = fc_gray, size = rel(0.9), hjust = 1),
      legend.title = element_text(color = fc_gray, face = "bold"),
      legend.text = element_text(color = fc_gray),
      plot.margin = margin(10, 10, 10, 10)
    )
}


# ---- Color and Fill Scales ----
scale_fill_fc <- function(..., discrete = TRUE) {
  if (discrete) {
    scale_fill_manual(values = rep(unname(fc_colors), length.out = 20), ...)
  } else {
    scale_fill_gradientn(colours = unname(fc_colors), ...)
  }
}

scale_color_fc <- function(..., discrete = TRUE) {
  if (discrete) {
    scale_color_manual(values = rep(unname(fc_colors), length.out = 20), ...)
  } else {
    scale_color_gradientn(colours = unname(fc_colors), ...)
  }
}

# ---- Default ggplot theme ----
theme_set(theme_fc())


# ============================================================
# Fullerton College Global Plotly Theme
# ============================================================

library(plotly)
library(extrafont)

# Load font (run once per session after import)
# font_import(pattern = "Century Gothic", prompt = FALSE)
loadfonts(device = "win")

# Brand colors
fc_blue <- "#0F406B"
fc_gray <- "#333333"

# ---- Theme function ----
fc_plotly_theme <- function(p) {
  p %>%
    layout(
      margin = list(l = 90, r = 40, t = 60, b = 60),
      font = list(family = "Century Gothic", color = fc_gray),
      xaxis = list(
        titlefont = list(family = "Century Gothic", size = 14, color = fc_blue),
        tickfont = list(family = "Century Gothic", size = 12, color = fc_gray),
        showgrid = FALSE,
        zeroline = FALSE,
        showline = FALSE
      ),
      yaxis = list(
        titlefont = list(family = "Century Gothic", size = 14, color = fc_blue),
        tickfont = list(family = "Century Gothic", size = 12, color = fc_gray),
        showgrid = FALSE,
        zeroline = FALSE,
        showline = FALSE
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hoverlabel = list(
        font = list(family = "Century Gothic", size = 12, color = "white"),
        bgcolor = fc_blue,
        bordercolor = fc_gray
      )
    )
}

# Healthy Minds Functions####

data_path <- c(
  "../data/Fullerton_clean_VW.xlsx",
  "data/Fullerton_clean_VW.xlsx"
)
df <- read_xlsx(data_path[file.exists(data_path)][1])


#df <- read_xlsx("../data/Fullerton_clean_VW.xlsx")
#df <- read_xlsx("data/Fullerton_clean_VW.xlsx")

# Set default font for flextable
flextable::set_flextable_defaults(font.family = "Century Gothic")

# Ensure College + year exist
df <- df %>%
  mutate(
    year = "2025"
  )

label_map <- c(
  #race, disaggregated
  # "race_asian" = "Asian", #broader category of all asian subgroups
  "asian_southeast" = "Southeast Asian",
  "asian_south" = "South Asian",
  "asian_east" = "East Asian",
  "asian_filipin" = "Filipino",
  "asian_other" = "Other Asian",
  "race_ainaan" = "American Indian / Alaska Native",
  "race_pi" = "Pacific Islander / Native Hawaiian",
  "race_mides" = "Middle Eastern / North African",
  "race_his" = "Hispanic / Latine",
  "race_black" = "Black / African American",
  "race_white" = "White",
  "race_other" = "Other Race / Ethnicity",

  #gender
  "gender_nb" = "Non-binary / Genderqueer",
  "gender_trans" = "Transgender",
  "gender_male" = "Male",
  "gender_female" = "Female",

  #sexual orientation
  "sexual_bi" = "Bisexual",
  "sexual_g" = "Gay",
  "sexual_l" = "Lesbian",
  "sexual_pan" = "Pansexual",
  "sexual_queer" = "Queer",
  "sexual_asexual" = "Asexual",
  "sexual_quest" = "Questioning / Unsure",
  "sexual_lgbt" = "LGBTQIA2S+",
  "sexual_het" = "Non-LGBTQIA2S+",

  #firstgen
  "firstgen" = "First-Gen",
  "contgen" = "Continuing-Gen",

  #class format
  "class_ip" = "In-Person",
  "class_online" = "Online",
  "class_hybrid" = "Hybrid",

  #pell grant
  "pell_yes" = "Pell Grant Recipient",
  "pell_no" = "Non-Pell Grant Recipient",

  # age group
  "age_under19" = "Under 19",
  "age_20to24" = "20 to 24",
  "age_25to29" = "25 to 29",
  "age_30to34" = "30 to 34",
  "age_35to39" = "35 to 39",
  "age_40to49" = "40 to 49",
  "age_over50" = "50 and older",

  #disability
  "any_disab" = "Any Disability",
  "disab_reg" = "Registered Disability",
  "disab_noreg" = "Unregistered Disability",
  "disab_none" = "No Disability",

  #grades
  "gr_A" = "Mostly A's",
  "gr_B" = "Mostly B's",
  "gr_C" = "Mostly C's",
  "gr_D" = "Mostly D's",
  "gr_F" = "Mostly F's",

  #food secure
  "food_insecure" = "Food Insecure",
  "food_secure" = "Food Secure",
  #housing secure
  "housing_insecure" = "Housing Insecure",
  "housing_secure" = "Housing Secure"
)

label_map_race <- label_map[grepl("^race_|^asian_", names(label_map))]
label_map_gender <- label_map[grepl("^gender_", names(label_map))]
label_map_sexual <- label_map[grepl("^sexual_", names(label_map))]
label_map_sexual2 <- label_map[
  grepl("^sexual_", names(label_map)) & names(label_map) != "sexual_lgbt"
]

label_map_grades <- label_map[grepl("^gr_", names(label_map))]
label_map_class <- label_map[grepl("^class_", names(label_map))]
label_map_age <- label_map[grepl("^age_", names(label_map))]


label_map_pell <- label_map[grepl("^pell_", names(label_map))]

label_map_disab <- label_map[grepl("^disab_|^any_disab", names(label_map))]
label_map_gen <- label_map[names(label_map) %in% c("firstgen", "contgen")]

label_map_food <- label_map[grepl("^food_", names(label_map))]
label_map_house <- label_map[grepl("^house_", names(label_map))]


predictors <- names(label_map)

outcomes <- c(
  "positiveMH", #diener flourishing scale
  "dep_any", #any depression, phq-9, kroenke et al., 2001
  "anx_any", #any anxiety, gad-7, spitzer et al., 2006
  "ed_any_sde", #any eating disorder, scoff scale, morgan et al., 1999
  "sib_any", #any non-suicidal self-injury
  "sui_idea", #suicidal ideation
  "dx_any", #any mental health diagnosis
  "sui_plan", #suicidal plan - prob no use
  "sui_att", #suicidal attempt - prob no use
  "anymhprob", #dep, anx, ed, sib, sui_idea
  "lonely", #loneliness scale (3 items, ucla)
  "audit_risk", #AUDIT, alcohol use disorders identification test

  #interpersonal violence
  "IPV_1",
  "IPV_2",
  "IPV_3",
  "IPV_4",
  "IPV_5",
  "any_IPV",

  "lonely",
  "sex_assault",
  "alc_any",
  "binge1pl",
  "binge3pl",
  "smok_cig",
  "betting_1",
  "dx_any",

  "drug_mar",
  "drug_coc",
  "drug_her",
  "drug_opi",
  "drug_benzo",
  "drug_met",
  "drug_stim",
  "drug_ect",
  "drug_keta",
  "drug_lsd",
  "drug_psilo",
  "drug_kratom",
  "drug_other",
  "sub_any",

  "nicotine_type_cig",
  "nicotine_type_vape",
  "nicotine_type_chew",
  "nicotine_type_zyn",
  "nicotine_type_cess",
  "nico_any"
)

# -----------------------------------------------------------
# 1. Construct summary table long → then pivot wide
# -----------------------------------------------------------

summary_tables <- lapply(outcomes, function(outcome) {
  df %>%
    select(year, all_of(predictors), all_of(outcome)) %>%
    pivot_longer(
      cols = all_of(predictors),
      names_to = "Predictor",
      values_to = "PredictorValue"
    ) %>%
    group_by(
      year,
      Outcome = outcome,
      Predictor,
      PredictorValue,
      !!sym(outcome)
    ) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(!is.na(PredictorValue)) %>%
    group_by(year, Predictor, PredictorValue, Outcome) %>%
    summarise(
      total = sum(count[!is.na(!!sym(outcome))]),
      n_1 = sum(count[!!sym(outcome) == 1], na.rm = TRUE),
      pct = round(100 * n_1 / total, 1),
      .groups = "drop"
    )
}) %>%
  bind_rows() %>%
  filter(PredictorValue == 1) %>%
  distinct(year, Predictor, PredictorValue, Outcome, .keep_all = TRUE)


# ---- Pivot wide ----
summary_wide <- summary_tables %>%
  pivot_wider(
    names_from = Outcome,
    values_from = c(total, n_1, pct),
    names_glue = "{Outcome}_{.value}"
  )


# --------------------------------------------------
# 2. Add overall row for each outcome
# --------------------------------------------------
overall_rows <- df %>%
  summarise(
    across(
      all_of(outcomes),
      list(
        total = ~ sum(!is.na(.)),
        n_1 = ~ sum(. == 1, na.rm = TRUE),
        pct = ~ round(100 * sum(. == 1, na.rm = TRUE) / sum(!is.na(.)), 1)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(
    year = "2025",
    Predictor = "Overall",
    PredictorValue = 1
  )


summary_wide <- bind_rows(summary_wide, overall_rows)


# --------------------------------------------------------------
# 3. Apply label map and clean table for presentation
# --------------------------------------------------------------

summary_wide_display <- summary_wide %>%
  mutate(
    Predictor_label = recode(Predictor, !!!label_map, .default = Predictor)
  ) %>%
  relocate(Predictor_label, .after = Predictor)


# ---------------------
# 4. Flextable output
# ---------------------
ft_clean <- summary_wide_display %>%
  flextable() %>%
  set_header_labels(
    year = "Year",
    Predictor = "Predictor (code)",
    Predictor_label = "Predictor"
  ) %>%
  autofit() %>%
  theme_vanilla()

ft_clean


## Creating demographics bar chart

make_demographic_bar <- function(
  df,
  vars,
  labels,
  actual_demo = NULL,
  title = NULL,
  color = "#0F406B"
) {
  library(dplyr)
  library(tidyr)
  library(plotly)

  # Filter out rows with all NA across selected variables
  df_filtered <- df %>%
    filter(if_any(all_of(vars), ~ !is.na(.)))

  total_valid <- nrow(df_filtered)

  # Frequency and percentage table
  demo_table <- df_filtered %>%
    summarise(across(all_of(vars), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Category",
      values_to = "Freq"
    ) %>%
    mutate(
      Category = as.character(Category),
      Percent = round(100 * Freq / total_valid, 1),
      Label = labels[Category]
    ) %>%
    arrange(desc(Percent))

  # Merge with reference population data (optional)
  if (!is.null(actual_demo)) {
    demo_table <- demo_table %>%
      left_join(actual_demo, by = c("Category" = "Category")) %>%
      mutate(
        Diff = Percent - Actual_Percent,
        Diff_label = ifelse(
          Diff > 0,
          paste0("+", round(Diff, 1), " pts"),
          paste0(round(Diff, 1), " pts")
        )
      )
  }

  # Plotly horizontal bar chart
  p <- plot_ly(
    data = demo_table,
    x = ~Percent,
    y = ~ reorder(Label, Percent),
    type = "bar",
    orientation = "h",
    text = ~ paste0(Percent, "%"),
    textposition = "outside",
    hovertext = ~ paste0(
      "<b><span style='color:#FFD600;'>",
      Label,
      "</span></b><br>",
      Freq,
      " of ",
      total_valid,
      " respondents",
      if (!is.null(actual_demo)) {
        paste0(
          "<br>% Survey Respondents: ",
          Percent,
          "% (",
          Diff_label,
          ")<br>% 2025 Spring Students: ",
          Actual_Percent,
          "%"
        )
      } else {
        ""
      }
    ),
    hoverinfo = "text",
    marker = list(color = color)
  ) %>%
    layout(
      title = title,
      xaxis = list(
        title = "Percent of Respondents",
        range = c(0, max(demo_table$Percent) * 1.15)
      ),
      yaxis = list(title = ""),
      autosize = TRUE
    ) %>%
    config(
      responsive = TRUE
    )

  fc_plotly_theme(p)
}

############# MAKE DEMO PLOT ############################

make_demographic_plot <- function(
  summary_wide,
  outcome,
  label_map
) {
  library(dplyr)
  library(plotly)
  library(stringr)

  # --- Outcome handling
  pct_col <- outcome
  total_col <- gsub("_pct$", "_total", outcome)

  outcome_label <- dplyr::case_when(
    outcome == "dep_any_pct" ~ "Depression",
    outcome == "anx_any_pct" ~ "Anxiety",
    outcome == "ed_any_sde_pct" ~ "Eating Disorder",
    outcome == "sib_any_pct" ~ "Self-Injury",
    outcome == "sui_idea_pct" ~ "Suicidal Ideation",
    outcome == "anymhprob_pct" ~ "Any Mental Health Condition",
    TRUE ~ outcome
  )

  # --- Filter + recode
  df <- summary_wide %>%
    filter(Predictor %in% names(label_map)) %>%
    mutate(
      PredictorValue = recode(Predictor, !!!label_map),
      pct = as.numeric(.data[[pct_col]]),
      total = as.numeric(.data[[total_col]]),
      small_n = total < 25
    )

  # --- Collapse to ONE ROW PER PREDICTOR (CRITICAL)
  df_plot <- df %>%
    group_by(PredictorValue) %>%
    summarise(
      pct = mean(pct, na.rm = TRUE),
      total = sum(total, na.rm = TRUE),
      small_n = any(small_n),
      .groups = "drop"
    )

  # --- Axis labels (with wrapping + asterisk)
  df_plot <- df_plot %>%
    mutate(
      PredictorLabel = ifelse(
        nchar(PredictorValue) > 18,
        gsub(" / ", " /<br>", PredictorValue),
        PredictorValue
      ),
      PredictorLabel = ifelse(
        small_n,
        paste0(PredictorLabel, "*"),
        PredictorLabel
      )
    )

  # --- Stable ordering (now guaranteed)
  df_plot <- df_plot %>%
    arrange(desc(pct)) %>%
    mutate(
      PredictorValue = factor(PredictorValue, levels = PredictorValue)
    )

  # --- Hover text
  df_plot <- df_plot %>%
    mutate(
      hovertext = paste0(
        "<b>",
        PredictorValue,
        "</b><br>",
        outcome_label,
        ": ",
        round(pct, 1),
        "%<br>",
        "Respondents: ",
        total,
        ifelse(
          small_n,
          "<br><b><span style='color:#E64B35;'>⚠️ Small sample size (&lt;25)</span></b>",
          ""
        )
      )
    )

  # --- Plot
  plot_ly(
    data = df_plot,
    x = ~PredictorValue,
    y = ~pct,
    type = "bar",
    color = I("#0F406B"),
    text = ~ paste0(round(pct, 1), "%"),
    textposition = "outside",
    hovertext = ~hovertext,
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(
        title = "",
        tickangle = -45,
        tickmode = "array",
        tickvals = df_plot$PredictorValue,
        ticktext = df_plot$PredictorLabel
      ),
      yaxis = list(
        title = paste0("% Experiencing ", outcome_label),
        range = c(0, max(df_plot$pct, na.rm = TRUE) * 1.15)
      ),
      annotations = list(
        list(
          text = "* Fewer than 25 respondents",
          xref = "paper",
          yref = "paper",
          x = 1,
          y = 1.05,
          showarrow = FALSE,
          xanchor = "right"
        )
      )
    ) %>%
    fc_plotly_theme() %>%
    layout(autosize = TRUE) %>%
    config(responsive = TRUE)
}


################## MAKE HEATMAP ################################

make_heatmap_plotly <- function(
  summary_wide,
  label_map,
  year = "2025"
) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(plotly)
  library(stringr)
  library(forcats)

  # ------------------------------------------------------------
  # Label wrapper
  # ------------------------------------------------------------
  wrap_label <- function(label, width = 18) {
    label <- as.character(label)

    if (grepl("/", label, fixed = TRUE)) {
      slash_pos <- gregexpr("/", label, fixed = TRUE)[[1]][1]
      return(paste0(
        substr(label, 1, slash_pos),
        "<br>",
        substr(label, slash_pos + 1, nchar(label))
      ))
    }

    if (nchar(label) <= width) {
      return(label)
    }

    spaces <- gregexpr(" ", label)[[1]]
    if (spaces[1] == -1) {
      return(label)
    }

    mid <- nchar(label) / 2
    break_at <- spaces[which.min(abs(spaces - mid))]
    paste0(
      substr(label, 1, break_at - 1),
      "<br>",
      substr(label, break_at + 1, nchar(label))
    )
  }

  wrap_labels <- function(x) vapply(x, wrap_label, FUN.VALUE = character(1))

  # ------------------------------------------------------------
  # Outcome map + midpoints
  # ------------------------------------------------------------
  outcome_map <- c(
    "dep_any_pct" = "Depression",
    "anx_any_pct" = "Anxiety",
    "ed_any_sde_pct" = "Eating Disorder",
    "sib_any_pct" = "Self-Injury",
    "sui_idea_pct" = "Suicidal Ideation",
    "anymhprob_pct" = "Any Mental Health Condition"
  )
  outcome_order <- c(
    "Any Mental Health Condition",
    "Depression",
    "Anxiety",
    "Eating Disorder",
    "Self-Injury",
    "Suicidal Ideation"
  )

  midpoints <- c(
    "Depression" = 46,
    "Anxiety" = 39,
    "Eating Disorder" = 37,
    "Self-Injury" = 26,
    "Suicidal Ideation" = 15,
    "Any Mental Health Condition" = 72
  )

  # ------------------------------------------------------------
  # Filter input - EXACTLY like ggplot
  # ------------------------------------------------------------
  df <- summary_wide %>%
    mutate(Predictor = as.character(Predictor)) %>%
    filter(
      Predictor %in% names(label_map)
    )

  # Remove duplicates - keep the row with higher total (more complete data)
  df <- df %>%
    group_by(Predictor) %>%
    arrange(desc(anymhprob_total)) %>%
    slice(1) %>%
    ungroup()

  # STOP if empty
  if (nrow(df) == 0) {
    stop(
      "No rows available after filtering."
    )
  }

  # ------------------------------------------------------------
  # Clean labels and make numeric - EXACTLY like ggplot
  # ------------------------------------------------------------
  df <- df %>%
    mutate(Group = recode(Predictor, !!!label_map, .default = Predictor)) %>%
    mutate(across(all_of(names(outcome_map)), as.numeric))

  # Also ensure n and total columns are numeric
  n_cols <- str_replace(names(outcome_map), "_pct$", "_n_1")
  total_cols <- str_replace(names(outcome_map), "_pct$", "_total")
  all_numeric_cols <- c(n_cols, total_cols)
  all_numeric_cols <- all_numeric_cols[all_numeric_cols %in% names(df)]

  if (length(all_numeric_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(all_numeric_cols), as.numeric))
  }

  # ------------------------------------------------------------
  # Long format - DON'T drop any columns during pivot
  # ------------------------------------------------------------
  df_long <- df %>%
    pivot_longer(
      cols = all_of(names(outcome_map)),
      names_to = "OutcomeCode",
      values_to = "Percent"
    ) %>%
    mutate(
      Outcome = outcome_map[OutcomeCode],
      Outcome_chr = as.character(Outcome)
    ) %>%
    select(
      Group,
      Outcome,
      Outcome_chr,
      Percent,
      OutcomeCode,
      dep_any_n_1,
      dep_any_total,
      anx_any_n_1,
      anx_any_total,
      ed_any_sde_n_1,
      ed_any_sde_total,
      sib_any_n_1,
      sib_any_total,
      sui_idea_n_1,
      sui_idea_total,
      anymhprob_n_1,
      anymhprob_total
    )

  # Extract n_cases and n_total based on which outcome this row is
  df_long <- df_long %>%
    mutate(
      n_cases = case_when(
        OutcomeCode == "dep_any_pct" ~ dep_any_n_1,
        OutcomeCode == "anx_any_pct" ~ anx_any_n_1,
        OutcomeCode == "ed_any_sde_pct" ~ ed_any_sde_n_1,
        OutcomeCode == "sib_any_pct" ~ sib_any_n_1,
        OutcomeCode == "sui_idea_pct" ~ sui_idea_n_1,
        OutcomeCode == "anymhprob_pct" ~ anymhprob_n_1,
        TRUE ~ NA_real_
      ),
      n_total = case_when(
        OutcomeCode == "dep_any_pct" ~ dep_any_total,
        OutcomeCode == "anx_any_pct" ~ anx_any_total,
        OutcomeCode == "ed_any_sde_pct" ~ ed_any_sde_total,
        OutcomeCode == "sib_any_pct" ~ sib_any_total,
        OutcomeCode == "sui_idea_pct" ~ sui_idea_total,
        OutcomeCode == "anymhprob_pct" ~ anymhprob_total,
        TRUE ~ NA_real_
      )
    ) %>%
    select(Group, Outcome, Outcome_chr, Percent, n_cases, n_total)

  # Drop outcomes with all NA
  df_long <- df_long %>% filter(!is.na(Percent))

  if (nrow(df_long) == 0) {
    stop("All Percent values are NA for the selected predictors.")
  }

  # ------------------------------------------------------------
  # Reorder groups: high → low - EXACTLY like ggplot
  # ------------------------------------------------------------
  order_vec <- df_long %>%
    filter(Outcome == "Any Mental Health Condition") %>%
    arrange(desc(Percent)) %>%
    pull(Group)

  df_long <- df_long %>% mutate(Group = factor(Group, levels = order_vec))

  # ------------------------------------------------------------
  # Per-outcome binning - EXACTLY like ggplot
  # ------------------------------------------------------------
  clamp <- function(x, low, high) pmax(pmin(x, high), low)

  bins_info <- df_long %>%
    group_by(Outcome_chr) %>%
    summarise(
      mid = midpoints[Outcome_chr][1],
      subset_min = min(Percent, na.rm = TRUE),
      subset_max = max(Percent, na.rm = TRUE),
      range_max = max(subset_max - mid, mid - subset_min),
      slice = range_max / 5,
      .groups = "drop"
    ) %>%
    mutate(
      bin_breaks = pmap(
        list(mid, slice, subset_min, subset_max),
        function(mid, slice, subset_min, subset_max) {
          b_raw <- c(
            mid - 3 * slice,
            mid - 1 * slice,
            mid + 1 * slice,
            mid + 3 * slice
          )
          b_raw <- clamp(b_raw, subset_min, subset_max)
          b <- c(subset_min, b_raw, subset_max)
          eps <- 1e-6
          for (i in 2:length(b)) {
            if (b[i] <= b[i - 1]) {
              b[i] <- b[i - 1] + eps
            }
          }
          b
        }
      )
    )

  # ------------------------------------------------------------
  # Join and classify - EXACTLY like ggplot
  # ------------------------------------------------------------
  fc_gray <- "#555555"

  df_long <- df_long %>%
    left_join(bins_info, by = "Outcome_chr") %>%
    rowwise() %>%
    mutate(
      step_bin = cut(
        Percent,
        breaks = bin_breaks,
        include.lowest = TRUE,
        labels = paste0("Step", 1:5)
      ),
      text_color = case_when(
        step_bin %in% c("Step4", "Step5") ~ "white",
        TRUE ~ fc_gray
      )
    ) %>%
    ungroup()

  # ------------------------------------------------------------
  # Create hover text with sample sizes
  # ------------------------------------------------------------
  df_long <- df_long %>%
    mutate(
      hover_text = paste0(
        "<b><span style='color:#FFD600;'>",
        Outcome,
        ": </span>",
        round(Percent, 1),
        "%</b><br>",
        n_cases,
        "  of ",
        n_total,
        " respondents"
      )
    )

  # ------------------------------------------------------------
  # Factor ordering for plotting - EXACTLY like ggplot
  # ------------------------------------------------------------
  df_long <- df_long %>%
    mutate(
      Outcome = factor(
        Outcome,
        levels = outcome_order
      )
    )

  # ------------------------------------------------------------
  # Wide format for Plotly
  # ------------------------------------------------------------
  # Ensure no duplicates before pivot_wider
  df_wide <- df_long %>%
    select(Group, Outcome, Percent, step_bin, text_color, hover_text) %>%
    group_by(Group, Outcome) %>%
    slice(1) %>% # Keep first row if there are any duplicates
    ungroup() %>%
    pivot_wider(
      names_from = Outcome,
      values_from = c(Percent, step_bin, text_color, hover_text),
      names_sep = "___"
    )

  group_names <- levels(df_long$Group)

  outcome_names <- outcome_order[outcome_order %in% unique(df_long$Outcome)]

  percent_mat <- sapply(
    outcome_names,
    function(o) {
      df_wide[[paste0("Percent___", o)]]
    },
    simplify = TRUE
  )

  step_mat <- sapply(
    outcome_names,
    function(o) {
      df_wide[[paste0("step_bin___", o)]]
    },
    simplify = TRUE
  )

  color_mat <- sapply(
    outcome_names,
    function(o) {
      df_wide[[paste0("text_color___", o)]]
    },
    simplify = TRUE
  )

  hover_mat <- sapply(
    outcome_names,
    function(o) {
      df_wide[[paste0("hover_text___", o)]]
    },
    simplify = TRUE
  )

  step_num <- matrix(
    as.numeric(factor(step_mat, levels = paste0("Step", 1:5))),
    nrow = nrow(step_mat)
  )

  text_matrix <- matrix(
    paste0(round(percent_mat, 1), "%"),
    nrow = nrow(percent_mat)
  )

  # ------------------------------------------------------------
  # Colorscale
  # ------------------------------------------------------------
  fc_steps_5 <- c(
    "#FFD600",
    "#f5f3bf",
    "#B2C8E6",
    "#5A79A5",
    "#0F406B"
  )

  cs <- list(
    list(0.00, fc_steps_5[1]),
    list(0.25, fc_steps_5[2]),
    list(0.50, fc_steps_5[3]),
    list(0.75, fc_steps_5[4]),
    list(1.00, fc_steps_5[5])
  )

  # ------------------------------------------------------------
  # Plotly heatmap
  # ------------------------------------------------------------
  p <- plot_ly(
    x = wrap_labels(group_names),
    y = wrap_labels(outcome_names),
    z = t(step_num),
    type = "heatmap",
    colorscale = cs,
    zmin = 1,
    zmax = 5,
    showscale = FALSE,

    text = t(text_matrix),
    texttemplate = "%{text}",
    textfont = list(size = 12),
    textfont_color = t(color_mat),

    hovertext = t(hover_mat),
    hoverinfo = "text",

    xgap = 3,
    ygap = 3
  ) %>%
    layout(
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "", autorange = "reversed"),
      autosize = TRUE
    ) %>%
    config(
      responsive = TRUE
    ) %>%
    fc_plotly_theme()

  return(p)
}
# ------------------------------------------------------------

###################### MAKE DEMO BAR PLOTS BY OUTCOME ###########################

make_demographic_dropdown_plot <- function(
  label_map,
  bar_color = "#0F406B",
  x_max = 95
) {
  library(dplyr)
  library(tidyr)
  library(plotly)

  outcome_map <- c(
    anymhprob = "Any Mental Health Condition",
    dep_any = "Depression",
    anx_any = "Anxiety",
    ed_any_sde = "Eating Disorder",
    sib_any = "Self-Injury",
    sui_idea = "Suicidal Ideation"
  )

  outcome_levels <- unname(outcome_map)

  # -------------------------------
  # Reshape data
  # -------------------------------
  plot_df <- summary_wide %>%
    filter(Predictor != "Overall") %>%
    pivot_longer(
      cols = matches("(_pct|_n_1|_total)$"),
      names_to = c("Outcome", ".value"),
      names_pattern = "^(.*)_(pct|n_1|total)$"
    ) %>%
    mutate(
      Outcome = recode(Outcome, !!!outcome_map),
      Outcome = factor(Outcome, levels = outcome_levels),
      Group = label_map[Predictor]
    ) %>%
    filter(!is.na(Group), !is.na(Outcome)) %>%
    mutate(
      pct_round = round(pct),
      Group_Label = if_else(total < 25, paste0(Group, "*"), Group),
      # Insert line break after " / " if present
      Group_Label_br = gsub(
        " / ",
        " /<br>",
        Group_Label
      ),
      Percent_Label = paste0(pct_round, "%"),
      Hover_Text = paste0(
        "<b><span style='color:#FFD600;'>",
        Group_Label,
        "</span><br>",
        pct_round,
        "%</b> of ",
        Group_Label,
        " students <br>reported symptoms of <i>",
        Outcome,
        ".</i><br>",
        n_1,
        " of ",
        total,
        " respondents"
      )
    )

  # -------------------------------
  # Ordering per outcome
  # -------------------------------
  group_orders <- plot_df %>%
    group_by(Outcome) %>%
    arrange(pct_round, .by_group = TRUE) %>% # low → high for horizontal
    summarise(Group_Order = list(Group_Label_br), .groups = "drop")

  outcomes <- group_orders$Outcome

  # -------------------------------
  # Build plot
  # -------------------------------
  fig <- plot_ly(showlegend = FALSE)

  for (i in seq_along(outcomes)) {
    df_i <- plot_df %>% filter(Outcome == outcomes[i])

    fig <- fig %>%
      add_bars(
        data = df_i,
        y = ~Group_Label_br,
        x = ~pct_round,
        orientation = "h",
        text = ~Percent_Label,
        textposition = "outside",
        textfont = list(size = 16),
        cliponaxis = FALSE,
        hovertext = ~Hover_Text,
        hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = bar_color),
        visible = i == 1
      )
  }

  # -------------------------------
  # Layout + dropdown + annotation title
  # -------------------------------
  fig <- fig %>%
    layout(
      margin = list(t = 120, l = 160),

      xaxis = list(
        title = "Percent of Students",
        range = c(0, x_max + 5)
      ),

      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = group_orders$Group_Order[[1]]
      ),

      updatemenus = list(
        list(
          type = "dropdown",
          active = 0,
          showactive = FALSE,
          x = 0.5,
          y = 1.18,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          font = list(size = 16),
          buttons = lapply(seq_along(outcomes), function(i) {
            list(
              method = "update",
              args = list(
                list(visible = seq_along(outcomes) == i),
                list(
                  annotations = list(
                    # Label annotation
                    list(
                      text = "<b>Select Mental Health Condition:</b>",
                      x = 0.5,
                      y = 1.25,
                      xref = "paper",
                      yref = "paper",
                      showarrow = FALSE,
                      xanchor = "center",
                      yanchor = "top",
                      font = list(
                        family = "Century Gothic",
                        size = 14,
                        color = fc_gray
                      )
                    )
                  ),

                  yaxis = list(
                    categoryorder = "array",
                    categoryarray = group_orders$Group_Order[[i]]
                  )
                )
              ),
              label = outcomes[i]
            )
          })
        )
      ),

      annotations = list(
        list(
          text = "<b>Select Mental Health Condition:</b>",
          x = 0.5,
          y = 1.25,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "top",
          font = list(
            family = "Century Gothic",
            size = 14,
            color = fc_gray
          )
        )
      ),
      autosize = TRUE
    ) %>%
    config(responsive = TRUE) %>%
    fc_plotly_theme()

  fig
}
