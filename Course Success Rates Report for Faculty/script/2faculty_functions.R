# ============================================================
# Core data manipulation & visualization
# ============================================================
library(tidyverse) # dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats
library(plotly) # Interactive charts
library(ggtext) # Rich text in ggplot
library(ggpubr) # Plot arrangement helpers
library(ragg) # High-quality rendering

# ============================================================
# Data import / export
# ============================================================
library(readxl) # Excel import
library(writexl) # Excel export
library(haven) # SPSS / SAS / Stata import
library(here) # Portable file paths

# ============================================================
# Reporting & tables
# ============================================================
library(flextable) # Publication-quality tables
library(htmltools) # HTML helpers
library(quarto) # Quarto rendering
library(bookdown) # Cross-referencing / long-form docs
library(kableExtra) # Enhanced tables in R Markdown

# ============================================================
# Data cleaning, summaries, and statistics
# ============================================================
library(janitor) # Clean variable names, tabyl
library(lubridate) # Dates
library(psych) # Descriptives
library(broom) # Tidy model output
library(Hmisc) # Correlations, rcorr
library(skimr) # Data summaries
library(glue)


# ============================================================
# Fonts & graphics
# ============================================================
library(extrafont) # Font management (e.g., Century Gothic)
loadfonts(device = "pdf", quiet = TRUE)


# ---- Fullerton College Color Palette ----
fc_colors <- c(
  "Blue" = "#0F406B",
  "Yellow" = "#FFD600",
  "Sky" = "#375E9E",
  "Creek" = "#5DCCD4",
  "Breeze" = "#BAEEED",
  "LemonZest" = "#F5F3BF",
  "DarkLemonZest" = "#ECE28A",
  "Rustic" = "#E6605B",
  "Elderberry" = "#1B0D42"
)

# Neutral tone for text, lines, etc.
fc_blue <- "#0F406B"
fc_gray <- "#333333"

# ---- FC ggplot Theme ----
# At the top of your QMD setup chunk
library(ragg)

# Update the theme
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

      plot.subtitle = element_text(
        color = fc_gray,
        hjust = 0.5
      ),

      axis.title = element_text(
        color = fc_gray,
        face = "bold"
      ),

      axis.text = element_text(color = fc_gray),

      # ---- MINIMAL BACKGROUND ----
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),

      axis.line.y = element_blank(),
      axis.line.x = element_line(color = fc_gray),

      plot.caption = element_text(
        color = fc_gray,
        size = rel(0.9),
        hjust = 1
      ),

      legend.title = element_text(
        color = fc_gray,
        face = "bold"
      ),

      legend.text = element_text(color = fc_gray),

      plot.margin = margin(10, 10, 10, 10)
    )
}


# FUNCTIONS #########################

# ============================================================
# Getting all ethnicity flags using raw MIS code
# ============================================================

derive_all_ethnicity <- function(
  df,
  mis_col = "MIS_ETHNICITY_CODE"
) {
  stopifnot(
    is.data.frame(df),
    mis_col %in% names(df)
  )

  mis_code <- df[[mis_col]]

  flags <- data.frame(
    ethn_black = ifelse(substr(mis_code, 15, 15) == "Y", 1L, 0L),

    ethn_native = ifelse(substr(mis_code, 16, 16) == "Y", 1L, 0L),

    ethn_asian = ifelse(
      grepl("Y", substr(mis_code, 6, 12)) |
        substr(mis_code, 14, 14) == "Y",
      1L,
      0L
    ),

    ethn_filipino = ifelse(substr(mis_code, 13, 13) == "Y", 1L, 0L),

    ethn_hispanic = ifelse(
      substr(mis_code, 1, 1) == "Y" |
        grepl("Y", substr(mis_code, 2, 5)),
      1L,
      0L
    ),

    ethn_pi = ifelse(
      grepl("Y", substr(mis_code, 17, 20)),
      1L,
      0L
    ),

    ethn_white = ifelse(substr(mis_code, 21, 21) == "Y", 1L, 0L)
  )

  cbind(df, flags)
}

# example of function use:
# can also use diff df name (e.g., dat): df <- derive_all_ethnicity(df)
# when the mis_col is named something else: df <- derive_all_ethnicity(df, mis_col = "RENAMED_MIS_COLUMN")

##########################################
# COURSE SUCCESS FUNCTIONS##################
#################################################

# READING DATA ####
df <- read.csv("data/course_data.csv")
# df <- read.csv("data/course_data_oie.csv")
df <- df[!duplicated(df), ]
#when reading manually

# Cleaning the data

## Getting ethnicity
df <- derive_all_ethnicity(df)

## lowercase all variable names
df <- df %>%
  rename_with(tolower)

## move ethnicity variables to end of df
df <- df %>%
  relocate(
    ethn_mena,
    ethn_unknown,
    mis_ethnicity_code,
    multi_ethnicity_group,
    ipeds_ethnicity,
    ethn_mena,
    ethn_unknown,
    .after = last_col()
  )

colnames(df)
head(df$success)

# FUNCTIONS ####




# ============================================================
# Overall faculty success summary sentence
# ============================================================
# summarize_overall_success <- function(
#   df,
#   faculty_id,
#   faculty_col = "faculty_id",
#   success_col = "success",
#   iss = 0.621
# ) {

#   library(dplyr)
#   library(scales)

#   # Filter faculty + valid success
#   df_fac <- df %>%
#     filter(
#       .data[[faculty_col]] == faculty_id,
#       !is.na(.data[[success_col]])
#     )

#   # Guardrail
#   if (nrow(df_fac) == 0) {
#     return("No valid course success data were available.")
#   }

#   # Overall success rate
#   overall_rate <- mean(df_fac[[success_col]], na.rm = TRUE)

#   # Above/below statement
#   comparison <- ifelse(
#     overall_rate >= iss,
#     "above",
#     "below"
#   )

#   # Difference
#   diff_pp <- abs(overall_rate - iss)

#   summary_sentence <- paste0(
#     "Your overall course success rate across all courses was ",
#     percent(overall_rate, accuracy = 0.1),
#     ", which is ",
#     comparison,
#     " the Institutional Set Standard."
#   )

#   return(summary_sentence)
# }



summarize_overall_success <- function(
  df,
  faculty_id_val,
  faculty_col = "faculty_id",
  success_col = "success",
  iss = 0.621
) {

  library(dplyr)
  library(scales)

# just to check bc it was screwing up
#  cat("faculty_id passed in:", faculty_id_val, "\n")

# ooo. issue was becuase it was faculty_id not faculty_id_val, which
# screws with dplyr since it thinks it's a column name. 
# So I changed the parameter name to faculty_id_val to avoid confusion.
# sigh. this took me a long time to figure out. sads. thank f for AI


df_fac <- df %>%
  filter(
    .data[[faculty_col]] == faculty_id_val,
    !is.na(.data[[success_col]])
  )

# cat("Rows selected:", nrow(df_fac), "\n")
# cat("Mean:", mean(df_fac[[success_col]], na.rm = TRUE), "\n")

  # Guardrail
  if (nrow(df_fac) == 0) {
    return("No valid course success data were available.")
  }

  # Overall success rate (student-weighted)
  overall_rate <- mean(df_fac[[success_col]], na.rm = TRUE)

  # Difference from ISS
  diff_pp <- overall_rate - iss

  comparison <- if (diff_pp >= 0) "above" else "below"

  paste0(
    "Your overall course success rate across all courses was ",
    percent(overall_rate, accuracy = 0.1),
    ", which is ",
    number(abs(diff_pp)*100, accuracy = 0.1),
    " percentage points ",
    comparison,
    " the Institutional Set Standard of ",
    percent(iss, accuracy = 0.1),
    "."
  )
}













# # ============================================================
# # Plotting OVERALL course success by faculty (VERTICAL BARS)
# # ============================================================
# plot_faculty_course_success <- function(
#   df,
#   faculty_id,
#   faculty_col = "faculty_id",
#   course_col = "course",
#   success_col = "success"
# ) {
#   library(dplyr)
#   library(ggplot2)

#   # Filter data
#   df_filtered <- df[
#     df[[faculty_col]] == faculty_id & !is.na(df[[success_col]]),
#   ]

#   plot_df <- df_filtered %>%
#     group_by(course = !!sym(course_col)) %>%
#     summarise(
#       success_rate = mean(!!sym(success_col)),
#       n = n(),
#       .groups = "drop"
#     ) %>%
#     arrange(success_rate)

#   p <- ggplot(
#     plot_df,
#     aes(
#       x = reorder(course, success_rate),
#       y = success_rate
#     )
#   ) +
#     # Shaded background area (add BEFORE geom_col)
#     geom_rect(
#       aes(xmin = -Inf, xmax = Inf, ymin = 0.621, ymax = 0.80),
#       fill = fc_colors["DarkLemonZest"],
#       alpha = 0.3, # adjust transparency as needed
#       inherit.aes = FALSE
#     ) +
#     geom_col(
#       fill = fc_colors["Blue"],
#       width = 0.6
#     ) +

#     # Above bar: % (top)
#     geom_text(
#       aes(label = scales::percent(success_rate, accuracy = 0.1)),
#       vjust = -1.2,
#       color = fc_gray,
#       fontface = "bold",
#       size = 4
#     ) +

#     # Above bar: N (just below percent)
#     geom_text(
#       aes(label = paste0("n=", n)),
#       vjust = -0.3,
#       color = fc_gray,
#       size = 3.5
#     ) +

#     scale_y_continuous(
#       labels = scales::percent,
#       limits = c(0, 1.05),
#       expand = expansion(mult = c(0, 0.05))
#     ) +

#     labs(
#       #  title = paste("Faculty ID:", faculty_id),
#       x = NULL,
#       y = "% Successful"
#     ) +

#     theme_fc() +
#     theme(
#       axis.text.x = element_text(
#         angle = 45,
#         hjust = 1,
#         vjust = 1
#       )
#     )

#   list(
#     data = plot_df,
#     plot = p
#   )
# }
# #plot_faculty_course_success(df, faculty_id = "@00006300")

# ============================================================
# Plotting OVERALL course success by faculty (DOUBLE BARS)- COMPARISON
# ============================================================

create_plot <- function(plot_df, fc_gray) {
  
  #last_course_pos <- length(levels(plot_df$course))
  dodge_width <- 0.8
bar_width <- 0.7

  ggplot(
    plot_df,
    aes(
      x = course,
      y = success_rate,
      fill = type_label
    )
  ) +
   
    # Shaded background area 
annotate(
  "rect",
  xmin = -Inf, xmax = Inf,
  ymin = 0.621, ymax = 1,
  fill = fc_colors["DarkLemonZest"],
  alpha = 0.3
) +
# annotate(
#   "segment",
#   x = 0.5,
#   xend = 6,
#   y = 0.621,
#   yend = 0.621,
#   linewidth = 0.5,
#   color = "#CCCCCC"
# ) +
# annotate(
#   "segment",
#   x = 0.5,
#   xend = 6,
#   y = 0.80,
#   yend = 0.80,
#   linewidth = 0.5,
#   linetype = "dashed",
#   color = "#CCCCCC"
# ) +
geom_hline(
  yintercept = 0.621,
  color = "#CCCCCC",
  linetype = "solid",
  linewidth = 0.5
) +
geom_hline(
  yintercept = 0.80,
  color = "#CCCCCC",
  linetype = "dashed",
  linewidth = 0.5
) +
annotate(
  "text",
 # x = last_course_pos + 1.2,
 x = 5.5,
  y = 0.621,
  hjust = 1,
#  label = "ISS:62.1%",
    label = " ",
  size = 2.5,
  color = "black"
) +

annotate(
  "text",
#  x = last_course_pos + 1.2,
x = 5.5,
  y = 0.80,
  hjust = 1,
 # label = "Stretch:80.0%",
   label = " ",
  size = 2.5,
  color = "black"
) +

    geom_col(
      position = position_dodge(width = dodge_width),
      width = bar_width
    ) +

    # Above bar: % (top)
    geom_text(
      aes(label = scales::percent(success_rate, accuracy = 0.1)),
      position = position_dodge(width = dodge_width),
      vjust = -1.2,
      color = fc_gray,
      fontface = "bold",
      size = 3.5
    ) +

    # Above bar: N (just below percent)
    geom_text(
      aes(label = paste0("n=", n)),
      position = position_dodge(width = dodge_width),
      vjust = -0.3,
      color = fc_gray,
      size = 3
    ) +

    theme_fc() +

    scale_fill_manual(
      values = c(
        "You" = "#0F406B",
        "All Sections" = "#FFD600"
      ),
      labels = c(
        "You" = "Your Section(s)",
        "All Sections" = "All Sections"
      )
    ) +
  scale_x_discrete(
  expand = expansion(add = c(0.5, .1))
) +

    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +
    coord_cartesian(ylim = c(0, 1.25), clip = "off") + # shows what's drawn

    labs(
      x = NULL,
      y = "% Successful"
    ) +

    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      legend.position = "top",
      legend.title = element_blank(),
       # Reduce space around the legend
  legend.margin = margin(0, 0, 0, 0),
  legend.box.margin = margin(0, 0, -10, 0),
    )
}


# ============================================================
# Plotting OVERALL course success by faculty (DOUBLE BARS)
# ============================================================
plot_faculty_success_compare <- function(
  .data,
  faculty_id,
  faculty_col = "faculty_id",
  course_col = "course",
  success_col = "success"
) {
  library(dplyr)
  library(ggplot2)
  library(tidyr)

  # Filter data for this faculty
  df_faculty <- .data[
    .data[[faculty_col]] == faculty_id & !is.na(.data[[success_col]]),
  ]

  # Get courses taught by this faculty
  courses_taught <- unique(df_faculty[[course_col]])

  # Calculate faculty success rates
  faculty_rates <- df_faculty %>%
    group_by(course = !!sym(course_col)) %>%
    summarise(
      You = mean(!!sym(success_col)),
      faculty_n = n(),
      .groups = "drop"
    )

  # Calculate overall success rates for those same courses
  overall_rates <- .data %>%
    filter(
      !!sym(course_col) %in% courses_taught & !is.na(!!sym(success_col))
    ) %>%
    group_by(course = !!sym(course_col)) %>%
    summarise(
      `All Sections` = mean(!!sym(success_col)),
      overall_n = n(),
      .groups = "drop"
    )

  # Combine
  plot_df <- faculty_rates %>%
    left_join(overall_rates, by = "course") %>%
    pivot_longer(
      cols = c(You, `All Sections`),
      names_to = "type_label",
      values_to = "success_rate"
    ) %>%
    mutate(
      n = if_else(type_label == "You", faculty_n, overall_n),
      type_label = factor(type_label, levels = c("You", "All Sections"))
    )

  # Order courses alphabetically
  course_order <- faculty_rates %>%
    arrange(course) %>%
    pull(course)

    n_courses <- length(course_order)

  plot_df$course <- factor(plot_df$course, levels = course_order)

  # Determine if we need to split into multiple plots
  n_courses <- length(course_order)
  courses_per_plot <- 5
  n_plots <- ceiling(n_courses / courses_per_plot)

  plots <- list()
  data_list <- list()

  for (i in 1:n_plots) {
    start_idx <- (i - 1) * courses_per_plot + 1
    end_idx <- min(i * courses_per_plot, n_courses)

    courses_subset <- course_order[start_idx:end_idx]

    plot_df_subset <- plot_df %>%
      filter(course %in% courses_subset) %>%
      mutate(course = factor(course, levels = courses_subset))

    p <- create_plot(plot_df_subset, fc_gray)

    plots[[i]] <- p

    data_list[[i]] <- plot_df_subset %>%
      select(course, type_label, success_rate, n) %>%
      pivot_wider(
        names_from = type_label,
        values_from = c(success_rate, n)
      )
  }

  return(list(
    data = data_list,
    plots = plots,
    n_plots = n_plots
  ))
}

result <- plot_faculty_success_compare(df, "@00003521")

# # Access plots consistently regardless of number:
result$plots[[1]] # First plot (always exists)
result$plots[[2]] # Second plot (if n_courses > 6)
# result$plots[[3]] # Second plot (if n_courses > 6)




# ============================================================
# Plotting OVERALL course success by faculty, by ETHNICITY
# ============================================================
plot_faculty_success_by_ethnicity <- function(
  df,
  faculty_id,
  faculty_col = "faculty_id",
  success_col = "success"
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # Ethnicity columns (explicit, reproducible)
  ethn_cols <- c(
    "ethn_black",
    "ethn_native",
    "ethn_asian",
    "ethn_filipino",
    "ethn_hispanic",
    "ethn_pi",
    "ethn_white",
    "ethn_mena",
    "ethn_unknown"
  )

  # Filter to faculty + valid success
  df_fac <- df[
    df[[faculty_col]] == faculty_id & !is.na(df[[success_col]]),
  ]

  # Pivot ethnicity flags to long
  plot_df <- df_fac %>%
    select(all_of(success_col), all_of(ethn_cols)) %>%
    pivot_longer(
      cols = all_of(ethn_cols),
      names_to = "ethnicity",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    group_by(ethnicity) %>%
    summarise(
      success_rate = mean(.data[[success_col]]),
      n = n(),
      .groups = "drop"
    )

  # Clean labels
  plot_df <- plot_df %>%
    mutate(
      ethnicity = recode(
        ethnicity,
        ethn_black = "Black/\nAfrican American",
        ethn_native = "American Native/\nAlaska Native",
        ethn_asian = "Asian",
        ethn_filipino = "Filipino",
        ethn_hispanic = "Latine",
        ethn_pi = "Pacific Islander/\nNative Hawaiian",
        ethn_white = "White",
        ethn_mena = "Middle Eastern/\nNorth African",
        ethn_unknown = "Other/Unknown"
      )
    )

  # Plot
  p <- ggplot(
    plot_df,
    aes(x = reorder(ethnicity, success_rate), y = success_rate)
  ) +
annotate(
  "rect",
  xmin = -Inf, xmax = Inf,
  ymin = 0.621, ymax = 1,
  fill = fc_colors["DarkLemonZest"],
  alpha = 0.3
) +
geom_hline(
  yintercept = 0.621,
  color = "#CCCCCC",
  linetype = "solid",
  linewidth = 0.5
) +
geom_hline(
  yintercept = 0.80,
  color = "#CCCCCC",
  linetype = "dashed",
  linewidth = 0.5
) +

    geom_col(
      fill = fc_colors["Blue"],
      width = 0.6
    ) +

    # Above bar: % (top)
    geom_text(
      aes(label = scales::percent(success_rate, accuracy = 0.1)),
      vjust = -1.2,
      color = fc_gray,
      fontface = "bold",
      size = 3.5
    ) +

    # Above bar: N (just below percent)
    geom_text(
      aes(label = paste0("n=", n)),
      vjust = -0.3,
      color = fc_gray,
      size = 3
    ) +

    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +

    coord_cartesian(ylim = c(0, 1.25), clip = "off") + # shows what's drawn

    labs(
      #  title = paste("Faculty ID:", faculty_id),
      x = NULL,
      y = "% Successful"
    ) +

    theme_fc() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  list(
    data = plot_df,
    plot = p
  )
}


# finding lowest/highest and summarizing
summarize_ethnicity_gap <- function(result) {
  library(dplyr)
  library(scales)

  ethn_summary <- result$data # %>%
  # filter(n >= 10) # this filters only groups with more than 10 people

  # Guardrail
  if (nrow(ethn_summary) == 0) {
    return("In your course(s), no ethnicity groups meet the minimum sample size threshold.")
  }

  max_rate <- max(ethn_summary$success_rate)
  min_rate <- min(ethn_summary$success_rate)

  highest_groups <- ethn_summary %>%
    filter(success_rate == max_rate) %>%
    pull(ethnicity)

  lowest_groups <- ethn_summary %>%
    filter(success_rate == min_rate) %>%
    pull(ethnicity)

summary_sentence <- paste0(
  "As shown in @fig-ethnicity, there is a ",
  number((max_rate - min_rate)*100, accuracy = 0.1),
  " percentage-point difference between the group",
  ifelse(length(highest_groups) > 1, "s", ""),
  " with the highest course success rate (",
  gsub("\n", "", paste(highest_groups, collapse = ", ")),
  ") and the group",
  ifelse(length(lowest_groups) > 1, "s", ""),
  " with the lowest course success rate (",
  gsub("\n", "", paste(lowest_groups, collapse = ", ")),
  ")."
)

  summary_sentence
}

# plot_faculty_success_by_ethnicity_di <- function(
#   df,
#   faculty_id,
#   faculty_col = "faculty_id",
#   success_col = "success"
# ) {
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)

#   ethn_cols <- c(
#     "ethn_black",
#     "ethn_native",
#     "ethn_asian",
#     "ethn_filipino",
#     "ethn_hispanic",
#     "ethn_pi",
#     "ethn_white",
#     "ethn_mena",
#     "ethn_unknown"
#   )

#   # ---- Filter to faculty - use BASE R like the working function ----
#   df_fac <- df[
#     df[[faculty_col]] == faculty_id & !is.na(df[[success_col]]),
#   ]

#   # ---- Summarise by ethnicity ----
#   plot_df <- df_fac %>%
#     select(all_of(c(success_col, ethn_cols))) %>%
#     pivot_longer(
#       cols = all_of(ethn_cols),
#       names_to = "ethnicity",
#       values_to = "flag"
#     ) %>%
#     filter(flag == 1) %>%
#     group_by(ethnicity) %>%
#     summarise(
#       success_rate = mean(.data[[success_col]]),
#       n = n(),
#       .groups = "drop"
#     )

#   # ---- PPG-1 DI calculation ----
#   total_successes <- sum(plot_df$success_rate * plot_df$n)
#   total_n <- sum(plot_df$n)

#   plot_df <- plot_df %>%
#     mutate(
#       other_successes = total_successes - (success_rate * n),
#       other_n = total_n - n,
#       other_success_rate = other_successes / other_n,
#       ppg1 = success_rate - other_success_rate,
#       E_raw = 1.96 * sqrt((success_rate * (1 - success_rate)) / n),
#       E = pmax(E_raw, 0.02),
#       DI = ifelse(ppg1 <= -E, "TRUE", "FALSE") # Create as character directly
#     )

#   # ---- Clean labels AFTER DI calculation ----
#   plot_df <- plot_df %>%
#     mutate(
#       ethnicity = recode(
#         ethnicity,
#         ethn_black = "Black/\nAfrican American",
#         ethn_native = "American Native/\nAlaska Native",
#         ethn_asian = "Asian",
#         ethn_filipino = "Filipino",
#         ethn_hispanic = "Latine",
#         ethn_pi = "Pacific Islander/\nNative Hawaiian",
#         ethn_white = "White",
#         ethn_mena = "Middle Eastern/\nNorth African",
#         ethn_unknown = "Other/Unknown"
#       )
#     )

#   # ---- Plot with DI coloring ----
#   p <- ggplot(
#     plot_df,
#     aes(
#       x = reorder(ethnicity, success_rate),
#       y = success_rate,
#       fill = DI
#     )
#   ) +
#     geom_col(width = 0.6) +

#     geom_text(
#       aes(label = scales::percent(success_rate, accuracy = 0.1)),
#       vjust = -1.5,
#       color = fc_gray,
#       fontface = "bold",
#       size = 4
#     ) +

#     geom_text(
#       aes(label = paste0("n=", n)),
#       vjust = -0.5,
#       color = fc_gray,
#       size = 3.5
#     ) +

#     scale_fill_manual(
#       values = c(
#         "FALSE" = unname(fc_colors["Blue"]),
#         "TRUE" = unname(fc_colors["Rustic"])
#       ),
#       guide = "none"
#     ) +

#     scale_y_continuous(
#       labels = scales::percent,
#       limits = c(0, 1.05),
#       expand = expansion(mult = c(0, 0.05))
#     ) +

#     labs(
#       title = paste("Faculty ID:", faculty_id),
#       #  subtitle = "Red bars indicate disproportionate impact (PPG-1)",
#       x = NULL,
#       y = "% Successful"
#     ) +

#     theme_fc() +
#     theme(axis.text.x = element_text(angle = 30, hjust = 1))

#   list(
#     data = plot_df,
#     plot = p
#   )
# }

# #plot_faculty_success_by_ethnicity_di(df, faculty_id = "@01222633")

# # Calculate disproportionate impact ####
# # ============================================================
# # Calculate PPG-1 Disproportionate Impact (CCCO-correct)
# # Reference group = all other students
# # ============================================================
# calculate_ppg1_di <- function(ethn_summary) {
#   stopifnot(
#     all(c("ethnicity", "success_rate", "n") %in% names(ethn_summary))
#   )

#   total_successes <- sum(ethn_summary$success_rate * ethn_summary$n)
#   total_n <- sum(ethn_summary$n)

#   ethn_summary %>%
#     mutate(
#       # All other students
#       other_successes = total_successes - (success_rate * n),
#       other_n = total_n - n,
#       other_success_rate = other_successes / other_n,

#       # PPG-1 gap
#       ppg1 = success_rate - other_success_rate,

#       # Margin of error (95% CI)
#       E_raw = 1.96 * sqrt((success_rate * (1 - success_rate)) / n),
#       E = pmax(E_raw, 0.02),

#       # Disproportionate Impact flag (PPG-1)
#       DI = ppg1 <= -E
#     ) %>%
#     arrange(ppg1)
# }
# # ============================================================
# # Generate PPG-1 Disproportionate Impact sentence
# # ============================================================
# ppg1_di_sentence <- function(ppg1_table) {
#   stopifnot("DI" %in% names(ppg1_table))

#   di_groups <- ppg1_table %>%
#     filter(DI) %>%
#     pull(ethnicity) %>%
#     gsub("\n", "", .)

#   if (length(di_groups) == 0) {
#     paste0(
#       "Using the Percentage Point Gap Minus One (PPG-1) method and accounting for sampling error, ",
#       "no ethnicity groups show evidence of disproportionate impact when compared to all other students."
#     )
#   } else {
#     paste0(
#       "Using the Percentage Point Gap Minus One (PPG-1) method and accounting for small sample sizes, ",
#       "the following group",
#       ifelse(length(di_groups) > 1, "s show", " shows"),
#       " evidence of disproportionate impact when compared to all other students: ",
#       paste0("**", di_groups, "**", collapse = ", "),
#       "."
#     )
#   }
# }

# ============================================================
# Plotting OVERALL course success by faculty, by FIRST-GEN
# (with explicit Unknown category)
# ============================================================
plot_faculty_success_by_first_gen <- function(
  df,
  faculty_id,
  faculty_col = "faculty_id",
  success_col = "success",
  first_gen_col = "first_gen"
) {
  library(ggplot2)

  # ------------------------------------------------------------
  # Base R filtering
  # ------------------------------------------------------------
  keep <- df[[faculty_col]] == faculty_id &
    !is.na(df[[success_col]])

  df_fac <- df[keep, , drop = FALSE]

  # ------------------------------------------------------------
  # Normalize FIRST-GEN categories (Base R)
  # ------------------------------------------------------------
  fg <- df_fac[[first_gen_col]]

  fg_clean <- ifelse(
    is.na(fg) | fg == "" | fg == "X",
    "Unknown",
    fg
  )

  # Recode expected values
  fg_clean[fg_clean == 1] <- "First-Gen"
  fg_clean[fg_clean == 0] <- "Continuing-Gen"

  # Force all other values to Unknown
  fg_clean[
    !fg_clean %in% c("First-Gen", "Continuing-Gen", "Unknown")
  ] <- "Unknown"

  df_fac$first_gen_clean <- fg_clean

  # ------------------------------------------------------------
  # Base R aggregation
  # ------------------------------------------------------------
  success_rate <- tapply(
    df_fac[[success_col]],
    df_fac$first_gen_clean,
    mean
  )

  n <- table(df_fac$first_gen_clean)

  plot_df <- data.frame(
    first_gen = names(success_rate),
    success_rate = as.numeric(success_rate),
    n = as.integer(n[names(success_rate)]),
    row.names = NULL
  )

  #ordering
  plot_df$first_gen <- factor(
    plot_df$first_gen,
    levels = c("First-Gen", "Continuing-Gen", "Unknown")
  )
  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------
  p <- ggplot(
    plot_df,
    aes(x = first_gen, y = success_rate)
  ) +

    # Shaded background area 
   annotate(
  "rect",
  xmin = -Inf, xmax = Inf,
  ymin = 0.621, ymax = 1,
  fill = fc_colors["DarkLemonZest"],
  alpha = 0.3
) +
geom_hline(
  yintercept = 0.621,
  color = "#CCCCCC",
  linetype = "solid",
  linewidth = 0.5
) +
geom_hline(
  yintercept = 0.80,
  color = "#CCCCCC",
  linetype = "dashed",
  linewidth = 0.5
) +

    geom_col(
      fill = fc_colors["Blue"],
      width = 0.6
    ) +

    geom_text(
      aes(label = scales::percent(success_rate, accuracy = 0.1)),
      vjust = -1.2,
      color = fc_gray,
      fontface = "bold",
      size = 4
    ) +

    geom_text(
      aes(label = paste0("n=", n)),
      vjust = -0.3,
      color = fc_gray,
      size = 3.5
    ) +

    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +

    coord_cartesian(ylim = c(0, 1.2), clip = "off") + # shows what's drawn

    labs(
      x = NULL,
      y = "% Successful"
    ) +

    theme_fc()

  list(
    data = plot_df,
    plot = p
  )
}


# ============================================================
# Finding lowest/highest and summarizing FIRST-GEN gap
# ============================================================
summarize_first_gen_gap <- function(result) {
  library(scales)

  fg_summary <- result$data

  # Keep only First Gen vs Continuing Gen
  fg_compare <- fg_summary[
    fg_summary$first_gen %in% c("First-Gen", "Continuing-Gen"),
    ,
    drop = FALSE
  ]

  # Guardrails
  if (nrow(fg_compare) < 2) {
    return(
      "There is insufficient data to compare first-generation and continuing-generation students."
    )
  }

  # Extract rates
  fg_rate <- fg_compare$success_rate[fg_compare$first_gen == "First-Gen"]
  cg_rate <- fg_compare$success_rate[fg_compare$first_gen == "Continuing-Gen"]

  # Additional guardrail
  if (length(fg_rate) == 0 || length(cg_rate) == 0) {
    return(
      "Insufficient data to compare first-generation and continuing-generation students."
    )
  }

  diff <- fg_rate - cg_rate

  paste0(
  "As shown in @fig-firstgen, there is a ",
  number(abs(diff) * 100, accuracy = 0.1),
  " percentage-point difference in course success rates between first-generation and ",
  "continuing-generation students",
  if (diff > 0) {
    ", with a higher course success rate among first-generation students."
  } else if (diff < 0) {
    ", with a higher course success rate among continuing-generation students."
  } else {
    ", with no observed difference."
  }
)
}




# ============================================================
# Plotting OVERALL course success by faculty, by GENDER
# ============================================================
plot_faculty_success_by_gender <- function(
  df,
  faculty_id,
  faculty_col = "faculty_id",
  success_col = "success"
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # Create combined Other/Unknown category
  df <- df %>%
    mutate(
      gender_unknownother = if_else(
        gender_nb == 1 |
          gender_trans == 1 |
          gender_unknown == 1,
        1,
        0
      )
    )

  # Gender columns
  gender_cols <- c(
    "gender_f",
    "gender_m",
    "gender_unknownother"
  )

  # Filter to faculty + valid success
  df_fac <- df[
    df[[faculty_col]] == faculty_id & !is.na(df[[success_col]]),
  ]

  # Pivot gender flags to long
  plot_df <- df_fac %>%
    select(all_of(success_col), all_of(gender_cols)) %>%
    pivot_longer(
      cols = all_of(gender_cols),
      names_to = "gender",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    group_by(gender) %>%
    summarise(
      success_rate = mean(.data[[success_col]]),
      n = n(),
      .groups = "drop"
    )

  # Clean labels
  plot_df <- plot_df %>%
    mutate(
      gender = recode(
        gender,
        gender_f = "Female",
        gender_m = "Male",
        gender_unknownother = "NB/trans/Unknown"
      )
    )

  # Plot
  p <- ggplot(
    plot_df,
    aes(x = reorder(gender, success_rate), y = success_rate)
  ) +

    # Shaded background area
   annotate(
  "rect",
  xmin = -Inf, xmax = Inf,
  ymin = 0.621, ymax = 1,
  fill = fc_colors["DarkLemonZest"],
  alpha = 0.3
) +
geom_hline(
  yintercept = 0.621,
  color = "#CCCCCC",
  linetype = "solid",
  linewidth = 0.5
) +
geom_hline(
  yintercept = 0.80,
  color = "#CCCCCC",
  linetype = "dashed",
  linewidth = 0.5
) +

    geom_col(
      fill = fc_colors["Blue"],
      width = 0.6
    ) +

    # Above bar: %
    geom_text(
      aes(label = scales::percent(success_rate, accuracy = 0.1)),
      vjust = -1.2,
      color = fc_gray,
      fontface = "bold",
      size = 4
    ) +

    # Above bar: n
    geom_text(
      aes(label = paste0("n=", n)),
      vjust = -0.3,
      color = fc_gray,
      size = 3.5
    ) +

    scale_y_continuous(
      labels = scales::percent,
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +

    coord_cartesian(ylim = c(0, 1.25), clip = "off") +

    labs(
      x = NULL,
      y = "% Successful"
    ) +

    theme_fc() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1)
    )

  list(
    data = plot_df,
    plot = p
  )
}

# result <- plot_faculty_success_by_gender(df, faculty_id = "@01531929")
# result


# ============================================================
# Finding lowest/highest and summarizing
# ============================================================
summarize_gender_gap <- function(result) {
  library(dplyr)
  library(scales)

  gender_summary <- result$data

  # Guardrail
  if (nrow(gender_summary) == 0) {
    return("In your course(s), no gender groups meet the minimum sample size threshold.")
  }

  max_rate <- max(gender_summary$success_rate)
  min_rate <- min(gender_summary$success_rate)

  highest_groups <- gender_summary %>%
    filter(success_rate == max_rate) %>%
    pull(gender)

  lowest_groups <- gender_summary %>%
    filter(success_rate == min_rate) %>%
    pull(gender)

summary_sentence <- paste0(
  "As shown in @fig-gender, there is a ",
  number((max_rate - min_rate)*100, accuracy = 0.1),
  " percentage-point difference between the group",
  ifelse(length(highest_groups) > 1, "s", ""),
  " with the highest course success rate (",
  gsub("\n", "", paste(highest_groups, collapse = ", ")),
  ") and the group",
  ifelse(length(lowest_groups) > 1, "s", ""),
  " with the lowest course success rate (",
  gsub("\n", "", paste(lowest_groups, collapse = ", ")),
  ")."
)

  summary_sentence
}
# summarize_gender_gap(result)






################################################
## DROPPED STUDENTS
######################################################
df_drop <- read.csv("data/dropped_students.csv")
# df_drop <- read.csv("data/dropped_students_oie.csv")
df_drop <- df_drop[!duplicated(df_drop), ]

nrow(df_drop)

# ## Getting ethnicity
# df_drop <- derive_all_ethnicity(df_drop)

# ## lowercase all variable names
df_drop <- df_drop %>%
  rename_with(tolower)

# ## move ethnicity variables to end of df
# df_drop <- df_drop %>%
#   relocate(
#     ethn_mena,
#     ethn_unknown,
#     mis_ethnicity_code,
#     multi_ethnicity_group,
#     ipeds_ethnicity,
#     ethn_mena,
#     ethn_unknown,
#     .after = last_col()
#   )

# Remove duplicate rows
# so if a student took 3 classes of one instructor, it counts only once
df_drop <- df_drop %>%
  distinct(faculty_id, stud_pidm, course_dropped_date, .keep_all = T)
 # distinct(faculty_id, stud_pidm, crn, course_dropped_date, .keep_all = T)
colnames(df_drop)


# drop_reason_labels <- c(
#   work = "Work schedule conflict",
#   financial = "Financial challenges",
#   costs = "Course costs too high",
#   not_enough_financial_aid = "Insufficient or late financial aid",
#   difficult_to_manage_coursework = "Coursework too difficult to manage",
#   moving_away = "Moving away or transferring schools",
#   adequate_childcare = "Childcare issues",
#   transportation = "Transportation problems",
#   unforeseen_emergency = "Emergency, illness, or unforeseen circumstance",
#   low_grade = "Low grade or fell behind academically",
#   more_academic_support = "Needed more academic support",
#   class_is_not_required = "Class not required for program",
#   not_a_good_fit = "Class format was not a good fit",
#   share_why = "Additional explanation (open response)",
#   what_help = "What could help the student stay enrolled (open response)"
# )



#his sums up all reasons across students vs. looking at it by student.
plot_faculty_drops_by_reason <- function(
  df_drop,
  faculty_id,
  faculty_col = "faculty_id"
) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  drop_cols <- c(
    "work",
    "financial",
    "costs",
    "not_enough_financial_aid",
    "difficult_to_manage_coursework",
    "moving_away",
    "adequate_childcare",
    "transportation",
    "unforeseen_emergency",
    "low_grade",
    "more_academic_support",
    "class_is_not_required",
    "not_a_good_fit"
  )

  # -----------------------------------------------------------
  # Filter to one faculty (base R for render stability)
  # -----------------------------------------------------------
  df_fac <- df_drop[
    df_drop[[faculty_col]] == faculty_id,
    ,
    drop = FALSE
  ]

  # -----------------------------------------------------------
  # Pivot reasons to long + count
  # -----------------------------------------------------------
  plot_df <- df_fac %>%
    select(all_of(drop_cols)) %>%
    pivot_longer(
      cols = all_of(drop_cols),
      names_to = "drop_reason",
      values_to = "flag"
    ) %>%
    filter(flag == 1) %>%
    group_by(drop_reason) %>%
    summarise(
      n = n(),
      .groups = "drop"
    ) %>%
    complete(
      drop_reason = drop_cols,
      fill = list(n = 0)
    )

  # -----------------------------------------------------------
  # If no reported reasons
  # -----------------------------------------------------------
  if (sum(plot_df$n) == 0) {
    return(list(
      data = plot_df,
      plot = NULL,
      message = "No students selected a drop reason, so no graph is displayed."
    ))
  }

  # -----------------------------------------------------------
  # Human-readable labels
  # -----------------------------------------------------------
  plot_df <- plot_df %>%
    mutate(
      drop_reason = recode(
        drop_reason,
        work = "Work schedule\nconflict",
        financial = "Financial\nchallenges",
        costs = "Course costs\ntoo high",
        not_enough_financial_aid = "Insufficient or late\nfinancial aid",
        difficult_to_manage_coursework = "Coursework too\ndifficult to manage",
        moving_away = "Moving away or\ntransferring schools",
        adequate_childcare = "Childcare\nissues",
        transportation = "Transportation\nproblems",
        unforeseen_emergency = "Emergency,\nillness, or crisis",
        low_grade = "Low grade or\nfell behind",
        more_academic_support = "Needed more\nacademic support",
        class_is_not_required = "Class not required\nfor program",
        not_a_good_fit = "Class format\nnot a good fit"
      )
    )

  # -----------------------------------------------------------
  # Plot
  # -----------------------------------------------------------
  p <- ggplot(
    plot_df,
    aes(x = reorder(drop_reason, n), y = n)
  ) +
    geom_col(
      fill = fc_colors["Blue"],
      width = 0.6
    ) +
    geom_text(
      aes(label = n),
      hjust = -0.15,
      color = fc_gray,
      fontface = "bold",
      size = 4
    ) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Number of students"
    ) +
    theme_fc()

  list(
    data = plot_df,
    plot = p,
    message = NULL
  )
}

# #testing
# result <- plot_faculty_drops_by_reason(
#   df_drop   = df_drop,
#   faculty_id = "@01531929"
# )


# Check for message first
if (!is.null(result$message)) {
  print(result$message)
} else {
  # View the summarized data table
  print(result$data)
  # Display the plot
  print(result$plot)
}




## This includes frequency but calc is wrong, i think


# plot_faculty_drops_by_reason <- function(
#   df_drop,
#   faculty_id,
#   faculty_col = "faculty_id"
# ) {
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)

#   drop_cols <- c(
#     "work",
#     "financial",
#     "costs",
#     "not_enough_financial_aid",
#     "difficult_to_manage_coursework",
#     "moving_away",
#     "adequate_childcare",
#     "transportation",
#     "unforeseen_emergency",
#     "low_grade",
#     "more_academic_support",
#     "class_is_not_required",
#     "not_a_good_fit"
#   )

#   # -----------------------------------------------------------
#   # Filter to one faculty (base R for render stability)
#   # -----------------------------------------------------------
#   df_fac <- df_drop[
#     df_drop[[faculty_col]] == faculty_id,
#     ,
#     drop = FALSE
#   ]

#   # -----------------------------------------------------------
#   # Pivot reasons to long + count
#   # -----------------------------------------------------------
# # -----------------------------------------------------------
# # Pivot reasons to long + count (student-level)
# # -----------------------------------------------------------
# n_responded <- df_fac %>%
#   filter(rowSums(across(all_of(drop_cols), ~ .x == 1), na.rm = TRUE) >= 1) %>%
#   nrow()

# plot_df <- df_fac %>%
#   filter(rowSums(across(all_of(drop_cols), ~ .x == 1), na.rm = TRUE) >= 1) %>%
#   select(all_of(drop_cols)) %>%
#   pivot_longer(
#     cols      = all_of(drop_cols),
#     names_to  = "drop_reason",
#     values_to = "flag"
#   ) %>%
#   filter(flag == 1) %>%
#   group_by(drop_reason) %>%
#   summarise(
#     n   = n(),
#     pct = round(n / n_responded * 100, 1),
#     .groups = "drop"
#   ) %>%
#   complete(
#     drop_reason = drop_cols,
#     fill = list(n = 0, pct = 0)
#   )

#   # -----------------------------------------------------------
#   # If no reported reasons
#   # -----------------------------------------------------------
#   if (sum(plot_df$n) == 0) {
#     return(list(
#       data = plot_df,
#       plot = NULL,
#       message = "No students selected a drop reason, so no graph is displayed."
#     ))
#   }

#   # -----------------------------------------------------------
#   # Human-readable labels
#   # -----------------------------------------------------------
#   plot_df <- plot_df %>%
#     mutate(
#       drop_reason = recode(
#         drop_reason,
#         work = "Work schedule\nconflict",
#         financial = "Financial\nchallenges",
#         costs = "Course costs\ntoo high",
#         not_enough_financial_aid = "Insufficient or late\nfinancial aid",
#         difficult_to_manage_coursework = "Coursework too\ndifficult to manage",
#         moving_away = "Moving away or\ntransferring schools",
#         adequate_childcare = "Childcare\nissues",
#         transportation = "Transportation\nproblems",
#         unforeseen_emergency = "Emergency,\nillness, or crisis",
#         low_grade = "Low grade or\nfell behind",
#         more_academic_support = "Needed more\nacademic support",
#         class_is_not_required = "Class not required\nfor program",
#         not_a_good_fit = "Class format\nnot a good fit"
#       )
#     )

#   # -----------------------------------------------------------
#   # Plot
#   # -----------------------------------------------------------
#   p <- ggplot(
#     plot_df,
#     aes(x = reorder(drop_reason, n), y = n)
#   ) +
#     geom_col(
#       fill = fc_colors["Blue"],
#       width = 0.6
#     ) +
#    geom_text(
#   aes(label = paste0(pct, "%\n(n=", n, ")")),
#   hjust = -0.1,
#   color = fc_gray,
#   fontface = "bold",
#   size = 4
# ) +
#     coord_flip() +
#     labs(
#       x = NULL,
#       y = "Number of students"
#     ) +
#     theme_fc()

#   list(
#     data = plot_df,
#     plot = p,
#     message = NULL
#   )
# }



# plot_faculty_drops_by_reason(
#   df_drop   = df_drop,
#   faculty_id = "@01234567"
# )



##########################
##COMMENTS#################


plot_faculty_drop_open_responses <- function(
  df_drop,
  faculty_id,
  faculty_col = "faculty_id"
) {
  library(dplyr)
  library(tidyr)
  
  # Filter to one faculty
  df_fac <- df_drop[
    df_drop[[faculty_col]] == faculty_id,
  ]
  
  # Select and prepare the open response columns
  response_df <- df_fac %>%
    select(share_why, what_help) %>%
    # Replace empty strings and NA with actual NA
    mutate(
      share_why = na_if(share_why, ""),
      what_help = na_if(what_help, "")
    ) %>%
    # Keep only rows where at least one response exists
    filter(!is.na(share_why) | !is.na(what_help)) %>%
    # Add row numbers
    mutate(
      `#` = row_number(),
      `Why Drop` = share_why,
      `What Could Have Helped` = what_help
    ) %>%
    select(`#`, `Why Drop`, `What Could Have Helped`)
  
  # Check if there are no responses
  if (nrow(response_df) == 0) {
    return(list(
      data = NULL,
      message = "No students provided open-ended responses about their drop reasons."
    ))
  }
  
  list(
    data = response_df,
    message = NULL
  )
}


# Create open response table
# result_open <- plot_faculty_drop_open_responses(
#   df_drop   = df_drop,
#   faculty_id = "@01531929"
# )

# # Check for message first
# if (!is.null(result_open$message)) {
#   print(result_open$message)
# } else {
#   # View the table
# knitr::kable(result_open$data)
# }




