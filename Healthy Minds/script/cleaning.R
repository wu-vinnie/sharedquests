################## Libraries ##################
library(tidyverse) # includes dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats, tibble
library(haven)
library(sjlabelled)
library(glue)
library(readxl)
library(writexl)
library(here)
library(flextable)


# Using haven to read in .sav type file in R

setwd(
  "C:/Users/vwu/OneDrive - Fullerton College/Documents/vwu-Code/mission-log/Healthy Minds"
)

# dat <- read_sav("data/Fullerton-clean.sav")

# # Convert SAS file to native R DF conversion
# dat <- dat %>%
#   mutate(across(where(haven::is.labelled), ~ sjlabelled::as_label(.))) %>%
#   as.data.frame()

# colnames(dat)

dat <- read.csv("data/Fullerton-clean.csv")

# lgbtqia2s+ ####

#---------------------------
# 1. Clean text for fuzzy matching
#---------------------------

dat <- dat %>%
  mutate(
    sexual_text_clean = sexual_text %>%
      tolower() %>%
      trimws() %>%
      gsub("[^a-z0-9 ]", " ", .) %>%
      gsub("\\s+", " ", .)
  )

# Fuzzy heterosexual pattern
straight_pattern <- "straight|str8|hetero|heterosexual"


#---------------------------
# 2. Restore heterosexual from original + new recode
#    (sexual_h was original variable)
#---------------------------

dat <- dat %>%
  mutate(
    sexual_h_fixed = case_when(
      sexual_h == 1 ~ 1, # keep original 585 heterosexuals
      sexual_selfID == 1 &
        str_detect(sexual_text_clean, straight_pattern) ~ 1,
      TRUE ~ 0 # all others = not hetero
    )
  )


#---------------------------
# 3. LGBT core identity flags
#---------------------------

lgbt_core <- c(
  "sexual_l",
  "sexual_g",
  "sexual_bi",
  "sexual_queer",
  "sexual_quest",
  "sexual_asexual",
  "sexual_pan"
)


#---------------------------
# 4. Create NEW binary: sexual_lgbt (0/1)
#    Independent from heterosexuality
#---------------------------

dat <- dat %>%
  mutate(
    sexual_lgbt = case_when(
      if_any(all_of(lgbt_core), ~ .x == 1) ~ 1, # any LGBT identity checked

      # selfID == 1 but text NOT indicating straight → treat as LGBT
      sexual_selfID == 1 &
        !str_detect(sexual_text_clean, straight_pattern) ~ 1,

      TRUE ~ 0
    )
  )


#---------------------------
# 5. Create NEW binary: sexual_het (0/1)
#    Independent from LGBT
#---------------------------

dat <- dat %>%
  mutate(
    sexual_het = case_when(
      sexual_h_fixed == 1 ~ 1, # restored hetero indicator
      TRUE ~ 0
    )
  )


#---------------------------
# 6. Optional: conflict flag (1 if both indicators are 1)
#---------------------------

dat <- dat %>%
  mutate(
    sexual_conflict = if_else(sexual_het == 1 & sexual_lgbt == 1, 1, 0)
  )


#---------------------------
# 7. Cleanup temporary variable
#---------------------------

dat <- dat %>%
  select(-sexual_h_fixed)

table(dat$sexual_het, dat$sexual_lgbt)
# note: some students identified as both het and lgbtqia2s+

# gender ####

dat <- dat %>%
  mutate(
    gender_nb = if_else(
      gender_nonbin == 1 | gender_queer == 1,
      1,
      0
    )
  )

# first-gen ####

dat <- dat %>%
  mutate(
    # Fix type issues
    educ_par1 = as.numeric(na_if(as.character(educ_par1), "")),
    educ_par2 = as.numeric(na_if(as.character(educ_par2), "")),

    # Identify NA parents
    both_na = is.na(educ_par1) & is.na(educ_par2),

    # Identify cont-gen parents
    par1_cont = educ_par1 %in% 4:7,
    par2_cont = educ_par2 %in% 4:7,

    # CONT-GEN
    contgen = case_when(
      both_na ~ NA_real_, # both missing → NA
      par1_cont | par2_cont ~ 1, # either parent 4–7
      TRUE ~ 0
    ),

    # FIRST-GEN
    firstgen = case_when(
      both_na ~ NA_real_, # both missing → NA
      par1_cont | par2_cont ~ 0, # cont-gen overrides
      TRUE ~ 1 # neither 4–7 → first-gen
    )
  ) %>%
  select(-par1_cont, -par2_cont, -both_na)


table(dat$firstgen, useNA = "ifany")
table(dat$contgen, useNA = "ifany")
table(dat$firstgen, dat$contgen, useNA = "ifany")

331 / (331 + 496)
# 40.0% first-gen

# class format ####

dat <- dat %>%
  mutate(
    class_ip = case_when(
      covid_clsformat == 1 ~ 1,
      TRUE ~ 0
    ),

    class_hybrid = case_when(
      covid_clsformat == 2 ~ 1,
      TRUE ~ 0
    ),

    class_online = case_when(
      covid_clsformat == 3 ~ 1,
      TRUE ~ 0
    )
  )

# part/full time ####

dat <- dat %>%
  mutate(
    parttime = if_else(enroll == 2, 1, 0),
    fulltime = if_else(enroll == 1, 1, 0)
  )

# age group ####

dat <- dat %>%
  mutate(
    age_under19 = if_else(age < 20, 1, 0, missing = 0),
    age_20to24 = if_else(age >= 20 & age <= 24, 1, 0, missing = 0),
    age_25to29 = if_else(age >= 25 & age <= 29, 1, 0, missing = 0),
    age_30to34 = if_else(age >= 30 & age <= 34, 1, 0, missing = 0),
    age_35to39 = if_else(age >= 35 & age <= 39, 1, 0, missing = 0),
    age_40to49 = if_else(age >= 40 & age <= 49, 1, 0, missing = 0),
    age_over50 = if_else(age >= 50, 1, 0, missing = 0)
  )

colSums(dat %>% select(starts_with("age_")))


# pell grant ####

dat <- dat %>%
  mutate(
    pell_yes = if_else(pellgrant == 1, 1, 0),
    pell_no = if_else(pellgrant == 0, 1, 0)
  )

table(dat$living_sit)
(164 + 17 + 6) / (164 + 17 + 6 + 616) #23.3% no steady place to live
616 / (164 + 17 + 6 + 616) #76.7% steady place to live

# disability

vars <- c(
  "disab_1_1",
  "disab_1_2",
  "disab_1_3",
  "disab_1_4",
  "disab_1_5",
  "disab_1_6",
  "disab_1_7",
  "disab_1_8",
  "disab_1_9"
)

dat <- dat %>%
  mutate(
    # Did student select anything in 1–9?
    disab_any_1_to_9 = if_any(all_of(vars), ~ .x == 1),

    # Build final any_disab variable
    any_disab = case_when(
      disab_1_10 == 1 ~ NA_real_, # Prefer not to respond
      disab_1_11 == 1 ~ 0, # No disability
      disab_any_1_to_9 ~ 1, # Selected at least one disability
      TRUE ~ NA_real_ # No response at all
    )
  ) %>%
  select(-disab_any_1_to_9)

table(dat$any_disab, useNA = "ifany")

dat <- dat %>%
  mutate(
    disab_reg = case_when(
      any_disab == 1 & disab_2 == 1 ~ 1,
      any_disab == 1 & disab_2 == 2 ~ 0,
      any_disab == 0 ~ 0,
      TRUE ~ NA_real_ # covers any_disab == NA
    ),

    disab_noreg = case_when(
      any_disab == 1 & disab_2 == 2 ~ 1,
      any_disab == 1 & disab_2 == 1 ~ 0,
      any_disab == 0 ~ 0,
      TRUE ~ NA_real_ # covers any_disab == NA
    ),
    disab_none = case_when(
      any_disab == 0 ~ 1,
      any_disab == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(dat$disab_reg, useNA = "ifany")
table(dat$disab_noreg, useNA = "ifany")
table(dat$disab_none, useNA = "ifany")

####### NOTE ##########

#Consider using descriptives for these likert scale items:
# knowwher_bin #know where to get help for mental/emotional health at college
# stigma_pcv_1
# stigma_pcv_2
# belong1

outcomes <- c(
  "positiveMH",
  "dep_any",
  "anx_any",
  "ed_any_sde",
  "sib_any",
  "sui_idea",
  "dx_any",
  "anymhprob",
  "lonely",
  "audit_risk"
)
overall_outcomes <- dat %>%
  summarise(across(
    all_of(outcomes),
    list(
      total = ~ sum(!is.na(.x)),
      n_1 = ~ sum(.x == 1, na.rm = TRUE),
      pct = ~ round(sum(.x == 1, na.rm = TRUE) / sum(!is.na(.x)) * 100, 1)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Outcome", ".value"),
    names_pattern = "(.*)_(total|n_1|pct)"
  )
overall_outcomes


dat$food_insecure <- case_when(
  dat$food_worry == 1 ~ 0,
  dat$food_worry == 2 ~ 1,
  dat$food_worry == 3 ~ 1,
  TRUE ~ NA_real_
)
dat$food_secure <- case_when(
  dat$food_worry == 1 ~ 1,
  dat$food_worry == 2 ~ 0,
  dat$food_worry == 3 ~ 0,
  TRUE ~ NA_real_
)


round(prop.table(table(dat$food_insecure, useNA = "ifany")), 3) * 100

dat$housing_insecure <- case_when(
  dat$housing_worry == 1 ~ 0,
  dat$housing_worry == 2 ~ 1,
  dat$housing_worry == 3 ~ 1,
  TRUE ~ NA_real_
)

dat$housing_secure <- case_when(
  dat$housing_worry == 1 ~ 1,
  dat$housing_worry == 2 ~ 0,
  dat$housing_worry == 3 ~ 0,
  TRUE ~ NA_real_
)
round(prop.table(table(dat$housing_insecure, useNA = "ifany")), 3) * 100


dat$sex_assault <- case_when(
  dat$assault_sex == 1 ~ 1,
  dat$assault_sex == 2 ~ 0,
  dat$assault_sex == 3 ~ NA_real_,
  dat$assault_sex == 4 ~ NA_real_,
  TRUE ~ NA_real_
)
round(prop.table(table(dat$sex_assault, useNA = "ifany")), 3) * 100


dat <- dat %>%
  mutate(
    any_IPV = case_when(
      if_all(c(IPV_1, IPV_2, IPV_3, IPV_4, IPV_5), is.na) ~ NA_real_,
      rowSums(
        across(c(IPV_1, IPV_2, IPV_3, IPV_4, IPV_5), ~ . == 1),
        na.rm = TRUE
      ) >
        0 ~ 1,
      TRUE ~ 0
    )
  )


dat <- dat %>%
  mutate(
    # indicator: did respondent answer the nicotine question at all?
    nicotine_any_response = if_any(
      c(
        nicotine_type_cig,
        nicotine_type_vape,
        nicotine_type_chew,
        nicotine_type_zyn,
        nicotine_type_cess,
        nicotine_type_none
      ),
      ~ !is.na(.)
    )
  ) %>%
  mutate(
    across(
      c(
        nicotine_type_cig,
        nicotine_type_vape,
        nicotine_type_chew,
        nicotine_type_zyn,
        nicotine_type_cess,
        nicotine_type_none
      ),
      ~ if_else(
        nicotine_any_response & is.na(.),
        0,
        .
      )
    )
  ) %>%
  select(-nicotine_any_response)


dat <- dat %>%
  mutate(
    nico_any = case_when(
      if_all(
        c(
          nicotine_type_cig,
          nicotine_type_vape,
          nicotine_type_chew,
          nicotine_type_zyn,
          nicotine_type_cess
        ),
        is.na
      ) ~ NA_real_,
      if_any(
        c(
          nicotine_type_cig,
          nicotine_type_vape,
          nicotine_type_chew,
          nicotine_type_zyn,
          nicotine_type_cess
        ),
        ~ . == 1
      ) ~ 1,
      TRUE ~ 0
    )
  )

table(dat$nicotine_type_cig, useNA = "ifany")

drug_use_vars <- c(
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
  "drug_other"
)

drug_all_vars <- c(
  drug_use_vars,
  "drug_none"
)

dat <- dat %>%
  mutate(
    substance_any_response = if_any(
      all_of(drug_all_vars),
      ~ !is.na(.)
    )
  ) %>%
  mutate(
    across(
      all_of(drug_all_vars),
      ~ if_else(
        substance_any_response & is.na(.),
        0,
        .
      )
    )
  ) %>%
  select(-substance_any_response)
dat <- dat %>%
  mutate(
    sub_any = case_when(
      if_all(all_of(drug_use_vars), is.na) ~ NA_real_,
      if_any(all_of(drug_use_vars), ~ . == 1) ~ 1,
      TRUE ~ 0
    )
  )

write_xlsx(dat, "data/Fullerton_clean_VW.xlsx")

# #check out food and housing, and how it compares to mental health AND maybe grades

# ### for binary outcomes
# outcome_map <- c(
#   dep_any_pct = "Depression ",
#   anx_any_pct = "Anxiety",
#   ed_any_sde_pct = "Eating Disorder",
#   sib_any_pct = "Self-Injury",
#   sui_idea_pct = "Suicidal Ideation",
#   anymhprob_pct = "Any Mental Health Problem"
# )

# outcomes <- names(outcome_map)

# binary_summary <- dat %>%
#   select(food_insecure, all_of(outcomes)) %>%
#   pivot_longer(
#     cols = all_of(outcomes),
#     names_to = "Outcome",
#     values_to = "Value"
#   ) %>%
#   filter(!is.na(food_insecure), !is.na(Value)) %>%
#   group_by(Outcome, food_insecure) %>%
#   summarise(
#     n = n(),
#     prop_1 = mean(Value == 1),
#     .groups = "drop"
#   )

# binary_wide <- binary_summary %>%
#   select(Outcome, food_insecure, prop_1) %>%
#   pivot_wider(
#     names_from = food_insecure,
#     values_from = prop_1,
#     names_prefix = "food_insecure_"
#   )

# binary_wide
# plot_df <- binary_wide %>%
#   mutate(
#     food_insecure_0 = as.numeric(food_insecure_0),
#     food_insecure_1 = as.numeric(food_insecure_1)
#   ) %>%
#   pivot_longer(
#     cols = c(food_insecure_0, food_insecure_1),
#     names_to = "food_insecure",
#     values_to = "prop"
#   ) %>%
#   filter(!is.na(prop)) %>%
#   mutate(
#     food_insecure = recode(
#       food_insecure,
#       "food_insecure_0" = "Food Secure",
#       "food_insecure_1" = "Food Insecure"
#     ),
#     pct = prop * 100
#   )

# plot_ly(
#   data = plot_df,
#   x = ~Outcome,
#   y = ~pct,
#   color = ~food_insecure,
#   type = "bar",
#   hovertemplate = paste(
#     "<b>%{x}</b><br>",
#     "Status: %{color}<br>",
#     "Proportion: %{y:.1f}%<extra></extra>"
#   )
# ) %>%
#   layout(
#     barmode = "group",
#     yaxis = list(
#       title = "Proportion (%)",
#       range = c(0, 100)
#     ),
#     xaxis = list(
#       title = "",
#       tickangle = -45
#     ),
#     legend = list(
#       title = list(text = "Food Insecurity Status")
#     )
#   )

# ### for continuous otucomes

# continuous_outcomes <- c(
#   "belong1"
# )

# continuous_summary <- dat %>%
#   select(food_insecure, all_of(continuous_outcomes)) %>%
#   pivot_longer(
#     cols = all_of(continuous_outcomes),
#     names_to = "Outcome",
#     values_to = "Value"
#   ) %>%
#   filter(!is.na(food_insecure), !is.na(Value)) %>%
#   group_by(Outcome, food_insecure) %>%
#   summarise(
#     n = n(),
#     mean = mean(Value),
#     sd = sd(Value),
#     .groups = "drop"
#   )

# continuous_wide <- continuous_summary %>%
#   select(Outcome, food_insecure, mean) %>%
#   pivot_wider(
#     names_from = food_insecure,
#     values_from = mean,
#     names_prefix = "food_insecure_"
#   )
# continuous_wide

# dep_any_pct     = "Depression",
# anx_any_pct     = "Anxiety",
# ed_any_sde_pct  = "Eating Disorder",
# sib_any_pct     = "Self-Injury",
# sui_idea_pct   = "Suicidal Ideation",
# anymhprob_pct   = "Any Mental Health Problem"

#this is how anymhprob was created, using this logic

# The denominator rule is:

# Include a student if and only if:

# They have at least one 1 across the MH variables OR

# They have all 0s across the MH variables

# Exclude a student if:

# They have only 0s and NAs, with at least one NA and no 1s

mh_vars <- c(
  "dep_any",
  "anx_any",
  "ed_any_sde",
  "sib_any",
  "sui_idea"
)

mh_summary <- dat %>%
  mutate(
    # Count endorsements
    mh_ones = rowSums(across(all_of(mh_vars), ~ .x == 1), na.rm = TRUE),
    mh_zeros = rowSums(across(all_of(mh_vars), ~ .x == 0), na.rm = TRUE),
    mh_nas = rowSums(across(all_of(mh_vars), is.na)),

    # Denominator logic
    mh_in_denominator = case_when(
      mh_ones >= 1 ~ TRUE, # any 1 → include
      mh_ones == 0 & mh_nas == 0 ~ TRUE, # all 0s → include
      TRUE ~ FALSE # 0 + NA only → exclude
    ),

    # Indicators
    anymh1 = mh_ones >= 1,
    anymh2 = mh_ones >= 2
  ) %>%
  filter(mh_in_denominator) %>%
  summarise(
    # Denominator
    n = n(),

    # Counts
    anymh1_n = sum(anymh1),
    anymh2_n = sum(anymh2),

    # Percentages
    anymh1_pct = anymh1_n / n * 100,
    anymh2_pct = anymh2_n / n * 100
  )
mh_summary
