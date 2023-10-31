# Descriptive tables for the publication
# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  gt,
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  # devEMF,       # Save vectorized figures as EMF
  lubridate,     # manipulate date objects
  forcats
)

dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds"))) %>%
  mutate(age_cat2 = factor(case_when(     
    age >= 25 & age < 45 ~ "25-44",
    age >= 45 & age < 65 ~ "45-64")),
    Occupation_label_2 = str_replace(Occupation_label_2, " associate", ""),
    Occupation_label_2 = fct_relevel(
      fct_infreq(fct_lump_n(factor(Occupation_label_2), n = 20)),
      "Other", after = Inf),
    job_sector = str_to_sentence(str_remove(job_sector, "Secteur de la|Secteur de l’|Secteur des")),
    job_sector = str_replace(job_sector, " \\s*\\([^\\)]+\\)", ""),
    job_sector = str_wrap(job_sector, width = 60),
    job_sector = fct_relevel(
      fct_infreq(factor(job_sector)),
      "Other", after = Inf),
    burn_out = case_match(burn_out, 
                          1 ~ "Yes", 
                          0 ~ "No"),
    wfh = factor(case_when(
      wfh %in% c("Not possible") ~ "Never",
      .default = wfh),
      levels = c("Occasionally (irregular)", "1+ days/week", "Every day", "Never")
    )
  ) #%>% filter(serocovwork_exclusive == FALSE) # sensitivity checking only for non-serocovwork population


# Enter the variables for your covariates 
covariates <- c(
  "wfh_exposure"
  , "burnout_score"
  , "burnout_interp_dich30"
  , "burn_out"
  , "age_cat",
  # "age",
  "sex_en",
  "education_rec_en",
  "hh_income_cat_en",
  "finance_situation",
  "ethn_dich",
  # "country_residence",
  "hh_livewith_rec_en",
  "num_young_children_dich",
  "overcrowded",
  "noisy",
  "quiet_room",
  "health_general_dich",
  "mental_state_dich",
  "chronic_disease_en",
  # "smoking_rec_en", 
  # "alcohol_en",
  "wfh",
  "percent_change_cat",
  "supervision_short",
  "karasek_social_support_cat",
  "aggression_colleagues",
  "fear_losing_job_dich",
  "work_overtime_dich"
)

# Enter a "clean" name for your covariates
# (covariates need to be in the same order!)
clean_name <- c(
  "Teleworking group"
  , "EE-MBI score, median (IQR)"
  , "Severe emotional exhaustion"
  , "Diagnosed burnout"
  , "*Age group"
  # , "Age in years, median (IQR)"
  , "*Sex"
  , "*Education"
  , "*Household income"
  , "*Financial security"
  , "Ethnicity"
  # , "Country of residence"
  , "*Living arrangement"
  , "*Has young children"
  , "*Overcrowded household"
  , "*Noise level at home"
  , "*No access to a quiet room"
  , "*Self-reported general health"
  , "Self-reported mental state"
  , "Chronic condition"
  # , "Smoker status"
  # , "Alcohol consumption"
  , "Teleworking frequency"
  , "*Change in working hours"
  , "*Management duties"
  , "Work social support"
  , "Aggression from colleagues"
  , "Fears losing job"
  , "Works overtime"
)

in_regression = case_when(str_detect(clean_name, "[*]")~ TRUE,
                          .default = FALSE)
clean_name <- str_remove(clean_name,"[*]")


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Make a table of the variables and their "clean" names

# Make a table of the variables and their "clean" names
clean_labels <- tibble(covariates, clean_name, in_regression) %>% rename(var_label = covariates)
rm(clean_name) # remove these to avoid any confusion later


## Overall values ####
table_overall <- dat %>% select(all_of(covariates)) %>% select(-wfh_exposure) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    # digits = list(all_categorical() ~ c(0, 1)),
    percent = "col",                         # calculate percent column-wise
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    # digits = list(all_categorical() ~ 1),
    missing = "no",
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA);table_overall
## Add clean names
clean_table <- table_overall$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) %>% 
  # remove one of the levels for binary variables, to save some space in the table
  mutate(delete = case_when(
    variable == "burn_out" & label == "No" ~ 1,
    variable == "burnout_interp_dich30" & label == "Not Severe" ~ 1,
    variable == "num_young_children_dich" & label == "No young children" ~ 1,
    variable == "overcrowded" & label == "Not overcrowded" ~ 1,
    variable == "quiet_room" & label == "Yes" ~ 1,
    variable == "health_general_dich" & label == "Good" ~ 1,
    variable == "mental_state_dich" & label == "Good" ~ 1,
    variable == "chronic_disease_en" & label == "No" ~ 1,
    variable == "aggression_colleagues" & label == "No" ~ 1,
    variable == "fear_losing_job_dich" & label == "No" ~ 1,
    label == "Missing" ~ 1,                # Remove missing lines from tables, as these are implied in comparing to overall N
    .default = NA
  )) %>% filter(is.na(delete))

binary_keep <- clean_table %>% 
  filter((row_type == "level" & 
           variable %in% c(
             "burn_out",
             "burnout_interp_dich30",
             "num_young_children_dich",
             "overcrowded",
             "quiet_room",
             # "health_general_dich",
             # "mental_state_dich",
             "chronic_disease_en",
             "aggression_colleagues",
             "fear_losing_job_dich"
           ))) %>% select(variable, stat_0) %>% rename("stat_1" = "stat_0")

clean_table <- clean_table %>% 
  filter(!(row_type == "level" & 
            variable %in% c(
              "burn_out",
              "burnout_interp_dich30",
              "num_young_children_dich",
              "overcrowded",
              "quiet_room",
              # "health_general_dich",
              # "mental_state_dich",
              "chronic_disease_en",
              "aggression_colleagues",
              "fear_losing_job_dich"
            ))) %>% 
  left_join(binary_keep) %>% 
  mutate(stat_0 = if_else(is.na(stat_1), stat_0, stat_1)) %>% 
  select(-c(stat_1, delete))

table_overall$table_body <- clean_table ; rm(clean_table)
table_overall <-  table_overall %>% modify_column_hide(columns = c(stat_0)) # remove overall column
table_overall  %>% as_flex_table()

## By teleworking ####
table_telework <- dat %>% 
  select(all_of(covariates)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>%
  tbl_summary(
    by = wfh_exposure,
    percent = "col",                         # calculate percent row-wise
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    # digits = list(all_categorical() ~ 1),
    missing = "no",
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  add_n() %>%
  add_p(
    test = all_categorical() ~ "chisq.test",
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
  ) %>%
  # add_q() %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Group**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>% 
  add_overall() %>%
  modify_header(stat_0 ~ "**Overall**\nN = {N}");table_telework
  
## Add clean names
clean_table <- table_telework$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) %>%
  mutate(delete = case_when(
    variable == "burn_out" & label == "No" ~ 1,
    variable == "burnout_interp_dich30" & label == "Not Severe" ~ 1,
    variable == "num_young_children_dich" & label == "No young children" ~ 1,
    variable == "overcrowded" & label == "Not overcrowded" ~ 1,
    variable == "quiet_room" & label == "Yes" ~ 1,
    variable == "health_general_dich" & label == "Good" ~ 1,
    variable == "mental_state_dich" & label == "Good" ~ 1,
    variable == "chronic_disease_en" & label == "No" ~ 1,
    variable == "mental_state_dich" & label == "Good" ~ 1,
    variable == "aggression_colleagues" & label == "No" ~ 1,
    variable == "fear_losing_job_dich" & label == "No" ~ 1,
    label == "Missing" ~ 1,
    .default = NA
  )) %>% filter(is.na(delete))

binary_keep <- clean_table %>% 
  filter((row_type == "level" & 
            variable %in% c(
              "burn_out",
              "burnout_interp_dich30",
              "num_young_children_dich",
              "overcrowded",
              "quiet_room",
              # "health_general_dich",
              # "mental_state_dich",
              "chronic_disease_en",
              "aggression_colleagues",
              "fear_losing_job_dich"
            ))) %>% select(variable, stat_0:stat_5) %>% 
  rename("stat_0x" = "stat_0", "stat_1x" = "stat_1", "stat_2x" = "stat_2", "stat_3x" = "stat_3",
         "stat_4x" = "stat_4", "stat_5x" = "stat_5")

clean_table <- clean_table %>% 
  filter(!(row_type == "level" & 
             variable %in% c(
               "burn_out",
               "burnout_interp_dich30",
               "num_young_children_dich",
               "overcrowded",
               "quiet_room",
               # "health_general_dich",
               # "mental_state_dich",
               "chronic_disease_en",
               "aggression_colleagues",
               "fear_losing_job_dich"
             ))) %>% 
  left_join(binary_keep, join_by(variable)) %>% 
  mutate(stat_0 = if_else(is.na(stat_0x), stat_0, stat_0x),
         stat_1 = if_else(is.na(stat_1x), stat_1, stat_1x),
         stat_2 = if_else(is.na(stat_2x), stat_2, stat_2x),
         stat_3 = if_else(is.na(stat_3x), stat_3, stat_3x),
         stat_4 = if_else(is.na(stat_4x), stat_4, stat_4x),
         stat_5 = if_else(is.na(stat_5x), stat_5, stat_5x)
         ) %>% 
  select(-c(stat_0x:stat_5x, delete))

table_telework$table_body <- clean_table ; rm(clean_table)
table_telework <- table_telework %>% 
  modify_column_hide(columns = c(n))

table_telework %>% as_flex_table()

## Combine for main table - TW group only ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
table_combined <-
  tbl_merge(
    tbls = list(table_overall, 
                table_telework),
    tab_spanner = c(NA, 
                    "**Teleworking group**")
  ) ; table_combined %>% as_flex_table()
a <- table_combined$table_body %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid)

table_combined2 <- table_combined %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  add_footer_lines("Data are n (%) unless otherwise stated. % may not add up to 100% due to rounding.") %>%
  footnote(i = 2, j = 9, 
           value = as_paragraph(" Kruskal-Wallis rank sum test; Pearson's Chi-squared test"),
           ref_symbols = c("a"),
           part = "header") %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(3), border = fp_border_default(color = "grey")) %>%
  hline(i = c(3, 43, 48), border = fp_border_default(color = "black")) %>% # lines to indicate variable themes
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # # Fontsize 7
  height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 7, part = "all") %>% 
  width(j = 1, width = 3.9, unit = "cm") %>% 
  width(j = 2, width = 1.2, unit = "cm") %>% 
  width(j = c(3,4,7,8), width = 1.9, unit = "cm") %>% 
  width(j = c(5,6), width = 1.6, unit = "cm") %>%
  width(j = c(9), width = 1.2, unit = "cm") %>%
  line_spacing(space = 0.1, part = "body") %>%
  valign(valign = "bottom", part = "body") ; table_combined2

# Save as docx ####
save_as_docx(
  "Table 1: Participant summary" = table_combined2, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Exposure table_main_TW.docx")))
