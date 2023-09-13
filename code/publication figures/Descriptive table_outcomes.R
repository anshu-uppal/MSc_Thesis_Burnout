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

dat <- tibble(readRDS(here("data", "clean_dataset.rds"))) %>%
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

outcomes <- c(
  # "burnout_score", 
  "burnout_interp_dich30", 
  "burn_out")
outcomes_clean <- c(
  # "EE-MBI score", 
  "Severe emotional exhaustion", 
  "Diagnosed burnout")

# Enter the variables for your covariates 
var_included <- c(
  "wfh_exposure"
  , "age_cat",
  "sex_en",
  "education_rec_en",
  "ethn_dich",
  "country_residence",
  "hh_livewith_rec_en",
  "num_young_children_dich",
  "overcrowded",
  "noisy",
  "quiet_room",
  "health_general_dich",
  "mental_state_dich",
  "chronic_disease_en",
  "smoking_rec_en", 
  "alcohol_en",
  "wfh",
  "hh_income_cat_en",
  "finance_situation",
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
  "*Teleworking group"   
  , "*Age group, years"
  , "*Sex"
  , "*Education"
  , "Ethnicity"
  , "Country of residence"
  , "*Living arrangement"
  , "*Young children"
  , "*Household density"
  , "*Noise level at home"
  , "*Access to a quiet room"
  , "*Self-reported general health"
  , "Self-reported mental state"
  , "Chronic condition"
  , "Smoker status"
  , "Alcohol consumption"
  , "Teleworking frequency"
  , "*Household income"
  , "*Financial security"
  , "*Change in working hours"
  , "*Management duties"
  , "Work social support"
  , "Aggression from colleagues"
  , "Fears losing job"
  , "Works overtime"
)


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Make a table of the variables and their "clean" names
clean_labels <- tibble(var_included, clean_name) %>% rename(var_label = var_included)
rm(clean_name) # remove these to avoid any confusion later

## By outcomes ####
table_outcomes <- vector("list", length = length(outcomes))
for (i in 1:length(outcomes)) {
table_outcome <- dat %>% 
  select(all_of(var_included), outcomes[i]) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
  droplevels() %>% 
  tbl_summary(
    by = outcomes[i],
    percent = "row",                         # calculate percent row-wise
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ paste0("**", outcomes_clean[i],"**")) %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}"))
## Add clean names
clean_table <- table_outcome$table_body %>% left_join(clean_labels) %>% 
  mutate(label = if_else(row_type == "label", clean_name, label)) %>% select(-clean_name) %>% 
  mutate(delete = case_when(
    # variable == "burn_out" & label == "No" ~ 1,
    # variable == "burnout_interp_dich30" & label == "Not Severe" ~ 1,
    # variable == "num_young_children_dich" & label == "No young children" ~ 1,
    # variable == "overcrowded" & label == "Not overcrowded" ~ 1,
    # variable == "quiet_room" & label == "Yes" ~ 1,
    # variable == "health_general_dich" & label == "Good" ~ 1,
    # variable == "mental_state_dich" & label == "Good" ~ 1,
    # variable == "chronic_disease_en" & label == "No" ~ 1,
    # variable == "mental_state_dich" & label == "Good" ~ 1,
    # variable == "aggression_colleagues" & label == "No" ~ 1,
    # variable == "fear_losing_job_dich" & label == "No" ~ 1,
    label == "Missing" ~ 1,
    .default = NA
  )) %>% filter(is.na(delete))
table_outcome$table_body <- clean_table ; rm(clean_table)
table_outcomes[i] <- list(table_outcome)
}

## Combine for main table - outcomes only ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
table_combined <-
  tbl_merge(
    tbls = list(table_outcomes[[1]], 
                table_outcomes[[2]]),
    tab_spanner = c(outcomes_clean[1], 
                    outcomes_clean[2])
  ) 
a <- table_combined$table_body %>%           # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid) ; 
table_combined2 <- table_combined %>% 
  # as_gt(include = -cols_align) %>%
  # gt::tab_source_note(gt::md("Data are n (%) unless otherwise stated.")) %>%
  # tab_style(
  #   style =  list(
  #     cell_fill(color = "lightgrey")),
  #   locations = cells_body(rows = c(1,5))
  #   ) ; sex_table_combined
  as_flex_table() %>%
  # theme_zebra(odd_header = "transparent") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(3), border = fp_border_default(color = "grey")) %>%
  # hline(i = c(37, 54, 89), border = fp_border_default(color = "black")) %>% # lines to indicate variable themes
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # bg(part = "header", bg = "lightgrey") %>% 
  # bg(i = c(14,22) ,part = "body", bg = "white") %>%      # no background for continuous variables (count out the rows...)
  add_footer_lines("Data are n (%) unless otherwise stated. % may not add up to 100% due to rounding. Variables denoted with an asterisk (*) are covariates in the multivariable models") %>%
  # # Fontsize 9
  # height_all(height = 1, part = "body", unit = "cm") %>%
  # fontsize(size = 9, part = "all") %>%
  # width(j = 1, width = 5.3, unit = "cm") %>%
  # width(j = c(2:4,10), width = 2.1, unit = "cm") %>%
  # width(j = c(5:9), width = 1.9, unit = "cm") %>%
  # # Fontsize 7
  # height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 7, part = "all") %>%
  width(j = 1, width = 3.8, unit = "cm") %>%
  width(j = c(2:5), width = 1.7, unit = "cm") %>%
  line_spacing(space = 0.1, part = "body") %>%
  valign(valign = "bottom", part = "body") ; table_combined2

# Save as docx ####
save_as_docx(
  "Table 1: Participant summary" = table_combined2, 
  path = here("output", "publication figures", "Descriptive table_outcomes.docx"))
