pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  lubridate,     # manipulate date objects
  apyramid      # a package dedicated to creating age pyramids
)

# Read in data ####
# Input flow diagram data from file previously generated using "Flow diagram.R"
dat <- tibble(readRDS(here("data", "Generated datasets", "flow_diagram.rds")))
# # Alternative is source it directly (but sometimes issues with encoding? variable has young children gets thrown off when sourced:
# source(here::here("code", "Flow_diagram.R"), encoding = "UTF-8")

## Initial filters to get dataset that includes only invited people ####
dat <- dat %>% 
  filter(
    # Remove opt-outs
    !send_status %in% c("Suppressed"),
    # Remove people who didn't participate in serosurveys
    serocov_work.x | serocov_pop.x | sp3_juin_2021.x | sp2_novdec_2020.x | sp4_avril_2022.x |pop_pilote.x | work_pilote.x,
    # Remove aged 67 and over at inclusion
    age_inclusion < 67,
    # Keep only people in salaried position at inclusion
    work_situation.x %in% c("Salarié-e"),
    # remove failed emails
    !send_status %in% c("Bounced", "Failed"),
    # remove these not invited (who I think are people who did not create a Specchio account)
    !is.na(send_status)
    )

## Point recoding ####
# Recode Other to education level where possible, otherwise set other to NA
dat <- dat %>% 
  mutate(
    
    # Recode the "Other" from education level
    education_rec_en = factor(case_when(
      participant_id == "6d6cd138-29b0-11eb-9501-0050568507d9" ~ "Tertiary",  # brevet avocat
      participant_id == "9e00dd58-29ba-11eb-aa4e-0050568507d9" ~ "Secondary", # Secrétaire médicale
      participant_id == "d33787cc-3029-11eb-8bf2-0050568507d9" ~ "Secondary", # Attestation fédérale professionnelle ( ASA)
      participant_id == "8e0e18ce-b966-11ec-8a87-fa163eeab1c6" ~ "Secondary",  # Ecole privée de secrétariat
      participant_id == "a90f4ff0-30a8-11eb-9a35-0050568507d9" ~ "Primary", # Agent de sûreté à l'aéroport
      .default = education_rec_en
    )
    ,levels = c("Tertiary", "Secondary", "Primary")
    )
  )

## Variables to include ####
var_included <- c(
  "age_cat_invitation"
  , "age_invitation"
  , "sex_en"
  , "education_rec_en"
  , "ethn_dich"
  # , "country_residence"
  ,"hh_livewith_rec_en"
  ,"num_young_children_dich"
  ,"overcrowded"
  ,"noisy"
  ,"quiet_room"
  ,"health_general_dich"
  ,"mental_state_dich"
  , "chronic_disease_en"
  # , "smoking_rec_en"
  # , "alcohol_en"
  , "hh_income_cat_en"
  , "finance_situation"
)

# Make clean names
var_list_names <- list(
   "age_invitation" ~ "Age in years, median (IQR)"
  , "age_cat_invitation" ~ "Age group, years"
  , "sex_en" ~ "Sex"
  , "education_rec_en" ~ "Education"
  , "hh_income_cat_en" ~ "Household income"
  , "finance_situation" ~ "Financial security"
  , "hh_livewith_rec_en" ~ "Living arrangement"
  , "num_young_children_dich" ~ "Has young children"
  , "overcrowded" ~ "Overcrowded household"
  , "noisy" ~ "Noise level at home"
  , "quiet_room" ~ "No access to a quiet room"
  , "health_general_dich" ~ "Self-reported general health"
  , "mental_state_dich" ~ "Self-reported mental state"
  , "chronic_disease_en" ~ "Has chronic condition"
  # , "smoking_rec_en" ~ "Smoker status"
  # , "alcohol_en" ~ "Alcohol consumption"
  , "ethn_dich" ~ "Ethnicity"
  # , "country_residence" ~ "Country of residence"
)

# Table creation ####
## Table of totals for the variables included ####
tbl1_col <- dat %>% select(all_of(var_included)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    # digits = list(all_categorical() ~ c(0, 1)),
    percent = "col",                         # calculate percent column-wise
    label  = var_list_names,
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Invited**\nN = {n}") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA)
tbl1_col %>% as_flex_table()

## Add clean names
clean_table <- tbl1_col$table_body %>%
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

tbl1_col$table_body <- clean_table ; rm(clean_table)
tbl1_col <-  tbl1_col %>% 
  modify_column_hide(columns = c(stat_0))
tbl1_col  %>% as_flex_table()

## Table by response status ####
tb1_responded <- dat %>% 
  select(Responded, all_of(var_included)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missimng")) %>% 
  droplevels() %>% 
  tbl_summary(
    by = Responded,
    percent = "col",                         # calculate percent row-wise
    label  = var_list_names,
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Frequency**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
add_p(
  test = all_categorical() ~ "chisq.test",
  pvalue_fun = function(x) style_pvalue(x, digits = 3),
  test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
    ) %>% 
  add_overall() %>% 
modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>% 
  modify_header(stat_0 ~ "**Overall**\nN = {N}")
tb1_responded %>% as_flex_table()

## Add clean names
clean_table <- tb1_responded$table_body %>%
  filter(label != "Missing") %>% 
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
            ))) %>% select(variable, stat_0:stat_2) %>% 
  rename("stat_0x" = "stat_0", "stat_1x" = "stat_1", "stat_2x" = "stat_2")

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
         stat_2 = if_else(is.na(stat_2x), stat_2, stat_2x)
  ) %>% 
  select(-c(stat_0x:stat_2x, delete))

tb1_responded$table_body <- clean_table ; rm(clean_table)
# tb1_responded <- tb1_responded %>% 
#   modify_column_hide(columns = c(n))

tb1_responded %>% as_flex_table()

## Combine the main table ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
tbl1_wfh_cat_combined <-
  tbl_merge(
    tbls = list(tbl1_col, tb1_responded),
    tab_spanner = c(NA, NA)
  ) ; tbl1_wfh_cat_combined %>% as_flex_table()
a <- tbl1_wfh_cat_combined$table_body %>%           # Painful way to ID the variable labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid) ; 


tbl1_wfh_cat_combined2 <- tbl1_wfh_cat_combined %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  add_footer_lines("Data are n (%) unless otherwise stated. % may not add up to 100% due to rounding.") %>%
  footnote(i = 1, j = 6, 
           value = as_paragraph(" Kruskal-Wallis rank sum test; Pearson's Chi-squared test"),
           ref_symbols = c("a"),
           part = "header") %>% 
  # theme_zebra(odd_header = "transparent") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(3), border = fp_border_default(color = "grey")) %>%
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>%           # Add background for all variable labels
  # bg(part = "header", bg = "lightgrey") %>% 
  # bg(i = c(12) ,part = "body", bg = "white") %>%                    # Remove background for Age (continuous)
  line_spacing(space = 0.3, part = "body") %>%
  fontsize(size = 7, part = "all") %>% 
  height_all(height = 0, part = "body", unit = "cm") %>% 
  width(j = 1, width = 4.3, unit = "cm") %>% 
  width(j = 2, width = 1.2, unit = "cm") %>% 
  width(j = c(3,4,5), width = 2.5, unit = "cm") %>%
  width(j = 6, width = 1.2, unit = "cm") %>%
  valign(valign = "center", part = "body") ; tbl1_wfh_cat_combined2

# # Output main table into emf
# emf(file = here("output", "publication figures", "Exposure table_main.emf"), width = 9, height = 13, bg = "white")
# plot(tbl1_wfh_cat_combined) ;   dev.off()

save_as_docx(
  "Table 1: Characteristics of respondents vs. non-respondents" = tbl1_wfh_cat_combined2, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Non-response summary.docx")))



# # Look at recruitment source ####
# ## Invitations by source ####
# dat %>%
#   count(pop_source) %>%
#   mutate(percent = 100 * n / sum(n)) %>% 
#   ggplot(aes(x = pop_source, y = percent))+
#   geom_col()+
#   labs(title = "Invitations by participant recruitment source")+
#   # geom_text(aes(label = paste0(round(percent,1), " (N=", n,")")), nudge_y = 1)+
#   geom_text(aes(label = paste0(round(percent,1), "%")), nudge_y = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 10))
# 
# 
# ## Respondents by source ####
# dat %>%
#   group_by(Responded) %>%
#   count(pop_source) %>%
#   mutate(percent = 100 * n / sum(n)) %>% 
#   ggplot(aes(x = pop_source, y = percent))+
#   geom_col()+
#   labs(title = "Respondents by participant recruitment source")+
#   # geom_text(aes(label = paste0(round(percent,1), " (N=", n,")")), nudge_y = 1)+
#   geom_text(aes(label = paste0(round(percent,1), "%")), nudge_y = 1)+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 10))+
#   facet_wrap(.~Responded)
