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
                               0 ~ "No")
         )


# Descriptive table of participant demographics ####
var_included <- c(
  "burnout_interp_dich30"
  , "burnout_score"
  , "burn_out"
  , "sex_en"
  , "age_cat"
  # , "age_cat2"
  , "age"
  , "education_rec_en"
  , "hh_income_cat_en"
  , "finance_situation"
  ,"percent_change_cat"
  ,"supervision_short" # which variable to use for financial security? or too linked with income?
  ,"hh_livewith_rec_en"
  ,"num_young_children_dich"
  ,"overcrowded"
  ,"noisy"
  ,"quiet_room"
  ,"health_general_dich"
)

## Table of totals for the variables included ##
tbl1_col <- dat %>% select(all_of(var_included)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    # digits = list(all_categorical() ~ c(0, 1)),
    percent = "col",                         # calculate percent column-wise
    label  = list(
      "burnout_interp_dich30" ~ "Emotional Exhaustion"
      , "burnout_score" ~ "EE-MBI score, median (IQR)"
      , "burn_out" ~ "Diagnosed burnout"
      , "sex_en" ~ "Sex"
      , "age" ~ "Age in years, median (IQR)"
      , "age_cat" ~ "Age group, years"
      # , "age_cat2" ~ "Age group, years"
      , "education_rec_en" ~ "Education"
      , "hh_income_cat_en" ~ "Household income"
      , "finance_situation" ~ "Financial security"
      , "percent_change_cat" ~ "Contract percentage"
      , "supervision_short" ~ "Management role"
      , "hh_livewith_rec_en" ~ "Living arrangement"
      , "num_young_children_dich" ~ "Young children"
      , "overcrowded" ~ "Household density"
      , "noisy" ~ "Noise level at home"
      , "quiet_room" ~ "Access to a quiet room"
      , "health_general_dich" ~ "Self-reported general health"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA)
tbl1_col %>% as_flex_table()

## Main Descriptive table by teleworking status ####
tbl1_telework <- dat %>% 
  select(wfh_exposure, all_of(var_included)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missimng")) %>% 
  droplevels() %>% 
  tbl_summary(
    by = wfh_exposure,
    percent = "col",                         # calculate percent row-wise
    label  = list(
      "burnout_interp_dich30" ~ "Emotional Exhaustion"
      , "burnout_score" ~ "EE-MBI score, median (IQR)"
      , "burn_out" ~ "Diagnosed burnout"
      , "sex_en" ~ "Sex"
      , "age" ~ "Age in years, median (IQR)"
      , "age_cat" ~ "Age group, years"
      # , "age_cat2" ~ "Age group, years"
      , "education_rec_en" ~ "Education"
      , "hh_income_cat_en" ~ "Household income"
      , "finance_situation" ~ "Financial security"
      , "percent_change_cat" ~ "Contract percentage"
      , "supervision_short" ~ "Management role"
      , "hh_livewith_rec_en" ~ "Living arrangement"
      , "num_young_children_dich" ~ "Young children"
      , "overcrowded" ~ "Household density"
      , "noisy" ~ "Noise level at home"
      , "quiet_room" ~ "Access to a quiet room"
      , "health_general_dich" ~ "Self-reported general health"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Frequency**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}"))
  # add_overall() 
tbl1_telework %>% as_flex_table()

## Combine the main table ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
tbl1_wfh_cat_combined <-
  tbl_merge(
    tbls = list(tbl1_col, tbl1_telework),
    tab_spanner = c(NA, "**Teleworking frequency relative to before the pandemic**")
  ) 
a <- tbl1_wfh_cat_combined$table_body %>%           # Painful way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid) ; 
tbl1_wfh_cat_combined2 <- tbl1_wfh_cat_combined %>% 
  # as_gt(include = -cols_align) %>%
  # gt::tab_source_note(gt::md("Data are n (%) unless otherwise stated.")) %>%
  # tab_style(
  #   style =  list(
  #     cell_fill(color = "lightgrey")),
  #   locations = cells_body(rows = c(1,5))
  #   ) ; tbl1_wfh_cat_combined
  as_flex_table() %>%
  # theme_zebra(odd_header = "transparent") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>%
  # autofit() %>%
  # set_table_properties(width = .1, layout = "autofit") %>%
  vline(j = c(2), border = fp_border_default(color = "grey")) %>%
  fontsize(size = 9, part = "all") %>% 
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "lightgrey") %>% 
  # bg(part = "header", bg = "lightgrey") %>% 
  bg(i = c(4,17) ,part = "body", bg = "white") %>% 
  add_footer_lines("Data are n (%) unless otherwise stated") %>% 
  height_all(height = 1, part = "body", unit = "cm") %>% 
  width(j = 1, width = 5.3, unit = "cm") %>% 
  width(j = 2:7, width = 2.1, unit = "cm") %>% 
  valign(valign = "center", part = "body") %>% 
  line_spacing(space = 0.5, part = "body") ; tbl1_wfh_cat_combined2


# # Output main table into emf
# emf(file = here("output", "publication figures", "Exposure table_main.emf"), width = 9, height = 13, bg = "white")
# plot(tbl1_wfh_cat_combined) ;   dev.off()

save_as_docx(
  "Table 1: Participant summary" = tbl1_wfh_cat_combined2, path = here("output", "publication figures", "Exposure table_main.docx"))





# Occupation descriptive tables ####
## Table of totals for the variables included ##
tbl1_col_occup <- dat %>% select(Occupation_label_2) %>% 
  tbl_summary(
    percent = "col",                         # calculate percent column-wise
    label  = list(
      "Occupation_label_2" ~ "ISCO Occupation group"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>% 
  modify_spanning_header(everything() ~ NA) %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %") 
tbl1_col_occup %>% as_flex_table()

## Teleworking status ####
tbl1_telework_occup <- dat %>% 
  select(wfh_exposure, Occupation_label_2) %>% 
  tbl_summary(
    by = wfh_exposure,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "Occupation_label_2" ~ "ISCO Occupation group"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Frequency**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_telework_occup %>% as_flex_table()

# ## Burnout ###
# dat %>% group_by(Occupation_label_2) %>% 
# count(burn_out) %>% 
#   mutate(percent = scales::percent(round(n/sum(n, na.rm = TRUE),4), accuracy = 0.1)) %>% 
#   filter(burn_out == "Yes") %>% flextable() %>% as

## Burnout ####
tbl1_burnout_occup <- dat %>% 
  select(burn_out, Occupation_label_2) %>% 
  tbl_summary(
    by = burn_out,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "Occupation_label_2" ~ "ISCO Occupation group"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Diagnosed burnout**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_burnout_occup %>% as_flex_table()

## Emotional Exhaustion ####
tbl1_exhaustion_occup <- dat %>% 
  select(burnout_interp_dich30, Occupation_label_2) %>% 
  tbl_summary(
    by = burnout_interp_dich30,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "Occupation_label_2" ~ "ISCO Occupation group"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Emotional Exhaustion**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_exhaustion_occup %>% as_flex_table()

## Contract change ####
tbl1_contract_occup <- dat %>% 
  select(percent_change_cat, Occupation_label_2) %>% 
  tbl_summary(
    by = percent_change_cat,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "Occupation_label_2" ~ "ISCO Occupation group"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Change in contract**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_contract_occup %>% as_flex_table()


## Combine table ####
tbl1_occup_combined <-
  tbl_merge(
    tbls = list(tbl1_col_occup, tbl1_telework_occup, tbl1_contract_occup, tbl1_exhaustion_occup, tbl1_burnout_occup),
    tab_spanner = c(NA, "**WFH frequency**", "**Contract Percentage**", "**Emotional Exhaustion**", "**Diagnosed Burnout**")
  ) %>% as_flex_table() %>% autofit() %>%  theme_zebra() %>% 
  vline(j = c(2, 7,10, 12), border = fp_border_default()) %>% 
  align(align = "center", part = "all")
tbl1_occup_combined

### Output main table into emf ####
emf(file = here("output", "publication figures", "Exposure table_occupations.emf"), 
    width = 13, height = 7,
    bg = "white")
plot(tbl1_occup_combined) ;   dev.off()

  
  
### ### ### ### ### ### ### ### ###
# Job sector descriptive tables ####
### ### ### ### ### ### ### ### ###
  
## Table of totals for the variables included ##
tbl1_col_jobsector <- dat %>% select(job_sector) %>% 
  tbl_summary(
    percent = "col",                         # calculate percent column-wise
    label  = list(
      "job_sector" ~ "Job sector"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>% 
  modify_spanning_header(everything() ~ NA) %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %") 
tbl1_col_jobsector %>% as_flex_table()

## Teleworking status ####
tbl1_telework_jobsector <- dat %>% 
  select(wfh_exposure, job_sector) %>% 
  tbl_summary(
    by = wfh_exposure,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "job_sector" ~ "Job sector"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Frequency**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_telework_jobsector %>% as_flex_table()

# ## Burnout ###
# dat %>% group_by(job_sector) %>% 
# count(burn_out) %>% 
#   mutate(percent = scales::percent(round(n/sum(n, na.rm = TRUE),4), accuracy = 0.1)) %>% 
#   filter(burn_out == "Yes") %>% flextable() %>% as

## Burnout ####
tbl1_burnout_jobsector <- dat %>% 
  select(burn_out, job_sector) %>% 
  tbl_summary(
    by = burn_out,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "job_sector" ~ "Job sector"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Diagnosed burnout**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_burnout_jobsector %>% as_flex_table()

## Emotional Exhaustion ####
tbl1_exhaustion_jobsector <- dat %>% 
  select(burnout_interp_dich30, job_sector) %>% 
  tbl_summary(
    by = burnout_interp_dich30,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "job_sector" ~ "Job sector"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Emotional Exhaustion**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_exhaustion_jobsector %>% as_flex_table()


## Combine table ####
tbl1_jobsector_combined <-
  tbl_merge(
    tbls = list(tbl1_col_jobsector, tbl1_telework_jobsector, tbl1_exhaustion_jobsector, tbl1_burnout_jobsector),
    tab_spanner = c(NA, "**WFH frequency**", "**Emotional Exhaustion**", "**Diagnosed Burnout**")
  ) %>% as_flex_table() %>% autofit() %>%  theme_zebra() %>% 
  vline(j = c(2, 7,9), border = fp_border_default()) %>% 
  align(align = "left", part = "all")
tbl1_jobsector_combined

### Output main table into emf ####
emf(file = here("output", "publication figures", "Exposure table_job sector.emf"), 
    width = 13, height = 7,
    bg = "white")
plot(tbl1_jobsector_combined) ;   dev.off()


# Teleworking and decreased percentage ####
## Table of totals for the variables included ##
tbl1_col_telework_changework <- dat %>% select(percent_change_cat) %>% 
  tbl_summary(
    percent = "col",                         # calculate percent column-wise
    label  = list(
      "percent_change_cat" ~ "Contract percentage"
      ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>% 
  modify_spanning_header(everything() ~ NA) %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %") 
tbl1_col_telework_changework %>% as_flex_table()

## Change in contract ####
tbl1_telework_changework <- dat %>% 
  select(wfh_exposure, percent_change_cat) %>% 
  tbl_summary(
    by = wfh_exposure,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      "percent_change_cat" ~ "Contract percentage"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Frequency**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  # add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %")
tbl1_telework_changework %>% as_flex_table()

## Combine table ####
tbl1_telework_changework_combined <-
  tbl_merge(
    tbls = list(tbl1_col_telework_changework, tbl1_telework_changework),
    tab_spanner = c(NA, "**Teleworking Frequency**")
  ) %>% as_flex_table() %>% autofit() %>%  theme_zebra() %>% 
  # vline(j = c(1), border = fp_border_default()) %>%
  align(align = "center", part = "all")
tbl1_telework_changework_combined


### Output main table into emf ####
emf(file = here("output", "publication figures", "Exposure table_job sector.emf"), 
    width = 13, height = 7,
    bg = "white")
plot(tbl1_jobsector_combined) ;   dev.off()