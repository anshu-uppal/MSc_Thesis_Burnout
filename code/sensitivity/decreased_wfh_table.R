# Descriptive table for decreased teleworking
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
  mutate(
    decrease_binary = case_when(
      wfh_exposure %in% c("Decrease") ~ "Decrease",
      !wfh_exposure %in% c("Decrease") ~ "No decrease",
      .default = NA),
    not_possible = case_when(
      wfh_exposure == "Not possible" ~ 1,
      wfh_exposure != "Not possible" ~ 0,
      .default = NA),
    age_cat2 = factor(case_when(     
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
  ) %>% filter(wfh_dich == "Yes") # filter for only those who can work from home


# Enter the variables for your covariates 
var_included <- c(
  "decrease_binary"
  , "Occupation_label_2"
)

# Enter a "clean" name for your covariates
# (covariates need to be in the same order!)
clean_name <- c(
  "Decreased Telework"
  , "ISCO Occupation label"

)


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Make a table of the variables and their "clean" names
clean_labels <- tibble(var_included, clean_name) %>% rename(var_label = var_included)
rm(clean_name) # remove these to avoid any confusion later


## Overall values ####
table_overall <- dat %>% select(all_of(var_included)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    # digits = list(all_categorical() ~ c(0, 1)),
    percent = "col",                         # calculate percent column-wise
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**\nN = {n}") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA)
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
    variable == "mental_state_dich" & label == "Good" ~ 1,
    .default = NA
  )) %>% filter(is.na(delete))
table_overall$table_body <- clean_table ; rm(clean_table)
table_overall %>% as_flex_table()

## By Change in working hours ####
table_decrease_binary <- dat %>% 
  select(all_of(var_included)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missimng")) %>% 
  droplevels() %>% 
  tbl_summary(
    by = decrease_binary,
    percent = "col",                         # calculate percent row-wise
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Decreased telework?**") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}"))
## Add clean names
clean_table <- table_decrease_binary$table_body %>% left_join(clean_labels) %>% 
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
    .default = NA
  )) %>% filter(is.na(delete))
table_decrease_binary$table_body <- clean_table ; rm(clean_table)
table_decrease_binary %>% as_flex_table()

## Combine for main table ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
table_combined <-
  tbl_merge(
    tbls = list(table_overall, table_decrease_binary),
    tab_spanner = c(NA, "Decreased Telework")
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
  vline(j = c(2), border = fp_border_default(color = "grey")) %>%
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  # bg(part = "header", bg = "lightgrey") %>% 
  bg(i = c(9,17) ,part = "body", bg = "white") %>%      # no background for continuous variables (count out the rows...)
  add_footer_lines("Data are n (%) unless otherwise stated") %>%
  # # Fontsize 9
  # height_all(height = 1, part = "body", unit = "cm") %>%
  # fontsize(size = 9, part = "all") %>%
  # width(j = 1, width = 5.3, unit = "cm") %>%
  # width(j = c(2:4,10), width = 2.1, unit = "cm") %>%
  # width(j = c(5:9), width = 1.9, unit = "cm") %>%
  # # Fontsize 7
  height_all(height = 0, part = "body", unit = "cm") %>%
  fontsize(size = 7, part = "all") %>% 
  width(j = 1, width = 4.2, unit = "cm") %>% 
  width(j = c(2:3), width = 1.8, unit = "cm") %>% 
  line_spacing(space = 0.1, part = "body") %>%
  valign(valign = "bottom", part = "body") ; table_combined2

# Save as docx ####
save_as_docx(
  "Table 1: Participant summary" = table_combined2, path = here("output", "publication figures", "sensitivity figures",  "decreased wfh by occupation.docx"))
