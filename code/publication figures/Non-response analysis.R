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
dat <- tibble(readRDS(here("data", "flow diagram.rds")))
# # Alternative is source it directly (but sometimes issues with encoding? variable has young children gets thrown off when sourced:
# source(here::here("code", "Flow diagram.R"), encoding = "UTF-8")

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

## Variables to include ####
var_included <- c(
  "age_cat_invitation"
  , "age_invitation"
  , "sex_en"
  , "education_rec_en"
  , "ethn_dich"
  , "country_residence"
  ,"hh_livewith_rec_en"
  ,"num_young_children_dich"
  ,"overcrowded"
  ,"noisy"
  ,"quiet_room"
  ,"health_general_dich"
  ,"mental_state_dich"
  , "chronic_disease_en"
  , "smoking_rec_en"
  , "alcohol_en"
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
  , "num_young_children_dich" ~ "Young children"
  , "overcrowded" ~ "Household density"
  , "noisy" ~ "Noise level at home"
  , "quiet_room" ~ "Access to a quiet room"
  , "health_general_dich" ~ "Self-reported general health"
  , "mental_state_dich" ~ "Self-reported mental state"
  , "chronic_disease_en" ~ "Chronic condition"
  , "smoking_rec_en" ~ "Smoker status"
  , "alcohol_en" ~ "Alcohol consumption"
  , "ethn_dich" ~ "Ethnicity"
  , "country_residence" ~ "Country of residence"
)

# Table creation ####
## Table of totals for the variables included ####
tbl1_col <- dat %>% select(all_of(var_included)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
  # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
  droplevels() %>% 
  tbl_summary(
    # digits = list(all_categorical() ~ c(0, 1)),
    percent = "col",                         # calculate percent column-wise
    label  = var_list_names,
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  # add_n() %>%
  bold_labels() %>% 
  # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Invited**\nN = {n}") %>%
  modify_footnote(all_stat_cols() ~ NA) %>% 
  modify_spanning_header(everything() ~ NA)
tbl1_col %>% as_flex_table()

## Table by response status ####
tbl1_telework <- dat %>% 
  select(Responded, all_of(var_included)) %>% 
  mutate(across(where(is.character), as.factor)) %>%
  mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
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
  # modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}\n({style_percent(p)}%)")) %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}"))
# add_overall() 
tbl1_telework %>% as_flex_table()

## Combine the main table ####
# !! Try to get it so that only the levels have a different color, and the labels are white !!
tbl1_wfh_cat_combined <-
  tbl_merge(
    tbls = list(tbl1_col, tbl1_telework),
    tab_spanner = c(NA, NA)
  ) 
a <- tbl1_wfh_cat_combined$table_body %>%           # Painful way to ID the variable labels and for later coloring
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
  # bg(part = "all", bg = "grey") %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>%           # Add background for all variable labels
  # bg(part = "header", bg = "lightgrey") %>% 
  # bg(i = c(12) ,part = "body", bg = "white") %>%                    # Remove background for Age (continuous)
  add_footer_lines("Data are n (%) unless otherwise stated") %>% 
  fontsize(size = 7, part = "all") %>% 
  height_all(height = 0, part = "body", unit = "cm") %>% 
  width(j = 1, width = 4.2, unit = "cm") %>% 
  width(j = 2, width = 1.9, unit = "cm") %>% 
  width(j = 3, width = 2.5, unit = "cm") %>%
  width(j = 4, width = 2, unit = "cm") %>%
  valign(valign = "center", part = "body") %>% 
  line_spacing(space = 0.1, part = "body") ; tbl1_wfh_cat_combined2

# # Output main table into emf
# emf(file = here("output", "publication figures", "Exposure table_main.emf"), width = 9, height = 13, bg = "white")
# plot(tbl1_wfh_cat_combined) ;   dev.off()

save_as_docx(
  "Table 1: Characteristics of respondents vs. non-respondents" = tbl1_wfh_cat_combined2, path = here("output", "publication figures", "Non-response summary.docx"))



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
