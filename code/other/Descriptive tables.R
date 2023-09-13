# Load packages and dataset ####
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

# Read in merged dataset
merged <- tibble(readRDS("merged_dataset.rds"))

### ### ### ### ### ### ### ##
### Variables of interest ####
### ### ### ### ### ### ### ##

### See also the Schrempft et al. (2023) paper ###

# Age (years): age
# Sex (male, female): sex_en
# Ethnicity (white european, other): ethn
# Education level (primary, secondary, tertiary): education_rec_en
# Employment status (Employed, Self-employed, Unemployed, Retired, Other economically inactive)
# Household income
# Living circumstances
# Household density, ratio of people to bedrooms (Overcrowded vs not)
# Housing conditions
# Urbanicity
# Marital status: relation
# Number of children: children_n
# Number of people living in household: hh_composition
# BMI: bmi
# Burnout score: burnout_score
# Siegrist score: score_work_overinvestment_scale
# Karasek score: karasek_social_support
# Distance proxy between home and work using nip_house and nip_work?

# skim(dat.s) # have a general look at the dataset variables

### ### ### ### ### ### ### ### ### ###
### Dataset wrangling ####
### ### ### ### ### ### ### ### ### ###

dat <- merged %>% filter(work_situation.y == "En activité salariée") %>% # only include people salaried at the time of the interview
  mutate(
    date_soumission.y = as_date(date_soumission.y),
    date_soumission.x = as_date(date_soumission.x),
    birthdate = ymd(birthdate),
    sex_en = factor(case_match(sex_en, # recode "Other" as NA
                               "Male" ~ "Male",
                               "Female" ~ "Female",
                               "Other" ~ NA)),
    age = time_length(date_soumission.y - birthdate, "years"), # Age when completed santé-travail quest.
    age_cat = factor(case_when(     # Split ages into categories
      age >= 18 & age < 25 ~ "18-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65             ~ "65+"
    )),
    # job_sector = str_remove(job_sector, pattern = "Secteur de la"), # shorten the job_sector text
    Swiss_nat = factor(if_else(Swiss_nat == "TRUE", "Swiss", "Other")),
    children_n = if_else(children=="Non", 0, children_n), # Add zeros for people with no children
    hh_composition = factor(case_match(
      hh_composition,
      0 ~ "0",
      1 ~ "1",
      2 ~ "2",
      3 ~ "3",
      c(4,5,6,7) ~ "4+"
    )),
    wfh_cat = recode(wfh, # recode from French to English and categorise
                     "Non, jamais" = "Never/not possible",
                     "Non concerné-e (mon activité professionnelle n’est pas réalisable à distance)" = "Never/not possible",
                     "Oui, parfois (mais pas régulièrement)" = "Occasionally",
                     "Oui, à temps partiel (au moins une fois par semaine)" = "Occasionally",
                     "Oui, à temps complet (tous les jours)" = "Always"
    ),
    wfh_dich = recode(wfh, # recode from French to English and dichotomise
                      "Non, jamais" = "Never/not possible",
                      "Non concerné-e (mon activité professionnelle n’est pas réalisable à distance)" = "Never/not possible",
                      "Oui, parfois (mais pas régulièrement)" = "Yes",
                      "Oui, à temps partiel (au moins une fois par semaine)" = "Yes",
                      "Oui, à temps complet (tous les jours)" = "Yes"
    ),
    wfh = recode(wfh, # recode from French to English
                 "Non, jamais" = "Never",
                 "Non concerné-e (mon activité professionnelle n’est pas réalisable à distance)" = "Not possible",
                 "Oui, parfois (mais pas régulièrement)" = "Occasionally (irregular)",
                 "Oui, à temps partiel (au moins une fois par semaine)" = "1+ days/week",
                 "Oui, à temps complet (tous les jours)" = "Every day"
    ),
    feeling_exhausted_dich = recode(feeling_exhausted,     # Dichotomise the "Feeling exhausted at work" question
                                    "Oui" = "Yes",
                                    "Plutôt oui" = "Yes",
                                    "Plutôt non" = "No",
                                    "Non" = "No"),
    burnout_interp = recode(burnout_interp,                     # recode EE MBI interpreation into English
                            "Épuisement modéré" = "Moderate",
                            "Épuisement faible" = "Weak",
                            "Épuisement fort" = "Strong"
    ),
    relation = recode(relation,
                      "En couple, non marié-e" = "In relationship (unmarried)",
                      "Marié-e ou en partenariat enregistré" = "Married / civil partnership",
                      "Divorcé-e ou séparé-e" = "Divorces / separated",
                      "Célibataire" = "Single",
                      "Veuf-ve" = "Widowed",
                      "Other" = "Other"
    ),
    hh_livewith_rec_en = recode(hh_livewith_rec_fr,
                                "En cohabitation avec d'autres personnes, sans enfant" = "Other people, no children",
                                "Seul-e" = "Alone",
                                "En couple, avec vos enfants ou ceux de votre conjoint-e" = "Couple, with children",
                                "En parent seul, avec vos enfants" = "Single parent, with children"
    )
  )

### ### ### ### ### ### ### ### ### ### ### ### ### #
### Descriptive variables - individual summaries ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

### Date of entry into study
dat %>% 
  ggplot(aes(x=date_soumission.x, fill = serocov_work.y))+
  geom_histogram(binwidth = 10)

# BMI --> odd values to be removed?
dat %>%
ggplot(aes(x=height, y = weight, color = bmi>50))+
  geom_point()+
  geom_label(aes(label = if_else(bmi>50, round(bmi,0), NA)),
             vjust = "inward"
             # position = "dodge"
             )+
  theme_bw()

# Age and sex --> should also try and compare this to geneva population on same graph?
dat %>% filter(sex_en != "Other") %>% 
  apyramid::age_pyramid(age_group = "age_cat",
                      split_by = "sex_en"
                      # , proportional = TRUE     # Display as % of all cases instead of counts
                      )

# Marital status
dat %>% 
  ggplot(aes(x=relation, fill = factor(children_n)))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 30, hjust = 1, size = 15),
        axis.title.x = element_blank())

# Number of children
dat %>% 
  ggplot(aes(x=children_n))+
  geom_histogram()

# Living situation and number of children
dat %>% 
  ggplot(aes(x=hh_livewith_rec_en, fill = factor(children_n)))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 20, hjust = 1, size = 18),
        axis.title.x = element_blank())

# Living situation and household composition
dat %>% 
  ggplot(aes(x=hh_livewith_rec_en, fill = hh_composition))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 20, hjust = 1, size = 13),
        axis.title.x = element_blank())
  
# Education
# Household income


# Summary tables ####
# gt_summary table stratified by variables of interest
tbl1_row <- dat %>% select(
  # wfh, 
  # wfh_cat,
  # burn_out, 
  feeling_exhausted_dich,
  # burnout_interp,
  # serocov_work.y, serocov_pop.y,sp4_avril_2022.y,
  sex_en, age_cat, bmi_4_cat_en, education_rec_en, Swiss_nat
  # , relation, children, hh_livewith, hh_composition, zone_house,
  # housing_condition, noisy, hh_income_cat_en, smoking_rec_en,
  # multiple_salaried_jobs, job_sector, workplace_size, supervision,
  # wfh_change, wfh_balance, sedentary_work
               ) %>% 
  tbl_summary(
    by = feeling_exhausted_dich,            # choose variable for stratification, comment out the others
    # by = wfh_cat,                                                      
    # by = burn_out,
    # by = burnout_interp,
    percent = "row",                         # calculate percent row-wise
    label  = list(
      # wfh_cat ~ "Do you work from home?",
      # wfh ~ "Do you work from home?",
      # burn_out ~ "Clinically diagnosed burnout",
      # burnout_interp ~ "Emotional Exhaustion (MBI) category",
      sex_en   ~ "Sex",
      age_cat ~ "Age category (years)",
      education_rec_en ~ "Education",
      Swiss_nat ~ "Nationality"
                ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing",                                    # how missing values should display
    # type = all_continuous() ~ "continuous2",       # indicate that you want to print multiple statistics for continuous variables
    # statistic = all_continuous() ~ c(
    #   # "{N_nonmiss}",                               # line 1: Number of non-missing values
    #   "{mean} ({sd})",                             # line 2: mean and SD
    #   "{median} ({p25}, {p75})",                   # line 3: median and IQR
    #   "{min}, {max}") 
) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Exhaustion at work**") %>% 
  modify_footnote(all_stat_cols() ~ "Row-wise %") ;tbl1_row
  # add_n() %>%                                       # Number of non-missing values
  # add_overall(                                       # Add an overall column for totals
  #   last = TRUE,
  #   statistic = ~"{n} ({p}%)"
  #   # digits = ~ c(1, 0)
  # ) #%>% 
  # modify_spanning_header(all_stat_cols() ~    # Update spanner title
  #                               #   "**Teleworking frequency**"
  #                               # "**Clinical Burnout**"
  #                               "**Exhaustion at work**"
  #                               # "**Emotional Exhaustion**"
  #                               )
tbl1_row

tbl1_col <- dat %>% select(
  # wfh, 
  # wfh_cat,
  # burn_out, 
  # feeling_exhausted_dich,
  # burnout_interp,
  # serocov_work.y, serocov_pop.y,sp4_avril_2022.y,
  sex_en, age_cat, bmi_4_cat_en, education_rec_en, Swiss_nat
  # , relation, children, hh_livewith, hh_composition, zone_house,
  # housing_condition, noisy, hh_income_cat_en, smoking_rec_en,
  # multiple_salaried_jobs, job_sector, workplace_size, supervision,
  # wfh_change, wfh_balance, sedentary_work
) %>% 
  tbl_summary(
    percent = "col",                         # calculate percent column-wise
    label  = list(
      # wfh_cat ~ "Do you work from home?",
      # wfh ~ "Do you work from home?",
      # burn_out ~ "Clinically diagnosed burnout",
      # burnout_interp ~ "Emotional Exhaustion (MBI) category",
      sex_en   ~ "Sex",
      age_cat ~ "Age category (years)",
      education_rec_en ~ "Education",
      Swiss_nat ~ "Nationality"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                   # how missing values should display
  ) %>%
  add_n() %>%                                        # Number of non-missing values
  modify_header(all_stat_cols() ~ "**Overall**") %>% 
  modify_spanning_header(everything() ~ NA) %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %"); tbl1_col

tbl1_final <-
  tbl_merge(
    tbls = list(tbl1_col, tbl1_row),
    tab_spanner = c(NA, "**Exhaustion at work**")
      ) ; tbl1_final

#%>% 
  add_p() # error with housing_condition and job_sector, as they have grouping levels that are too small
# Also not really clear to me how these p-values are being calculated -- maybe don't look at those for now






### ### ### ### ### ### ### ### ### #
### Other random bits and pieces ####  
### ### ### ### ### ### ### ### ### #
  
# Create table for wfh ~ age category
dat %>% tabyl(ethnicity_dich, wfh) %>% 
  adorn_totals(where = "row") %>%             # add total row
  adorn_percentages(denominator = "row") %>%  # convert counts to proportions
  adorn_pct_formatting(digits = 1) %>%        # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    # row_name = "Age Category",
    # col_name = "WFH frequency",
    placement = "combined") %>%               # this is necessary to print as image
  flextable::flextable() %>%                  # convert to pretty image
  flextable::autofit()                        # format to one line per row 

dat %>%
  # group_by(sex) %>%
  count(wfh) %>%
  mutate(Percent = scales::percent(n / sum(n), accuracy = 0.1))
  

dat %>% 
  get_summary_stats(
    age, hh_composition, children_n, bmi, 
    burnout_score, score_work_overinvestment_scale, karasek_social_support,
    type = "common"
  )
