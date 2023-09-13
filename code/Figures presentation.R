# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  # skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  lubridate,    # manipulate date objects
  ggfortify,    # plotting tools to check GLM assumptions
  apyramid,     # a package dedicated to creating age pyramids
  ggExtra,      # package for showing distributions of edges of geom_point()
  car,           # Used for checking non-collinearity assumption for regressions
  plotly
)

# Read in clean dataset (see "Merge code.R" and "Cleaning merged dataset")
dat <- tibble(readRDS(here("data", "clean_dataset.rds")))
# drop_na(wfh_exposure, # Drop NAs in explanatory variables (2908 --> 2887)
#                age_cat,sex_en,education_rec_en,
#                hh_income_cat_en, finance_situation,
#                hh_livewith_rec_en, overcrowded,
#                noisy, quiet_room,
#                health_general_dich)


# Age pyramid
dat %>% filter(sex_en != "Other") %>% # filter out the "Other" selections
  mutate(age_cat = factor(age_cat, levels = c("25-34", "35-44", "45-54", "55-64"))) %>%
  apyramid::age_pyramid(age_group = "age_cat",
                        split_by = "sex_en"
                        , proportional = TRUE     # Display as % of all cases instead of counts
  )+
  labs(title = "Age groups by sex", x = "Age category", y = "Percent of participants")+
  theme(legend.position = c(0.15,0.22),
        legend.margin = margin(0,2,2,2),
        legend.title = element_blank())

dat %>% filter(sex_en != "Other") %>% # filter out the "Other" selections
  count(sex_en) %>% 
  mutate(percent = n/sum(n))
  
# Age two categories 
a<-dat %>% mutate(age_cat2 = factor(case_when(     
  age >= 25 & age < 45 ~ "25-44",
  age >= 45 & age < 65 ~ "45-64"))
)
a %>% 
  count(age_cat2) %>% 
  mutate(percent = n/sum(n))

# Descriptive table of participant demographics ####
var_included <- c(
  "sex_en",
  # "age_cat",
  "age_cat2",
  "education_rec_en",
  "hh_income_cat_en",
  "finance_situation"
  ,"percent_change_cat"
  # ,"supervision_short" # which variable to use for financial security? or too linked with income?
  # ,"hh_livewith_rec_en"
  # ,"num_young_children_dich"
  # ,"overcrowded"
  # ,"noisy"
  # ,"quiet_room"
  # ,"health_general_dich"
                  )

# ## Table of totals for the variables included ##
# tbl1_col <- dat %>% select(all_of(var_included)) %>% 
#   mutate(age_cat = factor(age_cat, levels = c("25-34", "35-44", "45-54", "55-64"))) %>%
#   tbl_summary(
#     percent = "col",                         # calculate percent column-wise
#     label  = list(
#       sex_en   ~ "Sex",
#       age_cat ~ "Age category (years)"
#       education_rec_en ~ "Education",
#       hh_income_cat_en ~ "Household Income"
#       #, Swiss_nat ~ "Nationality"
#       ),
#     type = all_categorical() ~ "categorical",                 # force all categorical levels to display
#     missing_text = "Missing"                                   # how missing values should display
#   ) %>%
#   add_n() %>%                                        # Number of non-missing values
#   bold_labels() %>% 
#   modify_header(all_stat_cols() ~ "**Overall**") %>% 
#   modify_spanning_header(everything() ~ NA) %>% 
#   modify_footnote(all_stat_cols() ~ "Column-wise %") ; tbl1_col



# Descriptive table by teleworking status
tbl1_telework <- dat %>% 
  mutate(age_cat = factor(age_cat, levels = c("25-34", "35-44", "45-54", "55-64")),
         age_cat2 = factor(case_when(     
           age >= 25 & age < 45 ~ "25-44",
           age >= 45 & age < 65 ~ "45-64"))) %>% 
  select(wfh_exposure, all_of(var_included)) %>% 
  tbl_summary(
    by = wfh_exposure,
    percent = "col",                         # calculate percent row-wise
    label  = list(
      "sex_en" ~ "Sex",
      # "age_cat" ~ "Age group, years",
      "age_cat2" ~ "Age group, years",
      "education_rec_en" ~ "Education",
      "hh_income_cat_en" ~ "Household income"
      , "finance_situation" ~ "Financial security"
      , "percent_change_cat" ~ "Change in working hours"
      # "supervision_short" ~ "Management role",
      # "hh_livewith_rec_en" ~ "Living arrangement", "num_young_children_dich" ~ "Young children",
      # "overcrowded" ~ "Household density",
      # "noisy" ~ "Noise level at home",
      # "quiet_room" ~ "Access to a quiet room",         
      # "health_general_dich" ~ "Self-reported general health"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Teleworking Group**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>%
  add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %") %>% as_flex_table() ; tbl1_telework

# Education




# EE-MBI score

dat %>%
  ggplot(aes(x = burnout_score, fill = burnout_interp_dich30))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("lightseagreen", "#F8766D"))+
  labs(title = "Histogram of EE-MBI score (Severe \u2265 30)", x = "EE-MBI score")+
  theme_classic()+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())

dat %>% 
  count(burnout_interp_dich30) %>% 
  mutate(percent = n / sum(n))

dat %>% 
  count(burn_out) %>% 
  mutate(percent = n / sum(n))

dat %>%
  ggplot(aes(x = factor(burn_out), y = burnout_score))+
  geom_boxplot()+
  labs(title = "EE-MBI score by diagnosed burnout")+
  theme_classic()


# Diagnosed burnout and EE-MBI

dat %>% 
  mutate(burnout_score_cat = cut_width(burnout_score,width = 5, center = 2.5, closed = "left")) %>%
  # select(burnout_score, burnout_score_cat) %>% filter(burnout_score == 30) # doublecheck how the intervals are structured
  ggplot(aes(x=burnout_score_cat, y = burn_out, fill = burnout_interp_dich30))+
  geom_bar(position = "dodge", stat = "summary", na.rm = TRUE, fun = "mean")+
  theme_classic()+
  scale_fill_manual(values = c("lightseagreen", "#F8766D"))+
  labs(x = "EE-MBI score in 5-point intervals", y = "Proportion diagnosed with burnout",
       title = "Proportion diagnosed with burnout in the last 12 months,\nwithin each EE-MBI score interval",
       fill = "EE-MBI classification")+
  theme(axis.text.x = element_text(angle = 30, size = 11),
        legend.position = c(0.2,0.8))




# WFH
dat %>% mutate(wfh_change = case_match(wfh_change,
                                   "De manière inchangée" ~ "No change",
                                   "Plus qu’avant" ~ "Increase",
                                   "Moins qu’avant" ~ "Decrease")) %>% 
  ggplot(aes(x = wfh_updated, fill = wfh_change))+
  geom_bar()+
  labs(x = "Telework status", 
       title = "Distribution of Teleworking exposure variable",
       fill = "Telework frequency \nrelative to before \nthe pandemic")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 10, size = 11),
        legend.position = c(0.9,0.7))

dat %>%
  group_by(education_rec_en) %>%
  count(wfh_exposure) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  ggplot(aes(x = wfh_exposure, y = scales::percent(percent)))+
  geom_col()+
  labs(x = "Teleworking frequency and changes since the pandemic", title = "Distribution of Teleworking exposure variable")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 20))+
  facet_wrap(.~education_rec_en)



mcovariates <- c("wfh_exposure",
                 "age_cat","sex_en","education_rec_en",
                 "hh_income_cat_en", "finance_situation",
                 "hh_livewith_rec_en", "overcrowded",
                 "noisy", "quiet_room",
                 "health_general_dich"
)
mcov_names <- c("Teleworking frequency and change since pandemic",
                "Age category",
                "Sex",
                "Education",
                "Income",
                "Financial situation",
                "Living arrangement",
                "Overcrowding",
                "Noisy environment",
                "Access to quiet room",
                "General health (self-assessed)")
mcovariates <- cbind(mcovariates, mcov_names)

for (i in 1:length(mcovariates[,1])) {
  fig <- dat %>%
    ggplot(aes(x= get(mcovariates[i,1]), y = burnout_score))+
    geom_boxplot()+
    labs(y = "EE-MBI score", x = mcovariates[i,2])+
    theme(axis.text.x = element_text(angle = 20))
  
  print(fig)
}


# Entry into study

dat %>% 
  ggplot(aes(x=date_soumission.x, fill = serocov_work.y))+
  geom_histogram(binwidth = 10)+
  theme(legend.position = c(0.8,0.8))

# Positive impact on health
dat %>%
  group_by(positive_impact) %>%
  count(wfh_exposure) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  ggplot(aes(x = wfh_exposure, y = scales::percent(percent)))+
  geom_col()+
  labs(x = "Teleworking frequency and changes since the pandemic", title = "Distribution of Teleworking exposure variable")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 20))+
  facet_wrap(.~positive_impact)

ggplotly( dat %>% 
            ggplot(aes(x = physical_interaction, y = social_interaction, color = wfh_trich, label = paste(profession.y, "\n", Occupation_label)))+
            geom_point()+
            labs(color = "Teleworking")+
            geom_hline(yintercept = 0.5)+
            geom_vline(xintercept = 0.4)+
            theme_bw())

dat %>% group_by(wfh_exposure) %>% 
  count(hh_income_cat_en) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = hh_income_cat_en, y = percent))+
  labs(title = "Income group by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)

dat %>% group_by(wfh_exposure) %>% 
  count(sex_en) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = sex_en, y = percent))+
  labs(title = "Sex by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)

dat %>% group_by(wfh_exposure) %>% 
  count(percent_change_cat) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = percent_change_cat, y = percent))+
  labs(title = "Change in working hours by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)

dat %>% group_by(wfh_exposure) %>% 
  count(age_cat) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = age_cat, y = percent))+
  labs(title = "Age category by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)

dat %>% group_by(wfh_exposure) %>% 
  count(supervision_short) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = supervision_short, y = percent))+
  labs(title = "Managerial duties by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)

dat %>% group_by(wfh_exposure) %>% 
  count(work_life_balance) %>% 
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  ggplot(aes(x = work_life_balance, y = percent))+
  labs(title = "Work-life balance by Teleworking exposure")+
  geom_col()+
  facet_wrap(.~wfh_exposure)


## EE-MBI classification - job_sector
dat %>%
  mutate(job_sector = str_to_sentence(str_remove(job_sector, "Secteur de la|Secteur de l’|Secteur des")),
         job_sector = str_replace(job_sector, " \\s*\\([^\\)]+\\)", ""),
    job_sector = str_wrap(job_sector, width = 60)) %>% 
  # filter(!is.na(sex_en)) %>%
  group_by(job_sector) %>%
  count(burnout_interp_dich30) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich30 == "Severe") %>%
  # mutate(job_sector =  fct_reorder(job_sector,percent)) %>% 
  ggplot(aes(x = reorder(job_sector, percent), y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), hjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Severe EE-MBI")+
  coord_flip()+
  ylim(0, 50)+
  # facet_wrap(.~sex_en)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15))

dat %>%
  # mutate(Occupation_label_2 = str_remove(Occupation_label_2, "associate ")) %>% 
  # filter(!is.na(sex_en)) %>%
  group_by(Occupation_label_2) %>%
  count(burnout_interp_dich30) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich30 == "Severe") %>%
  # mutate(job_sector =  fct_reorder(job_sector,percent)) %>% 
  ggplot(aes(x = reorder(Occupation_label_2, percent), y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), hjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Severe EE-MBI")+
  coord_flip()+
  ylim(0, 55)+
  # facet_wrap(.~sex_en)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15))


## Burnout - job_sector
dat %>%
  mutate(job_sector = str_to_sentence(str_remove(job_sector, "Secteur de la|Secteur de l’|Secteur des")),
         job_sector = str_replace(job_sector, " \\s*\\([^\\)]+\\)", ""),
         job_sector = str_wrap(job_sector, width = 60)) %>% 
  # filter(!is.na(sex_en)) %>%
  group_by(job_sector) %>%
  count(burn_out) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burn_out == 1) %>%
  # mutate(job_sector =  fct_reorder(job_sector,percent)) %>% 
  ggplot(aes(x = reorder(job_sector, percent), y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), hjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Diagnosed Burnout")+
  coord_flip()+
  ylim(0, 20)+
  # facet_wrap(.~sex_en)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15))


## Insurance premium by country of residence and nationality ####
tbl1_insurance <- dat %>%
  mutate(age_cat2 = factor(case_when(
    age >= 25 & age < 45 ~ "25-44",
    age >= 45 & age < 65 ~ "45-64"))) %>% 
  mutate(
    in_france = case_when(
      str_length(nip_house)>4 ~ "France",
      .default = "Switzerland"),
    country_swiss = paste0(Swiss_nat, " and ", in_france)
  ) %>% 
  select(country_swiss, in_france, Swiss_nat, health_insurance_cat) %>% 
  tbl_summary(
    by = health_insurance_cat,
    percent = "col",                         # calculate percent row-wise
    label  = list(
      "Swiss_nat" ~ "Nationality",
      "in_france" ~ "Country of residence",
      "country_swiss" ~ "Residence and Nationality"
    ),
    type = all_categorical() ~ "categorical",                 # force all categorical levels to display
    missing_text = "Missing"                                  # how missing values should display
  ) %>% 
  bold_labels() %>% 
  modify_spanning_header(all_stat_cols() ~ "**Insurance premium**") %>%
  modify_header(all_stat_cols() ~ paste0("**{level}**","\nN = {n}")) %>%
  add_overall() %>% 
  modify_footnote(all_stat_cols() ~ "Column-wise %") %>% as_flex_table() ; tbl1_insurance

## look at insurance by country
a <- dat %>%
  mutate(
    in_france = case_when(
      str_length(nip_house)>4 ~ "France",
      .default = "Switzerland"),
    country_swiss = paste0(Swiss_nat, " and ", in_france)
  ) %>%
  filter(country_swiss %in% c(
    "Swiss and Switzerland"
    , "Other and Switzerland"
    ),
         health_insurance_cat == "No Swiss insurance") %>% 
  select(country_swiss, nat_lang, nat_lang_other, job_sector, profession.y, profession_other)
