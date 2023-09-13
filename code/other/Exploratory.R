library(tidyverse)
library(lubridate)
library(flextable)
# library(knitr)
# library(webshot) # for flextable output
# library(patchwork)

# R|

## Read in merged dataset (see "Merge code.R" file) --> Sante-Travail dataset merged with "2023-01-03-1808_ALLincs_ALLparticipants.rds"
dat <- readRDS("merged_dataset.rds")

# Functions ####

# Function for table formatting, from Nick
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", digits = 2, na_str = "NA")
  return(x)
}

# ## convert population source from wide to long ===>> NEEDS WORK
# datx <- dat %>% pivot_longer(
#   # cols = c(corona_immunitas_dec_2020, kids_pilote:serocov_kids, serocov_work),
#   cols = c(aleatoires_dec_2020:corona_immunitas_dec_2020, kids_pilote:work_pilote),
#                       names_to = "participant_source",
#                       values_to = "TF") %>% 
#   mutate(TF = if_else(TF == FALSE, NA, TF)) %>% 
#   drop_na(TF)
# 
# length(unique(datx$participant_id))
# 

## From Nick
dat %>%
  count(employed) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()

dat %>%
  count(not_employed_reason) %>%
  drop_na() %>% 
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()

# Filter only for those employed in the last 12 months
dat = dat %>% 
  filter(employed == "Oui")

# Current work situation
dat %>%
  count(work_situation.y) %>%
  drop_na() %>%
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()

# Make dichotomised variable
dat <- dat %>% mutate(work_situation.y_dich = if_else(work_situation.y == "En activité salariée",
                                                    "En activité salariée", if_else(is.na(work_situation.y), NA, "Other")))
# Comments from "Other" for current work situation
comm_other <- dat %>%
  select(participant_id, work_situation_other) %>%
  drop_na() %>% 
  arrange(work_situation_other) #%>% 
  # mutate(bo_comm = str_detect(work_situation.y_other, "Burn|burn|epuis|Epuis|Épuis|épuis|mala|Mala|AI|A-I|a-i|inval|Inval")) %>% 
  # filter(bo_comm == TRUE)
write.csv2(comm_other, "Comments_Other salaried.csv", row.names = FALSE)

other_burnout <- dat %>% filter(work_situation.y == "Other",
                                burn_out == "Non") %>% 
  select(work_situation_other)

# Sedentarism ####
dat %>%
  filter(work_situation.y=="En activité salariée") %>%
  count(sedentary_work) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()

# Number hours  --> Need to combine with the number of minutes into a third variable
dat %>%
  filter(work_situation.y=="En activité salariée") %>% 
  count(seated_hrs) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()

#Number minutes
dat %>%
  filter(work_situation.y=="En activité salariée") %>% 
  count(seated_mins) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  myflextable() %>%
  autofit()


# Emotional Exhaustion ####
dat %>%
  filter(work_situation.y=="En activité salariée") %>%
  # group_by(work_situation.y_dich) %>% # dichotomised to salaried vs. other
  # group_by(work_situation.y) %>%
  # group_by(job_sector) %>% 
  # group_by(serocov_work) %>%
  # group_by(serocov_schools) %>%
  # group_by(serocov_pop) %>%
  # group_by(burnout_score) %>%
  # group_by(burnout_interp) %>%   # the burnout scale seems to correspond quite nicely with reporting of clinical burnout episodes
  # group_by(fear_losing_job) %>% 
  # group_by(work_life_balance) %>% 
  group_by(wfh) %>% 
  # group_by(sedentary_work) %>% 
  # group_by(wfh_change) %>%
  # group_by(supervision) %>%
  # group_by(years_of_service) %>% 
  count(feeling_exhausted) %>%
  mutate(percent_exhausted = round(100 * n / sum(n),1)) %>% 
  filter(feeling_exhausted == "Oui" | feeling_exhausted == "Plutôt oui") %>% 
  # ggplot(aes(x=feeling_exhausted, y=n)) +
  # geom_col()
  # ggplot(aes(x=wfh, y=percent_exhausted, fill = feeling_exhausted))+
  # geom_col(position = "dodge")+
  # theme(axis.text.x=element_text(angle = 40, hjust = 1))
  myflextable() %>%
  autofit()

#Burnout ####
dat %>%
  filter(work_situation.y=="En activité salariée") %>%
  # group_by(work_situation.y_dich) %>% # dichotomised to salaried vs. other
  # group_by(work_situation.y) %>%
  # group_by(job_sector) %>% 
  # group_by(serocov_work) %>%
  # group_by(serocov_schools) %>%
  # group_by(serocov_pop) %>%
  # group_by(burnout_score) %>%
  # group_by(burnout_interp) %>%   # the burnout scale seems to correspond quite nicely with reporting of clinical burnout episodes
  # group_by(sedentary_work) %>% 
  # group_by(wfh_change) %>%
    group_by(wfh) %>% 
  # group_by(supervision) %>%
  # group_by(years_of_service) %>% 
  count(burn_out) %>%
  mutate(percent_burnout = 100 * n / sum(n)) %>%
  filter(burn_out == "Oui") %>%
  # select(-burn_out) %>% 
  myflextable() %>%
  autofit()

# Number of hours by burnout or not
dat %>% 
  # filter(work_situation.y=="En activité salariée") %>% 
  ggplot(aes(x=burn_out, y=hours_week))+
  geom_boxplot()

dat %>% 
  group_by(burn_out) %>% 
  summarise(hours_week = mean(hours_week, na.rm = TRUE))

# Ratio of Number of hours worked as function of percentage they are officially working
dat <- dat %>% 
  mutate(percentage_num = as.numeric(str_remove(percentage,"%"))/100,
         ratio_hours_percentage = hours_week / (40*percentage_num))

dat %>% 
  # filter(work_situation.y=="En activité salariée") %>% 
  ggplot(aes(x=burn_out, y=ratio_hours_percentage))+
  geom_boxplot()

dat %>% 
  group_by(burn_out) %>% 
  summarise(ratio_hours_percentage = mean(ratio_hours_percentage, na.rm = TRUE))


# Maslach Burnout Index ####
dat %>% 
  ggplot(aes(x=burn_out, y=burnout_score))+
  geom_boxplot()

dat %>%
  filter(work_situation.y=="En activité salariée") %>%
  # group_by(work_situation.y_dich) %>% # dichotomised to salaried vs. other
  # group_by(work_situation.y) %>%
  group_by(job_sector) %>% 
  # group_by(serocov_work) %>%
  # group_by(serocov_schools) %>%
  # group_by(serocov_pop) %>%
  # group_by(burnout_score) %>%
  # group_by(burnout_interp) %>%   # the burnout scale seems to correspond quite nicely with reporting of clinical burnout episodes
  # group_by(sedentary_work) %>% 
  # group_by(wfh_change) %>%
  # group_by(supervision) %>%
  # group_by(years_of_service) %>% 
  summarise(mean_MBI = mean(burnout_score, na.rm = TRUE)) %>%
  myflextable() %>%
  autofit()

dat %>% 
  filter(work_situation.y=="En activité salariée") %>% 
  ggplot(aes(x=ratio_hours_percentage, y=burnout_score))+
  geom_point()

dat %>% 
  group_by(burn_out) %>% 
  summarise(hours_week = mean(hours_week, na.rm = TRUE))


# Karasek Scale ####

# Organizing work
dat %>% 
  filter(work_situation.y=="En activité salariée") %>%
  group_by(wfh) %>%
  count(karasek_scale_1_work_organisation) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  # filter(karasek_scale_1_work_organisation == "Pas d’accord" | karasek_scale_1_work_organisation == "Pas du tout d’accord") %>%
  myflextable() %>%
  autofit()
  
  


### Final Comments ####
# R|
final_comm <- dat %>%
  select(participant_id, work_situation.y, burn_out, impact_pandemic_comment) %>%
  drop_na() %>% 
  arrange(impact_pandemic_comment) #%>% 
  # mutate(bo_comm = str_detect(impact_pandemic_comment, "Burn|burn|epuis|Epuis|Épuis|épuis|mala|AI|A-I|a-i|inval|Inval")) %>% 
  # filter(bo_comm == TRUE)
write.csv2(final_comm, "Final comments.csv", row.names = FALSE)

final_comm_other <- dat %>%
  filter(work_situation.y_dich == "Other") %>% 
  select(impact_pandemic_comment) %>%
  drop_na() %>% 
  arrange(impact_pandemic_comment) #%>% 
  mutate(bo_comm = str_detect(impact_pandemic_comment, "Burn|burn|epuis|Epuis|Épuis|épuis|mala|AI|A-I|a-i|inval|Inval")) %>% 
  filter(bo_comm == TRUE)
