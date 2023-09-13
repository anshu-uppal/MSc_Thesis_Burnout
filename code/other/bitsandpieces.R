a <- dat_all %>%
  filter(sedentary_work == "Other") %>% 
  select(participant_id,sedentary, sedentary_work, sedentary_work_other) %>% 
  drop_na(sedentary_work_other)

dat_all %>% 
  ggplot(aes(x=seated_time_standardized, fill = sedentary_work))+
  geom_histogram()

a <- dat_all %>% 
  select(hours_week, percentage, seated_hrs, seated_mins, sedentary_work, seated_time_standardized, seated_time) %>% 
  filter(seated_time_standardized >0.8)



# overcrowding --> doublecheck with how overcrowding was calculated in the Schrempft et al. paper?
a <- dat_all %>% select(hh_composition, hh_composition_cat, bedroom_n, overcrowded)
a %>% count(overcrowded) %>%
  mutate(percent = 100 * n / sum(n))



newmerge %>%
  select(burn_out, burnout_score, burnout_interp,
         # mh_sentiment_good_mood:mh_sentiment_interesting
         mh_feeling_nervous:mh_feeling_good_for_nothing
         # mh_lack_company:mh_isolated, 
         # mh_perceived_stress1_disturbed:mh_perceived_stress2_accumulation
  ) %>% 
  tbl_summary(by = burnout_interp,
              percent = "row"
  )


dat_all %>% 
  ggplot(aes(x = date_soumission))+
  geom_histogram()

dat %>% 
  filter(gh_participant == TRUE) %>% 
  filter(events_burn_out == FALSE) %>%
  group_by(events_burn_out) %>%
  count(burn_out) %>%
  mutate(percent = 100 * n / sum(n))

b %>% select(burnout_interp, burn_out,
             # events_wedding:events_99,
             #            health, morale, life_quality_score, sentiment_good_mood:sentiment_interesting,
             #            health_event, sym_bother_work:sym_bother_daily,
             #            symp_impact_personal_activities:symp_impact_morale,
             # focus:happy,
             # lack_company:isolated,
             # reliable_ppl:practical_help,
             # new_psy_help:health_professional,
             # mh_low_interest:mh_negative_emotions,
             # fatigue_1_tired:fatigue_2_memory,
             GHQ12_score:GHQ12_interpretation_3out,
             oslo_interpretation, UCLA_score,
             GAD2_score:PHQ2_interpretation,
             who_interpretation,
             sheehan_score,
             chalder_binary_interpretation
) %>% 
  tbl_summary(by= burn_out,
              percent = "row")



# Read in the all participants dataset
monthly <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/Bases_for_sharing/2023-02-02-1137_SanteTravail_ALLparticipants.rds"))
dupcheck <- monthly %>% janitor::get_dupes(participant_id) # check for duplicates


dat_all %>%
  ggplot(aes(x=weight.y, y=weight.x, color = abs(1-(weight.x/weight.y))>=0.2))+
  geom_point()


a <- dat_all %>% 
  filter(gh_participant == TRUE,
         events_burn_out == FALSE,
         work_situation.y == "Other"
         ) %>% 
  # group_by(events_burn_out) %>%
  # count(work_situation.y) %>%
  # mutate(percent = 100 * n / sum(n))
  select(participant_id, work_situation_other, work_situation_rec_en)

dat %>% 
  filter(!is.na(sex_en)) %>% 
  # filter(gh_participant == TRUE) %>%
  # group_by(age_cat) %>%
  count(sex_en) %>%
  mutate(percent = 100 * n / sum(n))



## Want to create a new variable that is a composite of wfh and wfh_change:
### Teleworking:
# not possible
# never (reference category)
# yes, increase
# yes, same
# yes, decrease

dat %>% select(burnout_interp, burn_out,
               karasek_scale_1_too_much_work:karasek_scale_3_colleagues_interested,
               feeling_exhausted_dich,
               positive_impact:work_sick,
               work_overinvestment_scale_rushed:work_overinvestment_scale_trouble_sleeping
) %>% 
  tbl_summary(by = burn_out,
              percent = "row")



dat %>% 
  select(wfh, wfh_change, 
         # wfh_balance, 
         pandemic_change_reason_working_arrangements, new_working_arrangements_work_from_home) %>% 
  tbl_summary(by = wfh,
              type = all_categorical() ~ "categorical",                 # force all categorical levels to display
              missing_text = "Missing"                                   # how missing values should display
  ) %>% 
  add_n()                                       # Number of non-missing values



dat %>% 
  group_by(hh_livewith_rec_en) %>% 
  count(hh_livewith) %>%
  mutate(percent = 100 * n / sum(n))


pacman::p_load(
  tidyverse,
  here,
  gtsummary,
  modelr,
  lubridate
)


dat <- tibble(readRDS(here("data", "clean_dataset.rds")))

dat %>% ggplot(aes(x = wfh, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = wfh_change, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = wfh_balance, y = burnout_score)) + geom_boxplot()


dat %>% ggplot(aes(x = relation, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = noisy, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = quiet_room, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = workplace_size, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = workplace_size_dich, y = burnout_score)) + geom_boxplot()


dat %>% ggplot(aes(x = fear_losing_job, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = fear_losing_job_change, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = fear_losing_job_dich, y = burnout_score)) + geom_boxplot()

dat %>% ggplot(aes(x = contract_cdi, y = burnout_score)) + geom_boxplot()


dat %>% ggplot(aes(x = exercise_hard, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = exercise_moderate, y = burnout_score)) + geom_boxplot()

dat %>% ggplot(aes(x = exercise_hard_dich, y = burnout_score)) + geom_boxplot()

dat %>% ggplot(aes(x = health_general, y = burnout_score)) + geom_boxplot()
dat %>% ggplot(aes(x = mental_state, y = burnout_score)) + geom_boxplot()+facet_wrap(.~burn_out)
dat %>% ggplot(aes(x = mental_state, y = burnout_score)) + geom_boxplot()+facet_wrap(.~ feeling_exhausted+burn_out)



dat %>%
  # group_by(burnout_interp_dich) %>%
  count(burn_out) %>%
  mutate(percent = 100 * n / sum(n))

dat %>%
  group_by(serocov_work.y) %>%
  count(education_rec_en) %>%
  mutate(percent = 100 * n / sum(n))

tw <- dat %>% 
  select(participant_id, wfh, wfh_updated, profession.y, job_sector, job_sector_other, ISCO, Occupation_label, 
         physical_interaction, social_interaction) %>%
  filter(wfh == "Never") %>% 
  arrange(Occupation_label)

write.csv2(tw, here("data", "wfh_updated.csv"))

tw <- tw %>% 
  mutate(wfh_updated = factor(case_when(
    physical_interaction == 0 ~ "Not possible",
    Occupation_label %in% c(
      "Air traffic controllers", "Ambulance workers","Fire-fighters", 
      "Food processing and related trades workers",
      "Medical assistants", "Stock clerks", "Physiotherapists", "Dentists"
      ) ~ "Not possible",
    Occupation_label == "Protective services workers" & profession.y != "Fonctionnaire" ~ "Not possible",
    profession.y == "Vendeuse" ~ "Not possible",
    Occupation_label == "Life science technicians and related associate professionals" & profession.y != "Assistante diplômée" ~ "Not possible",
    .default = wfh_updated),
    levels = c("Never", "Not possible", "Occasionally (irregular)", "1+ days/week", "Every day"))
    )




b <- dat %>% 
  group_by(job_sector) %>%
  summarise(n = n_distinct(participant_id),
            mean_p_change = round(mean(percent_change, na.rm = TRUE), 2))

b <-dat %>% 
  group_by(hh_livewith_rec_en, num_small_children_dich) %>%
  count(hh_livewith_rec_en_children)

b <-dat %>% 
  select(hours_week, percentage, overwork_hours, overwork_ratio)

dat %>% 
  filter(overwork_ratio < 2 & overwork_ratio > 0.4) %>% 
  ggplot(aes(x = overwork_ratio, y = score_work_overinvestment_scale))+
  geom_point()+
  geom_smooth()

summary(lm(data = dat, score_work_overinvestment_scale ~ overwork_ratio))


