pacman::p_load(
  tidyverse,
  gtsummary,
  forestplot,
  devEMF, # print figures into a vectorized format
  here
)

### ### ### ### ### ### ### ### ### ###
# Section that needs your input ####
### ### ### ### ### ### ### ### ### ###

# update dat to use your dataset
dat <- tibble(readRDS(here("data", "clean_dataset.rds")))

dat %>% group_by(Occupation_label_2) %>% 
  count(wfh_updated) %>%
  mutate(percent = n/sum(n)) %>% 
  filter(!wfh_updated %in% c("Never", "Not possible")) %>% 
  mutate(Occupation_label_2 = fct_reorder(Occupation_label_2, desc(percent))) %>% 
  ggplot(aes(x = Occupation_label_2, y = percent, fill = wfh_updated))+
  geom_col()+
  coord_flip()


cases_analysis <- dat %>% select(burnout_score,
                                 burnout_interp_dich30,
                                 burn_out,
                                 wfh_exposure,
                                 age_cat,
                                 sex_en,
                                 education_rec_en,
                                 hh_income_cat_en,
                                 finance_situation,
                                 percent_change_cat,
                                 supervision_short,
                                 hh_livewith_rec_en,
                                 num_young_children_dich,
                                 overcrowded,
                                 noisy, 
                                 quiet_room, 
                                 health_general_dich)  

complete_cases <- cases_analysis[complete.cases(cases_analysis),]
