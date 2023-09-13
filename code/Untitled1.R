#trial commit

ghq <- dat %>%
  filter(gh_participant == TRUE) %>% 
  filter(events_burn_out == FALSE) 

ghq %>% ggplot(aes(x = chalder_global_score, y = burnout_score_sqrt1))+
  geom_point()+
  geom_smooth()


# symp_new --> new symptoms in last 12 months that lasted over 4 weeks (from GHQ-march/april 2022, Q6)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ symp_new)

ghq %>%
  ggplot(aes(x=factor(symp_new), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ symp_new)
summary(m1)



# Oslo social support outside of work --> from GHQ-march/april 2022, Q26-28)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ oslo_interpretation)

ghq %>%
  ggplot(aes(x=factor(oslo_interpretation), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ oslo_interpretation)
summary(m1)


# PHQ2 --> from GHQ-march/april 2022, Q29)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ PHQ2_interpretation)

ghq %>%
  ggplot(aes(x=factor(PHQ2_interpretation), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ PHQ2_interpretation)
summary(m1)

# GAD2 --> from GHQ-march/april 2022, Q29)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ GAD2_interpretation)

ghq %>%
  ggplot(aes(x=factor(GAD2_interpretation), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ GAD2_interpretation)
summary(m1)


# Chalder fatigue --> from GHQ-march/april 2022, Q29)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ chalder_binary_interpretation)

ghq %>%
  ggplot(aes(x=factor(chalder_binary_interpretation), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ chalder_binary_interpretation)
summary(m1)


# GHQ1, three outcomes --> from GHQ-march/april 2022, Q4)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ GHQ12_interpretation_3out)

ghq %>%
  ggplot(aes(x=factor(GHQ12_interpretation_3out), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ GHQ12_interpretation_3out)
summary(m1)

# GHQ1, two outcomes --> from GHQ-march/april 2022, Q4)
ghq %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ GHQ12_interpretation_2out)

ghq %>%
  ggplot(aes(x=factor(GHQ12_interpretation_2out), y=burnout_score_sqrt1))+
  geom_boxplot()

m1 <- glm(data = ghq, burnout_score_sqrt1 ~ GHQ12_interpretation_2out)
summary(m1)



m1 <- glm(data = ghq, burnout_score_sqrt1 ~  wfh_exposure)
m2 <- glm(data = ghq, burnout_score_sqrt1 ~  oslo_interpretation)
m3 <- glm(data = ghq, burnout_score_sqrt1 ~  wfh_dich + oslo_interpretation)
summary(m3)


dat %>% 
  # filter(gh_participant == TRUE) %>%
  group_by(dgs_binary_victim_aggression) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n))


m1 <- glm(data = dat, burnout_score_sqrt1 ~ fear_losing_job)
summary(m1)

dat %>%
  filter(!is.na(sex_en)) %>% 
  ggplot(aes(x=factor(wfh_cat), y=burnout_score_sqrt1))+
  geom_boxplot() + facet_wrap(.~hh_livewith_rec_en)



dat %>%
  ggplot(aes(x=factor(work_weekends_dich), y=burnout_score_sqrt1))+
  geom_boxplot()

dat %>%
  ggplot(aes(x=burnout_score_sqrt1))+
  geom_histogram()+
  facet_wrap(.~ wfh_trich+work_overtime_dich)

dat %>%
  group_by(health_insurance, burnout_interp_dich) %>% 
  count(burn_out) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  filter(burn_out ==1) %>% 
  ggplot(aes(x=health_insurance, y = percent))+
  geom_col()+
  facet_wrap(.~ burnout_interp_dich)
