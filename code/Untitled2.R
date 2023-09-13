table1 <- dat %>%  select(sex,age_strata,education_3_cat,finance_situation,health_reported,
                          chronic_condition,total_sdq,roche_n_interp,test_pos_infection,
                          test_pos_infection_symp,vaccin_covid,symptoms_at_least_4_weeks,
                          duration_4_6_weeks,duration_6_8_weeks,duration_8_12_weeks,
                          duration_12_more_weeks,diagnosis_lc,symp_gravity,symp_gravity_12,
                          n_symptoms,n_symptoms_12)  %>% 
  tbl_summary(by = roche_n_interp, missing = "no",statistic = list(all_continuous() ~ "{median} ({IQR})")) %>%  
  add_p() %>%  
  add_overall()
table1



m1 <- glm(data = dat, burnout_score_sqrt1 ~ noisy*quiet_room, family = gaussian)
summary(m1)
