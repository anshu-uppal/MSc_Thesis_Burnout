m45 <- glm(burnout_score ~ wfh_exposure + age_cat + sex_en + education_rec_en + 
      hh_income_cat_en + finance_situation + hh_livewith_rec_en + 
      overcrowded + noisy + quiet_room + health_general_dich + dgs_binary_victim_aggression,
    data = dat,
    family = "gaussian")

tbl_regression(m45,
                           pvalue_fun = ~ style_pvalue(.x, digits = 2),
                           intercept = TRUE,
) %>%
  add_n() %>% 
  bold_p() %>%
  bold_labels()




severe <- 27 # cutoff for whether MBI score is classified as severe or not
a <- dat %>% mutate(
  Severe_MBI = case_when(
    burnout_score >= severe ~ 1,
    burnout_score < severe ~ 0,
    .default = burnout_score))

m46 <- glm(Severe_MBI ~ wfh_exposure + age_cat + sex_en + education_rec_en + 
             hh_income_cat_en + finance_situation + hh_livewith_rec_en + 
             overcrowded + noisy + quiet_room + health_general_dich + dgs_binary_victim_aggression,
           data = a,
           family = "binomial")

tbl_regression(m46,
               exponentiate = TRUE,
               pvalue_fun = ~ style_pvalue(.x, digits = 2)
) %>%
  bold_p() %>%
  bold_labels()


m47 <- glm(burn_out ~ wfh_exposure + age_cat + sex_en + education_rec_en + 
             hh_income_cat_en + finance_situation + hh_livewith_rec_en + children_n_dich + 
             overcrowded + noisy + quiet_room + health_general_dich,
           data = a,
           family = "binomial")

tbl_regression(m47,
               exponentiate = TRUE,
               pvalue_fun = ~ style_pvalue(.x, digits = 2)
) %>%
  bold_p() %>%
  bold_labels()
