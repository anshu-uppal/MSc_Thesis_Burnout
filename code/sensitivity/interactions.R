## Merge datasets ##
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics
  gtsummary
)
dat <- tibble(readRDS(here("data", "clean_dataset.rds"))) %>% 
  filter(sex_en %in% c("Male", "Female")) %>% 
  mutate(sex_en = droplevels(sex_en))

dat <- dat %>% mutate(
  burnout_interp_dich30 = case_when(
    burnout_interp_dich30 == "Severe" ~ 1,
    burnout_interp_dich30 == "Not Severe" ~ 0,
    .default = NA),
  decrease_contract = case_when(
    percent_change_cat == "Reduced" ~ "Contract reduced",
    percent_change_cat != "Reduced" ~ "Contract not reduced",
    .default = NA),
  decreased_teleworking = case_when(
    wfh_exposure == "Decrease" ~ 1,
    .default = 0),
    wfh_updated = factor(wfh_updated, levels = c("Occasionally (irregular)", "1+ days/week", 
                                                 "Every day", "Never",  "Not possible")
                         )
)

# Interactions for sex and exposure variable ####
sex_int_ee <- glm(burnout_interp_dich30 ~ sex_en*wfh_exposure, 
                  family = "binomial",
                  data = dat) %>% 
  tbl_regression(exponentiate = FALSE
                 ,label  = list(
                   "sex_en" ~ "Sex",
                   # "wfh_updated" ~ "Teleworking frequency"
                   "wfh_exposure" ~ "Teleworking group"
                 )
                 ) %>% 
  bold_labels() %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1) #%>% add_global_p()
sex_int_ee

sex_int_bo <- glm(burn_out ~ sex_en*wfh_exposure, 
                  family = "binomial",
                  data = dat) %>% 
  tbl_regression(exponentiate = TRUE
                 ,label  = list(
                   "sex_en" ~ "Sex",
                   "wfh_exposure" ~ "Teleworking group"
                 )
  ) %>% 
  bold_labels() %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1) #%>% add_global_p()
sex_int_bo



# continuous MBI score
# Interactions for sex and exposure variable ####
sex_int_eembi <- glm(burnout_score ~ sex_en*wfh_exposure, 
                  family = "gaussian",
                  data = dat) %>% 
  tbl_regression(exponentiate = FALSE
                 ,label  = list(
                   "sex_en" ~ "Sex",
                   # "wfh_updated" ~ "Teleworking frequency"
                   "wfh_exposure" ~ "Teleworking group"
                 )
  ) %>% 
  bold_labels() %>% 
  bold_p(t = 0.1) %>% add_global_p()
sex_int_eembi


# Interactions for decreased contract percentage and exposure variable ####
decr_int_ee <- glm(burnout_interp_dich30 ~ decrease_contract*wfh_exposure, 
                  family = "binomial",
                  data = dat) %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>% 
  add_n(location = "level") %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1) %>% 
  add_global_p()
decr_int_ee

decr_int_bo <- glm(burn_out ~ decrease_contract*wfh_exposure, 
                  family = "binomial",
                  data = dat) %>% 
  tbl_regression(exponentiate = TRUE)  %>% 
  bold_labels() %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1) #%>% add_global_p()
decr_int_bo

# Interactions for decreased contract percentage and decreased teleworking ####
## Filtered for only people that do some teleworking, so that we're comparing those that decreased telework vs other teleworkers
teleworker_data <- dat %>% filter(wfh_dich == "Yes")
decr_int_ee <- glm(burnout_interp_dich30 ~ decrease_contract*decreased_teleworking, 
                   family = "binomial",
                   data = teleworker_data) %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  bold_labels() %>% 
  add_n() %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1) %>% 
  add_global_p() ; decr_int_ee

decr_int_bo <- glm(burn_out ~ decrease_contract*decreased_teleworking, 
                   family = "binomial",
                   data = teleworker_data) %>% 
  tbl_regression(exponentiate = TRUE)  %>% 
  bold_labels() %>% 
  add_nevent(location = "level") %>% # add number of events of the outcome
  bold_p(t = 0.1); decr_int_bo
