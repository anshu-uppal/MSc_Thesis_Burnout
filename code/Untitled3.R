"points_burn_out_scale_1_much_effort"
"points_burn_out_scale_2_too_stressed"


dat %>%
  ggplot(aes(x = (points_burn_out_scale_1_much_effort)))+
  geom_histogram()

dat %>% 
  group_by(wfh_exposure, wfh_balance) %>% 
  summarise(effort = mean(points_burn_out_scale_1_much_effort, na.rm = TRUE),
            stressed = mean(points_burn_out_scale_2_too_stressed, na.rm = TRUE)) %>% 
  ggplot(aes(x = wfh_exposure, y=stressed, fill = wfh_balance))+
  geom_col(position = "dodge")

mcovariates <- c("wfh_exposure","age","sex_en")

ulin <- dat %>%
  select(points_burn_out_scale_1_much_effort, all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = points_burn_out_scale_1_much_effort,
    method.args = list(family = gaussian),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  # add_global_p() %>% # add global p-value
  # add_nevent() %>% # add number of events of the outcome
  # add_q() %>% # adjusts global p-values for multiple testing
  bold_p() %>% # bold p-values under a given threshold (default 0.05)
  # bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  add_n(location = "level") %>% 
  bold_labels() #%>% 
# as_gt() %>%
# gt::tab_header(title = "Univariate linear regression",
#                subtitle = "EE-MBI score (continuous)")

# Check residuals of ulin

ulin
