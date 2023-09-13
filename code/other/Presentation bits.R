dat %>% 
  mutate(burn_out = factor(burn_out),
         # burnout_interp_dich = case_match(burnout_interp_dich,
         #                              "Mild" ~ 0,
         #                              "Severe" ~ 1)
         ) %>% 
  select(burnout_interp_dich, burn_out) %>% 
  tbl_uvregression(
    method = glm,
    y = burn_out,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = FALSE
  ) %>%
  # add_global_p() %>% # add global p-value
  add_nevent(location = "level") %>% # add number of events of the outcome
  # add_q() %>% # adjusts global p-values for multiple testing
  bold_p() %>% # bold p-values under a given threshold (default 0.05)
  # bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  add_n(location = "level") #%>%



dat %>%
  group_by(burnout_interp_dich) %>%
  count(burnout_definition) %>%
  mutate(percent = 100 * n / sum(n))

dat %>%
  # group_by(burnout_interp_dich) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n))



### Logistic Burnout definition
mcovariates <- c("wfh_exposure",
                 "age_cat","sex_en","education_rec_en",
                 "hh_income_cat_en", "finance_situation", # which variable to use for financial security? or too linked with income?
                 "hh_livewith_rec_en", "children_n_dich", "overcrowded", # "zone_house" to be added as well?
                 "noisy", "quiet_room",
                 # "work_interruption",
                 "health_general_dich"
)
# Univariable logistic regression
uv_bo_definition_log <- dat %>%
  select(burnout_harmonized, all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = burnout_harmonized,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = FALSE
  ) %>%
  # add_global_p() %>% # add global p-value
  add_nevent(location = "level") %>% # add number of events of the outcome
  # add_q() %>% # adjusts global p-values for multiple testing
  bold_p() %>% # bold p-values under a given threshold (default 0.05)
  # bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  add_n(location = "level") %>%
  bold_labels() #%>% 
# as_gt() %>%
# gt::tab_header(title = "Univariate logistic regression",
#                subtitle = paste0("Severe = 1 when EE-MBI >= ",severe))


# Automatic inclusion of pre-defined explanatory variables
outcome <- "burnout_harmonized"           # Define outcome
# Create model
m4 <- mcovariates %>%                      ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
  str_c(outcome," ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",                  ## define the family as binomial for logistic regression
      data = dat)                          ## define your dataset


# Organize outputs into a table
mv_bo_definition_log <- tbl_regression(m4,
                             exponentiate = TRUE,
                             pvalue_fun = ~ style_pvalue(.x, digits = 2)
) %>%
  bold_p() %>%
  bold_labels()

# **Table combining Univariable and Multivariable logistic regressions for Burnout definition**  
  # i.e. EE-MBI score >= 27 AND Exhaustion at work = "Yes"
tbl_merge(
  tbls = list(uv_bo_definition_log, mv_bo_definition_log),
  tab_spanner = c("**Univariable regression**", "**Multivariable regression**")
)



dat %>%
  ggplot(aes(x = burnout_score, fill = factor(burnout_definition)))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("lightseagreen", "#F8766D"))+
  labs(title = "Histogram of EE-MBI score \n(Severe \u2265 27 & Exhaustion from work = `Yes`)", x = "EE-MBI score")+
  theme_classic()+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())


dat %>% 
  # filter(!is.na(sex_en)) %>% 
  group_by(education_rec_en) %>%
  count(wfh_exposure) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  ggplot(aes(x = wfh_exposure, y = percent))+
  geom_col()+
  geom_text(aes(label = round(percent,1)), vjust = -0.2)+
  theme_bw()+
  labs(x = "Teleworking frequency and changes since the pandemic ")+
  theme(axis.text.x = element_text(angle = 15))+
  facet_wrap(.~education_rec_en)

## EE-MBI classification - job_sector
dat %>%
  mutate(job_sector = str_wrap(job_sector, width = 60)) %>% 
  # filter(!is.na(sex_en)) %>%
  group_by(job_sector) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich == "Severe") %>%
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
        axis.text.y = element_text(size = 9))

## EE-MBI classification - age category
dat %>%
  filter(!is.na(sex_en)) %>% 
  mutate(age_cat = factor(age_cat, levels = c("18-29", "30-39", "40-49", "50-59", "60+"))) %>% 
  group_by(age_cat, sex_en) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich == "Severe") %>%
  # mutate(job_sector =  fct_reorder(job_sector,percent)) %>% 
  ggplot(aes(x = age_cat, y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), vjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Severe EE-MBI", x = "Age category", title = "Percent with Severe EE-MBI among men and women")+
  # coord_flip()+
  # ylim(0, 50)+
  facet_wrap(.~sex_en)+
  theme(
    # axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9))


## Work impact on health
dat %>%
  group_by(positive_impact) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich == "Severe") %>%
  ggplot(aes(x = positive_impact, y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), vjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Severe EE-MBI", x = "Does work have a positive impact on health?", title = "Severe EE-MBI by perception of health impacts of job")+
  theme(
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9))

## Work life balance and EE-MBI
dat %>%
  group_by(work_life_balance) %>%
  count(burnout_interp_dich) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  filter(burnout_interp_dich == "Severe") %>%
  ggplot(aes(x = work_life_balance, y = percent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = paste0(round(percent,1), " (N=",N,")")), vjust = -0.2)+
  theme_classic()+
  labs(y = "Percent with Severe EE-MBI", x = "Do you manage to maintain a good work-life balance?", title = "Severe EE-MBI by perception of work-life balance")+
  theme(
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9))

## Diagnosed burnout
dat %>% 
  filter(!is.na(sex_en)) %>%
  group_by(sex_en) %>%
  count(burn_out) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  filter(burn_out != 0) %>% 
  ggplot(aes(x = burn_out, y = percent))+
  geom_col()+
  geom_text(aes(label = round(percent,1)), vjust = -0.2)+
  theme_bw()+
  labs(x = "Diagnosed burnout")+
  facet_wrap(.~sex_en)+
  theme(axis.text.x = element_text(angle = 15))





dat %>%
  filter(!is.na(sex_en)) %>%
  group_by(sex_en) %>% 
  summarise(MBI = round(mean(burnout_score, na.rm = TRUE),1)) %>% 
  ggplot(aes(x = sex_en, y = MBI))+
  geom_col()+
  geom_text(aes(label = MBI), vjust = -0.2)+
  labs(x = "Sex")+
  # facet_wrap(.~sex_en)+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 10, hjust = 1, size = 13))




### Entry into study ####
dat %>%
  ggplot(aes(x=date_soumission.y, fill = serocvovwork_exclusive))+
  geom_histogram(binwidth = 10)+
  labs(title = "Santé-Travail submission dates", fill = "SEROCoV-Work only")+
  theme_bw()+
  theme(legend.position = c(0.65,0.6),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10))



dat %>% 
  filter(sex_en != "Other") %>% # filter out the "Other" selections
  # filter(serocvovwork_exclusive == FALSE) %>% 
  mutate(age_cat = factor(age_cat, levels = c("18-29", "30-39", "40-49", "50-59", "60+"))) %>% 
  apyramid::age_pyramid(age_group = "age_cat",
                        split_by = "sex_en"
                        # , proportional = TRUE     # Display as % of all cases instead of counts
  )+
  labs(title = "Age groups by sex", x = "Age category")+
  theme(legend.position = c(0.15,0.22))

dat %>% 
  filter(!is.na(hh_income_cat_en)) %>% 
  count(hh_income_cat_en) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  ggplot(aes(x=hh_income_cat_en, y = percent))+
  labs(title = "Household income distribution")+
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(percent,1)), vjust = -0.2)+
  theme(axis.text.x=element_text(size = 18, angle = 20),
        axis.title.x = element_blank())

## Percent working overtime ####
dat %>%
  group_by(wfh_trich) %>%
  count(work_overtime_dich) %>%
  mutate(percent = 100 * n / sum(n),
         N = sum(n)) %>% 
  ggplot(aes(x = wfh_trich, y = percent, fill = work_overtime_dich, label = paste0(round(percent,1), " (N=",n,")")))+
  geom_col(position = position_dodge())+
  geom_text(position = position_dodge(width = 0.9), vjust = -0.2)+
  theme_classic()+
  labs(y = "Percent", x = "Do you work work from home?", title = "Overtime frequency by teleworking situation")+
  # facet_wrap(.~wfh_trich)+
  theme(
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9))