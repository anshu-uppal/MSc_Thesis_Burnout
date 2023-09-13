## Publication figures 

pacman::p_load(
  tidyverse,
  forestplot,
  here,
  gtsummary    # summary statistics and tests
)

# Read in clean dataset (see "Merge code.R" and "Cleaning merged dataset")
dat <- tibble(readRDS(here("data", "clean_dataset.rds")))
# Relevel to show "Yes, no change" as reference level?
dat <- dat %>% mutate(wfh_exposure = factor(wfh_exposure, 
                                            levels = c("Yes, no change", "Yes, increase", "Yes, decrease", "Never", "Not possible")),
                      age_cat = factor(age_cat, levels = c("60+", "50-59", "40-49", "30-39", "18-29"))
)
# define main exposure and other explanatory variables of interest - can be used for linear and logistic regressions
mcovariates <- c("burnout_interp_dich30", "age_cat","sex_en","education_rec_en",
                 "hh_income_cat_en",
                 "finance_situation", 
                 # "contract_cdi",  
                 "percent_change_cat", 
                 "supervision_short", 
                 "hh_livewith_rec_en", "num_young_children_dich", "overcrowded", # "zone_house" to be added as well?
                 "noisy", "quiet_room", 
                 # "teleworkability",
                 # "work_interruption",
                 "health_general_dich"
)

# Create multivariate regression model
a <- dat %>% mutate(
  decrease_binary = case_when(
    wfh_exposure == "Yes, decrease" ~ 1,
    wfh_exposure != "Yes, decrease" ~ 0,
    .default = NA),
  not_possible = case_when(
    wfh_exposure == "Not possible" ~ 1,
    wfh_exposure != "Not possible" ~ 0,
    .default = NA))

uv_decreased_log <- a %>%
  select(
    decrease_binary,
    # not_possible, 
    all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = decrease_binary,
    # y = not_possible,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

## Forest plots ####

## get data from the regression tables

## Univariate data ("unadjusted")
uv_decreased <- decreased_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
                                             label, n_obs, n_event, estimate, conf.low, 
                                             conf.high, ci, p.value) %>% 
  filter(row_type == "level") %>% 
  mutate(estimate = round(estimate,2),
         model = "Unadjusted",
         new_variable = case_match(       # relabel to look nice for the plots
           variable,
           "burnout_interp_dich30" ~ "Level of emotional exhaustion",
           "age_cat" ~ "Age group, years",
           "sex_en" ~ "Sex",
           "education_rec_en" ~ "Education",
           "hh_income_cat_en" ~ "Household income",
           "finance_situation" ~ "Financial security",
           "contract_cdi" ~ "Unlimited contract?",
           "percent_change_cat" ~ "Change in working hours",
           "supervision_short" ~ "Management role",
           "hh_livewith_rec_en" ~ "Living arrangement", "num_young_children_dich" ~ "Young children",
           "overcrowded" ~ "Household density",
           "noisy" ~ "Noise level at home",
           "quiet_room" ~ "Access to a quiet room",         
           "health_general_dich" ~ "Self-reported general health"
         ),
         new_label = case_when(         # Create a new label to take into account the reference lines
           reference_row == TRUE ~ paste0(new_variable, " (ref: ", label, ")"),
           .default = paste0("      ",label)
         ),
         is.summary = reference_row,
         N_uv = if_else(reference_row == TRUE, N, NA),
         N_mv = NA,
         n_comb_uv = paste0(n_obs, " (",n_event,")"),   # combine N and N event into a single variable
         n_comb_mv = NA,
         n_perc_uv = paste0(n_obs, " (",round(n_event / n_obs *100, 1), ")"),
         n_perc_mv = NA,
         p.sig = case_when(                         # show p-values symbolically
           reference_row ~ NA,
           p.value < 0.001 ~ "***",
           p.value < 0.01 ~ "**",
           p.value <= 0.05 ~ "*",
           .default = NA
         ),
         full_estimate_uv = case_when(            # combine OR, CI, and p-value symbol in one variable
           reference_row == TRUE ~ NA,
           is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")")#,p.sig)
         ),
         full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
  )

## Final forest plot with both values in one figure ####
uv_decreased %>%
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv, 
                           n_perc_uv,
                           full_estimate_uv),
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             clip = c(0.45, 2),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,     # Show x-axis on log scale)
             is.summary = reference_row,
             # title = "ff",
             align = "lll",
             xlab = "OR (odds of decreased teleworking relative to reference group)",
             xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2)),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
  ) %>%
  fp_set_style(box = c("#4271bd"),
               line = c("#4271bd"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )) %>% 
  fp_add_header(new_label = c("", ""),
                # n_comb_uv = c("N (diagnosed", "burnout)") %>% fp_align_center(),
                n_perc_uv = c("N", "(% cases)") %>% fp_align_left(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left()
                ) %>% 
  fp_add_lines("#999999") %>% 
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)
