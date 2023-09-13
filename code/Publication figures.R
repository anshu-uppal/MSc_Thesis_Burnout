## Publication figures 

pacman::p_load(
  tidyverse,
  forestplot,  # package for creating the forestplots
  here,
  devEMF,      # print figures into a vectorized format
  gtsummary    # summary statistics and tests
)

# Read in clean dataset (see "Merge code.R" and "Cleaning merged dataset")
dat <- tibble(readRDS(here("data", "clean_dataset.rds")))

# Try out having only two age groups
# dat <- dat %>% mutate(age_cat = factor(case_when(     
#                         age >= 25 & age < 45 ~ "25-44",
#                         age >= 45 & age < 65 ~ "45-64")))

# Remove SERO-CoV-WORK
# dat <- dat %>% filter(serocovwork_exclusive == FALSE)

# Stratify by sex
# dat <- dat %>% filter(sex_en == "Female")



# define main exposure and other explanatory variables of interest - can be used for linear and logistic regressions
mcovariates <- c(
                 "wfh_exposure",
                 "age_cat",
                 "sex_en",
                 "education_rec_en",
                 "hh_income_cat_en",
                 "finance_situation",
                 "percent_change_cat",
                 # "years_of_service_en",
                 "supervision_short", # which variable to use for financial security? or too linked with income?
                 "hh_livewith_rec_en",
                 "num_young_children_dich",
                 "overcrowded",
                 "noisy", 
                 "quiet_room", 
                 # "teleworkability",
                 # "work_interruption",
                 "health_general_dich"
)

# Create univariate regression model
#### Dichotomised EE-MBI score ####
severe <- 30 # cutoff for whether MBI score is classified as severe or not
a <- dat %>% mutate(
  Severe_MBI = case_when(
    burnout_score >= severe ~ 1,
    burnout_score < severe ~ 0,
    .default = burnout_score),
  feeling_exhausted_dich = case_when(        # Added this in there in case we want to check this outcome for comparison
    feeling_exhausted_dich == "Yes" ~ 1,
    feeling_exhausted_dich == "No" ~ 0,
    .default = NA)
  )

uv_eembi_log <- a %>%
  select(
    Severe_MBI,
    # feeling_exhausted_dich,
    all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = Severe_MBI,
    # y = feeling_exhausted_dich,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

# Automatic inclusion of pre-defined explanatory variables
outcome <- "Severe_MBI"           # Define outcome
# outcome <- "feeling_exhausted_dich"
# Create model
m2 <- mcovariates %>%                      ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
  str_c(outcome," ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",                  ## define the family as binomial for logistic regression
      data = a)                          ## define your dataset

# Organize outputs into a table
mv_eembi_log <- tbl_regression(m2, exponentiate = TRUE)


## Forest plots ####

## get data from the regression tables

## Univariate data ("unadjusted")
uv_mbi <- uv_eembi_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
                                         label, n_obs, n_event, estimate, conf.low, 
                                         conf.high, ci, p.value) %>% 
  filter(row_type == "level") %>% 
  mutate(estimate = round(estimate,2),
         model = "Unadjusted",
         new_variable = case_match(       # relabel to look nice for the plots
           variable,
           "wfh_exposure" ~ "Teleworking group",
           "age_cat" ~ "Age group, years",
           "sex_en" ~ "Sex",
           "education_rec_en" ~ "Education",
           "hh_income_cat_en" ~ "Household income",
           "finance_situation" ~ "Financial security",
           "percent_change_cat" ~ "Change in working hours",
           "years_of_service_en" ~ "Time in service",
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
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
           ),
         full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
         )

# Multivariable regression data ("adjusted")
mv_mbi <- mv_eembi_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
                                         label, n_obs, n_event, estimate, conf.low, 
                                         conf.high, ci, p.value) %>% 
  filter(row_type == "level")%>% 
  mutate(estimate = round(estimate,2),
         model = "Adjusted",
         new_variable = case_match(
           variable,
           "wfh_exposure" ~ "Teleworking group",
           "age_cat" ~ "Age group, years",
           "sex_en" ~ "Sex",
           "education_rec_en" ~ "Education",
           "hh_income_cat_en" ~ "Household income",
           "finance_situation" ~ "Financial security",
           "percent_change_cat" ~ "Change in working hours",
           "years_of_service_en" ~ "Time in service",
           "supervision_short" ~ "Management role",
           "hh_livewith_rec_en" ~ "Living arrangement", "num_young_children_dich" ~ "Young children",
           "overcrowded" ~ "Household density",
           "noisy" ~ "Noise level at home",
           "quiet_room" ~ "Access to a quiet room",         
           "health_general_dich" ~ "Self-reported general health"
         ),
         new_label = case_when(
           reference_row == TRUE ~ paste0(new_variable, " (ref: ", label, ")"),
           .default = paste0("      ",label)
         ),
         is.summary = reference_row,
         N_uv = NA,
         N_mv = if_else(reference_row == TRUE, N, NA),
         n_comb_uv = NA,
         n_comb_mv = paste0(n_obs, " (",n_event,")"),
         n_perc_uv = NA,
         n_perc_mv = paste0(n_obs, " (",round(n_event / n_obs *100, 1), ")"),
         p.sig = case_when(
           reference_row ~ NA,
           p.value < 0.001 ~ "***",
           p.value < 0.01 ~ "**",
           p.value <= 0.05 ~ "*",
           .default = NA
         ),
         full_estimate_uv = NA,
         full_estimate_mv = case_when(
           reference_row == TRUE ~ NA,
           is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
         )
  )

# fill in the empty rows for the two tables' full estimates (they need to be identical for the combined forest plot)
uv_mbi <- uv_mbi %>% mutate(full_estimate_mv = mv_mbi$full_estimate_mv
                          , n_comb_mv = mv_mbi$n_comb_mv
                          , N_mv = mv_mbi$N_mv
                          , n_perc_mv = mv_mbi$n_perc_mv
)
mv_mbi <- mv_mbi %>% mutate(full_estimate_uv = uv_mbi$full_estimate_uv
                          , n_comb_uv = uv_mbi$n_comb_uv
                          , N_uv = uv_mbi$N_uv
                          , n_perc_uv = uv_mbi$n_perc_uv
)

# Row bind the two tables 
combi <- rbind(uv_mbi, mv_mbi)
combi <- combi %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                          full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))


## Final forest plot with both values in one figure ####
ee_mbi_plot <- combi %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.45, 4),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "ff",
             xlab = "OR (odds of severe EE−MBI relative to reference group)",
             xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.98)
             )
  ) %>% 
  fp_decorate_graph(graph.pos = 3) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(new_label = c("", ""),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_left(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; ee_mbi_plot


### ### ### ### ### ### ###
### Diagnosed burnout #####

# Filter out people who submitted the inclusion questionnaire less than one year 
# before the sante travail questionnaire 
a <- dat #%>% filter(difftime(date_soumission.y, date_soumission.x, "days") > 365)

uv_bo_log <- a %>%
  select(burn_out, all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = burn_out,
    method.args = list(family = binomial),
    exponentiate = TRUE)


# Automatic inclusion of pre-defined explanatory variables
outcome <- "burn_out"           # Define outcome
# Create model
mbo <- mcovariates %>%                      ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
  str_c(outcome," ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",                  ## define the family as binomial for logistic regression
      data = a)                          ## define your dataset

# Organize outputs into a table
mv_bo_log <- tbl_regression(mbo,
                               exponentiate = TRUE)

## Forest plots ####

## get data from the regression tables

## Univariate data ("unadjusted")
uv_bo <- uv_bo_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
                                             label, n_obs, n_event, estimate, conf.low, 
                                             conf.high, ci, p.value) %>% 
  filter(row_type == "level") %>% 
  mutate(estimate = round(estimate,2),
         model = "Unadjusted",
         new_variable = case_match(       # relabel to look nice for the plots
           variable,
           "wfh_exposure" ~ "Teleworking group",
           "age_cat" ~ "Age group, years",
           "sex_en" ~ "Sex",
           "education_rec_en" ~ "Education",
           "hh_income_cat_en" ~ "Household income",
           "finance_situation" ~ "Financial security",
           "percent_change_cat" ~ "Change in working hours",
           "years_of_service_en" ~ "Time in service",
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
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")", p.sig)
           ),
         full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
  )

# Multivariable regression data ("adjusted")
mv_bo <- mv_bo_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
                                             label, n_obs, n_event, estimate, conf.low, 
                                             conf.high, ci, p.value) %>% 
  filter(row_type == "level")%>% 
  mutate(estimate = round(estimate,2),
         model = "Adjusted",
         new_variable = case_match(
           variable,
           "wfh_exposure" ~ "Teleworking group",
           "age_cat" ~ "Age group, years",
           "sex_en" ~ "Sex",
           "education_rec_en" ~ "Education",
           "hh_income_cat_en" ~ "Household income",
           "finance_situation" ~ "Financial security",
           "percent_change_cat" ~ "Change in working hours",
           "years_of_service_en" ~ "Time in service",
           "supervision_short" ~ "Management role",
           "hh_livewith_rec_en" ~ "Living arrangement", "num_young_children_dich" ~ "Young children",
           "overcrowded" ~ "Household density",
           "noisy" ~ "Noise level at home",
           "quiet_room" ~ "Access to a quiet room",         
           "health_general_dich" ~ "Self-reported general health"
         ),
         new_label = case_when(
           reference_row == TRUE ~ paste0(new_variable, " (ref: ", label, ")"),
           .default = paste0("      ",label)
         ),
         is.summary = reference_row,
         N_uv = NA,
         N_mv = if_else(reference_row == TRUE, N, NA),
         n_comb_uv = NA,
         n_comb_mv = paste0(n_obs, " (",n_event,")"),
         n_perc_uv = NA,
         n_perc_mv = paste0(n_obs, " (",round(n_event / n_obs *100, 1), ")"),
         p.sig = case_when(
           reference_row ~ NA,
           p.value < 0.001 ~ "***",
           p.value < 0.01 ~ "**",
           p.value <= 0.05 ~ "*",
           .default = NA
         ),
         full_estimate_uv = NA,
         full_estimate_mv = case_when(
           reference_row == TRUE ~ NA,
           is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig)
         )
  )

# fill in the empty rows for the two tables' full estimates (they need to be identical for the combined forest plot)
uv_bo <- uv_bo %>% mutate(full_estimate_mv = mv_bo$full_estimate_mv
                          , n_comb_mv = mv_bo$n_comb_mv
                          , N_mv = mv_bo$N_mv
                          , n_perc_mv = mv_bo$n_perc_mv
                          
                          )
mv_bo <- mv_bo %>% mutate(full_estimate_uv = uv_bo$full_estimate_uv
                          , n_comb_uv = uv_bo$n_comb_uv
                          , N_uv = uv_bo$N_uv
                          , n_perc_uv = uv_bo$n_perc_uv
                          )


## !! CAUTION !! Need to talk to Elsa and Nick, may need to report both N separately, 
## or remove missing from all observations?

# Row bind the two tables 
combi_bo <- rbind(uv_bo, mv_bo)
combi_bo <- combi_bo %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                                full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))

## Final forest plot with both values in one figure ####
diagnosed_bo_plot <- combi_bo %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.45, 4),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "ff",
             xlab = "OR (relative to reference group)",
             xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.98)
             )
  ) %>% 
  fp_decorate_graph(graph.pos = 3) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(new_label = c("", ""),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_left(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; diagnosed_bo_plot


# # ## Print PDFs ####
# pdf(file = "diagnosed_bo.pdf",width = 30, height = 10,
#       colormodel = "cmyk", paper = "a4r") ; diagnosed_bo_plot ; dev.off()
# pdf(file = "ee_mbi.pdf",width = 18, height = 11,
#     colormodel = "cmyk", paper = "a4r") ; ee_mbi_plot ; dev.off()

## Print EMF files (vectorized)
emf(file = "diagnosed_bo.emf",width = 9, height = 8, bg = "white") ; diagnosed_bo_plot ; dev.off() 
emf(file = "ee_mbi.emf",width = 9, height = 8, bg = "white") ; ee_mbi_plot ; dev.off() 



# Combine the two graphs (minus CIs) ####
## Final forest plot with both values in one figure ####
ee_mbi_plot2 <- combi %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           # full_estimate_uv, full_estimate_mv,
                           n_perc_uv
  ),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.45, 4),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "ff",
             xlab = "",
             xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.98)
             )
  ) %>% 
  # fp_decorate_graph(graph.pos = 5) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(new_label = c("", ""),
                # full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                # full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left(),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; ee_mbi_plot2

#############################################

diagnosed_bo_plot2 <- combi_bo %>% group_by(model) %>%
  forestplot(labeltext = c(
    # full_estimate_uv, 
    # full_estimate_mv, 
    n_perc_uv
    ),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.45, 4),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
            # new_page = FALSE,
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "Diagnosed occupational burnout",
             xlab = "",
             xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.98)
             )
  ) %>% 
  fp_decorate_graph(graph.pos = 1) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(
                # full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                # full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left(),
                n_perc_uv = c("N", "(% cases)") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; diagnosed_bo_plot2

emf(file = "combined without CI.emf",width = 9.6, height = 7, bg = "white")  # full is width = 15, height = 8
## Combine the two figures
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = c(2.2,1)))) # set 1.6 for full
pushViewport(viewport(layout.pos.col = 1))
plot(ee_mbi_plot2)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
plot(diagnosed_bo_plot2)
popViewport(2)
dev.off()


# Combine the two graphs (with CIs) ####
## Final forest plot with both values in one figure ####
ee_mbi_plot3 <- combi %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv,
  ),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
  lty.ci = c(1, 2),                                  # Set the linetypes
  clip = c(0.45, 6),                               # Cut off the x-axis
  vertices = TRUE,
  ci.vertices = TRUE,
  ci.vertices.height = 0.15,
  boxsize = .5,
  graphwidth = unit(2, "inches"),
  colgap = unit(3, "mm"),
  line.margin = .1, # Add this to avoid crowding
  xlog = TRUE,      # Show x-axis on log scale
  mean = estimate,
  lower = conf.low,
  upper = conf.high,
  align = "llll",
  # hrzl_lines = list("7" = gpar(lty = 1)),
  is.summary = reference_row,
  # title = "ff",
  xlab = "",
  xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)
             , log(5), log(6)
             ),
  xticks.digits = 1,
  grid = structure(c(1), 
                   gp=gpar(lty=1, lwd=1, col = "#999999"))
  , legend_args = fpLegend(
    gp = gpar(col = "transparent", fill = "transparent"),
    padding = unit(1, "mm"),
    pos = list(x = 0.5, y = 0.98)
  )
  ) %>% 
  # fp_decorate_graph(graph.pos = 5) %>%
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(new_label = c("", ""),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left(),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; ee_mbi_plot3

#############################################

diagnosed_bo_plot3 <- combi_bo %>% group_by(model) %>%
  forestplot(labeltext = c(
    full_estimate_uv,
    full_estimate_mv,
    n_perc_uv
  ),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
  lty.ci = c(1, 2),                                  # Set the linetypes
  clip = c(0.45, 6),                               # Cut off the x-axis
  vertices = TRUE,
  ci.vertices = TRUE,
  ci.vertices.height = 0.15,
  boxsize = .5,
  graphwidth = unit(2, "inches"),
  colgap = unit(3, "mm"),
  line.margin = .1, # Add this to avoid crowding
  xlog = TRUE,      # Show x-axis on log scale
  mean = estimate,
  lower = conf.low,
  upper = conf.high,
  # new_page = FALSE,
  align = "llll",
  # hrzl_lines = list("7" = gpar(lty = 1)),
  is.summary = reference_row,
  # title = "Diagnosed occupational burnout",
  xlab = "",
  xticks = c(log(0.5), log(0.666), log(1), log(1.5), log(2), log(3), log(4)
             , log(5), log(6)
             ),
  xticks.digits = 1,
  grid = structure(c(1), 
                   gp=gpar(lty=1, lwd=1, col = "#999999"))
  , legend_args = fpLegend(
    gp = gpar(col = "transparent", fill = "transparent"),
    padding = unit(1, "mm"),
    pos = list(x = 0.5, y = 0.98)
  )
  ) %>% 
  fp_decorate_graph(graph.pos = 1) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.9)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(
    full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
    full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left(),
    n_perc_uv = c("N", "(% cases)") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; diagnosed_bo_plot3

emf(file = "combined with CI.emf",width = 14.7, height = 8, bg = "white")  # full is width = 14.7, height = 8
## Combine the two figures
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = c(1.6,1)))) # set 1.6 for full
pushViewport(viewport(layout.pos.col = 1))
plot(ee_mbi_plot3)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
plot(diagnosed_bo_plot3)
popViewport(2)
dev.off()
