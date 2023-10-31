# Here's a template for anyone that would like to display unadjusted and 
# adjusted odds ratios in a forest plot
# ! This won't work if you're including interaction terms !
# (but you can probably adapt it)

pacman::p_load(
  tidyverse,
  gtsummary,
  forestplot,
  here,
  forcats,
  devEMF # print figures into a vectorized format
)


### ### ### ### ### ### ### ### ### ###
# Section that needs your input ####
### ### ### ### ### ### ### ### ### ###

# update dat to use your dataset
dat <- tibble(readRDS(here("data", "clean_dataset.rds")))
# dat <- filt

# Make sure your outcome_1 is binary (1/0)
dat <- dat %>% mutate(
  burnout_interp_dich30 = case_when(
    burnout_interp_dich30 == "Severe" ~ 1,
    burnout_interp_dich30 == "Not Severe" ~ 0,
    .default = NA),
  decrease_binary = case_when(
    wfh_exposure == "Decrease" ~ 1,
    wfh_exposure != "Decrease" ~ 0,
    .default = NA),
  not_possible = case_when(
    wfh_exposure == "Not possible" ~ 1,
    wfh_exposure != "Not possible" ~ 0,
    .default = NA),
  wfh_binary = case_when(
    wfh_dich == "Yes" ~ 1,
    wfh_dich == "No" ~ 0,
    .default = NA)) %>% 
  filter(sex_en %in% c("Male", "Female")) %>% mutate(sex_en = droplevels(sex_en))


# Choose sex
sex_choose <- c("Male", "Female")

# Enter the variable for your outcome_1
outcomes <- c("burnout_interp_dich30", "burn_out", "burnout_score")

# Enter the variable for your exposure
exposure <- "wfh_exposure"

# Enter the variables for your covariates
covariates <- c(
  "age_cat",
  "education_rec_en",
  "hh_income_cat_en",
  "finance_situation",
  "percent_change_cat",
  "supervision_short", # which variable to use for financial security? or too linked with income?
  "hh_livewith_rec_en",
  "num_young_children_dich",
  "overcrowded",
  "noisy", 
  "quiet_room", 
  "health_general_dich"
  # , "karasek_social_support_cat"
  # , "Occupation_label_2"
  # , "job_sector"
)

# Enter a "clean" name for your exposure and covariates
# (covariates need to be in the same order!)
clean_name <- c(
  "Teleworking group"       #Exposure
  , "Age group, years"    # Rest of covariates
  , "Education"
  , "Household income"
  , "Financial security"
  , "Change in working hours"
  , "Management role"
  , "Living arrangement"
  , "Young children"
  , "Household density"
  , "Noise level at home"
  , "Access to a quiet room"
  , "Self-reported general health"
  # , "Social support"
  # , "ISCO Occupation label"
  # , "Job sector"
)


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Make a table of the variables and their "clean" names
variable <- c(exposure, covariates) 
clean_labels <- tibble(variable, clean_name)
rm(variable, clean_name) # remove these to avoid any confusion later

# Open the clean_labels table to double-check!!

### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Outcome 1_Severe exhaustion ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###
plot_data_outcome1 <- vector("list", length = length(sex_choose))
for (i in 1:length(sex_choose)) {
  sex_set <- dat %>% filter(sex_en == sex_choose[i])
  ## Create your univariate logistic regression models ####
  uv_model_1 <- sex_set %>% 
    select(all_of(outcomes[1]), all_of(exposure), all_of(covariates)) %>% 
    tbl_uvregression(
      method = glm,           # Set the family
      exponentiate = TRUE,
      y = outcomes[1],
      method.args = list(family = binomial)
    )
  # uv_model_1
  
  ## Create multivariable logistic regression model ####
  # Create model
  mv_model_1 <- c(exposure, covariates) %>%     ## begin with vector of exposure and covariate column names
    str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
    str_c(outcomes[1]," ~ ", .) %>%        ## combine the names of variables of interest with outcome_1 in formula style
    glm(family = "binomial",                  ## define the family as binomial for logistic regression
        data = sex_set) %>%                         ## define your dataset
    tbl_regression(exponentiate = TRUE)
  # mv_model_1
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## Extract estimates and CIs from the models ####
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  ### UV model estimates ####
  uv_estimates_1 <- uv_model_1$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    select(clean_name, var_type,  reference_row, row_type, N, 
           label, n_obs, n_event, estimate, conf.low, 
           conf.high, ci, p.value) %>%
    filter(row_type == "level") %>% 
    mutate(estimate = round(estimate,2),
           model = "Unadjusted",
           new_label = case_when(         # Create a new label to take into account the reference lines
             reference_row == TRUE ~ paste0(clean_name, " (ref: ", label, ")"),
             .default = paste0("      ",label)
           ),
           is.summary = reference_row,
           N_uv = if_else(reference_row == TRUE, N, NA), # Get N from the UV models
           N_mv = NA,   # same for MV but will be filled in later steps
           n_comb_uv = paste0(n_obs, " (",n_event,")"),   # combine UV N and N event into a single variable
           n_comb_mv = NA,  # Same for mv, but it's empty here and will be filled in later steps
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
             n_event == 0 ~ "NA",
             is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
             reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
           ),
           full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
    )
  
  
  ### MV Model estimates ####
  mv_estimates_1 <- mv_model_1$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    select(clean_name, var_type,  reference_row, row_type, N, 
           label, n_obs, n_event, estimate, conf.low, 
           conf.high, ci, p.value) %>% 
    filter(row_type == "level")%>% 
    mutate(estimate = round(estimate,2),
           model = "Adjusted",
           new_label = case_when(
             reference_row == TRUE ~ paste0(clean_name, " (ref: ", label, ")"),
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
             n_event == 0 ~ "NA",
             is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
             reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
           )
    )
  
  
  ## fill in the empty rows for the two tables' full estimates ####
  # (they need to be identical for the combined forest plot)
  uv_estimates_1 <- uv_estimates_1 %>% mutate(full_estimate_mv = mv_estimates_1$full_estimate_mv
                                              , n_comb_mv = mv_estimates_1$n_comb_mv
                                              , N_mv = mv_estimates_1$N_mv
                                              , n_perc_mv = mv_estimates_1$n_perc_mv
  )
  mv_estimates_1 <- mv_estimates_1 %>% mutate(full_estimate_uv = uv_estimates_1$full_estimate_uv
                                              , n_comb_uv = uv_estimates_1$n_comb_uv
                                              , N_uv = uv_estimates_1$N_uv
                                              , n_perc_uv = uv_estimates_1$n_perc_uv
  )
  
  # Row bind the two tables to make a final combined table
  combi_1 <- rbind(uv_estimates_1, mv_estimates_1)
  # Optional remove p-value symbols if you don't need them
  combi_1 <- combi_1 %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                                full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))
  
  # Store data for each of the outcomes in a list
  plot_data_outcome1[i] <- list(combi_1)
  
}




## Plot male ####

plot_male_outcome_1 <- plot_data_outcome1[[1]] %>% group_by(model) %>%
  filter(clean_name == "Teleworking group") %>% # Can restrict figure to only some of the variables
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.66, 6.07),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),   # Set graphwidth
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             
             # Set the estimate and the confidence intervals
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             
             # Set alignment of the column values
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "*ISCO occupation group is adjusted for but not displayed here",
             xlab = "Odds ratio (with 95% CI)",
             
             # Set the tick marks on the x-axis as needed
             xticks = c(           
               log(0.666), log(1), log(1.5), log(2)
               , log(3), log(4), log(6)
             ),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             # Optional legend box formatting and position
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.88)
             )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 5) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  # Add lines for each group
  fp_add_lines("#999999") %>%
  # Name the headers
  fp_add_header(new_label = c("", ""),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_center(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_center(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)

## Plot Female ####
plot_female_outcome_1 <- plot_data_outcome1[[2]] %>% group_by(model) %>%
  filter(clean_name == "Teleworking group") %>% # Can restrict figure to only some of the variables
  forestplot(labeltext = c(
    full_estimate_uv, full_estimate_mv, n_perc_uv
  ),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
  lty.ci = c(1, 2),                                  # Set the linetypes
  clip = c(0.66, 6.07),                               # Cut off the x-axis
  vertices = TRUE,
  ci.vertices = TRUE,
  ci.vertices.height = 0.15,
  boxsize = .5,
  graphwidth = unit(2, "inches"),   # Set graphwidth
  colgap = unit(3, "mm"),
  line.margin = .1, # Add this to avoid crowding
  xlog = TRUE,      # Show x-axis on log scale
  
  # Set the estimate and the confidence intervals
  mean = estimate,
  lower = conf.low,
  upper = conf.high,
  
  # Set alignment of the column values
  align = "llll",
  # hrzl_lines = list("7" = gpar(lty = 1)),
  is.summary = reference_row,
  # title = "*ISCO occupation group is adjusted for but not displayed here",
  xlab = "Odds ratio (with 95% CI)",
  
  # Set the tick marks on the x-axis as needed
  xticks = c(           
    log(0.666), log(1), log(1.5), log(2)
    , log(3), log(4), log(6)
  ),
  xticks.digits = 1,
  grid = structure(c(1), 
                   gp=gpar(lty=1, lwd=1, col = "#999999"))
  # Optional legend box formatting and position
  , legend_args = fpLegend(
    gp = gpar(col = "transparent", fill = "transparent"),
    padding = unit(1, "mm"),
    pos = list(x = 0.5, y = 0.88)
  )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 1) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  # Add lines for each group
  fp_add_lines("#999999") %>%
  # Name the headers
  fp_add_header(
    full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_center(),
    full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_center(),
    n_perc_uv = c("N", "(% cases)  ") %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)

## Print the plot ####
emf(file = here("output", "publication figures", "sensitivity figures", "sex_severe exhaustion.emf"),
    width = 15.5, height = 2.5, bg = "white")  # full is width = 15.5, height = 8.5
## Combine the two figures
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = c(1.6,1)))) # set 1.6 for full
pushViewport(viewport(layout.pos.col = 1))
plot(plot_male_outcome_1)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
plot(plot_female_outcome_1)
popViewport(2)
dev.off()




### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Outcome 2_Diagnosed burnout ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###
plot_data_outcome2 <- vector("list", length = length(sex_choose))
for (i in 1:length(sex_choose)) {
  sex_set <- dat %>% filter(sex_en == sex_choose[i])
  ## Create your univariate logistic regression models ####
  uv_model_1 <- sex_set %>% 
    select(all_of(outcomes[2]), all_of(exposure), all_of(covariates)) %>% 
    tbl_uvregression(
      method = glm,           # Set the family
      exponentiate = TRUE,
      y = outcomes[2],
      method.args = list(family = binomial)
    )
  # uv_model_1
  
  ## Create multivariable logistic regression model ####
  # Create model
  mv_model_1 <- c(exposure, covariates) %>%     ## begin with vector of exposure and covariate column names
    str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
    str_c(outcomes[2]," ~ ", .) %>%        ## combine the names of variables of interest with outcome_1 in formula style
    glm(family = "binomial",                  ## define the family as binomial for logistic regression
        data = sex_set) %>%                         ## define your dataset
    tbl_regression(exponentiate = TRUE)
  # mv_model_1
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  ## Extract estimates and CIs from the models ####
  ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  ### UV model estimates ####
  uv_estimates_1 <- uv_model_1$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    select(clean_name, var_type,  reference_row, row_type, N, 
           label, n_obs, n_event, estimate, conf.low, 
           conf.high, ci, p.value) %>%
    filter(row_type == "level") %>% 
    mutate(estimate = round(estimate,2),
           model = "Unadjusted",
           new_label = case_when(         # Create a new label to take into account the reference lines
             reference_row == TRUE ~ paste0(clean_name, " (ref: ", label, ")"),
             .default = paste0("      ",label)
           ),
           is.summary = reference_row,
           N_uv = if_else(reference_row == TRUE, N, NA), # Get N from the UV models
           N_mv = NA,   # same for MV but will be filled in later steps
           n_comb_uv = paste0(n_obs, " (",n_event,")"),   # combine UV N and N event into a single variable
           n_comb_mv = NA,  # Same for mv, but it's empty here and will be filled in later steps
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
             n_event == 0 ~ "NA",
             is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
             reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
           ),
           full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
    )
  
  
  ### MV Model estimates ####
  mv_estimates_1 <- mv_model_1$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    select(clean_name, var_type,  reference_row, row_type, N, 
           label, n_obs, n_event, estimate, conf.low, 
           conf.high, ci, p.value) %>% 
    filter(row_type == "level")%>% 
    mutate(estimate = round(estimate,2),
           model = "Adjusted",
           new_label = case_when(
             reference_row == TRUE ~ paste0(clean_name, " (ref: ", label, ")"),
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
             n_event == 0 ~ "NA",
             is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
             reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
           )
    )
  
  
  ## fill in the empty rows for the two tables' full estimates ####
  # (they need to be identical for the combined forest plot)
  uv_estimates_1 <- uv_estimates_1 %>% mutate(full_estimate_mv = mv_estimates_1$full_estimate_mv
                                              , n_comb_mv = mv_estimates_1$n_comb_mv
                                              , N_mv = mv_estimates_1$N_mv
                                              , n_perc_mv = mv_estimates_1$n_perc_mv
  )
  mv_estimates_1 <- mv_estimates_1 %>% mutate(full_estimate_uv = uv_estimates_1$full_estimate_uv
                                              , n_comb_uv = uv_estimates_1$n_comb_uv
                                              , N_uv = uv_estimates_1$N_uv
                                              , n_perc_uv = uv_estimates_1$n_perc_uv
  )
  
  # Row bind the two tables to make a final combined table
  combi_1 <- rbind(uv_estimates_1, mv_estimates_1)
  # Optional remove p-value symbols if you don't need them
  combi_1 <- combi_1 %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                                full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))
  
  # Store data for each of the outcomes in a list
  plot_data_outcome2[i] <- list(combi_1)
  
}




## Plot Male ####

plot_male_outcome_2 <- plot_data_outcome2[[1]] %>% group_by(model) %>%
  filter(clean_name == "Teleworking group") %>% # Can restrict figure to only some of the variables
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.66, 6.07),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             graphwidth = unit(2, "inches"),   # Set graphwidth
             colgap = unit(3, "mm"),
             line.margin = .1, # Add this to avoid crowding
             xlog = TRUE,      # Show x-axis on log scale
             
             # Set the estimate and the confidence intervals
             mean = estimate,
             lower = conf.low,
             upper = conf.high,
             
             # Set alignment of the column values
             align = "llll",
             # hrzl_lines = list("7" = gpar(lty = 1)),
             is.summary = reference_row,
             # title = "*ISCO occupation group is adjusted for but not displayed here",
             xlab = "Odds ratio (with 95% CI)",
             
             # Set the tick marks on the x-axis as needed
             xticks = c(           
              log(0.666), log(1), log(1.5), log(2)
               , log(3), log(4), log(6)
             ),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             # Optional legend box formatting and position
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.88)
             )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 5) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  # Add lines for each group
  fp_add_lines("#999999") %>%
  # Name the headers
  fp_add_header(new_label = c("", ""),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_center(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_center(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)

## Plot Female ####
plot_female_outcome_2 <- plot_data_outcome2[[2]] %>% group_by(model) %>%
  filter(clean_name == "Teleworking group") %>% # Can restrict figure to only some of the variables
  forestplot(labeltext = c(
    full_estimate_uv, full_estimate_mv, n_perc_uv
  ),
  fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
  lty.ci = c(1, 2),                                  # Set the linetypes
  clip = c(0.66, 6.07),                               # Cut off the x-axis
  vertices = TRUE,
  ci.vertices = TRUE,
  ci.vertices.height = 0.15,
  boxsize = .5,
  graphwidth = unit(2, "inches"),   # Set graphwidth
  colgap = unit(3, "mm"),
  line.margin = .1, # Add this to avoid crowding
  xlog = TRUE,      # Show x-axis on log scale
  
  # Set the estimate and the confidence intervals
  mean = estimate,
  lower = conf.low,
  upper = conf.high,
  
  # Set alignment of the column values
  align = "llll",
  # hrzl_lines = list("7" = gpar(lty = 1)),
  is.summary = reference_row,
  # title = "*ISCO occupation group is adjusted for but not displayed here",
  xlab = "Odds ratio (with 95% CI)",
  
  # Set the tick marks on the x-axis as needed
  xticks = c(           
    log(0.666), log(1), log(1.5), log(2)
    , log(3), log(4), log(6)
  ),
  xticks.digits = 1,
  grid = structure(c(1), 
                   gp=gpar(lty=1, lwd=1, col = "#999999"))
  # Optional legend box formatting and position
  , legend_args = fpLegend(
    gp = gpar(col = "transparent", fill = "transparent"),
    padding = unit(1, "mm"),
    pos = list(x = 0.5, y = 0.88)
  )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 1) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               # Font settings
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  # Add lines for each group
  fp_add_lines("#999999") %>%
  # Name the headers
  fp_add_header(
    full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_center(),
    full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_center(),
    n_perc_uv = c("N", "(% cases)  ") %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)

## Print the plot ####
emf(file = here("output", "publication figures", "sensitivity figures", "sex_diagnosed burnout.emf"),
    width = 15.5, height = 2.5, bg = "white")  # full is width = 15.5, height = 8.5
## Combine the two figures
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = c(1.6,1)))) # set 1.6 for full
pushViewport(viewport(layout.pos.col = 1))
plot(plot_male_outcome_2)
popViewport()
pushViewport(viewport(layout.pos.col = 2))
plot(plot_female_outcome_2)
popViewport(2)
dev.off()

