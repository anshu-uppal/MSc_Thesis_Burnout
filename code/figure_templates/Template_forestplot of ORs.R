# Here's a template for anyone that would like to display unadjusted and 
# adjusted odds ratios in a forest plot
# ! This won't work if you're including interaction terms !
# (but you can probably adapt it)

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

# Make sure your outcome is binary (1/0)
dat <- dat %>% mutate(burnout_interp_dich30 = case_when(
  burnout_interp_dich30 == "Severe" ~ 1,
  burnout_interp_dich30 == "Not Severe" ~ 0,
  .default = NA
  ))

# Enter the variable for your outcome
outcome <- "burnout_interp_dich30"

# Enter the variable for your exposure
exposure <- "wfh_exposure"

# Enter the variables for your covariates
covariates <- c(
  "age_cat"
  , "sex_en"
  , "education_rec_en"
  , "hh_income_cat_en"
  , "finance_situation"
  , "percent_change_cat"
  )

# Enter a "clean" name for your exposure and covariates
# (covariates need to be in the same order!)
clean_name <- c(
  "Teleworking group"       #Exposure
  , "Age group, years"    # Rest of covariates
  , "Sex"
  , "Education"
  , "Household income"
  , "Financial security"
  , "Change in working hours"
)


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Make a table of the variables and their "clean" names
variable <- c(exposure, covariates) 
clean_labels <- tibble(variable, clean_name) ; clean_labels # verify that everything matches up
rm(variable, clean_name) # remove these to avoid any confusion later

# Create your univariate logistic regression model ####
uv_model <- dat %>% 
  select(all_of(outcome), all_of(exposure), all_of(covariates)) %>% 
  tbl_uvregression(
    method = glm,           # Set the family
    exponentiate = TRUE,
    y = outcome,
    method.args = list(family = binomial)
    ) ; uv_model


# Create multivariable logistic regression model ####
# Create model
mv_model <- c(exposure, covariates) %>%     ## begin with vector of exposure and covariate column names
  str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
  str_c(outcome," ~ ", .) %>%        ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",                  ## define the family as binomial for logistic regression
      data = dat) %>%                         ## define your dataset
  tbl_regression(exponentiate = TRUE) ; mv_model
  

### ### ### ### ### ### ### ### ### ### ### ### ###
# Extract estimates and CIs from the models ####
### ### ### ### ### ### ### ### ### ### ### ### ###

### UV model estimates ####
uv_estimates <- uv_model$table_body %>% # Extract the estimates table
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
           is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
         ),
         full_estimate_mv = NA                    # create empty mv variable to be filled later (vice versa for the mv table)
  )
  

### MV Model estimates ####
mv_estimates <- mv_model$table_body %>% # Extract the estimates table
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
           is.na(p.sig) & reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")"),
           reference_row == FALSE ~ paste0(round(estimate,2), " (", ci,")",p.sig),
         )
  )


# fill in the empty rows for the two tables' full estimates ####
# (they need to be identical for the combined forest plot)
uv_estimates <- uv_estimates %>% mutate(full_estimate_mv = mv_estimates$full_estimate_mv
                            , n_comb_mv = mv_estimates$n_comb_mv
                            , N_mv = mv_estimates$N_mv
                            , n_perc_mv = mv_estimates$n_perc_mv
)
mv_estimates <- mv_estimates %>% mutate(full_estimate_uv = uv_estimates$full_estimate_uv
                            , n_comb_uv = uv_estimates$n_comb_uv
                            , N_uv = uv_estimates$N_uv
                            , n_perc_uv = uv_estimates$n_perc_uv
)

# Row bind the two tables to make a final combined table
combi <- rbind(uv_estimates, mv_estimates)
# Optional remove p-value symbols if you don't need them
combi <- combi %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                          full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))





## Forest plot with UV and MV estimates combined ####
combined_plot <- combi %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           n_perc_uv, full_estimate_uv,
                           n_perc_mv, full_estimate_mv
                           ),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.45, 4),                               # Cut off the x-axis
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
             # title = "ff",
             xlab = "Odds ratio (with 95% CI)",
             
             # Set the tick marks on the x-axis as needed
             xticks = c(           
               log(0.5), log(0.666), log(1), log(1.5), log(2)
               , log(3), log(4)
                        ),
             xticks.digits = 1,
             grid = structure(c(1), 
                              gp=gpar(lty=1, lwd=1, col = "#999999"))
             # Optional legend box formatting and position
             , legend_args = fpLegend(
               gp = gpar(col = "transparent", fill = "transparent"),
               padding = unit(1, "mm"),
               pos = list(x = 0.5, y = 0.98)
             )
  ) %>% 
  # Choose position of the forestplot
  fp_decorate_graph(graph.pos = 4) %>% 
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
                n_perc_mv = c("N", "(% cases)  ") %>% fp_align_center(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_center()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; combined_plot


# Print image to PDF
pdf(file = "Template_binary outcome.pdf",
    width = 9, height = 7,  # Play around with dimensions
    paper = "a4r",   # Can set the orientation to landscape
    colormodel = "cmyk") ; combined_plot ; dev.off()

emf(file = here("output", "publication figures", "Template_binary outcome.emf"), 
    width = 10.5, height = 8.5, bg = "white") ; combined_plot ; dev.off()
