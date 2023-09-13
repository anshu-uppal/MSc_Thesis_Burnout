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
# Relevel to show "Yes, no change" as reference level?
dat <- dat %>% mutate(wfh_exposure = factor(wfh_exposure, 
                                            levels = c("Yes, no change", "Yes, increase", "Yes, decrease", "Never", "Not possible"))
                      )
# Remove SERO-CoV-WORK
# dat <- dat %>% filter(serocovwork_exclusive == FALSE)

# Stratify by sex
# dat <- dat %>% filter(sex_en == "Female")


# Define outcome of interest
outcome <- "decrease_binary"
# outcome <- "not_possible"
# outcome <- "wfh_binary"

# define main exposure and other explanatory variables of interest - can be used for linear and logistic regressions
mcovariates <- c(
  # "burnout_interp_dich30", 
                 "age_cat",
                 "sex_en",
                 "education_rec_en",
                 "hh_income_cat_en",
                 "finance_situation", 
                 # "contract_cdi",  
                 "percent_change_cat", 
                 # "years_of_service_en",
                 "supervision_short", 
                 "hh_livewith_rec_en", 
                 "num_young_children_dich", 
                 "overcrowded", 
                 "noisy", 
                 "quiet_room", 
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
    .default = NA),
  wfh_binary = case_when(
    wfh_dich == "Yes" ~ 1,
    wfh_dich == "No" ~ 0,
    .default = NA))

# Univariate
uv_decreased_log <- a %>%
  select(
    all_of(outcome),
    all_of(mcovariates)) %>%
  tbl_uvregression(
    method = glm,
    y = outcome,
    method.args = list(family = binomial),
    exponentiate = TRUE
  )

# Automatic inclusion of pre-defined explanatory variables for multivariable
# Create model
m2 <- mcovariates %>%                      ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%                 ## combine all names of the variables of interest separated by a plus
  str_c(outcome," ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",                  ## define the family as binomial for logistic regression
      data = a)                          ## define your dataset

# Organize outputs into a table
mv_decreased_log <- tbl_regression(m2, exponentiate = TRUE)


## Forest plots ####

## get data from the regression tables

## Univariate data ("unadjusted")
uv_decreased <- uv_decreased_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
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
mv_decreased <- mv_decreased_log$table_body %>% select(variable, var_type,  reference_row, row_type, N, 
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
uv_decreased <- uv_decreased %>% mutate(full_estimate_mv = mv_decreased$full_estimate_mv
                            , n_comb_mv = mv_decreased$n_comb_mv
                            , N_mv = mv_decreased$N_mv
                            , n_perc_mv = mv_decreased$n_perc_mv
)
mv_decreased <- mv_decreased %>% mutate(full_estimate_uv = uv_decreased$full_estimate_uv
                            , n_comb_uv = uv_decreased$n_comb_uv
                            , N_uv = uv_decreased$N_uv
                            , n_perc_uv = uv_decreased$n_perc_uv
)

# Row bind the two tables 
combi_decreased <- rbind(uv_decreased, mv_decreased)
combi_decreased <- combi_decreased %>% mutate(full_estimate_uv = str_remove_all(full_estimate_uv, "[*]"),
                          full_estimate_mv = str_remove_all(full_estimate_mv, "[*]"))


## Final forest plot with both values in one figure ####
decreased_plot <- combi_decreased %>% group_by(model) %>%
  forestplot(labeltext = c(new_label, 
                           # n_comb_uv,  
                           n_perc_uv,
                           full_estimate_uv, full_estimate_mv),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),  # Set shapes of the symbols
             lty.ci = c(1, 2),                                  # Set the linetypes
             clip = c(0.25, 2),                               # Cut off the x-axis
             vertices = TRUE,
             ci.vertices = TRUE,
             ci.vertices.height = 0.15,
             boxsize = .5,
             # graphwidth = unit(2, "inches"),
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
             xlab = "OR (odds of decreased teleworking relative to reference group)",
             xticks = c(
               # log(0.15),
               log(0.25), 
               log(0.33), log(0.5), log(0.666), log(1), log(1.5), log(2)
               # , log(3)
               # , log(4)
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
  fp_decorate_graph(graph.pos = 3) %>% 
  # fp_add_lines(h_3 = gpar(lty = 2)) %>%
  fp_set_style(box = c("#4271bd", "#990011FF"),
               line = c("#4271bd", "#990011FF"),
               txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 0.9),
                                label = gpar(cex = 0.9),
                                ticks = gpar(cex = 0.9),
                                xlab = gpar(cex = 1)
               )
  ) %>%
  fp_add_lines("#999999") %>%
  fp_add_header(new_label = c("", ""),
                n_perc_uv = c("N", "(% cases)  ") %>% fp_align_left(),
                full_estimate_uv = c("Unadjusted", "OR (95% CI)") %>% fp_align_left(),
                full_estimate_mv = c("Adjusted", "OR (95% CI)") %>% fp_align_left()
  ) %>%
  fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE) ; decreased_plot

emf(file = "decreased_teleworking.emf",width = 9, height = 8, bg = "white") ; decreased_plot ; dev.off() 
