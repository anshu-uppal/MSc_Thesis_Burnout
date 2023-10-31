
#Table combined with p-value symbols

pacman::p_load(
  tidyverse,
  gtsummary,
  forestplot,
  here,
  forcats,
  data.table,
  gt,
  devEMF # print figures into a vectorized format
)

# Section that needs your input -------------------------------------------

# update dat to use your dataset
dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))

# Update outcome levels for easier readability
dat <- dat %>% mutate(
  burn_out = factor(case_when(
    burn_out == 1 ~ "Yes",
    burn_out == 0 ~ "No",
    .default = NA), levels = c("No", "Yes")),
  burnout_interp_dich30 = factor(case_when(
    burnout_interp_dich30 == "Severe" ~ "Yes",
    burnout_interp_dich30 == "Not Severe" ~ "No",
    .default = NA), levels = c("No", "Yes"))
  )


# Enter the variable for your outcome
outcomes <- c("burnout_score","burnout_interp_dich30", "burn_out")
# Set the family that will be used in the glm models
outcomes_type <- c("gaussian", "binomial", "binomial")
outcomes_table <- tibble(outcomes, outcomes_type)

# Enter the variables for your covariates 
covariates <- c(
  "wfh_exposure"
  , "age_cat",
  # "age",
  "sex_en",
  "education_rec_en",
  "hh_income_cat_en",
  "finance_situation",
  "ethn_dich",
  # "country_residence",
  "hh_livewith_rec_en",
  "num_young_children_dich",
  "overcrowded",
  "noisy",
  "quiet_room",
  "health_general_dich",
  "mental_state_dich",
  "chronic_disease_en",
  # "smoking_rec_en", 
  # "alcohol_en",
  "wfh",
  "percent_change_cat",
  "supervision_short",
  "karasek_social_support_cat",
  "aggression_colleagues",
  "fear_losing_job_dich",
  "work_overtime_dich"
)

# Enter a "clean" name for your exposure and covariates
# (covariates need to be in the same order!)
# Add a * to denote that the variable was used in the full models
clean_name <- c(
  "Teleworking group"
  , "*Age group"
  # , "Age in years, median (IQR)"
  , "*Sex"
  , "*Education"
  , "*Household income"
  , "*Financial security"
  , "Ethnicity"
  # , "Country of residence"
  , "*Living arrangement"
  , "*Has young children"
  , "*Overcrowded household"
  , "*Noise level at home"
  , "*Access to a quiet room"
  , "*Self-reported general health"
  , "Self-reported mental state"
  , "Chronic condition"
  # , "Smoker status"
  # , "Alcohol consumption"
  , "Teleworking frequency"
  , "*Change in working hours"
  , "*Management duties"
  , "Work social support"
  , "Aggression from colleagues"
  , "Fears losing job"
  , "Works overtime"
)

in_regression = case_when(str_detect(clean_name, "[*]")~ TRUE,
                          .default = FALSE)
clean_name <- str_remove(clean_name,"[*]")


### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Section that does NOT necessarily need your input ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Make a table of the variables and their "clean" names
variable <- c(covariates)
clean_labels <- tibble(variable, clean_name, in_regression)
rm(variable, clean_name) # remove these to avoid any confusion later

# Open the clean_labels table to double-check!!

### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Summary estimates binary outcomes ####
### ### ### ### ### ### ### ### ### ### ### ### ### ###
table_data <- vector("list", length = length(outcomes))
for (i in 2:length(outcomes)) {
  table_outcomes <- dat %>% 
    select(all_of(c(covariates, outcomes[i]))) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
    # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>% 
    droplevels() %>%
    tbl_summary(
      by = outcomes[i],
      percent = "row",                         # calculate percent row-wise
      type = all_categorical() ~ "categorical",                 # force all categorical levels to display
      missing_text = "Missing"                                  # how missing values should display
    ) %>% 
    add_p(
      test = all_categorical() ~ "chisq.test",
      pvalue_fun = function(x) style_pvalue(x, digits = 3),
      test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
      );table_outcomes

  table_estimates <- table_outcomes
  table_estimates$table_body <- table_estimates$table_body %>% # Extract the estimates table
    left_join(clean_labels) %>%  # Clean the variable names
    filter(row_type != "missing") %>% 
    mutate(
      label = if_else(row_type == "label", clean_name, label),
      outcome = outcomes[i],
      # p.value_2 = round(p.value, 3),
      # p.value_2 = if_else(p.value_2 == 0.000, "<0.001", paste0(p.value_2)),
      p.sig = case_when(                         # show p-values symbolically
        is.na(p.value) ~ NA,
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value <= 0.05 ~ "*",
        .default = ""
      ),
      in_regression = case_when(in_regression & row_type == "label" ~ TRUE,
                                .default = FALSE)
      # , stat_2 = if_else(is.na(stat_2), p.sig, stat_2)
      )
  
  # Here's an optional section to remove the binary terms, but I think it doesn't apply for these tables, as we're interested in comparing the outcomes in the different groups, even if binary
  
  # clean_table <- table_estimates$table_body %>% 
  # mutate(delete = case_when(
  #   variable == "num_young_children_dich" & label == "No young children" ~ 1,
  #   variable == "overcrowded" & label == "Not overcrowded" ~ 1,
  #   variable == "quiet_room" & label == "Yes" ~ 1,
  #   variable == "health_general_dich" & label == "Good" ~ 1,
  #   variable == "mental_state_dich" & label == "Good" ~ 1,
  #   variable == "chronic_disease_en" & label == "No" ~ 1,
  #   variable == "aggression_colleagues" & label == "No" ~ 1,
  #   variable == "fear_losing_job_dich" & label == "No" ~ 1,
  #   .default = NA
  # )) %>% filter(is.na(delete))
  # 
  # binary_keep <- clean_table %>% 
  #   filter((row_type == "level" & 
  #             variable %in% c(
  #               "num_young_children_dich",
  #               "overcrowded",
  #               "quiet_room",
  #               "chronic_disease_en",
  #               "aggression_colleagues",
  #               "fear_losing_job_dich"
  #             ))) %>% select(variable, stat_1:stat_2) %>% rename("stat_1x" = "stat_1", "stat_2x" = "stat_2")
  # 
  # clean_table <- clean_table %>% 
  #   filter(!(row_type == "level" & 
  #              variable %in% c(
  #                "num_young_children_dich",
  #                "overcrowded",
  #                "quiet_room",
  #                "chronic_disease_en",
  #                "aggression_colleagues",
  #                "fear_losing_job_dich"
  #              ))) %>% 
  #   left_join(binary_keep) %>% 
  #   mutate(stat_1 = if_else(is.na(stat_1x), stat_1, stat_1x),
  #          stat_2 = if_else(is.na(stat_2x), stat_2, stat_2x)) %>% 
  #   select(-c(stat_1x:stat_2x, delete))
  # 
  # table_estimates$table_body <- clean_table ; rm(clean_table)
  
  # Store data for each of the outcomes in a list
  table_estimates <- table_estimates %>% 
    # modify_column_hide(columns = p.value) %>% 
    modify_header(all_stat_cols() ~ "**{level}**\nN = {n}")
  table_data[i] <- list(table_estimates)
}

# MBI Score ####
# MBI Score outcomes ####
score_outcomes <- vector("list", length = length(covariates))

for (i in 1:length(covariates)) {
  score_outcome <- dat %>% 
    select(all_of(covariates), burnout_score) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    # mutate_if(is.factor, ~fct_na_value_to_level(., level = "Missing")) %>%
    # mutate_all(~fct_na_value_to_level(., level = "Missing")) %>%
    # droplevels() %>%
    # drop_na() %>% 
    group_by(get(covariates[i])) %>% 
    # group_by(wfh_exposure) %>% 
    summarise(
      median_MBI_score = round(median(burnout_score, na.rm = TRUE),1)
      , iqr_MBI_score = paste0(format(round(quantile(burnout_score, na.rm = TRUE, 1/4),1), nsmall = 1),
                               "-",
                               format(round(quantile(burnout_score, na.rm = TRUE, 3/4),1), nsmall = 1)
      )
      # , n = n()
    ) %>% 
    rename("label" = "get(covariates[i])") %>% 
    mutate(
      variable = paste(covariates[i])
      , med_iqr = paste0(median_MBI_score, " (", iqr_MBI_score, ")")
      # , reference = if_else(row_number() == 1, TRUE, FALSE)
    )
  
  score_outcomes[i] <- list(score_outcome)
} ; score_outcomes <- rbindlist(score_outcomes)

# UV model to get the global p values
uv_model_mbi <- dat %>% 
  select(burnout_score, all_of(covariates)) %>% 
  tbl_uvregression(
    method = glm,           # Set the family
    exponentiate = FALSE,
    y = burnout_score,
    method.args = list(family = "gaussian")
  ) %>% 
  add_global_p(
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) ; uv_model_mbi

uv_model <- uv_model_mbi
uv_model$table_body <- uv_model$table_body %>% 
  left_join(clean_labels) %>%  # Clean the variable names
  left_join(score_outcomes, by = c("variable", "label")) %>%
  relocate(med_iqr, .after = n_obs) %>% 
  filter(row_type != "missing") %>% 
  mutate(
    label_2 = if_else(row_type == "label", paste0(clean_name, " (n=",stat_n,")"), label),
    label = if_else(row_type == "label", clean_name, label),
    outcome = outcomes[i],
    p.sig = case_when(                         # show p-values symbolically
      is.na(p.value) ~ NA,
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      .default = ""
    ),
    p.value_2 = if_else(p.value < 0.001, "<0.001", paste0(round(p.value,3))),
    in_regression = case_when(in_regression & row_type == "label" ~ TRUE,
                              .default = FALSE)
    # , med_iqr = if_else(is.na(med_iqr), p.sig, med_iqr)
    ) %>% 
  relocate(label_2, .after = label)
  
# Store data for each of the outcomes in a list
uv_model_final <- uv_model %>% 
  modify_column_hide(columns = c(label, p.value,ci, estimate, stat_n)) %>% 
  modify_column_unhide(columns = c(label_2,med_iqr, n_obs, p.value_2)) %>% 
  modify_header(
    label_2 = "**Characteristic**",
    n_obs = "**N**",
    med_iqr = "**Score**\nmedian (IQR)",
    p.value_2 = "**p-value**"
  ) ; uv_model_final %>% as_flex_table()

# Combine the tables ####
table_combined <-
  tbl_merge(
    tbls = list(uv_model_final, 
                table_data[[2]],
                table_data[[3]])
    , tab_spanner = c(
      paste0("**EE-MBI        **"),
      "**Severe EE-MBI**",
      "**Diagnosed burnout**"
      )
  ) ; table_combined %>% as_flex_table()

# ID the group labels and for later coloring
a <- table_combined$table_body %>%  
  rowid_to_column() %>% 
  filter(row_type == "label")
groupnames <- (a$rowid)

# ID the variables that are used in the full models
c <- table_combined$table_body %>%  # Crude way to ID the group labels and for later coloring
  rowid_to_column() %>% 
  filter(in_regression_1)
model_vars <- (c$rowid)

final_table <- table_combined %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  footnote(i = 2, j = 4, 
           value = as_paragraph(" Kruskal-Wallis rank sum test"),
           ref_symbols = c("a"),
           part = "header") %>% 
  footnote(i = 2, j = c(5,6,8,9), 
           value = as_paragraph(" n (%), may not add up to 100% due to rounding"),
           ref_symbols = c("b"),
           part = "header") %>% 
  footnote(i = 2, j = c(7,10), 
           value = as_paragraph(" Pearson's Chi-squared test"),
           ref_symbols = c("c"),
           part = "header") %>% 
  # add_footer_lines("* p < 0.05 , ** p < 0.01, *** p < 0.001") %>% 
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(align = "left", part = "body", j = 1) %>% 
  align(align = "right", part = "header", j = 1) %>% 
  vline(j = c(2,4,7), border = fp_border_default(color = "grey")) %>%
  bg(i = groupnames, part = "body", bg = "#EFEFEF") %>% 
  bold(i = groupnames, j = 1, bold = TRUE, part = "body") %>%
  # height_all(height = 0, part = "body") %>%
  line_spacing(space = 0.3, part = "body") %>%
  # padding(padding.top = 4, part = "all") %>% 
  # padding(padding.bottom = 5, part = "all") %>% 
  fontsize(size = 7, part = "all") %>% 
  # autofit() %>% 
  # paginate(group = "label") %>% 
  width(j = 1, width = 4.8, unit = "cm") %>%
  width(j = 2, width = 1.2, unit = "cm") %>%
  width(j = 3, width = 2.2, unit = "cm") %>%
  width(j = c(4,7,10), width = 1.2, unit = "cm") %>%
  width(j = c(5,8), width = 1.8, unit = "cm") %>%
  width(j = c(6,9), width = 1.6, unit = "cm") %>%
  valign(valign = "bottom", part = "body") ; final_table

# Save as docx ####
save_as_docx(
  "Table 2: Participant characteristics according to outcome" = final_table, path = here("output", "publication figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Characteristics_outcomes.docx")))
