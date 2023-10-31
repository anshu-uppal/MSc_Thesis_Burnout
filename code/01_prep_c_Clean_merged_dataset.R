# Load packages and dataset ####
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  janitor,      # adding totals and percents to tables
  lubridate     # manipulate date objects
)

# Read in merged dataset (see "Merge code.R" for how this was made)
merged <- tibble(readRDS(here("data", "Generated datasets", "merged_dataset.rds")))
### ### ### ### ### ### ### ### ### ###
### Dataset cleaning and wrangling ####
### ### ### ### ### ### ### ### ### ###

dat_all <- merged %>%
  filter(
    # remove people who were not working in the last 12 months
    !is.na(work_situation.y) 
    ) %>% 
  ## Calculate number of children born in 2012 or later (i.e. 10 and under at the time of santé-travail)
  rowwise() %>%
  mutate(num_young_children = sum(c_across(starts_with("child_age_y")) >= 2012, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ## Continue with cleaning variables
  mutate(
    ## Did the participant also fill in the general health questionnaire of Apr/May 2022?
    # gh_participant = !is.na(date_soumission),
    # Dichotomise salaried vs other
    work_situation.y_dich = if_else(work_situation.y == "En activité salariée",
                                    "En activité salariée", if_else(is.na(work_situation.y), NA, "Other")),
    
    # Convert submission dates to same format
    date_soumission.y = as_date(date_soumission.y),  # Date of Santé-travail questionnaire submission
    date_soumission.x = as_date(date_soumission.x),  # Date of Inclusion questionnaire submissions
    
    # Is the participant exclusively from the SeroCOV-work recruitment?
    serocovwork_exclusive = serocov_work.x == TRUE & serocov_pop.x == FALSE & pop_pilote.x == FALSE & 
      sp3_juin_2021.x == FALSE & sp2_novdec_2020.x == FALSE & sp4_avril_2022.x == FALSE,
    # From the representative surveys?
    serocov_representative = serocov_pop.x | sp3_juin_2021.x | sp2_novdec_2020.x | sp4_avril_2022.x | pop_pilote.x, 
    # Not included in SeroCOV-work?
    not_serocovwork = serocov_representative == TRUE & serocov_work.x == FALSE,
    
    # Survey recruitment source
    pop_source = NA,
    pop_source = case_when(
      serocov_work.x == TRUE & serocov_pop.x == FALSE & 
        sp3_juin_2021.x == FALSE & sp2_novdec_2020.x == FALSE & sp4_avril_2022.x == FALSE ~ "SEROCoV-WORK",
      is.na(pop_source) & serocov_pop.x == TRUE ~ "SP1",
      is.na(pop_source) & sp2_novdec_2020.x == TRUE ~ "SP2",
      is.na(pop_source) & sp3_juin_2021.x == TRUE ~ "SP3",
      is.na(pop_source) & sp4_avril_2022.x == TRUE ~ "SP4",
      is.na(pop_source) & pop_pilote.x == TRUE ~ "Pop_pilote",
      is.na(pop_source) & work_pilote.x == TRUE ~ "work_pilote",
      .default = "Other"
    ),
    
    # Don't recode sex
    sex_en = factor(sex_en, levels = c("Male", "Female", "Other")),
    

    # Age at the time of completing santé-travail quest.
    birthdate = ymd(birthdate),
    age = time_length(date_soumission.y - birthdate, "years"), 
    
    # Split ages into categories, relevel to have age group 30-39 as reference
    age_cat = factor(case_when(     
      age >= 18 & age < 25 ~ "18-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65             ~ "65+"
    )),
    
    # New bmi variable setting anything over 75 to NA
    bmi_new = if_else(bmi>= 75, NA, bmi),
    bmi_4_cat_en = factor(bmi_4_cat_en, levels = c("Normal weight", "Underweight",
                          "Overweight", "Obese")),
    bmi_4_cat_en_new = if_else(bmi>= 75, NA, bmi_4_cat_en),
    # # weight fluctuation over 10% between inclusion and GH questionnaires
    # weight_flux_10 = abs(1-(weight.x/weight.y))>=0.1,

    
    # Shorten and relevel the job_sector labels
    job_sector = str_to_sentence(str_remove(job_sector, "Secteur de la|Secteur de l’|Secteur des")),
    job_sector = str_replace(job_sector, " \\s*\\([^\\)]+\\)", ""),
    job_sector = str_wrap(job_sector, width = 60),  # wrap sector so that it's better displayed
    job_sector = relevel(factor(job_sector), ref = "Administration publique"),
    
    # Translate job sector
    job_sector_en = factor(case_match(job_sector,
                                      "Administration publique" ~ "Public administration"
                                      ,"Activités juridiques, comptabilité, secrétariat" ~ "Legal, accounting, secretarial"
                                      ,"Agriculture, sylviculture, horticulture, entretien des\nespaces verts, élevage, pêche, etc." ~ "Agriculture, forestry, livestock, fishing"
                                      ,"Ambassade, organisation internationale" ~ "Embassy / international organisations"
                                      ,"Arts, spectacle, musée, bibliothèque" ~ "Arts, museums, libraries"
                                      ,"Banques, assurances" ~ "Banking, insurance"
                                      ,"Bureaux d’études, recherche et développement, architecture"  ~ "Design, research and development, architecture"
                                      ,"Commerce" ~ "Trade"
                                      ,"Construction ou de la rénovation" ~ "Construction, renovation"
                                      ,"Enseignement, recherche" ~ "Education, research"
                                      ,"Extraction de matières premières" ~ "Extraction of raw materials"
                                      ,"Hébergement et restauration" ~ "Accommodation and catering"
                                      ,"Immobilier, agences" ~ "Real estate"
                                      ,"Industrie, de la fabrication de biens" ~ "Industry, goods manufacturing"
                                      ,"Information et communication" ~ "Information and communication"
                                      ,"Other" ~ "Other"
                                      ,"Petite enfance" ~ "Early childhood (care, education)"
                                      ,"Production ou de la distribution" ~ "Production or distribution"
                                      ,"Santé, social, médico-social" ~ "Healthcare and social services"
                                      ,"Sécurité, secours" ~ "Security, rescue"
                                      ,"Services à la personne" ~ "Personal services"
                                      ,"Services de conseils" ~ "Consulting"
                                      ,"Services domestiques" ~ "Domestic services"
                                      ,"Transports et de l’entreposage" ~ "Transportation and storage"
                                      ,.default = NA
    )),
    job_sector_en = fct_relevel(job_sector_en, c("Public administration", "Healthcare and social services"), after = 0),
    
    # Relevel occupation label2
    Occupation_label_2 = relevel(factor(Occupation_label_2), ref = "Business and administration professionals"),
    
    # Categorise people as having Swiss or Other nationality
    Swiss_nat = factor(if_else(Swiss_nat == "TRUE", "Swiss", "Other"), 
                       levels = c("Swiss", "Other")),
    
    # Add zeros for people with no children
    children_n = if_else(children=="Non", 0, children_n),
    children_n_dich = factor(case_when(
      children_n <= 3 ~ factor(children_n),
      children_n > 3 ~ "4+",
      .default = factor(children_n))),
    
    # Dichotomise children after 2005
    num_young_children_dich = factor(case_when(
      num_young_children == 0 ~ "No young children",
      num_young_children > 0 ~ "Has young children",
      .default = NA),
      levels = c("No young children", "Has young children")),
    
    # Relevel income
    hh_income_cat_en = factor(case_when(hh_income_cat_en == "Don't know/don't wish to answer" ~ "Don't want to answer",
                                        .default = hh_income_cat_en),
                                        levels = c("High", "Middle", "Low", "Don't want to answer")),
    
    # Recode Financial situation
    finance_situation = factor(case_match(
      finance_situation,
      "Je suis à l’aise, l’argent n’est pas une source d’inquiétude et il m'est facile d’épargner" ~ "Comfortable",
      "Mes revenus permettent de couvrir mes dépenses et de pallier d’éventuels imprévus mineurs" ~ "Can cover expenses",
      "Je dois faire attention à mes dépenses et un imprévu pourrait me mettre en difficulté financière" ~ "Cautious / struggling",
      "Je n’arrive pas à couvrir mes besoins avec mon revenu et j'ai besoin d’un soutien externe pour fonctionner (endettement, crédits, aides financières diverses)" ~ "Cautious / struggling",
      "Je ne souhaite pas répondre" ~ "Don't want to answer"
                                      )
                               ),
    
    # Supervisory role
    supervision_short = factor(case_match(
      supervision,
      "Oui, et c’est (c'était) ma tâche principale" ~ "Main duty",
      "Oui, mais ce n’est (n'était) pas ma tâche principale" ~ "Some duties",
      "Non" ~ "None"),
      levels = c("None", "Some duties", "Main duty")),
    
    # Group 4+ people in a household into one category
    hh_composition = if_else(hh_livewith == "Seul-e", 0, hh_composition),
    hh_composition_cat = factor(case_match(
      hh_composition,
      0 ~ "0",
      1 ~ "1",
      2 ~ "2",
      3 ~ "3",
      c(4,5,6,7) ~ "4+"
    )),
    
    # Measure of overcrowding
    bedroom_n = if_else(bedroom_n == 0, 1, bedroom_n), # recode "0" bedrooms as 1? "0" probably indicates a studio?
    overcrowded = factor(if_else(((hh_composition+1)/bedroom_n) > 2, "Overcrowded", "Not overcrowded"), 
                         levels = c("Not overcrowded", "Overcrowded")
                         ),
    
    # Shorten the "non-concerné-e" bit
    wfh_change = factor(case_when(
      wfh_change == "Non concerné-e (vous n’aviez pas d’emploi avant la pandémie ou vous avez changé d’emploi)" ~ "Non concerné-e",
      .default = wfh_change
                        ),
      levels = c("De manière inchangée", "Plus qu’avant", "Moins qu’avant", "Non concerné-e")),
    
    # recode wfh variable from French to English
    wfh = factor(case_match(
      wfh,
      "Non, jamais" ~ "Never",
      "Non concerné-e (mon activité professionnelle n’est pas réalisable à distance)" ~ "Not possible",
      "Oui, parfois (mais pas régulièrement)" ~ "Occasionally (irregular)",
      "Oui, à temps partiel (au moins une fois par semaine)" ~ "1+ days/week",
      "Oui, à temps complet (tous les jours)" ~ "Every day"),
      levels = c("Never", "Not possible", "Occasionally (irregular)", "1+ days/week", "Every day")
    ),
    
    # # Update wfh "Never" status using the ISCO-08 definitions to account for "teleworkability" of profession
    # wfh_updated = factor(case_when(
    #   wfh == "Never" & physical_interaction == 0 ~ "Not possible",   ## Only recode "Never" to "Not possible", when teleworkability index = 0
    #   .default = wfh),
    #   levels = c("Never", "Not possible", "Occasionally (irregular)", "1+ days/week", "Every day")
    # ),
    
    wfh_updated = factor(case_when(
      wfh == "Never" & physical_interaction == 0 ~ "Not possible",         # Recode when teleworkability index = 0
      # Recode other professions where teleworking is not possible
      wfh == "Never" & Occupation_label_full %in% c(
        "Air traffic controllers", "Ambulance workers","Fire-fighters",
        "Food processing and related trades workers",
        "Medical assistants", "Stock clerks", "Physiotherapists", "Dentists"
      ) ~ "Not possible",
      wfh == "Never" & Occupation_label_full == "Protective services workers" & profession.y != "Fonctionnaire" ~ "Not possible",
      wfh == "Never" & profession.y == "Vendeuse" ~ "Not possible",
      wfh == "Never" & Occupation_label_full == "Life science technicians and related associate professionals" &
        !profession.y %in% c("Assistante diplômée") ~ "Not possible",
      .default = wfh),
      levels = c("Never", "Not possible", "Occasionally (irregular)", "1+ days/week", "Every day")),
    
    # # Use this instead if you don't want to apply the above changes to the wfh variable:
    # wfh_updated = factor(wfh, 
    #                      levels = c("Never", "Not possible", "Occasionally (irregular)", "1+ days/week", "Every day")),

    # Re-categorise wfh_updated 
    wfh_cat = factor(case_match(
      wfh_updated, 
      "Never" ~ "Never",
      "Not possible" ~ "Not possible",
      "Occasionally (irregular)" ~ "Occasionally",
      "1+ days/week" ~ "Occasionally",
      "Every day" ~ "Always"),
      levels = c("Never", "Not possible", "Occasionally", "Always"))
    ,
    
    # Dichotomise wfh_updated
    wfh_dich = factor(case_match(wfh_updated, 
                              "Never" ~ "No",
                              "Not possible" ~ "No",
                              "Occasionally (irregular)" ~ "Yes",
                              "1+ days/week" ~ "Yes",
                              "Every day" ~ "Yes"),
                       levels = c("Yes", "No")
    ),
    # "Trichotomise" wfh_updated
    wfh_trich = factor(case_match(wfh_updated, 
                              "Never" ~ "Never",
                              "Not possible" ~ "Not possible",
                              "Occasionally (irregular)" ~ "Yes",
                              "1+ days/week" ~ "Yes",
                              "Every day" ~ "Yes"),
                      levels = c("Never", "Not possible", "Yes")
    ),
    
    # Make main exposure group for WFH - factorize and adjust order of levels
    wfh_exposure = factor(case_when(
        wfh_trich == "Never" ~ "Never",
        wfh_trich == "Not possible" ~ "Not possible",
        wfh_trich == "Yes" & wfh_change == "Plus qu’avant" ~ "Increase",
        wfh_trich == "Yes" & wfh_change == "De manière inchangée" ~ "No change",
        wfh_trich == "Yes" & wfh_change == "Moins qu’avant" ~ "Decrease",
        wfh_trich == "Yes" & wfh_change == "Non concerné-e" ~ "Not applicable"),
        levels = c("No change", "Decrease", "Increase", "Not possible", "Never", "Not applicable")
        ),
    
    wfh_balance_dich = factor(case_match(wfh_balance,
                                  c("Oui", "Plutôt oui") ~ "Yes",
                                  c("Non", "Plutôt non", "Ne sais pas") ~ "No",
                                  # "Ne sais pas" ~ "Don't know"  # Lump these in with No?
                                  ),
                              levels = c("Yes", 
                                         # "Don't know", 
                                         "No")
      
    ),
    
    wfh_balance_combined = factor(case_when(
        wfh_trich == "Never" ~ "Never",
        wfh_trich == "Not possible" ~ "Not possible",
        wfh_trich == "Yes" & wfh_balance_dich == "Yes" ~ "Yes, and better balance",
        wfh_trich == "Yes" & wfh_balance_dich == "No" ~ "Yes, but not better balance",
        wfh_trich == "Yes" & wfh_balance_dich == "Don't know" ~ "Yes, but don't know",
      )),
    
    # Relevel and dichotomise the "Feeling exhausted at work" question
    feeling_exhausted = factor(case_match(feeling_exhausted,
                                      "Oui" ~ "Yes",
                                      "Plutôt oui" ~ "Rather yes",
                                      "Plutôt non" ~ "Rather no",
                                      "Non" ~ "No"),
                               levels = c("No", "Rather no", "Rather yes", "Yes")),
    feeling_exhausted_dich = factor(case_match(feeling_exhausted,     
                                    "Yes" ~ "Yes",
                                    "Rather yes" ~ "Yes",
                                    "Rather no" ~ "No",
                                    "No" ~ "No"),
                                    levels = c("No", "Yes")),
    
    # Convert clinical burnout from Yes/No to 1/0
    burn_out = case_match(burn_out,
      "Oui" ~ 1,
      "Non" ~ 0
    ),
    
    # Include a square root transformation of the continuous burnout_score
    burnout_score_sqrt1 = sqrt(burnout_score+1),
    # recode EE MBI interpreation into English
     burnout_interp = case_match(burnout_interp,                     
                            "Épuisement modéré" ~ "Moderate",
                            "Épuisement faible" ~ "Mild",
                            "Épuisement fort" ~ "Severe"
    ),
    
    # Create dichotomised MBI with score >= 27 as severe
    burnout_interp_dich27 = if_else(burnout_score >= 27, "Severe",
                                  if_else(burnout_score<27, "Not Severe", NA)),
    
    # Create dichotomised MBI with score >= 30 as severe
    burnout_interp_dich30 = if_else(burnout_score >= 30, "Severe",
                                  if_else(burnout_score < 30, "Not Severe", NA)),
    
    # Harmonized definition for combining EE-MBI >= 27 & feeling exhausted = yes
    burnout_harmonized = case_when(
      is.na(burnout_interp_dich30) ~ NA,
      burnout_interp_dich30 == "Severe" & feeling_exhausted_dich == "Yes" ~ 1,
      .default = 0
    ),
    
    ## Relationship status recode into English
    relation = case_match(relation,
                      "En couple, non marié-e" ~ "In relationship (unmarried)",
                      "Marié-e ou en partenariat enregistré" ~ "Married / civil partnership",
                      "Divorcé-e ou séparé-e" ~ "Divorced / separated",
                      "Célibataire" ~ "Single",
                      "Veuf-ve" ~ "Widowed",
                      "Other" ~ "Other"
    ),
    
    ## Living situation --> not considering age of children
    hh_livewith_rec_en = factor(case_match(hh_livewith,
                                "En couple, sans enfant" ~ "Couple without children",
                                "En cohabitation, avec d'autres personnes (famille, amis, collocataires, etc.)" ~ "With other adults",
                                "Seul-e" ~ "Alone",
                                "En couple, avec vos enfants ou ceux de votre conjoint-e" ~ "Couple with children",
                                "En parent seul, avec vos enfants" ~ "Single with children"),
                                levels = c("Couple without children", "Couple with children", 
                                           "Single with children", "With other adults", "Alone")
    ),
    ## Living situation taking into account age of children
    hh_livewith_rec_en_children = factor(case_when(
      hh_livewith_rec_en == "Couple with children" & num_young_children_dich == "Has young children" ~ "Couple with young children",
      hh_livewith_rec_en == "Couple with children" & num_young_children_dich == "No young children" ~ "Couple with children aged over 5",
      hh_livewith_rec_en == "Single with children" & num_young_children_dich == "Has young children" ~ "Single with young children",
      hh_livewith_rec_en == "Single with children" & num_young_children_dich == "No young children" ~ "Single with children aged over 5",
      .default = hh_livewith_rec_en),
      levels = c("Couple without children", "Couple with young children", "Couple with children aged over 5", 
                 "Single with young children", "Single with children aged over 5", "With other adults", "Alone")),
    
    # Housing condition has similar responses from previous surveys that need to be combined
    housing_condition = case_match(housing_condition,
                               "Appartement avec un accès à l’extérieur (balcon, terrasse, cour ou jardin)" ~ "Appartement ou studio avec un accès à l’extérieur (balcon, terrasse, cour ou jardin)",
                               "Appartement sans accès à l’extérieur" ~ "Appartement ou studio sans accès à l’extérieur"
    ),
    
    # Health_general --> dichotomise
    health_general_dich = case_match(health_general,
                                 c("Très bonne", "Bonne") ~ "Good",
                                 c("Moyenne", "Mauvaise", "Très mauvaise") ~ "Not good"
                                 ),
    
    # mental_state --> dochotomise
    mental_state_dich = case_match(mental_state,
                                   c("Très bon", "Bon") ~ "Good",
                                   c("Moyen", "Mauvais", "Très mauvais") ~ "Not good"
                                   ),
    
    # Dichotomise workplace size
    workplace_size_dich = case_match(workplace_size,
                                     c("Moins de 5 salarié-es", "De 5 à 9 salarié-es", "De 10 salarié-es à 49 salarié-es", 
                                       "De 50 salarié-es à 99 salarié-es", "De 100 salarié-es à 499 salarié-es") ~ "Under 500 employees",
                                     c("500 salarié-es à 999 salarié-es", "1000 et plus") ~ "Over 500 employees"
                                     ),
    
    # Recode sedentary_work into English
    sedentary_work = case_match(sedentary_work,
                            "Principalement sédentaire (ex : vous travaillez principalement sur ordinateur / écran, travail de bureau ou administratif)" ~ "Mainly sedentary",
                            "Principalement avec un effort physique (ex : votre travail implique d’être très souvent physiquement actif, en mouvement)" ~ "Mainly physical",
                            "Sédentaire et actif à la fois" ~ "Mix of sedentary and physical activity",
                            "Other" ~ "Other"
                            ),
    
    # Dichotomize ethnicity to White European / Other
    ethn_dich = factor(case_when(
      ethnicity_dct %in% c("Européenne - Caucasienne") ~ "White European",
      ethnicity_dct %in% c("Other", "Subcontinent indien (Inde, Pakistan, Bangladesh, Népal, Bhoutan, Sri Lanka, Maldives)",
                             "Africaine (Afrique sub-saharienne ou afro-américain-e)", "Latino-américain-e") ~ "Other",
      .default = NA),
      levels = c("White European", "Other")),
    # Include seated time in hours
    seated_time = (seated_hrs*60 + seated_mins)/60,
    # Standardize seated time to number of hours worked per week
    seated_time_standardized = seated_time / hours_week,
  
    # fear losing job
    fear_losing_job = factor(fear_losing_job, levels = 
                               c("Non, pas du tout", "Non, pas tellement", 
                                 "Oui, assez ou passablement", "Oui, beaucoup")
                               ),
    fear_losing_job_dich = case_match(fear_losing_job,
                                  c("Oui, beaucoup", "Oui, assez ou passablement") ~ "Yes",
                                  c("Non, pas tellement", "Non, pas du tout") ~ "No"
                                  ),
    
    # Years of service
    years_of_service_en = factor(case_match(years_of_service,
                                 "Moins de 6 mois" ~ "Less than 6 months",
                                 "De 6 mois à 1 an" ~ "6-12 months",
                                 "De plus d’1 an à 5 ans" ~ "1-5 years",
                                 "De plus de 5 ans à 10 ans" ~ "5-10 years",
                                 "Plus de 10 ans" ~ "Over 10 years"),
                                 levels = c("Over 10 years", "5-10 years", "1-5 years", "6-12 months", "Less than 6 months")
                                 ),
    
    # Type of contract
    contract_type = factor(case_when(
      contract_cdi == TRUE ~ "Unlimited",
      contract_cdd == TRUE ~ "Limited",
      # contract_seasonal == TRUE ~ "Seasonal",
      # contract_intern == TRUE ~ "Internship",
      # contract_unemployment == TRUE ~ "Subsidized",
      .default = "Other"),
      levels = c("Unlimited", "Limited", "Other")
    ),
    
    # Percent change in working contract
    percent_change =  readr::parse_number(as.character(percentage)) - readr::parse_number(as.character(work_rata)),
    percent_change_cat = factor(case_when(     
      percent_change == 0 ~ "No change",
      percent_change < 0 ~ "Reduced",
      percent_change > 0 ~ "Increased",
      .default = NA
    ),
    levels = c("No change", "Increased", "Reduced")
    ), 
    
    # Measure of overwork -> hours worked beyond their expected percentage hours (although, in some fields, e.g. doctors, 100% can correspond to 50 hours?)
    overwork_hours = hours_week - (readr::parse_number(as.character(percentage))*0.4),
    overwork_ratio = hours_week / (readr::parse_number(as.character(percentage))*0.4),
    
    # Work interruptions
    work_interruption = factor(case_match(work_interruption,
                                      "Non" ~ "No",
                                      "Oui, moins d’un mois en tout" ~ "Less than 1 month",
                                      "Oui, de 1 à moins de 3 mois en tout" ~ "Between 1-3 months",
                                      "Oui, de 3 à moins de 6 mois en tout" ~ "Between 3-6 months", 
                                      "Oui, 6 mois ou plus" ~ "6+ months"),
                                      levels = c("No", "Less than 1 month", "Between 1-3 months", "Between 3-6 months",  "6+ months")
                               ),
    work_interruption_dich = case_match(work_interruption,
                                        "No" ~ "No",
                                        c("Less than 1 month", "Between 1-3 months", "Between 3-6 months",  "6+ months") ~ "Yes"
                                        ),
    exercise_hard_dich = case_match(exercise_hard,
                                    c("Jamais", "Parfois") ~ "Not regularly",
                                    c("Une fois par semaine", "2-3 fois par semaine", 
                                      "4-5 fois par semaine", "Tous les jours ou presque") ~ "Regularly"
                                    ),
    exercise_moderate_dich = case_match(exercise_moderate,
                                        c("Jamais", "Parfois") ~ "Not regularly",
                                        c("Une fois par semaine", "2-3 fois par semaine", 
                                          "4-5 fois par semaine", "Tous les jours ou presque") ~ "Regularly"
                                        ),
    
    # Noisy
    noisy = factor(case_match(noisy,
                       "Non, pas du tout bruyant" ~ "Not noisy",
                       "Peu bruyant" ~ "A little noisy",
                       "Oui, moyennement bruyant" ~ "Moderately noisy",
                       "Oui, très bruyant" ~ "Very noisy"),
                   levels = c("Not noisy", "A little noisy", "Moderately noisy", "Very noisy")
    ),
    
    # Quiet room -> Dichotomise and relevel
    quiet_room = factor(case_match(quiet_room,
                                   "Oui" ~ "Yes",
                                   c("Difficilement", "Non") ~ "No"), 
                        levels = c("Yes", "No")
                        ),
    
    # Relevel Karasek (Q22)
    karasek_scale_1_work_organisation = factor(karasek_scale_1_work_organisation,
                                               levels = c("Tout à fait d’accord", "D’accord", 
                                                          "Pas d’accord", "Pas du tout d’accord")
                                               ),
    karasek_can_balance_work_life = factor(case_match(karasek_scale_1_work_life_balance, # reverse the scale
                                                               c("Pas du tout d’accord", "Pas d’accord") ~ "Yes",
                                                               c("D’accord", "Tout à fait d’accord") ~ "No"),
                                                    levels = c("Yes", "No")
    ),
    karasek_scale_1_work_satisfaction = factor(karasek_scale_1_work_satisfaction,
                                               levels = c("Tout à fait d’accord", "D’accord", 
                                                          "Pas d’accord", "Pas du tout d’accord")
    ),
    karasek_scale_1_work_satisfaction_dich = factor(case_match(karasek_scale_1_work_satisfaction, # reverse the scale
                                                      c("Pas du tout d’accord", "Pas d’accord") ~ "No",
                                                      c("D’accord", "Tout à fait d’accord") ~ "Yes"),
                                           levels = c("Yes", "No")
    ),
    
    # Karasek social support categories
    # These categories are just made up, need to check literature in case we want to include this!
    karasek_social_support_cat = factor(case_when(
      karasek_social_support < 16 ~ "Low",
      karasek_social_support >= 16 & karasek_social_support < 24 ~ "Medium",
      karasek_social_support >= 24 ~ "High",
      .default = NA),
      levels = c("High", "Medium", "Low")
    ),
    
    # Relevel Work while (Q31+33+34)
    work_life_balance_dich = factor(case_match(work_life_balance,
                                    c("Toujours", "Souvent") ~ "Yes",
                                    c("Parfois", "Rarement", "Jamais") ~ "No"),
                                    levels = c("Yes", "No")
                                    ),
    work_life_balance = factor(work_life_balance, levels = c("Toujours", "Souvent", "Parfois", "Rarement", "Jamais")),
    work_weekends = factor(work_weekends, levels = c("Non, jamais", "Oui, mais pas de manière régulière", "Oui, régulièrement")),
    work_sick = factor(work_sick, levels = c("Non", "Oui", "Je n'ai pas été malade au cours des 12 derniers mois")),
    
    work_overtime_dich = factor(case_match(work_overtime,
                                           c("Tous les jours", "Souvent") ~ "Often",
                                           c("Rarement", "Jamais") ~ "Rarely/Never",
                                           "Parfois" ~ "Occasionally"),
                                levels = c("Rarely/Never", "Occasionally", "Often")),
    work_weekends_dich = factor(case_match(work_weekends,
                                           c("Oui, régulièrement") ~ "Regularly",
                                           c("Oui, mais pas de manière régulière") ~ "Occasionally",
                                           "Non, jamais" ~ "Never"),
                                levels = c("Never", "Occasionally", "Regularly")),
    work_sick_dich = factor(case_match(work_sick,
                                       "Non" ~ "No",
                                       "Oui" ~ "Yes",
                                       "Je n'ai pas été malade au cours des 12 derniers mois" ~ "Not applicable"),
                            levels = c("No", "Yes", "Not applicable")),
    
    # Teleworkability - using teleworkability indices
    teleworkability = factor(case_when(
      physical_interaction < 0.4 & social_interaction < 0.5 ~ "Low feasibility, low interaction",
      physical_interaction < 0.4 & social_interaction >= 0.5 ~ "Low feasibility, high interaction",
      physical_interaction >= 0.4 & social_interaction < 0.5 ~ "High feasibility, low interaction",
      physical_interaction >= 0.4 & social_interaction >= 0.5 ~ "High feasibility, high interaction",
      .default = NA),
      levels = c("High feasibility, low interaction", "High feasibility, high interaction", 
                 "Low feasibility, low interaction", "Low feasibility, high interaction")
    ),
    # #alternative teleworkability
    # teleworkability = factor(case_when(
    #   telework == 0 & social_interaction < 0.5 ~ "Low feasibility, low interaction",
    #   telework == 0 & social_interaction >= 0.5 ~ "Low feasibility, high interaction",
    #   telework == 1 & social_interaction < 0.5 ~ "High feasibility, low interaction",
    #   telework == 1 & social_interaction >= 0.5 ~ "High feasibility, high interaction",
    #   .default = NA),
    #   levels = c("Low feasibility, high interaction", "Low feasibility, low interaction",
    #              "High feasibility, high interaction", "High feasibility, low interaction")
    # ),
    
    # Health insurance
    health_insurance_cat = factor(case_when(
      health_insurance %in% c("CHF 1'000", "CHF 1'500", "CHF 2'000") ~ "1,000-2,000",
      health_insurance %in% c("Je ne sais pas", "Je ne sais pas ou je ne souhaite pas répondre") ~ "Don't know / no answer",
      health_insurance == "CHF 300" ~ "300",
      health_insurance == "CHF 500" ~ "500",
      health_insurance == "CHF 2'500" ~ "2,500",
      health_insurance == "Je n'ai pas d'assurance maladie en Suisse" ~ "No Swiss insurance",
      .default = "Don't know / no answer"
    ),
    levels = c("300", "500", "1,000-2,000", "2,500", "No Swiss insurance", "Don't know / no answer")),
    
    # Recode alcohol consumption to English
    alcohol_en = factor(case_match(alcohol,
                                   "Jamais" ~ "Never",                    
                                   "Occasionnellement" ~ "Occasionally",      
                                   "Une fois par semaine" ~ "Once per week",   
                                   "Quelques fois par semaine" ~ "A few times per week",
                                   "Une fois par jour" ~ "Once per day",     
                                   "Plusieurs fois par jour" ~ "Several times per day"),
                        levels = c("Never", "Occasionally", "Once per week", "A few times per week", 
                                   "Once per day", "Several times per day")
    ),
    # Recode chronic disease to English
    chronic_disease_en = factor(case_match(chronic_disease,
                                           "Non" ~ "No",
                                           "Oui" ~ "Yes"),
                                levels = c("No", "Yes")
    ),
    # Country of residence
    country_residence = factor(case_when(
      str_length(nip_house)>4 ~ "France",
      .default = "Switzerland"), levels = c("Switzerland", "France")
      ),
    
    # Urbanicity
    zone_house_en = factor(case_match(zone_house,
                                      "À la campagne" ~ "Rural",
                                      "En périphérie d’une ville" ~ "Urban outskirts",
                                      "En ville" ~ "Urban")),
    
    # Pandemic related change to work
    pandemic_related_work_change = factor(case_when(pandemic_change_reason_job_loss_pandemic| 
                                               pandemic_change_reason_career_move_pandemic| 
                                               pandemic_change_reason_job_change_pandemic|
                                               pandemic_change_reason_resignation_pandemic|
                                               pandemic_change_reason_hired ~ "Change", .default = "No change"),
                                          levels = c("No change", "Change")
                                          ),
    # Aggression in general in last 12 months
    aggression = factor(case_when(
      verbal_aggression_colleagues | physical_aggression_colleagues | verbal_aggression_clients | physical_aggression_clients |
      verbal_aggression_99 | physical_aggression_99 ~ "Yes",
      .default = "No"),
      levels = c("No", "Yes")),
    # Aggression from colleagues, collaborators, or managers in last 12 months
    aggression_colleagues = factor(case_when(
      verbal_aggression_colleagues | physical_aggression_colleagues ~ "Yes",
      .default = "No"), 
      levels = c("No", "Yes"))
    # Relevel DGS aggression
    # dgs_binary_victim_aggression
)



## Point recoding ####
dat_all <- dat_all %>% 
  mutate(
    
    # Recode the "Other" from education level
    education_rec_en = factor(case_when(
      participant_id == "6d6cd138-29b0-11eb-9501-0050568507d9" ~ "Tertiary",  # brevet avocat
      participant_id == "9e00dd58-29ba-11eb-aa4e-0050568507d9" ~ "Secondary", # Secrétaire médicale
      participant_id == "d33787cc-3029-11eb-8bf2-0050568507d9" ~ "Secondary", # Attestation fédérale professionnelle ( ASA)
      participant_id == "8e0e18ce-b966-11ec-8a87-fa163eeab1c6" ~ "Secondary",  # Ecole privée de secrétariat
      .default = education_rec_en
      ),
      levels = c("Tertiary", "Secondary", "Primary")
      ),
        )


# Apply filters
dat_all <- dat_all %>% 
  filter(
    !wfh_exposure %in% c("Not applicable"), ## Filter out the people who didn't work before the pandemic
         !age_cat %in% c("18-24", "65+")) %>%        ## Filter out those aged under 25
  mutate(wfh_exposure = droplevels(wfh_exposure),
         age_cat = factor(droplevels(age_cat)))

# only include people salaried at the time of the interview
dat <- dat_all %>% 
  filter(work_situation.y == "En activité salariée")


## Save the two datasets separately
saveRDS(dat_all, here("data", "Generated datasets", "clean_dataset_incl_salariee.rds")) # Save the dataset that includes non-salaried people
saveRDS(dat, here("data", "Generated datasets", "clean_dataset.rds")) # Save the cleaned dataset that excludes non-salaried people

rm(list = ls())
