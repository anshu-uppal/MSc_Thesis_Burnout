## Merge datasets ##
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics
  janitor,
  readxl,
  lubridate     # manipulate date objects
)

# Read in the all participants dataset
inclusion <- tibble(readRDS(here("data","2023-01-03-1808_ALLincs_ALLparticipants.rds"))) %>% # local file
# inclusion <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-01-03-1808_ALLincs_ALLparticipants.rds")) %>%
  filter(testeur == FALSE) %>%          # Remove the testers
  filter(!str_starts(codbar, "T")) %>%  # Remove any people with codbar beginning with T (also testers)
  # Remove people who didn't participate in serosurveys
  filter(serocov_work | serocov_pop | sp3_juin_2021 | sp2_novdec_2020 | sp4_avril_2022 |pop_pilote | work_pilote)
  

# Read in the Santé-travail dataset
work <- tibble(readRDS(here("data","2023-02-02-1137_SanteTravail_ALLparticipants.rds"))) # local file
# work <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-02-02-1137_SanteTravail_ALLparticipants.rds"))

# alternative merge to label columns with source (instead of .x, .y)
# merged_data <- inner_join(inclusion, work, by = "participant_id", suffix = c(".inc", ".work")) # Join the data

# Merge the two datasets
merged_data <- full_join(inclusion, work, by = "participant_id") %>% 
  filter(!is.na(date_soumission.y) # filter for only those that submitted the sante-travail questionnaire
         , testeur.x == FALSE            # Remove the testers
         , !str_starts(codbar.x, "T")    # Remove any people with codbar beginning with T (also testers)
         , testeur.y == FALSE            # Remove the testers
         , !str_starts(codbar.y, "T")    # Remove any people with codbar beginning with T (also testers)
  )

# ## General health questionnaire ####
# # gen_health <- tibble(readRDS(here("data","2023-02-27-1537_Export_general_health-202206301210_ALL.rds")))
# gen_health <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-02-27-1537_Export_general_health-202206301210_ALL.rds"))
# # dupcheck <- gen_health %>% janitor::get_dupes() # check for duplicates
# gh_merge <- full_join(merged_data, gen_health,
#                       by = "participant_id") %>% 
#   filter(!is.na(date_soumission.y)
#          # , !is.na(date_soumission)
#          )


# Read in the classified occupations data
# (generated from "01_prep_a_classify occupations.R")
occ_cat <- readRDS(here("data","Classified occupations.RDS"))


# ## Include general health questionnaire data
# final_merge <- left_join(gh_merge, occ_cat)

# Without general health questionnaire data
final_merge <- left_join(merged_data, occ_cat)

### Save final file ####
saveRDS(final_merge, here("data", "merged_dataset.rds")) # Save the merged dataset into the data folder

rm(list = ls())

# ## Mental health questionnaires ####
# mental_work <- tibble(readRDS(here("data","2021-09-30-1813_SC19_mental_health_06_2021-202109291352_WORK.rds")))
# mental_pop <- tibble(readRDS(here("data","2021-09-30-1813_SC19_mental_health_06_2021-202109291352_GENPOP.rds")))
# # Bind both mental health datasets
# mental_combined <- bind_rows(mental_work, mental_pop) 
# # dupcheck <- mental %>% janitor::get_dupes() # look at duplicates
# # remove duplicate rows
# mental_combined <- mental_combined %>%
#   distinct(across(), # reduces data frame to only unique rows (keeps first one of any duplicates)
#            .keep_all = TRUE) 
# 
# # Merge the mental health dataset with the Santé Travail dataset
# mh_merge <- full_join(merged_data, mental_combined,
#                       by = join_by(sugar_id == mh_sugarid)) %>% 
#   filter(!is.na(date_soumission.y))