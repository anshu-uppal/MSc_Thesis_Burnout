## Merge datasets ##
pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics
  janitor,
  readxl,
  lubridate     # manipulate date objects
)

# Read in the all participants dataset
inclusion <- tibble(readRDS(here("data", "Initial datasets", "2023-01-03-1808_ALLincs_ALLparticipants.rds"))) %>% # local file
# inclusion <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-01-03-1808_ALLincs_ALLparticipants.rds")) %>%
  filter(testeur == FALSE) %>%          # Remove the testers
  filter(!str_starts(codbar, "T")) %>%  # Remove any people with codbar beginning with T (also testers)
  # Remove people who didn't participate in serosurveys
  filter(serocov_work | serocov_pop | sp3_juin_2021 | sp2_novdec_2020 | sp4_avril_2022 |pop_pilote | work_pilote)
  

# Read in the Santé-travail dataset
work <- tibble(readRDS(here("data", "Initial datasets", "2023-02-02-1137_SanteTravail_ALLparticipants.rds"))) # local file
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

# Read in the classified occupations data
# (generated from "01_prep_a_classify occupations.R")
occ_cat <- readRDS(here("data", "Generated datasets", "Classified_occupations.RDS"))

# Without general health questionnaire data
final_merge <- left_join(merged_data, occ_cat)

### Save final file ####
saveRDS(final_merge, here("data", "Generated datasets", "merged_dataset.rds")) # Save the merged dataset into the data folder

rm(list = ls())