pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  lubridate,     # manipulate date objects
  stringi,       # String manipulation
  labourR,      # Classify Free-Text Occupations to ISCO codes
  data.table
)

# Read in clean dataset (see "Merge code.R" and "Cleaning merged dataset")
dat <- tibble(readRDS(here("data", "clean_dataset.rds"))) #%>% 
# filter(!is.na(sex_en))
# dat_all <- tibble(readRDS(here("data", "clean_dataset_incl_salariee.rds"))) #%>% 
# filter(!is.na(sex_en))
# occ_labels <- readxl::read_xlsx(here("data","do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
#                                 sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO))


# Keep the variables that can help for choosing how to assign ISCO-08 codes
occup <- dat %>% 
  select(profession.y, job_sector, supervision,
         participant_id) %>%
  # Make some changes to the free-text columns to make them easier to work with
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(
    Manager_status = case_when(
      str_detect(supervision, "Oui, et") ~ "Manager",
      .default = NA
    ),
    new_id = row_number()
  ) %>% 
  select(-supervision)


## Create data table to feed into occupation classification function
corpus <- data.table(
  p_id = occup$participant_id,
  id = occup$new_id,
  job_description = occup$profession.y,
  Manager_status = occup$Manager_status,
  job_sector = occup$job_sector
)

corpus_chat <- corpus %>% select(-p_id) # remove our participant ID, only keep generic ID

corpus_chat <- corpus_chat %>% filter(id < 20)
write.csv2(corpus_chat, here("data", "Automating ISCO.csv"), row.names = FALSE)

suggestions <- read_csv2(here("data", "Automating ISCO_suggestions.csv"))
