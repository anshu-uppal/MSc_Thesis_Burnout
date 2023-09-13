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
    supervision = case_when(
      str_detect(supervision, "Oui, et") ~ "Manager - ",
      .default = ""
    ),
    profession.y = paste0(supervision, profession.y)
    # profession.y = paste0(supervision, profession.y," (",job_sector,")")
    
    ) %>%
  add_count(profession.y, sort = TRUE)


## Create data table to feed into occupation classification function
corpus <- data.table(
  id = occup$participant_id,
  text = occup$profession.y,
  job_sector = occup$job_sector
)

# Assign languages and run function
corpus[, language := identify_language(text)]
corpus[, language := "fr"]
languages <- unique(corpus$language)
languages <- c("fr")
suggestions <- lapply(languages, function(lang) {
  classify_occupation(
    corpus = corpus[language == lang],
    lang = lang,
    isco_level = 3,
    num_leaves = 10
  )
}) %>% rbindlist

final <- left_join(corpus, suggestions) %>% 
  add_count(iscoGroup, sort = TRUE) 
  

# ### Leftovers automatic classification - after running through the "cleaning profession.R" file
# 
# ## Create data table to feed into occupation classification function
# corpus <- data.table(
#   id = occup_ISCO$participant_id,
#   text = occup_ISCO$profession.y,
#   # text = paste(occup_ISCO$profession.y, occup_ISCO$job_sector),
#   job_sector = occup_ISCO$job_sector
# )
# 
# # Assign languages and run function
# # corpus[, language := identify_language(text)]
# corpus[, language := "fr"]
# # languages <- unique(corpus$language)
# languages <- c("fr")
# suggestions <- lapply(languages, function(lang) {
#   classify_occupation(
#     corpus = corpus[language == lang],
#     lang = lang,
#     isco_level = 4,
#     num_leaves = 10
#   )
# }) %>% rbindlist
# 
# final <- left_join(corpus, suggestions) %>% 
#   add_count(iscoGroup, sort = TRUE) %>% 
#   filter(!is.na(iscoGroup))
