pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics 
  janitor,      # adding totals and percents to tables
  lubridate,     # manipulate date objects
  readxl,
  stringi       # String manipulation
)

# Read in the all participants dataset from local file
inclusion <- tibble(readRDS(here("data", "Initial datasets", "2023-01-03-1808_ALLincs_ALLparticipants.rds")))
# Alternative read-in from source
# inclusion <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-01-03-1808_ALLincs_ALLparticipants.rds"))

# Read in the Santé-travail dataset from local file
work <- tibble(readRDS(here("data", "Initial datasets", "2023-02-02-1137_SanteTravail_ALLparticipants.rds")))
# Alternative read-in from source
# work <- tibble(readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-02-02-1137_SanteTravail_ALLparticipants.rds"))

# alternative merge to label columns with source (instead of .x, .y)
# merged_data <- inner_join(inclusion, work, by = "participant_id", suffix = c(".inc", ".work")) # Join the data

# Merge the two datasets
merged_data <- full_join(inclusion, work, by = "participant_id") %>%
  mutate(
    # Convert submission date to date format
    date_soumission.y = as_date(date_soumission.y),  # Date of Santé-travail questionnaire submission
    # Age at the time of completing santé-travail quest.
    birthdate = ymd(birthdate),
    age = time_length(date_soumission.y - birthdate, "years")) %>% 
  filter(!is.na(date_soumission.y) # filter for only those that submitted the sante-travail questionnaire
         , testeur.x == FALSE            # Remove the testers
         , !str_starts(codbar.x, "T")    # Remove any people with codbar beginning with T (also testers)
         , testeur.y == FALSE            # Remove the testers
         , !str_starts(codbar.y, "T")    # Remove any people with codbar beginning with T (also testers)
         , employed == "Oui"             # Remove those not employed in previous 12 months
         # , work_situation.y == "En activité salariée" # remove those not in salaried employment at time of santé-travail questionnaire
         , age >= 25 & age < 65 # Remove those under 25 and over 64
         # remove those not working before the pandemic
         , !wfh_change %in% c("Non concerné-e (vous n’aviez pas d’emploi avant la pandémie ou vous avez changé d’emploi)")
  )

occ_labels <- readxl::read_xlsx(here("data", "Initial datasets", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

# Keep the variables that can be helpful for choosing how to assign ISCO-08 codes
# (key free-text information is sometimes in the profession_other, job_sector, job_sector_other variables)
occup <- merged_data %>% 
  select(profession.x, profession.y, job_sector, supervision,
         job_sector_other, profession_other,
         workplace_size, education_rec_en, hh_income, sedentary, 
         participant_id) %>%
  # Make some changes to the free-text columns to make them easier to work with
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(profession.y = stringi::stri_trans_general(str = profession.y, 
                                                    id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
         profession.y = str_trim(str_to_lower(profession.y)),             # Convert all capital letters to small letters
         
         job_sector_other = stringi::stri_trans_general(str = job_sector_other, 
                                                        id = "Latin-ASCII"),
         job_sector_other = str_trim(str_to_lower(job_sector_other)),
         
         profession_other = stringi::stri_trans_general(str = profession_other, 
                                                        id = "Latin-ASCII"),
         profession_other = str_trim(str_to_lower(profession_other)),
         ISCO = NA                                                               # Empty column that we'll fill in the next steps
  ) %>%
  add_count(profession.y, sort = TRUE) %>% 
  relocate(ISCO, .after = profession.y)


# Update the dataset to include the ISCO-08 codes and occupation labels, using the below definitions:
occup_ISCO <- occup %>% 
  mutate(ISCO = case_when(
    
    # Services managers
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "resource|ressource| rh | rh") ~ 1212,
    
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Ambassade") ~ 111,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Administration publique") ~ 1112,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "industrie|construction|production") ~ 132,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Information") ~ 133,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Enseignement|Santé|transports|Banques|Sécurité|comptabilité") ~ 134,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Hébergement") ~ 141,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Commerce") ~ 142,
    is.na(ISCO) & (str_detect(profession.x, "Chef-fe") | profession.y %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.y, "fonctionnai|emplo|responsa|direct") & str_detect(supervision, "Oui, et"))) &
      str_detect(job_sector, "Immobilier|Other|Arts") ~ 143,
    is.na(ISCO) & str_detect(profession.y, "ceo") ~ 112,
    
    
    # Gym / Sports teacher
    is.na(ISCO) & str_detect(profession.y, "professeur de gym|educateur sportif") ~ 342,
    
    
    # HR workers
    is.na(ISCO) & str_detect(profession.y, "resource|ressource| rh | rh| hr|hr ") | profession.y %in% c("rh") ~ 242,
    is.na(ISCO) & str_detect(profession.y, "planific|recrut") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other, "administration et ressources humaines") ~ 242,
    
    
    # Health managers
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "infir|medecin|pharma") ~ 1342,
    is.na(ISCO) & str_detect(profession.y, "cadre de sante|directrice d'iepa") ~ 1342,
    
    # Nurses
    is.na(ISCO) & str_detect(profession.y, "infi|sage femme|sage-femme") ~ 222,
    # Doctors
    is.na(ISCO) & str_detect(profession.y, "medecin") ~ 221,
    
    
    # Care workers
    is.na(ISCO) & str_detect(profession.y, "aide") & str_detect(profession.y, "domic|soig|soin") ~ 532,
    is.na(ISCO) & str_detect(profession.y, "assc|ambulan|a.s.s.c.|soins ems|chauffeur bls aed|opticien|employee ems|asam iepa") ~ 325,
    
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "assistant") & str_detect(profession.y, "medic|dent|soins|soci") ~ 325,
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "coordinat") ~ 325,
    
    # Social workers
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "social|educat|curat") ~ 2635,
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "accompagn") ~ 3221,
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "animat|socioprof|socioc|fonctionnaire a imad") ~ 3412,
    is.na(ISCO) & str_detect(job_sector, "Santé") & profession.y %in% c("ais") ~ 3412,
    is.na(ISCO) & str_detect(profession.y, "responsable animation") ~ 3412,
    is.na(ISCO) & str_detect(profession.y, "insertion") ~ 3412,
    is.na(ISCO) & str_detect(job_sector, "Santé") & profession.y %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & str_detect(job_sector, "Petite enfance") & profession.y %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & profession.y %in% c("maitre de readaptation") ~ 2263,
    is.na(ISCO) & str_detect(profession.y, "orthoptiste") ~ 2267,
    is.na(ISCO) & str_detect(profession.y, "sociolog|anthropolog|archeolog") ~ 2632,
    is.na(ISCO) & str_detect(job_sector, "Ambassade") & str_detect(profession.y, "develop") ~ 2635,
    
    # Religious worker
    is.na(ISCO) & str_detect(profession.y, "pastor|eglise|pasteur") & str_detect(profession.y, "assistant") ~ 3413,
    is.na(ISCO) & str_detect(profession.y, "pastor|eglise|pasteur") ~ 2636,
    
    
    # Education managers
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "enseign") ~ 1345,
    is.na(ISCO) & str_detect(profession.y, "direction d'ecole") ~ 1345,
    
    # Professors
    is.na(ISCO) & str_detect(profession.y, "professe|profress|prof a une universite") ~ 2310,
    is.na(ISCO) & str_detect(profession.y, "directeur d'une ecole de commerce") ~ 2320,
    
    # Childcare
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Petite enfance") & !str_detect(profession.y, "policier") ~ 1341,
    is.na(ISCO) & str_detect(job_sector, "Petite enfance") & str_detect(profession.y, "educat|anima") ~ 5311,
    is.na(ISCO) & str_detect(profession.y, "educatrice petite enfance|petite enfance") ~ 5311,
    is.na(ISCO) & str_detect(job_sector, "Enseignement") & str_detect(profession.y, "primaire") ~ 2341,
    # Other teaching professionals
    is.na(ISCO) & str_detect(profession.y, "enseignement|enseignan| formation|formateur|formatrice") ~ 235,
    is.na(ISCO) & str_detect(profession.y, "educatrice specialisee") ~ 2352,
    
    
    # Scientists / Science professionals
    is.na(ISCO) & str_detect(profession.y, "biologist") ~ 2131,
    is.na(ISCO) & str_detect(profession.y, "laborant|laborat|labonratine") ~ 3212,
    is.na(ISCO) & str_detect(profession.y, "geolog|geog") ~ 2114,
    is.na(ISCO) & str_detect(profession.y, "geom") ~ 2165,
    is.na(ISCO) & str_detect(profession.y, "chercheu|checheu|scientifique en recherche et developpement") ~ 213,
    is.na(ISCO) & str_detect(profession.y, "rechercher|doctorant") ~ 213,
    is.na(ISCO) & str_detect(profession.y, "graphisme|graphidme") ~ 2166,
    
    
    
    # Project managers
    is.na(ISCO) & str_detect(profession.y, "chef|manager|responsable|charge|gestion|coordinat") & 
      str_detect(profession.y, "project | projet|product |produit|program") ~ 1219,
    is.na(ISCO) & str_detect(profession.y, "planificateur|directrice de projets") ~ 1219,
    
    # Accounting managers
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "comptab") ~ 1211,
    is.na(ISCO) & str_detect(profession.y, "comptab|audit|fiduci|facturist") ~ 2411,
    
    # Security services
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Sécurité") & str_detect(profession.y, "yoga teacher") ~ 3423,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Sécurité") ~ 1349, # managers
    is.na(ISCO) & str_detect(job_sector, "Sécurité") & str_detect(profession.y, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(job_sector, "Sécurité") & str_detect(profession_other, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(profession.y, "policier|police|garde bain|gendarm|douanier") ~ 541, # Other security
    is.na(ISCO) & str_detect(profession.y, "agent") & str_detect(profession.y, "surete|securite") ~ 541, # Other security
    is.na(ISCO) & profession.y %in% c("enqueteur") ~ 335,
    is.na(ISCO) & str_detect(profession.y, "pompier") ~ 541,
    
    
    # Assistants / receptionists
    is.na(ISCO) & str_detect(profession.y, "assistant") & str_detect(profession.y, "medic")  ~ 3256,
    is.na(ISCO) & str_detect(profession.y, "assistant") & str_detect(profession.y, "pharma")  ~ 3213,
    is.na(ISCO) & str_detect(profession.y, "assistant") & str_detect(profession.y, "denta")  ~ 3251,
    is.na(ISCO) & str_detect(profession.y, "secretaire|secretar") & str_detect(profession.y, "medic|pharma|denta")  ~ 3344,
    is.na(ISCO) & str_detect(profession.y, "assistant") & str_detect(profession.y, "administrative|direction|gestion|parlem|gesti|parl|execu|")  ~ 334,
    is.na(ISCO) & str_detect(profession.y, "adjoint|gestion") & str_detect(profession.y, "administr") ~ 334,
    is.na(ISCO) & str_detect(profession.y, "huiss") ~ 334,
    is.na(ISCO) & !str_detect(profession.x, "Cadre") & str_detect(profession.y, "secretaire|secretar|telephonist") ~ 412,
    is.na(ISCO) & profession.y == "assistant" | profession.y == "assistante" ~ 334,
    is.na(ISCO) & str_detect(profession.y, "reception") ~ 4226,
    is.na(ISCO) & str_detect(profession.y, "hotesse") & str_detect(profession.y, "accueil") ~ 4226,
    is.na(ISCO) & str_detect(profession.y, "office manager") ~ 3341,
    
    # Other services
    is.na(ISCO) & str_detect(profession.y, "voyage") ~ 4221,
    is.na(ISCO) & str_detect(job_sector_other, "tourism") ~ 4221,
    is.na(ISCO) & str_detect(profession.y, "photograp") ~ 3431,
    is.na(ISCO) & str_detect(profession.y, "cameraman|videast") ~ 3521,
    is.na(ISCO) & str_detect(profession.y, "telecommunication|telemat") ~ 3522,
    is.na(ISCO) & str_detect(profession.y, "videoconference") ~ 352,
    is.na(ISCO) & str_detect(profession.y, "funera") ~ 5163,
    
    
    # Other health professionals
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "dentist|dentaire|pharma|dietet|audiol|optom|ergother|physio") ~ 134,
    is.na(ISCO) & str_detect(profession.y, "dentist|dentaire") ~ 2261,
    is.na(ISCO) & str_detect(profession.y, "pharma|drogu") ~ 2262,
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "preparatrice") ~ 3213,
    is.na(ISCO) & str_detect(profession.y, "dietet") ~ 2265,
    is.na(ISCO) & str_detect(profession.y, "logoped") ~ 2266,
    is.na(ISCO) & str_detect(profession.y, "audiol|optom|ergother") ~ 226,
    is.na(ISCO) & str_detect(profession.y, "physio") ~ 2264,
    # Psychologists / therapists / other
    is.na(ISCO) & str_detect(profession.y, "radiotherap") ~ 3211,
    is.na(ISCO) & str_detect(job_sector, "Santé") & str_detect(profession.y, "trm") ~ 3211,
    is.na(ISCO) & str_detect(profession.y, "psycho|therap") ~ 226,
    is.na(ISCO) & str_detect(profession.y, "sante en entreprise") ~ 2263,
    is.na(ISCO) & str_detect(profession.y, "senior emergency officer|medical affairs lead|regulateur sanitaire") ~ 2269,
    is.na(ISCO) & str_detect(job_sector, "Santé") & 
      str_detect(profession.y, "msp|aucun|responsable|affairs manager") ~ 2269,
    
    
    
    # Greffier / Juristes
    is.na(ISCO) & str_detect(profession.y, "greffier") | str_detect(profession_other, "greffier") ~ 2619,
    is.na(ISCO) & str_detect(profession.y, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(profession_other, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(profession.y, "compliance|conformite") ~ 2619,
    
    
    # IT
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "informatic|informatiq") ~ 1330,
    is.na(ISCO) & str_detect(profession.y, "informatic|informatiq|developpeur|helpdesk|helpdek") ~ 351,
    is.na(ISCO) & str_detect(profession.y, "information|programmatrice|solutions technologiques|solution engineer") ~ 251,
    is.na(ISCO) & profession.y %in% c("programmeur") ~ 251,
    is.na(ISCO) & profession.y %in% c("it", "consultant it", "developpeur") ~ 351,
    is.na(ISCO) & str_detect(profession.y, "charge de securite") ~ 2529,
    is.na(ISCO) & str_detect(profession.y, "conseil|administrat") & str_detect(profession.y, "donnee")  ~ 2511,
    # HR again
    is.na(ISCO) & str_detect(profession.y, "personn")  ~ 2423,
    
    
    
    
    # Banking / finance
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "banq") ~ 1346,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(profession.y, "financ") ~ 1211,
    is.na(ISCO) & str_detect(profession.y, "banq|fortune|financ|banker|bancair") ~ 241,
    is.na(ISCO) & str_detect(profession.y, "trader|trading") ~ 3311,
    is.na(ISCO) & str_detect(profession.y, "economist") ~ 2631,
    is.na(ISCO) & str_detect(profession.y, "patrimo|assuranc") ~ 241,
    is.na(ISCO) & str_detect(profession.y, "fiscalist|fscaliste|taxat") ~ 2411,
    is.na(ISCO) & str_detect(profession.y, "risk|risq") ~ 2413,
    is.na(ISCO) & str_detect(job_sector, "Banques") & str_detect(profession.y, "analyst") ~ 2413,
    is.na(ISCO) & str_detect(profession.y, "gerant d'investissements") ~ 241,
    is.na(ISCO) & str_detect(job_sector_other, "technologie & finance") ~ 241,
    
    
    
    
    
    
    ## Other manager positions not picked up already
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Santé") ~ 1342,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Administration publique") ~ 1112,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Ambassade") ~ 111,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Banques, assurances") ~ 1346,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Hébergement et restauration") ~ 141,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "industrie|construction|production") ~ 132,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Enseignement") ~ 1345,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & str_detect(job_sector, "Commerce") ~ 142,
    is.na(ISCO) & str_detect(supervision, "Oui, et") & !str_detect(job_sector, "Services domestiques") ~ 1439,
    
    ## Technicians
    is.na(ISCO) & str_detect(profession.y, "technicien") & str_detect(profession.y, "audiovis") ~ 352,
    is.na(ISCO) & str_detect(profession.y, "technicien") & str_detect(job_sector, "Santé") ~ 321,
    is.na(ISCO) & str_detect(profession.y, "technicien") & str_detect(job_sector, "Arts") ~ 265,
    is.na(ISCO) & str_detect(profession.y, "technicien") & str_detect(job_sector, "industrie|construction|production") ~ 311,
    is.na(ISCO) & str_detect(profession.y, "technicien") & str_detect(job_sector_other, "ascenseur") ~ 741,
    
    # Scientific collaborator
    is.na(ISCO) & str_detect(profession.y, "collabor|colabor|adjoint") & str_detect(profession.y, "scientif") ~ 213,
    is.na(ISCO) & str_detect(profession.y, "statisticien|analyste de donnees|data analyst") ~ 212,
    # is.na(ISCO) & str_detect(job_sector, "Enseignement") & str_detect(profession.y, "") ~ 212,
    
    
    
    # Architect
    is.na(ISCO) & str_detect(profession.y, "architect|urbanist") & !str_detect(profession.y, "informat") ~ 216,
    is.na(ISCO) & str_detect(profession_other, "architect|urbanist") ~ 216,
    
    is.na(ISCO) & str_detect(profession.y, "designer industriel") ~ 2163,
    is.na(ISCO) & str_detect(profession.y, "dessin") ~ 3118,
    is.na(ISCO) & str_detect(profession.y, "designer") ~ 216,
    
    
    # Historian
    is.na(ISCO) & str_detect(profession.y, "historien") ~ 2633,
    
    # Authors / journalists / interpreters
    is.na(ISCO) & str_detect(profession.y, "journalist") ~ 2642,
    is.na(ISCO) & str_detect(profession.y, "scripte") ~ 2641,
    is.na(ISCO) & str_detect(profession.y, "interpret|traduc|linguist") ~ 2643,
    is.na(ISCO) & str_detect(profession.y, "redact|monteur news") ~ 264,
    
    
    # Library
    is.na(ISCO) & str_detect(profession.y, "biblioth|archiv|documental|musee") ~ 262,
    
    # Process
    is.na(ISCO) & str_detect(profession.y, "quality manager") ~ 7543,
    # is.na(ISCO) & str_detect(profession.y, "control") & str_detect(profession.y, "qualit") ~ 7543,
    is.na(ISCO) & str_detect(profession.y, "qualit") ~ 7543,
    
    # Transport workers
    is.na(ISCO) & (str_detect(profession.y, "conduct|chauffeur|chauffer") | str_detect(profession_other, "conduct|chauffeur|chauffer")) &
      (str_detect(profession.y, "bus|tram|tpg") | str_detect(profession_other, "bus|tram|tpg")) ~ 8331,
    is.na(ISCO) & str_detect(profession.y, "pilot") & str_detect(profession.y, "locom") ~ 8311,
    is.na(ISCO) & str_detect(job_sector, "transports") & str_detect(profession.y, "regulate") ~ 8312,
    is.na(ISCO) & str_detect(profession.y, "conduct|chauffeur|conduczeur") ~ 832,
    is.na(ISCO) & str_detect(profession.y, "agent de train") ~ 5112,
    is.na(ISCO) & str_detect(profession.y, "pilot") & str_detect(profession.y, "ligne") ~ 3153,
    is.na(ISCO) & str_detect(profession.y, "control") & str_detect(profession.y, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(profession_other, "control") & str_detect(profession_other, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(profession.y, "control") & str_detect(job_sector, "transport") ~ 5112,
    is.na(ISCO) & str_detect(profession.y, "escale|aerop") | str_detect(profession_other, "escale|arop") ~ 511,
    is.na(ISCO) & str_detect(profession.y, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(profession_other, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(job_sector, "transports") & str_detect(profession.y, "horaire") ~ 4323,
    is.na(ISCO) & str_detect(profession.y, "gestionnaire parc vehicule|responsable du parc de vehicule|gestionnaire de parc mobilite") ~ 9623, # or 1324, # fleet manager?
    is.na(ISCO) & str_detect(profession.y, "adjoint responsable reseau") ~ 2164,
    is.na(ISCO) & str_detect(job_sector_other, "ingenieure mobilite") ~ 2164,
    is.na(ISCO) & str_detect(profession.y, "secteur transports|cff") ~ 3339,
    
    
    
    
    
    
    # Engineers
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "electr") ~ 215,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "telecom") ~ 2153,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "civil") ~ 2142,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "agron") ~ 2143,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "mecani") ~ 2144,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "chem") ~ 2145,
    is.na(ISCO) & str_detect(profession.y, "ingenie") & str_detect(profession.y, "medic") ~ 2149,
    is.na(ISCO) & str_detect(profession.y, "ingenie|engen|engineer") & str_detect(profession.y, "inform|infom|system|it|reseau|logiciel|banking") ~ 252,
    is.na(ISCO) & str_detect(profession.y, "ingenie") ~ 214,
    is.na(ISCO) & str_detect(profession.y, "transferts de technologie") ~ 252,
    is.na(ISCO) & str_detect(profession.y, "chimi") ~ 2145,
    is.na(ISCO) & str_detect(profession.y, "dispatcher en electricite") ~ 3113,
    
    
    
    # Electrician
    is.na(ISCO) & str_detect(profession.y, "electricien") ~ 741,
    is.na(ISCO) & str_detect(profession.y, "electr") & str_detect(profession.y, "meca") ~ 7412,
    is.na(ISCO) & str_detect(profession.y, "electronicien") ~ 742,
    
    
    # Mechanic
    is.na(ISCO) & str_detect(profession.y, "meca|depanneur|machinist") ~ 723,
    is.na(ISCO) & str_detect(job_sector, "transports") & str_detect(profession.y, "technicien") ~ 723,
    is.na(ISCO) & str_detect(profession.y, "serrurier-constucteur") ~ 721,
    
    
    # Plumber
    is.na(ISCO) & str_detect(profession.y, "plomb") ~ 7126,
    
    
    
    # Gardener / agriculture
    is.na(ISCO) & str_detect(profession.y, "jardinier|horticult|viti|vigner") ~ 611,
    is.na(ISCO) & str_detect(profession.y, "ouvrier agricole") ~ 921,
    is.na(ISCO) & str_detect(profession.y, "oenologue") ~ 2132,
    is.na(ISCO) & str_detect(profession.y, "cavist") ~ 816,
    
    
    # Concierge
    is.na(ISCO) & str_detect(profession.y, "concierge") ~ 5153,
    is.na(ISCO) & str_detect(job_sector_other, "concierge") ~ 515,
    
    
    # Postal worker / courier
    is.na(ISCO) & str_detect(profession.y, "poste|posta") | str_detect(profession_other, "poste|posta") ~ 4412,
    is.na(ISCO) & str_detect(profession.y, "coursier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other, "courrier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other, "livreur|livreuse|bagagist") ~ 9621,
    is.na(ISCO) & str_detect(profession.y, "agent de distribution") ~ 4412,
    
    ## Fonctionnaire / employe
    is.na(ISCO) & str_detect(profession.y, "fonctionn|fonnctionn") & str_detect(profession.y, "international") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(profession_other, "fonctionn") & str_detect(profession_other, "onu") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(profession.y, "diplomat") ~ 1112,
    is.na(ISCO) & (str_detect(profession.y, "fonction|emplo|aucune") | 
                     profession.y %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector, "Ambassade|Administration publique") ~ 242,
    is.na(ISCO) & (str_detect(profession.y, "fonction|emplo|aucune") | 
                     profession.y %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector, "Banque") ~ 242,
    is.na(ISCO) & (str_detect(profession.y, "fonction|emplo|aucune") |
                     profession.y %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(profession.y, "bureau|commerc|immobil") ~ 411,
    is.na(ISCO) & (str_detect(profession.y, "fonction|emplo|aucune") | 
                     profession.y %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector, "transports") ~ 832,
    is.na(ISCO) & (str_detect(profession.y, "fonction|emplo|aucune|analyst") | 
                     profession.y %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector, "Information et communication") ~ 251,
    is.na(ISCO) & str_detect(profession.y, "ong|bug") & str_detect(job_sector_other, "ong|organisation internationale") ~ 2635, #diplomats?
    
    
    # Government official
    is.na(ISCO) & str_detect(job_sector_other, "office des poursuites") ~ 335,
    
    ## Gestionnaire
    is.na(ISCO) & str_detect(profession.y, "gestion|administrat|responsabl|coordinat") & str_detect(job_sector, "Banque") ~ 241,
    is.na(ISCO) & str_detect(profession.y, "gestion|administrat|responsabl|coordinat|commerc") & str_detect(job_sector, "Administration publique") ~ 242,
    is.na(ISCO) & profession.y %in% c("gestionnaire", "controleur de gestion", "business analyst") ~ 242,
    
    ## Public relations
    is.na(ISCO) & str_detect(profession.y, "communication|event manager|evenem") & !str_detect(profession.y, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(job_sector_other, "communication|event manager|evenem") & !str_detect(profession.y, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(profession.y, "production") ~ 243,
    
    # Commerce
    is.na(ISCO) & str_detect(profession.y, "vendeu") ~ 522,
    is.na(ISCO) & str_detect(profession.y, "merchandiser") ~ 524,
    is.na(ISCO) & str_detect(profession.y, "commerce|vent|agent|fonction|responsabl|magasin") & str_detect(job_sector, "Commerce") ~ 524,
    is.na(ISCO) & str_detect(profession.y, "marketing manager") ~ 242,
    is.na(ISCO) & str_detect(profession.y, "achat|achet") ~ 3323,
    is.na(ISCO) & str_detect(profession.y, "courtier") ~ 3324,
    is.na(ISCO) & str_detect(profession.y, "caiss") ~ 523,
    is.na(ISCO) & str_detect(profession.y, "sales|vent") ~ 243,
    is.na(ISCO) & str_detect(profession.y, "immobil") ~ 411,
    is.na(ISCO) & str_detect(profession.y, "commerc") ~ 243,
    is.na(ISCO) & str_detect(profession.y, "charge") & str_detect(profession.y, "client") ~ 4229,
    
    
    
    
    
    # Food
    is.na(ISCO) & str_detect(profession.y, "aide de cuisine") ~ 941,
    is.na(ISCO) & str_detect(profession.y, "cuisinier|cuisiner") ~ 5120,
    is.na(ISCO) & str_detect(profession.y, "laboratoire alimentaire") ~ 213,
    is.na(ISCO) & str_detect(profession.y, "boucher") ~ 7511,
    is.na(ISCO) & str_detect(profession.y, "bar|barman|cafet|serveu|restaura") ~ 513,
    
    
    
    # Cleaners / maids
    is.na(ISCO) & str_detect(profession.y, "demenage") ~ 9333,
    is.na(ISCO) & str_detect(profession.y, "aide menagere|femme de menage|maison|gouvernant|menage") ~ 911,
    is.na(ISCO) & str_detect(profession.y, "proprete hygiene|pro-rete et higiene|agent d'entretien|agent de nettoyage") ~ 912,
    
    
    # Other admin
    is.na(ISCO) & str_detect(profession.y, "administr") ~ 334,
    is.na(ISCO) & str_detect(profession.y, "logisti|magasinier") ~ 432,
    
    
    # Consultants
    is.na(ISCO) & str_detect(profession.y, "conseil") & str_detect(profession.y, "scienti") ~ 242,
    is.na(ISCO) & str_detect(job_sector, "Ambassade") & str_detect(profession.y, "conseil|consult") ~ 2635,
    is.na(ISCO) & str_detect(profession.y, "consultant|business analyst|conseil") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other, "nations uni") & str_detect(profession.y, "advisor") ~ 242,
    
    
    
    # Other childcare
    is.na(ISCO) & str_detect(job_sector, "Petite enfance") ~ 531,
    
    # Performing artists
    is.na(ISCO) & str_detect(profession.y, "pianist|music|musiq|orchest|opera|comedien|comdedien|chorist") ~ 265,
    is.na(ISCO) & str_detect(job_sector, "Arts") & str_detect(profession.y, "artist|mediat|decorat") ~ 265,
    
    # Handicraft
    is.na(ISCO) & str_detect(profession.y, "bijou|horlog|joaill") ~ 731,
    is.na(ISCO) & str_detect(profession.y, "fleurist") ~ 7549,
    
    
    # Construction
    is.na(ISCO) & str_detect(profession.y, "peint") ~ 713,
    is.na(ISCO) & str_detect(profession.y, "calculat|direction de travaux") ~ 3112,
    
    # Miscellaneous
    is.na(ISCO) & str_detect(profession.y, "animatrice") ~ 3412,
    is.na(ISCO) & str_detect(profession.y, "syndicalist|symdicalist") ~ 1114,
    
    # Unclassifiable
    is.na(ISCO) & profession.y %in% c("pas d'activite non salariee","pas d'activite", "pas d'activite independante, seulement salarie",
                                      "aucun","non applicabel", "1 seule activite", "non, j'ai une activite salariee", 
                                      "pas d'aittes avtivites", "0", "::::", "je suis que salarie", "mon travail principal", 
                                      "pas  autre", "employe", "employee", "n/a", "pas d'activite non salariee", "actuellement j'ai une activite salariee",
                                      "non", "aucun", "aucune", "mere au foyer", "sans objet", ".", "ne souhaite pas repondre",
                                      "-", "--", "...", "?", "pas concerne", "na", "n.a.", "re´´´", "j'ai qu'une seule activite" 
    ) ~ 9999,
    is.na(ISCO) & str_detect(profession.y, "pas de plusieurs activite") ~ 9999,
    
    # Final cleanup
    is.na(ISCO) & str_detect(job_sector, "Administration") ~ 731,
    is.na(ISCO) & str_detect(job_sector, "construction") ~ 711,
    is.na(ISCO) & str_detect(job_sector, "Banques") ~ 241,
    is.na(ISCO) & str_detect(job_sector, "Information et communication") ~ 243,
    is.na(ISCO) & str_detect(job_sector, "transports") ~ 3339,
    is.na(ISCO) & str_detect(job_sector, "Ambassade") | str_detect(profession.y, "humanitarian") ~ 2635,
    is.na(ISCO) & str_detect(job_sector, "Hébergement") ~ 141,
    is.na(ISCO) & str_detect(job_sector, "Immobilier") ~ 3334,
    is.na(ISCO) & str_detect(job_sector, "Bureaux d’études") ~ 213,
    is.na(ISCO) & str_detect(job_sector, "production") ~ 311,
    # is.na(ISCO) & str_detect(job_sector, "Commerce") ~ ,
    # is.na(ISCO) & str_detect(job_sector, "Enseignement, recherche") ~ ,
    # is.na(ISCO) & str_detect(job_sector, "Santé, social, médico-social") ~ ,
    # is.na(ISCO) & str_detect(job_sector, "Other") ~ ,
    # is.na(ISCO) & str_detect(job_sector, "Secteur de l’industrie") ~ ,
    
    .default = ISCO
  )) %>% 
  left_join(., occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = profession.y) %>% 
  select(-n) %>% filter(!is.na(Occupation_label)
                        , !ISCO %in% c(9999)
                        ) %>%      # Remove rows with unassigned ISCO codes, to work on what's left
  add_count(
    job_sector,
    # profession.y, 
    sort = FALSE) %>% 
  arrange(desc(n), 
          # profession.y, 
          job_sector) #%>% 
  # select(profession.x, profession.y, profession_other, ISCO, job_sector, supervision, n)  # remove this line once you've got it all done

# Finalizing the file ####

# In the final file, keep only the relevant columns that will be merged with our full dataset
occup_ISCO_final <- occup_ISCO %>% select(participant_id, ISCO)

# # Read teleworkability indices from locally saved file
indices <- read_csv(here("data", "Initial datasets", "Telework ISCO indices.txt")) %>%     # read in the teleworkability indices (low "physical_interaction" = low teleworkability)
  janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# Read teleworkability indices from original source
# indices <- read_csv("https://raw.githubusercontent.com/m-sostero/telework-occupations/master/Telework%20ISCO%20indices.csv") %>%     # read in directly from source GitHub page
#   janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# Read in ISCO code occupation labels
indices_2 <- read_xlsx(here("data", "Initial datasets", "EWCS 3 digit.xlsx")) %>% mutate(isco_3 = as.numeric(isco3d)) %>% select(isco_3,telework, physicalb)
# Merge the indices with the ISCO labels
indices <- left_join(indices, indices_2)

# Read in the classified occupations data
# (generated from "01_prep_a_classify occupations.R")
occup_ISCO_final <- occup_ISCO_final %>% mutate(
  isco_full = ISCO,
  isco_3 = as.numeric(str_sub(ISCO, end = 3)),
  isco_2 = as.numeric(str_sub(ISCO, end = 2)),
  isco_1 = as.numeric(str_sub(ISCO, end = 1))
) %>% 
  select(-c(ISCO)) %>% # Remove the extra columns
  left_join(indices) # Merge with the indices dataframe

# Label the occupations from the ISCO classifications for each code level (from 4 = "full" down to level 1)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_full" = "ISCO")) %>% rename(Occupation_label_full = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_3" = "ISCO")) %>% rename(Occupation_label_3 = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_2" = "ISCO")) %>% rename(Occupation_label_2 = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_1" = "ISCO")) %>% rename(Occupation_label_1 = Occupation_label)

# # Save the final dataset
saveRDS(occup_ISCO_final, here("data","Generated datasets", "Classified_occupations.RDS"), ascii = TRUE)

