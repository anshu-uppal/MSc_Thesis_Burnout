a <- occup_ISCO %>% filter(str_detect(job_sector, "Other")) %>% select(-c(Occupation_label:job_sector)) %>% arrange(profession.y)
# a <- occup_ISCO %>% filter(str_detect(profession.y, "gestion")) %>%  arrange(job_sector)
a <- occup %>% filter(str_detect(profession.y, "entretien")) %>%  arrange(job_sector)
a <- occup_ISCO %>% filter(str_detect(profession.y, "technicien") & str_detect(job_sector, "industrie|construction|production")) %>%  arrange(job_sector)
# a <- occup_ISCO %>% filter(str_detect(job_sector, "Immobilier")) %>%  arrange(profession.y)
a <- occup_ISCO %>% filter(str_detect(job_sector, "Enseignement")) %>%  arrange(profession.y)
# a <- occup_ISCO %>% filter(str_detect(job_sector, "Santé")) %>%  arrange(profession.y)


# a <- occup %>% filter(str_detect(profession.y|profession_other, "control") & str_detect(profession.y|profession_other, "air|aer"))

a <- occup_ISCO %>% select(profession.x, profession.y, supervision, job_sector, job_sector_other, ISCO, hh_income)
a <- tibble::rowid_to_column(a, "ID")
a <- a %>% rename(profession_category = profession.x, profession = profession.y)
write.csv(a, "chatisco.csv", row.names = FALSE)

table_chat <- a %>% select(-profession_category)
