dat_de <- merged %>% 
  mutate(
    # Relevel occupation label2
    Occupation_label_2_new =     factor(str_replace(Occupation_label_2, "associate " ,""))

      # Occupation_label_2 = relevel(factor(Occupation_label_2), ref = "Business and administration professionals"),
  )

levels(dat_de$Occupation_label_2_new)

levels(dat_de$job_sector_en)

table(dat_de$job_sector, dat_de$job_sector_en) %>% flextable::autofit()

dat_de %>% count(job_sector_en) %>% arrange(-n) %>% 
mutate(percent = n / sum(n))
