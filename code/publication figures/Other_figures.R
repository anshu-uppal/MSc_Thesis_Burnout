dat <- tibble(readRDS(here("data", "Generated datasets", "clean_dataset.rds")))

# Proportion diagnosed with burnout in the last 12 months,\nwithin each EE-MBI score interval
dat %>% 
  mutate(burnout_score_cat = cut_width(burnout_score,width = 5, center = 2.5, closed = "left")) %>% 
  group_by(burnout_score_cat) %>% 
  summarise(
    burn_out = mean(burn_out, na.rm = TRUE),
    N = n()) %>% 
  mutate(row = row_number(),
         burnout_interp_dich30 = if_else(row < 7, "Not severe", "Severe")) %>% 
  ggplot(aes(x=burnout_score_cat, y = burn_out, fill = burnout_interp_dich30, ))+
  geom_col(position = "dodge")+
  # geom_bar(position = "dodge", stat = "summary", na.rm = TRUE, fun = "mean")+
  geom_text(aes(label = paste0("n=",N)), vjust = -0.2)+
  theme_classic()+
  scale_fill_manual(values = c("lightseagreen", "#F8766D"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "EE-MBI score in 5-point intervals", y = "Proportion diagnosed with burnout",
       # title = "Proportion diagnosed with burnout in the last 12 months,\nwithin each EE-MBI score interval",
       fill = "EE-MBI classification\n(score \u226530)")+
  theme(axis.text.x = element_text(angle = 20, size = 11),
        legend.position = c(0.2,0.8))

ggsave(file = paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Diagnosed burnout by EE-MBI score.svg"), 
       path = here::here("output", "publication figures"),
       height = 3.8, width = 6)
