pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  plotly
)
library(plotly)
ggplotly( dat %>% 
  # filter(wfh_trich == "Yes") %>% 
  ggplot(aes(y = physical_interaction, x = social_interaction, color = wfh, label = paste(profession.y, "\n", Occupation_label)))+
  geom_point()+
  labs(color = "Teleworking")+
  geom_vline(xintercept = 0.5)+
  geom_hline(yintercept = 0.4)+
  theme_bw()+
    facet_wrap(.~ wfh)
    )


a <- dat %>% select(Occupation_label, wfh_trich, physical_interaction, social_interaction, teleworkability, burnout_interp_dich30, burn_out)




fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

plot_ly(dat, x = ~physical_interaction, y = ~social_interaction, color = ~wfh_trich, mode = 'markers') %>% 
  add_trace(hoverinfo = 'profession.y')


plot_ly(data = dat, type = 'scatter',
    x = ~physical_interaction, 
    y = ~social_interaction,
    labels = profession.y,
    color = ~wfh_trich,
    hovertemplate = "%{label}")


# Teleworkability and WFH status
dat %>%
  filter(!is.na(teleworkability)) %>% 
  group_by(teleworkability) %>%
  count(wfh_exposure) %>%
  mutate(percent = 100 * n / sum(n)) %>% 
  ggplot(aes(x = wfh_exposure, y = percent))+
  geom_text(aes(label = round(percent,1), vjust = -.1))+
  geom_col()+
  labs(x = "Teleworking frequency and changes since the pandemic", title = "Distribution of Teleworking exposure variable \nby teleworkability of occupation")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 20))+
  facet_wrap(.~teleworkability)

dat %>%
  filter(!is.na(teleworkability)) %>%
  mutate(burnout_interp_dich30 = as.numeric(recode(burnout_interp_dich30, 
                                                 "Mild" = 0,
                                                 "Severe" = 1
  ))) %>% 
  group_by(teleworkability, wfh_exposure) %>%
  summarise(severe_MBI = mean(burnout_interp_dich30, na.rm = TRUE)) %>%
  ggplot(aes(x = wfh_exposure, y = severe_MBI))+
  geom_col()+
  facet_wrap(.~teleworkability)+
  geom_text(aes(label = paste(round(severe_MBI,2)), vjust = -.1))


dat %>%
  # filter(!is.na(teleworkability)) %>%
  mutate(burnout_interp_dich30 = as.numeric(recode(burnout_interp_dich30, 
                                                   "Mild" = 0,
                                                   "Severe" = 1
  ))) %>% 
  group_by(percent_change_cat) %>%
  summarise(severe_MBI = mean(burnout_score, na.rm = TRUE)) %>%
  ggplot(aes(x = percent_change_cat, y = severe_MBI))+
  geom_col()+
  geom_text(aes(label = paste(round(severe_MBI,2)), vjust = -.1))
