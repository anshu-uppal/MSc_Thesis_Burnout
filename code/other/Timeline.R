# See https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  tidyverse,
  here,
  scales,
  lubridate,
  readxl
)

df <- read_xlsx(here("data", "Timeline.xlsx"))
df$date <- with(df, ymd(sprintf('%04d%02d%02d', Year, Month, 1)))
type_levels <- c("Questionnaire", "Seroprevalence")
type_colors <- c("#0070C0", "#00B050")

df$Type <- factor(df$Type, levels=type_levels, ordered=TRUE)
