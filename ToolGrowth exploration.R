data("ToothGrowth")
View(ToothGrowth)
library(tidyverse)
filtered_tg <- filter(ToothGrowth, dose == 0.5)
view(filtered_tg)

arrange(filtered_tg,len)
arrange(filter(ToothGrowth, dose == 0.5), len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose== 0.5) %>% 
  arrange(len)

view(filtered_toothgrowth)

filtered_toothgrowth2 <- ToothGrowth %>% 
  filter(dose== 0.5) %>% 
  group_by(supp) %>% 
  summarize(mean_len = mean(len, na.rm = T), .group = "drop")

view(filtered_toothgrowth2)