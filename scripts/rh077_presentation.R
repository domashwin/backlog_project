library(tidyverse)
library(lubridate)


rh <- read_rds("data/rh077_working.rds")

rh %>% summarise(min = min(date), max = max(date))

avoid <- rh %>% filter(encounter_type == "new") %>% group_by(rx_start) %>% summarise(n = n()) %>%  mutate("%" = n/sum(n)*100)
avoid

                                                                                