library(tidyverse)
library(lubridate)


sep <- read.csv("september.csv") %>% filter(!is.na(month))
sep <- sep %>% mutate(date = make_date(year,month, day))


graphbar <- ggplot(sep, aes(x = date, y  = patients)) + geom_col() +theme_minimal()
graphbar
graphline <- ggplot(sep, aes(x = date, y  = patients)) + geom_point() + geom_line() + theme_minimal()
graphline

ggsave("septgraphbar.jpeg", graphbar)
ggsave("septgraphline.jpeg", graphline)


sepgraph <- sep %>% ggplot(aes(x = date, y = patients))
sepgraph + geom_area()
ggsave("sepgraphridge.jpeg")


sep <- sep %>% mutate(prop = (patients/(sum(patients)))) %>% mutate(perc = prop * 100)
ggplot(sep, aes(x = date, y = perc),) + geom_col()
sep %>% summarise(range(patients))


 