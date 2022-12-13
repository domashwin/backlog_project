#WORKSPACE=========
library(tidyverse)
library(lubridate)
library(rstatix)

#DATA IMPORT========
dat <- read.csv("data/data_raw.csv") %>% filter(!is.na(month))
dat <- dat %>% mutate(date = make_date(year,month))
write_rds(dat,"data/dat.rds")

audit <- dat %>% 
  pivot_wider(
    names_from = "cycle", 
    names_prefix = "cyc", 
    values_from = patients )


audit <- audit %>% mutate(delta = cyc2 - cyc1)

audit20 <- audit %>% select(!(delta)) %>%  filter(between(date,as_date("2020-05-01"), as_date("2021-05-01")))
write_rds(audit20, "data/audit20.rds")

#GRAPHS===================

auditline <- ggplot(audit, aes(x = date)) + 
  geom_line(aes(y = cyc1, colour = "blue")) + 
  geom_line(aes(y = cyc2, colour = "red")) + 
  geom_line(aes(y = cyc3, colour = "yellow")) +
  geom_line(aes(y = cyc4, colour = "orange"))
auditline

#May 20 to May 21 GRAPHS ===========  
  
##line graph====
audit20line <- ggplot(audit20, aes(x = date)) + 
  geom_line(aes(y = cyc1, colour = "blue")) + 
  geom_line(aes(y = cyc2, colour = "red")) +
  geom_line(aes(y = cyc3, colour = "yellow")) +
  geom_line(aes(y = cyc4, colour = "orange")) +
  labs(x = "Month", y = "Patients", colour = "Audit Cycle") + scale_color_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  theme_minimal()
audit20line
ggsave("figures/audit20line.jpeg", audit20line)

#ridge plot====
audit20ridge <- ggplot(audit20, aes(x = date)) + 
  geom_area(aes(y = cyc1, fill = "blue")) + 
  geom_area(aes(y = cyc2, fill = "red")) +
  geom_area(aes(y = cyc3, fill = "yellow")) +
  geom_area(aes(y = cyc4, fill = "orange")) +
  labs(x = "Month", y = "Patients", fill = "Audit Cycle") + 
  scale_fill_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  theme_minimal()
audit20ridge
ggsave("figures/audit20ridge.jpeg", audit20ridge)

##bar chart====
dat20 <- audit20 %>% pivot_longer(cols = 'cyc1':'cyc4',
  names_to = "cycle", 
  names_prefix = "cyc", 
  values_to = "patients" )
dat20col<- ggplot(dat20, aes(x = date, y = patients)) + 
  geom_col(aes(fill = cycle), position = "dodge") +
  labs(x = "Month", y = "Patients", fill = "Audit Cycle") +
  scale_fill_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  scale_x_date(limits = as.Date(c('2020-05-01', '2021-05-01')), date_breaks = "months" , date_labels = "%b-%y") +
  theme_minimal()
dat20col
ggsave("figures/dat20col.jpeg",dat20col)


#Stats==========
#note the statistics calculated below are only valid for comparing between two sets of data e.g. cyc1 vs cyc2
# - please see the stats.R script for the stats using audit20
#the paired Wilcoxon rank test is most appropriate in this context with two groups as this is the "before and after" comparison of the totals in each group
# significant p value indicates that there has been a shift in the median of the data
#unable to do this tests on 3 sets of data

wilcox.test(audit20$cyc1, audit20$cyc2, paired = TRUE)

#p is 0.001656 in a two tailed
#p is 0.0008281 in a one tailed where alternative is that median in cyc1 is greater than cyc2

#alternatively a chi squared test for trend (also called: Cochran Armitage test) can be done for the cycles in months:
#first construct a table

chiaudit <- rbind(c(audit20$cyc1), c(audit20$cyc2), c(audit20$cyc3))

dimnames(chiaudit) <- list(
  cycle = c("Cyc1", "Cyc2", "Cyc3"),
  month = c(audit20$mon))

chiaudit

prop_trend_test(chiaudit)

#this significant p value shows that although we expect patient number to increase each month, the proportion in the second cycle is significantly different

