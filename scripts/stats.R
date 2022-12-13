#Workspace ----
library(tidyverse)
library(lubridate)
library(rstatix)
library(gtsummary)

#Data Import ----
audit20 <- read_rds("data/audit20.rds")
dat20 <- read_rds("data/dat20.rds") %>% rename(cycle = Cle)
auditstats <- dat20  %>%  select(patients, cycle)

#Statistics ----

## Wilcox ----
#then calculate Wilcoxon Signed Rank test between the groups (one tailed - aiming for a null hypothesis that 1 is greater than 2, greater than 3, greater than 4 etc)

wilres <- wilcox_test(auditstats, patients ~ cycle)
wilres
wilpaires <- pairwise_wilcox_test(auditstats, patients ~ cycle)
wilpaires

#these results do not show a significant difference in the median of the backlog monthly patient numbers between audit cycles

## Chi ----
#chisquare tests calculated for the total each year
chistats <- dat20 %>% pivot_wider(names_from = "cycle", names_prefix = "C", values_from = patients) 
chistotals <- chistats %>%  select (C1:C4) %>% summarise(C1 = sum(C1), C2 = sum(C2), C3 = sum(C3), C4 = sum(C4))

#unpaired test
chires_totals <- chisq_test(chistotals)
chires_totals
chisq_descriptives(chires_totals)

#pairwise test
chipaires <- pairwise_chisq_gof_test(chistotals)
chipaires

## Trend----

#conducting the Cochran-Armitage test for trend or prop trend test
#first we have to create a sub-headered diminutive table:

trendstats <- audit20 %>%  mutate(audit_month = row_number()) %>% 
  select(audit_month, cyc1, cyc2, cyc3, cyc4) %>% rename(C1  = cyc1, C2 = cyc2, C3 = cyc3, C4 = cyc4)
trendstats

trendres <- prop_trend_test(trendstats, score = c(trendstats$audit_month))
trendres


#Trying to tabulate the results ----
library(gtsummary)
library(labelled)

 datsummary <- chistats %>% unite(monyear, mon, year, sep = " ", remove = TRUE) %>% mutate(aud_mon = row_number()) %>% select(!month)
 
 datsummary <- set_variable_labels(datsummary, monyear = "Month")
 
 datsummary %>% 
   select(monyear, C1, C2, C3) %>%
   tbl_summary()
  
 #trying to make the trend test function in rstatix a function that works in the tbl summary instead of the the built in tests
 rstatix_proptest <- function(data, variable, by, ...) {
   rstatix::prop_trend_test(data, as.formula(glue::glue("{variable} ~ {by}")))
 }


 #tbl_summary summarising a kruskal-wallis test for the data groups 
dat20 %>% 
  unite(monyear, mon, year, sep = " ", remove = TRUE) %>% 
  select(cycle, patients) %>% 
  tbl_summary(by = cycle) %>% 
  add_p(patients ~ 'kruskal.test')


trendaudit <- rbind(c(datsummary$C1), c(datsummary$C2), c(datsummary$C3), c(datsummary$C4))

dimnames(trendaudit) <- list(
  cycle = c("C1", "C2", "C3", "C4"),
  month = c(datsummary$monyear))

trendaudit %>% t() %>% prop_trend_test()

write_rds(trendaudit,"data/trendaudit.rds")
