library(tidyverse)
library(lubridate)
library(rstatix)


dat <- read.csv("data_raw.csv") %>% filter(!is.na(month))
dat <- oct %>% mutate(date = make_date(year,month))

audit <- oct %>% pivot_wider(names_from = "cycle", values_from = patients ) %>% rename(cyc1 = `1`, cyc2 = `2`)
audit <- audit %>% mutate(delta = cyc2 - cyc1)

audit20 <- audit %>% filter(between(date,as_date("2020-05-01"), as_date("2021-05-01")))


auditline <- ggplot(audit, aes(x = date)) + geom_line(aes(y = cyc1, colour = "blue")) + geom_line(aes(y = cyc2, colour = "red"))
auditline

#May 20 to May 21===========  
  

audit20line <- ggplot(audit20, aes(x = date)) + geom_line(aes(y = cyc1, colour = "blue")) + geom_line(aes(y = cyc2, colour = "red"))
audit20line
ggsave("audit20line.jpeg", audit20line)

audit20ridge <- ggplot(audit20, aes(x = date)) + geom_area(aes(y = cyc1, fill = "blue")) + geom_area(aes(y = cyc2, fill = "red"))
audit20ridge
ggsave("audit20ridge.jpeg", audit20ridge)

audit20 <- audit20 %>% mutate(deltaperc = delta/cyc1*100) 
summarise(audit20, median(deltaperc))


#Stats==========

#the paired Wilcoxon rank test is most appropriate in this context with two groups as this is the "before and after" comparison of the totals in each group
# significant p value indicates that there has been a shift in the median of the data

wilcox.test(audit20$cyc1, audit20$cyc2, paired = TRUE)

#p is 0.001656 in a two tailed
#p is 0.0008281 in a one tailed where alternative is that median in cyc1 is greater than cyc2

#alternatively a chi squared test for trend (also called: Cochran Armitage test) can be done for the cycles in months:
#first construct a table

chiaudit <- rbind(c(audit20$cyc1), c(audit20$cyc2))

dimnames(chiaudit) <- list(
  cycle = c("Cyc1", "Cyc2"),
  month = c(audit20$mon))

chiaudit

prop_trend_test(chiaudit)

#this significant p value shows that although we expect patient numebr to increase each month, the proportion in the second cycle is significantly different

