#Workspace
library(tidyverse)
library(lubridate)
library(gridExtra)
library(patchwork)
library(kableExtra)
setwd("/Users/dominikkurzeja/Docs/Medicine/Clinical Work/IMT OUH/Rheum QIP/Rheumatology Backlog")

#data import
rmd <- read_csv("data/remote_management_data.csv")
rmd$visit <- parse_date(rmd$visit,  "%d/%m/%Y")
rmd$prev_appt <- parse_date(rmd$prev_appt, "%d/%m/%Y")

#making the vasculitis type key
vasctype <- tibble(code = c(1,2,3,4), type = c("small", "medium", "large", "other"))
#making the clinic types


#Preliminary exploration ====
##number of remote management forms: ====
count(rmd) #1306

###number of forms per clinic type ====
byclinic <- rmd %>% group_by(clinic) %>% summarise(count = n())
byclinic

##incomplete data: ====
no_prev_appt <- rmd %>% filter(is.na(prev_appt)) %>% count() #746
no_prev_appt

#as percentage
no_prev_appt/count(rmd)*100   #57% approx
#more than half of the data 

###incomplete per clinc type ====
naclinic <- rmd %>% filter(is.na(prev_appt)) %>% group_by(clinic) %>%  summarise(count_na = n())
naclinic 

#Quarterly Data ====
dates <- rmd %>% filter(!is.na(prev_appt))

#number of encounters per month: 
dates %>% group_by(month = floor_date(visit, "month")) %>%
  summarize(encounters = n())


#dividing up into quarters starting in Nov 2020 when the data starts
dates <- dates %>% mutate(visit_month = floor_date(visit, "month"))
dates <- dates %>% mutate(quarter = quarter(visit_month, type = "year.quarter", fiscal_start = 11))

#number seen per quartr:
byquarter <- dates %>% group_by(quarter)
byquarter %>% summarise(reviewed= n())



#adding a months interval of time on backlog
dates$interval <- interval(dates$prev_appt, dates$visit) %/% months(1)

#SAVE DATA
write_csv(dates, "data/rmd_dates.csv")

#remove incorrect data for which there was an apparent negative date interval in months (n = 7)
dates <- dates %>% filter(interval >= 0)

dates %>% group_by(interval) %>% summarise(waiting = n())

#displaying numbers seen and waits per quarted
quarter_fu <- dates %>% group_by(quarter) %>% summarise(reviewed = n(), min_wait = min(interval), max_wait = max(interval), median_wait = median(interval))
quarter_fu

#numbers seen and waits per clinic
clinic_fu <- dates %>% group_by(clinic) %>% summarise(reviewed = n(), min_wait = min(interval), max_wait = max(interval), median_wait = median(interval))
clinic_fu

#grouping how many have waited for how long
fu3 <- dates %>%subset(interval<3) %>% summarise("<3 months" = n())
fu6 <- dates %>% subset(interval>=3 & interval <6) %>% summarise("3-6 months" = n())
fu12 <- dates %>% subset(interval>=6) %>% summarise(">6 months" = n())
  #554 in total with a follow up interval
 
fu_duration <- tibble("Time on Backlog" = c("<3 months", "3-6 months", ">6 months"), "Patients" = as.double(c(fu3,fu6,fu12)))
  
  

##Tables for Presentation slides====
tab1 <- quarter_fu %>% select(!min_wait) %>% 
  transmute("3 month quarter (from Nov 20)" = quarter, 
            "No. patients reviewed" = reviewed,
            "Max time on backlog (months)" = max_wait,
            "Median time on backlog (months)" = median_wait)
tab1 <- tab1 %>%  kbl() %>% kable_styling()
tab1

tab2 <- fu_duration  #follow up duration number of patients
tab2 <- tab2 %>%  kbl() %>% kable_styling()
tab2

tab3 <- byclinic %>% 
  transmute("Diagnosis" = clinic, "Patients reviewed (total)" = count) %>% 
  kbl() %>% kable_styling()
tab3

#subsets of month data in different groups
less3 <- dates %>% group_by(clinic, interval) %>% 
  subset(interval <3)
less6 <- dates %>% group_by(clinic, interval) %>% 
  subset(interval >=3 & interval <6)
less12 <- dates %>% group_by(clinic, interval) %>% 
  subset(interval >=6)

less3sum <- less3 %>% group_by(clinic) %>% summarise("<3 months" = n())
less6sum <- less6 %>% group_by(clinic) %>% summarise("3-6 months" = n()) 
less12sum <- less12 %>% group_by(clinic) %>% summarise(">6 months" = n()) 

#reconstructing a table of these subsets by clinic and by grouped time on backlog
clinic_backlog <- less3sum %>% left_join(less6sum) %>% left_join(less12sum)
clinic_backlog <- clinic_backlog %>% pivot_longer(cols = "<3 months":">6 months", names_to = "backlog", values_to = "patients")

#Visualisation====
byclinic_col <- byclinic %>%  ggplot(aes(x = clinic, y = count)) + geom_col(aes(fill = clinic), show.legend = FALSE) + scale_fill_brewer() + theme_minimal() + xlab("Diagnosis") + ylab ("Patients reviewed")
byclinic_col
ggsave("figures/byclinic_col.jpg", byclinic_col)

clinic_backlogcol <- clinic_backlog %>% ggplot(aes(x = clinic)) + geom_col(aes(y = patients, fill = backlog), position = "dodge") + scale_fill_brewer() + theme_minimal() + xlab("Diagnosis") + ylab ("Patients reviewed") 
clinic_backlogcol
ggsave("figures/clinic_backlogcol.jpg", clinic_backlogclol)

quarter_fu$quarter <- as.character(quarter_fu$quarter) 

quarter_fu %>% ggplot(aes(x = quarter, y = reviewed, fill = quarter)) + geom_col() + scale_fill_brewer() + theme_minimal() + xlab("Diagnosis") + ylab ("Patients reviewed") 

                       