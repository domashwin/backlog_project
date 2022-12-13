#MONTHLY BACKLOG REMOTE MANAGEMENT

#WORKSPACE----
library(tidyverse)
library(lubridate)
library(zoo)
library(RColorBrewer)
library(ggsci)

#Import the backlog data table
backlog_data <- as_tibble(read_rds("data/dat.rds"))
#subset for the May 2020 to May 2021 backlog period
backlog_data <- backlog_data %>% filter(!between(date, as.Date("2021-06-01"), as.Date("2021-09-01")))


#Import the remote management form data

remote_managment_data_may22 <- as.tibble(read_csv("data/remote_management_data_may22.csv", col_types = cols(form_date = col_date(format = "%Y-%m-%d"),  previous_appt = col_date(format = "%Y-%m-%d"))))
#1956 remote management forms submitted from all categories

remote_management_data <- remote_managment_data_may22 


remote_management_data$interval <- interval(remote_management_data$previous_appt, remote_management_data$form_date) %/% months(1)

backlog_data <- backlog_data %>% mutate("cycle_yearmon" = 
                          case_when(cycle == 1 ~ "Sep 2021",
                                    cycle == 2 ~ "Oct 2021",
                                    cycle == 3 ~ "Nov 2021",
                                    cycle == 4 ~ "Mar 2022"
                                    )) %>% 
  mutate("cycle_date" =
           case_when(cycle == 1 ~ as.Date("2021-09-17"),
                     cycle == 2 ~ as.Date("2021-10-17"),
                     cycle == 3 ~ as.Date("2021-11-17"),
                     cycle == 4 ~ as.Date("2022-03-04")))
  

backlog_data$cycle_yearmon <- as.yearmon(backlog_data$cycle_yearmon)

remote_management_data$remote_yearmon <- as.yearmon(remote_management_data$form_date)

#Grouping the number of patients seen on the backlog per month
backlog_totals <- backlog_data %>% group_by(cycle_yearmon, cycle_date) %>% summarise(total = sum(patients))  #representing the total number of patients on the backlog as each of these months in the audit cylce

backlog_totals$patients_seen <- c(NA, abs(diff(backlog_totals$total, lag = 1))) 
backlog_totals <-  backlog_totals %>% rowid_to_column("cycle") #calculating the number of patients seen per cycle of the  backlog

#-----

# identifying which audit cycle the remote management forms were sent out during
remote_management_data <- remote_management_data %>% 
  filter(between(form_date, as.Date("2021-09-17"), as.Date("2022-03-04"))) %>% 
  mutate(cycle = 
           case_when(between(form_date, as.Date("2021-09-17"), as.Date("2021-10-16")) ~ 2, #filtering by date
                     between(form_date, as.Date("2021-10-17"), as.Date("2021-11-16")) ~ 3,
                     between(form_date, as.Date("2021-11-17"), as.Date("2022-03-04")) ~ 4)
  ) 

#may need to be more accurate in terms of just patients who are on the backlog may-may?

#creating a table of the total number of remote forms completed per audit cycle
remote_totals <- remote_management_data %>%  group_by(cycle) %>% summarise(forms_completed = n())                     


# join tables ----
forms_data <- left_join(backlog_totals, remote_totals, by = "cycle")
forms_data <- forms_data %>% mutate(percentage_forms = (forms_completed/patients_seen * 100))

# MAY-20- May 21 12 months-----
#alternative method -----

remote_mays_previous <- remote_management_data %>% filter(!is.na(previous_appt))
remote_mays_previous <- remote_mays_previous %>% filter(between(previous_appt, as.Date("2020-05-01"), as.Date("2021-05-30")))


#now same as above grouping + joining just for the remote management forms
remote_totals_mays <- remote_mays_previous %>%  group_by(cycle) %>% summarise(forms_completed = n())                   

# join tables -----
forms_data_mays <- left_join(backlog_totals, remote_totals_mays, by = "cycle")
forms_data_mays <- forms_data_mays %>% mutate(percentage_forms = (forms_completed/patients_seen * 100))
#likely an under-estimate given the lack of complete form data


# graphs ----
forms_long <- forms_data_mays %>% rename("form" = forms_completed, "conventional" = patients_seen) %>%  pivot_longer(cols = c(form, conventional), names_to = "encounter", values_to = "patients") 
forms_long

p <- ggplot(data = forms_long) + geom_col(aes(x = cycle, y = patients, fill = encounter), position = "stack")
p 
 



p <- p +  scale_fill_manual(values = c("#087CBF","#FFC300"), name = "Encounter Type") + 
  xlab("Audit Cycle") + 
  ylab("Patients Removed from Backlog")
p <- p + theme_minimal()
p
g <- ggplot(data = forms_long) + geom_col(aes(x = cycle, y = patients, fill = encounter), position = "fill") + scale_y_continuous(labels = scales::percent) + scale_fill_npg() + theme_minimal()
g

#SAVING
ggsave("figures/backlog_fig2.png", g)
