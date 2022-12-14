#WORKSPACE=========
library(tidyverse)
library(lubridate)
library(rstatix)
library(cowplot)


#DATA IMPORT========
dat <- read.csv("data/data_raw.csv") %>% filter(!is.na(month))  #importing from the raw data .csv file
dat <- dat %>% mutate(date = make_date(year,month))             #converting the text dates into a date
write_rds(dat,"data/dat.rds")                                   #saving this object for easy recall in other scripts

audit <- dat %>%                                  #making an audit table with the "cyc" names
  pivot_wider(
    names_from = "cycle", 
    names_prefix = "cyc", 
    values_from = patients )


audit <- audit %>% mutate(delta = cyc2 - cyc1)  #now defunct making a delta column

audit20 <- audit %>% select(!(delta)) %>%  
  filter(between(date,as_date("2020-05-01"), as_date("2021-05-01")))  #making a data frame with only the May 2020 to May 2021 data




#GRAPHS===================

auditline <- ggplot(audit, aes(x = date)) +     #line graph with each different cycle
  geom_line(aes(y = cyc1, colour = "blue")) + 
  geom_line(aes(y = cyc2, colour = "red")) + 
  geom_line(aes(y = cyc3, colour = "yellow")) +
  geom_line(aes(y = cyc4, colour = "orange"))
auditline

##May 20 to May 21 GRAPHS ===========  

#note to self: make a function to make the labelling easier when I get time to

###line graph====
audit20line <- ggplot(audit20, aes(x = date)) +   #line graph from the 2020 data with each cycle
  geom_line(aes(y = cyc1, colour = "blue")) + 
  geom_line(aes(y = cyc2, colour = "red")) +
  geom_line(aes(y = cyc3, colour = "yellow")) +
  geom_line(aes(y = cyc4, colour = "orange")) +
  labs(x = "Month", y = "Patients", colour = "Audit Cycle") + scale_color_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  theme_minimal()
audit20line
ggsave("figures/audit20line.jpeg", audit20line)

###ridge plot====
audit20ridge <- ggplot(audit20, aes(x = date)) +  #ridge plot for the same time periods
  geom_area(aes(y = cyc1, fill = "blue")) + 
  geom_area(aes(y = cyc2, fill = "red")) +
  geom_area(aes(y = cyc3, fill = "yellow")) +
  geom_area(aes(y = cyc4, fill = "orange")) +
  labs(x = "Month", y = "Patients", fill = "Audit Cycle") + 
  scale_fill_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  theme_minimal()
audit20ridge


## MUCH BETTER RIDGE WITH BETTER COLOURS

dat20ridge <- ggplot(dat20, aes(x = date, y = patients, fill = cycle)) +
  geom_area() + 
  labs(x = NULL, y = "Patients", fill = "Audit Cycle") + 
  scale_fill_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  theme_minimal() 

dat20ridge

###bar chart====
dat20 <- audit20 %>% pivot_longer(cols = 'cyc1':'cyc4',
                                  names_to = "cycle",
                                  names_prefix = "cyc", 
                                  values_to = "patients")     #first have to create a new data frame which is a longer pivot of audit20 (effectively dat but with only the may-may dates)



dat20col<- ggplot(dat20, aes(x = date, y = patients)) +   #column chart with offset for each month
  geom_col(aes(fill = cycle), position = "dodge") +
  labs(x = "Month", y = "Patients", fill = "Audit Cycle") +
  scale_fill_brewer(labels = c("1 (Sep 21)", "2 (Oct 21)", "3 (Nov 21)", "4 (Mar 22)"), type = "qual", palette = "Set2") +
  scale_x_date(limits = as.Date(c('2020-05-01', '2021-05-01')), date_breaks = "months" , date_labels = "%b-%y") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat20col

#making a composite
backlog_fig <- plot_grid(dat20ridge + theme(legend.position = "none"),
                         dat20col + theme(legend.position = "none"),
                         ncol = 1, align = "v", labels = c("A", "B"), label_size = 12 )
backlog_fig

# extracting the legend
fig1_legend <- get_legend(dat20col + theme(legend.box.margin = margin(0, 0, 0, 12)))  # create some space to the left of the legend


# add the legend to the grid  we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
backlog_fig1 <- plot_grid(backlog_fig, fig1_legend, rel_widths = c(3, 0.75)) 
backlog_fig1 

#SAVING/WRITING----
#
write_rds(dat20, "data/dat20.rds") #dat20 table
write_rds(audit20, "data/audit20.rds")      #saving this for posterity

#graphs
ggsave("figures/dat20col.jpeg",dat20col)
ggsave("figures/audit20ridge.jpeg", audit20ridge)
ggsave("figures/dat20ridge.jpg", dat20ridge)

#figure
ggsave("figures/backlog_fig1.jpg", backlog_fig1)


