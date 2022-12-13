# WORKSPACE====
library(tidyverse)
library(lubridate)
library(zoo)
library(rlang)
library(data.table)
library(formattable)
library(janitor)
library(patchwork)
 
# IMPORT====
rh_orig <- read_csv("data/rh077_clean.csv")
rh <- rh_orig
# 20647 encounters documented with rh077


# TIDYING ====
# adding parsers for the date and adding a date month and character month column
rh$date <- parse_date(rh$date, "%d/%m/%Y")
rh <- rh %>%  mutate("mon" = floor_date(date)) %>% mutate("month" = as.yearmon(mon))
rh$fu_interval[rh$fu_interval == "urgent"] <- 1   #transforming "urgent" follow up into <1 month
rh <- rh %>% select(!("...22"))
             

# FUNCTIONS====

#alternative sum count
sumcount2 <- function(df, .groupvar) {
    summarycount <- df %>%
      group_by(!!.groupvar) %>% 
      summarise(n = n())
    return(summarycount)
  }


# fu_groups using cut
fugroups <- function(x)  {
  num <- as.integer(x)
  cut(num, c(-Inf,1,3,6,12, Inf),
  right = TRUE,
  labels = c("<1 month", "1-3 months", "3-6 months", "6-12 months", ">12 months"))
  }

#all NA removal (for use on the summary tables)
#use janitor for remove_empty - see below
         

# CLEANING====

# add in fu_groups column (re-do)
rh$fu_group <- fugroups(rh$fu_interval)


# SUMMARIES====
#generated summary counts of all of the different, relevant columns in the rh077 sheet
vars <- c(rh %>% select(encounter_type, clinic, cons_type, rx_choice, fu, form, fu_clinic, fu_group, month, avoid, procedures, ultrasound, injections, rx_start, antimalarial))

sum <- vars %>% 
  set_names(names(vars)) %>%    #setting the names from the variables
  map_dfr(~ sumcount2(rh, .x), .id = "column") #mapping these all to a new data frame containing the summary counts of them all

sum <- sum %>% rename("variable" = column, "n" = n, "Type" = "<chr>", "Factor" = '<fct>', "Month" = '<yearmon>', "Y/N" = '<lgl>') 

#saving outputs
write_rds(sum, "data/summarycount_output.rds")
write_csv(sum, "data/summarycount_output.csv")
write_csv(sum, "tables/summarycount_output.csv")  #new tables/ directory to save data tables of use in


## Split summary table ====
#alternative method do the above, which creates one object containing all of these lists
sumsplit <- split(sum, sum$variable)

#removing the empty columns

sumsplit <-  map(sumsplit, ~remove_empty(.x, c("cols")))

#TABLES====
#making the tables look nice - adding percentages columns
#creating percentage columns
sumsplit <- sumsplit %>% map(~ mutate(., percentage = n/sum(n)*100))

#saving the outcome summary tables
sum_filenames_csv <- paste0("tables/sumcounts/",names(sumsplit), ".csv")
sum_filenames_txt <- paste0("tables/sumcounts/",names(sumsplit), ".txt")
walk2(sumsplit, sum_filenames_csv,  write_csv)
walk2(sumsplit, sum_filenames_txt, write.table)
#making the tables pretty in power point



#GRAPHS====

#Creating a generic plot formula to replicate
plot_this <- function(a) {
  b <- if (!is.numeric(a[[1]])) {
    a[[1]] } else {
      a[[2]]
    }
  ggplot(data = a, aes(x = b , y = n, fill = b )) +
    geom_col(position= "dodge", show.legend = FALSE) + theme_minimal()
  
}
#this creates the plot I want it to create


sumgraphs_prep <- map(sumsplit, ~remove_constant(.x, na.rm = TRUE, quiet = TRUE))
set_names(sumgraphs_prep, nm = c("antimalarial","avoid","clinic", "cons_type","encounter_type", "form","fu","fu_clinic","fu_group","injections","month","procedures","rx_choice","rx_start","ultrasound"))

#removed a for loop here

graphs <- map2(sumgraphs_prep, names(sumgraphs_prep),
               ~ plot_this(.x) + ggtitle(.y)
               )
#now save these as graphs
for (i in 1:length(graphs)) {
  ggsave(filename=paste0("graph_", i,".png"), 
         plot=graphs[[i]], device = "png", path = "figures/rh077/sumcounts/")
  }

#pdf option
pdf("figures/rh077/sumcounts/plots.pdf")
for (i in 1:3) {
  print(graphs[[i]])
}
dev.off() 

