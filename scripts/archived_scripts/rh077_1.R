#Workspace ----
library(tidyverse)
library(lubridate)
library(rstatix)
library(gtsummary)

#Data Import ----
db <- read_csv("rh077.csv", na = "") 
db <- db %>% rename(mrn = 'Please enter the medical record number (MRN).')

#selecting the columns of interest, removing MRN and identifying features
clinics <- db %>% select('mrn', 'Start time', 
                         'What type of consultation did you have with the patient?', 
                         'What type of consultation was it?',              
                         'If this was a virtual consultation (Video or Telephone) then, in your opinion, have you avoided a face to face consultation, i.e. have you satisfactorily "moved the patient on" in their journey of...',
                         'Are you requesting a follow up appointment for this patient?',
                         "When do you want the patient's next appointment to be?", 
                         'Which clinic do you want the patient to attend?') %>% 
  rename(date =   'Start time', 
         cons_class = 'What type of consultation did you have with the patient?', 
         cons_type = 'What type of consultation was it?', 
         avoid = 'If this was a virtual consultation (Video or Telephone) then, in your opinion, have you avoided a face to face consultation, i.e. have you satisfactorily "moved the patient on" in their journey of...',
         followup = 'Are you requesting a follow up appointment for this patient?', 
         followup_int = "When do you want the patient's next appointment to be?", 
         clinc = 'Which clinic do you want the patient to attend?') %>% 
  arrange(date) %>% 
  mutate(id = row_number())

#Parsing the date times correctly given the formats are not standard
clinics$date
mdy <- mdy_hms(clinics$date) 
dmy <- dmy_hms(clinics$date) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give  mdy precedence over dmy 
clinics$date <- mdy
clinics

#Validity ----

#number with blank MRNs-this is 0
clinics %>% filter(is.na(mrn)) %>% summarise(count = n())
#number of duplicated MRNS - e g  more than one visit
count(db, duplicated(db$mrn))

#date range
clinics %>% filter(!is.na(date)) %>% summarise(min = min(date), max = max(date))
#only extracting the date

#extracting date
clinics$date <- as.Date(clinics$date)
print (clinics)


#Analysing the data ----

type_no <-  clinics %>% group_by(cons_type) %>% summarise(count = n()) #data frame of clinic types
type_no %>% filter(count>=30)     #summary of the number of different types of appointment

type_blank <- type_no %>% filter(is.na(cons_type)) #number of encounters without a type
type_blank

type_tot <- type_no %>% filter(count>=30) %>% summarise(Encouters = sum(count))  
type_tot

type_no %>% filter(count>30) %>% mutate(perc = count/sum(count)*100)


class_no <- clinics %>% group_by(cons_class) %>% summarise(count = n()) #data frame of clinic classes
class_no %>% filter(count>=30)

class_blank <- class_no %>% filter(is.na(cons_class))
class_blank

class_tot <- class_no %>% filter(count>=30) %>% summarise(Encouters = sum(count))    #total of how many clinics there are
class_tot

class_no %>% filter(count>30) %>% mutate(perc = count/sum(count)*100)


type_tot == class_tot #this is false, suggesting that there is not a complete data set, i e  not every clinic with a class has a type

#Visualisation----
#creating the groups
by_type <- clinics %>% group_by(cons_type) 

by_class <- clinics %>% group_by(cons_class)

type_bar <- type_no %>% filter(count>30) %>%  ggplot(aes(x = cons_type, y = count)) + geom_col()
type_bar
ggsave("type_bar.jpg")

class_bar <- class_no %>% filter(count>30) %>%  ggplot(aes(x = cons_class, y = count)) + geom_col() 
class_bar
ggsave("class_bar.jpg")

                   