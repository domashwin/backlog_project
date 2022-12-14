# MASTER SCRIPT FOR RHEUM OP BACKLOG DATA -----

## Environment ----

library(tidyverse)
library(lubridate)
library(knitr)
library(rstatix)
library(gtsummary)
library(labelled)
library(gridExtra)
library(patchwork)
library(kableExtra)
library(zoo)
library(ggpubr)
library(ggsci)
library(RColorBrewer)
library(cowplot)
library(rlang)
library(data.table)
library(formattable)
library(janitor)
library(extrafont)

## Data Import ----

### Audit Backlog data from September 2022 -----

# backlog audit data up to Sep 22 (Cycle 5)
september_22_backlog_audit <- read_csv("data/september 22 backlog audit.csv")
sepaud <- september_22_backlog_audit 

# forms
forms <- read_csv("data/forms_backlog.csv")



## RESULTS ----

### Backlog - Tables ----
sepaud %>% rename("Date of last encounter" = "yearmon") %>% kable()


### Backlog - Graphs ----

# making the yearmon a true "year mon" class
sepaud2 <-  sepaud
sepaud2$yearmon <-  sepaud2$yearmon %>% as.yearmon("%b-%y")

# longer format to permit graph
sepaud2 <- sepaud2 %>% pivot_longer(cols = c(C1:C5), names_to = "cycle", values_to = "patients")

# factorizing the cylce date to allow heirarchy
sepaud2$yearmon <- sepaud2$yearmon %>% as.character() %>% as_factor()

# taking the names to use in the plot as labels
nms <- c("C1 (baseline)","C2 (+1mo)","C3 (+2mo)","C4 (+6mo)","C5 (+12mo)")

sepaud2$cycle <- sepaud2$cycle %>% str_replace_all(c("C1" = "C1 (baseline)","C2" = "C2 (+1mo)","C3"= "C3 (+2mo)","C4" = "C4 (+6mo)", "C5" = "C5 (+12mo)"))

#### FIGURE 2 ----
# creating a column plot
sep_plot <- ggplot(data = sepaud2, aes(x = yearmon, y = patients)) + geom_col(aes(fill = cycle),  position = "dodge") 
sep_plot <- sep_plot + theme_minimal() + scale_fill_npg(alpha = .8) + ylim(0,600)
sep_plot <- sep_plot + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))  
sep_plot <- sep_plot + labs(tag = "A", y = "Patients", x = "Month", fill = "Audit Cycle")
sep_plot

ggsave("figures/september22/backlog_cols_sep22.png", sep_plot)

##### Alternate barplot in ggpubr style ----

p2 <- ggbarplot(sepaud2, "yearmon", "patients",
                fill = "cycle",
                palette = "npg",
                position = position_dodge(),
) + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))+ labs(y = "Patients", x = "Month", fill = "Audit Cycle", color = NULL)
p2 <- ggpar(p2, legend = "bottom",legend.title = NULL, font.family = "sans")
p2
ggsave("figures/september22/alt_backlog_cols_sep22.png", p2)


### Backlog - Statistics ----




## Forms - Graphs -----
forms %>% group_by(`Audit Cycle`)





