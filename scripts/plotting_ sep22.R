library(dplyr)
library(zoo)
library(tidyverse)
library(ggpubr)
library(ggsci)
library(readr)
september_22_backlog_audit <- read_csv("data/september 22 backlog audit.csv")

d <- september_22_backlog_audit

d$yearmon <-  d$yearmon %>% as.yearmon("%b-%y")



d1 <- d %>% pivot_longer(cols = c(C1:C5), names_to = "cycle", values_to = "patients")

d1$yearmon <- d1$yearmon %>% as.character() %>% as_factor()
nms <- c("C1 (baseline)","C2 (+1mo)","C3 (+2mo)","C4 (+6mo)","C5 (+12mo)")

d1$cycle <- d1$cycle %>% str_replace_all(c("C1","C2","C3","C4","C5"), nms)

pl <- ggplot(data = d1, aes(x = yearmon, y = patients)) + geom_col(aes(fill = cycle),  position = "dodge") 
pl <- pl + theme_minimal() + scale_fill_npg(alpha = .8) + ylim(0,600)
pl <- pl + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))  
pl <- pl + labs(tag = "A", y = "Patients", x = "Month", fill = "Audit Cycle")
pl

ggsave("backlog_cols_sep22.png", pl)

#==================================

f <- read_csv("forms_backlog.csv")

f %>% group_by(`Audit Cycle`)







install.packages("extrafont");library(extrafont)
font_import("Helvetica")



ggbarplot(df2, "dose", "len",
          fill = "supp", color = "supp", palette = "Paired",
          label = TRUE, lab.col = "white", lab.pos = "in")


ggplot(data = d1, aes(x = yearmon, y = patients)) + geom_col(aes(fill = cycle),  position = "dodge") 


p2 <- ggbarplot(d1, "yearmon", "patients",
                fill = "cycle",
                palette = "npg",
                position = position_dodge(),
) + theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))+ labs(y = "Patients", x = "Month", fill = "Audit Cycle", color = NULL)
p2 <- ggpar(p2, legend = "bottom",legend.title = NULL, font.family = "sans")

p2
