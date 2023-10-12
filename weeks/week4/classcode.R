# code for class, 2023-10-12

library(tidyverse)
library(fixest)
library(haven)


df <- read_dta("deserrannoetal.dta")
df <- df %>% filter(is.na(vote)==F)

groupstats <- df %>%
                group_by(group_unique) %>%
                summarize(groupsize = n(),
                            vote = mean(vote))
groupstats <- groupstats %>%
                group_by(vote) %>%
                summarize(sizemean = mean(groupsize),
                            sizesd = sd(groupsize))
groupstats

indstats <- df %>%
            group_by(vote) %>%
            summarize(agemean = mean(age, na.rm = T),
                        agesd = sd(age, na.rm = T))
indstats

statsall <- cbind(indstats, groupstats[,2:3])
statsall



# regressions
# does wealth of COMMITTEE MEMBERS differ by treatment status
feols(wealth ~ vote | branch, 
        cluster = "group_unique", 
        data = df %>% filter(CM==1))
feols(primary_school ~ vote | branch, 
        cluster = "group_unique", 
        data = df %>% filter(CM==1))










