# In-class code from week 6

library(tidyverse)
library(fixest)
library(did2s)

# Load data
df <- readRDS("weeks/week5and6/ateretal.rds")
colnames(df)

sums <- df %>%
        group_by(date) %>%
        summarize(adoption = mean(public_defender))
ggplot(sums) + 
    geom_line(aes(x = date, y = adoption))


