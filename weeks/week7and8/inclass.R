# In-class code from week 6

library(tidyverse)
library(fixest)
library(did2s)

# Load data
df <- readRDS("weeks/week5and6/ateretal.rds")
colnames(df)
df <- zap_labels(df)

sums <- df %>%
        group_by(date) %>%
        summarize(adoption = mean(public_defender))
ggplot(sums) + 
    geom_line(aes(x = date, y = adoption)) +
    xlab("Date") + ylab("Proportion adoption") + theme_minimal()

(reg1 <- feols(log_num_arrests ~ public_defender | district + date,
                data = df,
                cluster = "district"))

# DID2S
did2s <- did2s(data = df, yname = "log_num_arrests", first_stage = "1 | district + date", 
                second_stage = "public_defender", treatment = "public_defender", cluster_var = "district")





