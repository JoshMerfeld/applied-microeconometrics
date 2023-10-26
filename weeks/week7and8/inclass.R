# In-class code from week 6

library(tidyverse)
library(fixest)
library(did2s)

# Load data
df <- readRDS("weeks/week5and6/ateretal.rds")
colnames(df)
df <- zap_labels(df)

# Plot
sums <- df %>%
        group_by(date) %>%
        summarize(adoption = mean(public_defender))
ggplot(sums) + 
    geom_line(aes(x = date, y = adoption)) +
    xlab("Date") + ylab("Proportion adoption") + theme_minimal()


# TWFE
(reg1 <- feols(log_num_arrests ~ public_defender | district + date,
                data = df,
                cluster = "district"))


# Wild cluster bootstrap
library(fwildclusterboot)
(reg1 <- feols(log_num_arrests ~ public_defender | district + week_year,
                data = df,
                cluster = "district"))
boot_reg <- boottest(
                    reg1, 
                    clustid = c("district"), 
                    param = "public_defender", 
                    B = 9999,
                    type = "webb"
                    )
boot_reg




# DID2S
(did2s <- did2s(data = df, yname = "log_num_arrests", first_stage = "1 | district + date", 
                second_stage = "public_defender", treatment = "public_defender", cluster_var = "district"))




# event study
df <- df %>%
        mutate(first_date = ifelse(public_defender==1, week_year, NA)) %>%
        group_by(district) %>%
        mutate(first_date = min(first_date, na.rm = TRUE),
                event_weeks = week_year - first_date) %>%
        ungroup()
(did2s2 <- did2s(data = df, yname = "log_num_arrests", first_stage = "1 | district + date", 
                second_stage = "i(event_weeks, ref = 0)", treatment = "public_defender", cluster_var = "district"))

