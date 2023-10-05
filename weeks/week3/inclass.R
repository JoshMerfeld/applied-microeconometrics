# Purpose: This script is for week 3, class 2
# Author: Josh Merfeld
# Date: 2023-10-05
################

# libraries
library(tidyverse)
library(haven)
library(fixest)

# read in data
df <- read_dta("shrugdata.dta")

df <- df %>%
        mutate(prop_female = pc11_pca_tot_f/pc11_pca_tot_p,
                prop_male = pc11_pca_tot_m/pc11_pca_tot_p,
                sexratio_f_to_m = pc11_pca_tot_f/pc11_pca_tot_m,
                state = substr(df$shrid2, 4, 5),
                district = substr(df$shrid2, 7, 9))


reg1 <- feols(sexratio_f_to_m ~ log(pc11_pca_tot_p), data = df)
reg2 <- feols(sexratio_f_to_m ~ log(pc11_pca_tot_p), data = df, vcov = "HC1")
reg3 <- feols(sexratio_f_to_m ~ log(pc11_pca_tot_p) + state, data = df, vcov = "HC1")
reg4 <- feols(sexratio_f_to_m ~ log(pc11_pca_tot_p) | state, data = df, vcov = "HC1")



table <- etable(reg1, reg2, reg3, reg4)



feols(pc11_pca_p_lit ~ pc11_pca_tot_m + pc11_pca_tot_f, data = df, vcov = "HC1")






ggplot() + 
    geom_point(data = df, aes(x = log(pc11_pca_tot_p), y = sexratio_f_to_m)) +
    # add regression line
    geom_smooth(data = df, aes(x = log(pc11_pca_tot_p), y = sexratio_f_to_m), method = "lm", se = FALSE) +
    theme_minimal()














