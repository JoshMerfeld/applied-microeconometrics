# in class, week 9

library(tidyverse)
library(fixest)

df <- read_csv("pollution.csv")

head(df)

# first stage: pm25 = wind
# second stage: yield = pm25hat

(sumstats <- df %>%
            group_by(year) %>%
            summarize(mean_pm25 = mean(pm25, na.rm = TRUE),
                        mean_yield = mean(log(yield), na.rm = TRUE),
                        mean_rain = mean(rain_z, na.rm = TRUE),
                        mean_temp = mean(temp_mean, na.rm = TRUE),
                        mean_wind = mean(wind, na.rm = TRUE)))
colnames(sumstats) <- c("Year", "PM 2.5.", "Yield (log)", "Rain (z)", "Temp", "Wind")


# reduced form
# log(yield) = rain + temp + wind, add shrid + year FE
feols(log(yield) ~ rain_z + temp_mean + wind | shrid + year, data = df, cluster = "shrid")

feols(pm25 ~ rain_z + temp_mean + wind | shrid + year, data = df, cluster = "shrid")

# 2SLS
reg1 <- feols(log(yield) ~ rain_z + temp_mean | shrid + year | pm25 ~ wind,
                data = df, cluster = "shrid")

etable(reg1,
        stage = 2,
        fitstat = )


