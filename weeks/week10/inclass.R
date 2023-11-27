library(tidyverse)
library(readxl)
library(fixest)

#######
# load the xlsx file
#######
df <- read_excel("wdi.xlsx")
colnames(df)[1:3] <- c("country", "iso", "variable")

tempnames <- names(table(df$variable))

df$variable[df$variable==tempnames[1]] <- "empag"
df$variable[df$variable==tempnames[2]] <- "empind"
df$variable[df$variable==tempnames[3]] <- "empservices"
df$variable[df$variable==tempnames[4]] <- "emptopopratio"
df$variable[df$variable==tempnames[5]] <- "gdppc"
df$variable[df$variable==tempnames[6]] <- "gdppcgrowth"


df <- pivot_longer(data = df, cols = -c(1:3))

tempnames <- names(table(df$variable))

tempdf <- df %>% filter(variable==tempnames[1]) %>% select(-c("variable"))
colnames(tempdf)[ncol(tempdf)] <- tempnames[1]
for (i in 2:length(tempnames)){
    tempdf <- cbind(tempdf, (df %>% filter(variable==tempnames[i]))[,ncol(df)])
    colnames(tempdf)[ncol(tempdf)] <- tempnames[i]
}

tempdf[tempdf==".."] <- NA

for (i in tempnames){
    tempdf[i] <- as.numeric(as_vector(tempdf[i]))
}


summary(tempdf)

tempdf$name <- substr(tempdf$name, 1, 4)
tempdf$name <- as.numeric(tempdf$name)
tempdf <- tempdf %>% rename(year = name)



mean(tempdf$empag, na.rm = TRUE)
apply(tempdf[tempnames], 2, mean, na.rm = TRUE)
apply(tempdf[tempnames], 2, sd, na.rm = TRUE)


