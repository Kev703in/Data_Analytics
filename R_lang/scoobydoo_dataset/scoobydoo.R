library(readr)
library(readxl)
library(tidyverse)

scoobydoo <- read_csv("C:/Users/kev70/OneDrive/Desktop/Code to git/Data_Analytics/scoobydoo.csv")

view(scoobydoo)

mean_runtime = mean(scoobydoo$run_time)
mean_runtime

mean(scoobydoo$imdb)
mean(scoobydoo$imdb , na.rm = TRUE)
# [1] NA
# Warning message:
# In mean.default(scoobydoo$imdb) :
#   argument is not numeric or logical: returning NA"

## string NULL instead of na, so converting
scoobydoo$imdb[scoobydoo$imdb == "NULL"] <- NA

glimpse(scoobydoo)
is.numeric(scoobydoo$imdb)

## though they are technically NA, need to converted to numeric
scoobydoo$imdb <- as.numeric(scoobydoo$imdb)

mean(scoobydoo$imdb , na.rm = TRUE)
