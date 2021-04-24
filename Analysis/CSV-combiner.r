install.packages("tidyverse")
library(dplyr)
library(readr)
df <- list.files(path="C:/Users/jgles/dawg/Analysis/csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

head(df)