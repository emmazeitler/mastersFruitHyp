library(tidyverse)

fruit <- read_csv("Fruit_csv.csv")
fruit$Latency1 = gsub("NA", "", fruit$Latency) %>% as.numeric()

test <- fruit %>% 
  group_by(Streatment) %>% 
  summarize(x=mean(ZRemNo, na.rm=TRUE))

test2 <- fruit %>% 
  group_by(Streatment) %>% 
  summarize(x=mean(Rem, na.rm=TRUE))

test3 <- fruit %>% 
  group_by(Streatment) %>% 
  summarize(x=mean(Latency1, na.rm=TRUE))

