#### SET UP WORKSPACE ####
library(tidyverse)

d1 <- read.csv("02_Clean_Data/Fruit_csv.csv") 

#### Convert to factors ####

d1$Block.ID <- as.factor(d1$Block.ID)
d1$TimeID <- as.factor(d1$TimeID)
d1$TimeSiteID <- paste(d1$TimeID, d1$Block.ID, sep="_")
d1$TimeSiteID <- as.factor(d1$TimeSiteID)
d1$PairID <- as.factor(d1$PairID)

#### Correct latency ####
d1$Latency <- as.numeric(d1$Latency)
lat <- d1[, c("Latency")]
lat[is.na(lat)] <- 24
d1[, c("Latency")] <- lat

#### Correct Burn Type ####

bt <- d1[, c("Btreatment")]
bt[is.na(bt)] <- "SCRUB"
d1[, c("Btreatment")] <- bt

## --------------- CREATE DF FOR REMOVAL EVENT AND LATENCY -------------------

fruit_rem <- d1 %>% 
  select(PairID, Streatment, Rem) %>% 
  filter(Streatment == "FRUIT") %>% 
  select(-Streatment) %>% 
  rename(fruit = Rem)

control_rem <- d1 %>% 
  select(PairID, Streatment, Rem) %>% 
  filter(Streatment == "CONTROL") %>% 
  select(-Streatment) %>% 
  rename(fruit = Rem)

norem <- fruit_rem %>% 
  merge(control_rem, by="PairID") %>% 
  unite("norempair", 2:3, sep = "") 

norem$norempair[norem$norempair == '11'] <- '0'
norem$norempair[norem$norempair == '01'] <- '0'
norem$norempair[norem$norempair == '10'] <- '0'
norem$norempair[norem$norempair == '00'] <- '1'

d1 <- merge(d1, norem, by="PairID")

#### Finished ####

write_csv(d1, "02_Clean_Data/fruit_clean.csv")
