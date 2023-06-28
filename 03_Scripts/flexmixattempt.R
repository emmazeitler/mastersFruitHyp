library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)

d2 <- read_csv("02_Clean_Data/fruit_clean.csv")

d3 <- d2 %>% 
  filter(norempair == 0) 

d3$Block.ID <- as.factor(d3$Block.ID)
d3$TimeID <- as.factor(d3$TimeID)
d3$TimeSiteID <- as.factor(d3$TimeSiteID)

library(flexmix)


#####

fit <- flexmix(Latency ~ Streatment, 
               data=d3, 
               k = 2, 
               model = FLXMRglm(family = "gaussian"))

fitted(fit)
