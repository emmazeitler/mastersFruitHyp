##### SET UP WORKSPACE ####
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

#### Removal Event Model ####

modRem1 <- glmmTMB(data = d2, Rem ~ Streatment * Btreatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)

emmeans(modRem1, ~Streatment, type = "response")
remev.mod <- emmeans(modRem1, ~Streatment, type = "response") %>% as.data.frame()

confint(remev.mod, method = "boot", nsim = 1000)

# write_csv(remev.mod, "02_Clean_Data/remev_mod.csv")

#### Removal Amount Model ####

modRemNo2 <- glmmTMB(data = d3, ZRemNo ~ Streatment * Btreatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=nbinom2)

res_RemNo2 <- simulateResiduals(fittedModel = modRemNo2, n = 250)
hist(res_RemNo2)
plot(res_RemNo2)

Anova(modRemNo2)
summary(modRemNo2)

emmeans(modRemNo2, ~Streatment, type="response")
remno.mod <- emmeans(modRemNo2, ~Streatment, type="response") %>% as.data.frame()

# write_csv(remno.mod, "02_Clean_Data/remno_mod.csv")

#### Latency Model ####
hist(d3$Latency)

modLat2 <- glmmTMB(data = d3, Latency ~ Streatment * Btreatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family= Gamma(link="log"))

res_Lat2 <- simulateResiduals(fittedModel = modLat2, n = 250)
hist(res_Lat2)
plot(res_Lat2)

Anova(modLat2)
summary(modLat2)

emmeans(modLat2, ~Streatment, type = "response")
lat_mod <- emmeans(modLat2, ~Streatment, type = "response") %>% as.data.frame()
write_csv(lat_mod, "02_Clean_Data/lat_mod.csv")
