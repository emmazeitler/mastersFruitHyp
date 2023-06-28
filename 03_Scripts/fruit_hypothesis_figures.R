#Set up Workspace

library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(car)
library(readr)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)
library(dplyr)

d1 <- read.csv("02_Clean_Data/Fruit_csv.csv")

#Tidy data

d1$Block.ID <- as.factor(d1$Block.ID)
d1$TimeID <- as.factor(d1$TimeID)
d1$TimeSiteID <- paste(d1$TimeID, d1$Block.ID, sep="_")
d1$TimeSiteID <- as.factor(d1$TimeSiteID)
d1$PairID <- as.factor(d1$PairID)
d1$Latency1 = gsub("NA", "", d1$Latency) %>% as.numeric()
d1$Hour1 = gsub("NA", "", d1$Hour) %>% as.numeric()

#### Models and means
#Removal Event
modRem1 <- glmmTMB(data = d1, Rem ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=binomial)

emRem <- emmeans(modRem1, ~Streatment, type="response") %>% as.data.frame()

#Amount Removed 
modRemNo2 <- glmmTMB(data = d1, ZRemNo ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=nbinom2)

#Latency
modLat2 <- glmmTMB(data = d1 %>% filter(Rem == "1"), Latency1 ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=gaussian)

#### Figures ####
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot() + 
  labs(x = "Dung type", y = "Probability of removal") + 
  geom_jitter(data=d1 %>% filter(Streatment=='FRUITY'),
              aes(x=Streatment, y=Rem),
              height=0,
              width=.25,
              size=1,
              alpha=.1,
              color="#56B4E9") +
  geom_errorbar(data=emRem, 
                aes(x=Streatment, y=prob, ymin=(prob-SE), ymax=(prob+SE), color=Streatment), 
                width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  geom_point(data=emRem , 
             aes(x=Streatment, y=prob, color=Streatment), 
             size=5, 
             position=position_dodge(width=0.5)) +
  scale_color_manual(values=cbPalette) +
  theme(panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) + theme(text = element_text(size=20)) +
  theme(legend.position="none")

ggplot()+
  geom_jitter(data=d1,
               aes(x=Streatment, y=Rem,
              alpha = 0.5),
              height = 0.4,
              width = 0.25,
              color = "black")+
  geom_point(data = emRem,
             aes(x=Streatment, y=prob, color=Streatment),
             size=5)+
  geom_errorbar(data = emRem,
                aes(x=Streatment, y=prob, ymin=(lower.CL), ymax=(upper.CL), color=Streatment),
                width=.2, lwd=1.25, position=position_dodge(width=0.5))+
  scale_y_continuous(limits = c(0, 1), oob = scales::squish)+ 
  labs(x = "Dung type", y = "Probability of removal")+
  theme(legend.position = "none")
  
