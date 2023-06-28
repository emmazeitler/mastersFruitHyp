library(tidyverse)
library(glmmTMB)
library(car)
library(ggResidpanel)
library(DHARMa)
library(emmeans)
library(MuMIn)


d1 <- read.csv("02_Clean_Data/Fruit_csv.csv") #Run ZRemNo
#d1 <- read.csv("Fruit_NewDect_csv.csv") #Run Lat1
d1$Block.ID <- as.factor(d1$Block.ID)
d1$TimeID <- as.factor(d1$TimeID)
d1$TimeSiteID <- paste(d1$TimeID, d1$Block.ID, sep="_")
d1$TimeSiteID <- as.factor(d1$TimeSiteID)
d1$PairID <- as.factor(d1$PairID)


 write_csv(d1, "02_Clean_Data/fruit_clean.csv")
##Examine histograms##

##Removal event
ggplot(d1, aes(x = Rem)) + geom_histogram(bins=12, color="white") + facet_wrap(~Streatment, scales="free") + theme_bw(base_size = 16)

##Number of removed scat pellets 
# ggplot(d1, aes(x = ZRemNo)) + geom_histogram(bins=12, color="white") + facet_wrap(~Streatment, scales="free") + theme_bw(base_size = 16)

ggplot(d1, aes(x = ZRemNo)) + geom_histogram(bins=12, color="white") + facet_wrap(~Streatment, scales="free") + theme_bw(base_size = 16)

##Latency to removal event
ggplot(d1, aes(x = Latency1)) + geom_histogram(bins=12, color="white") + facet_wrap(~Streatment, scales="free") + theme_bw(base_size = 16)

##Hour of removal event
ggplot(d1, aes(x = Hour1)) + geom_histogram(bins=12, color="white") + facet_wrap(~Streatment, scales="free") + theme_bw(base_size = 16)

##Create models/plot residuals/calculate emmeans##

##Model for removal event
modRem1 <- glmmTMB(data = d1, Rem ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=binomial)

resRem1 <- simulateResiduals(fittedModel = modRem1, n = 250)
hist(resRem1)
plot(resRem1)

Anova(modRem1)
summary(modRem1)
emmeans(modRem1, ~Streatment, type = "response")
emmeans(modRem1, ~Btreatment, type = "response")

r.squaredGLMM(modRem1)

##Model for number of scat removed

#modRemNo2 <- glmmTMB(data = d1, ZRemNo ~ Btreatment * Streatment + (1|Block.ID) + (1|TimeID), family=nbinom2)

#modRemNo2 <- glmmTMB(data = d1, RemNo1 ~ BTreatment * Streatment + (1|PlotID), family=poisson)

#modRemNo2 <- glmmTMB(data = d1, RemNo1 ~ Btreatment * Streatment + (1|Block.ID) + (1|TimeID), family=nbinom2)

modRemNo2 <- glmmTMB(data = d1, ZRemNo ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=nbinom2)

modRemNo1 <- glmmTMB(data = d1, RemNo ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=nbinom2)


res_RemNo2 <- simulateResiduals(fittedModel = modRemNo2, n = 250)
hist(res_RemNo2)
plot(res_RemNo2)

Anova(modRemNo2)
Anova(modRemNo1)
summary(modRemNo2)
emmeans(modRemNo2, ~Streatment, type="response")

r.squaredGLMM(modRemNo2)

testOverdispersion(res_RemNo2)

##Model for Latency (with Hour interaction)

# modLat1 <- glmmTMB(data = d1 %>% filter(Rem == "1"), Latency1 ~ Btreatment * Streatment * Hour1 + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=gaussian)
# 
# res_Lat1 <- simulateResiduals(fittedModel = modLat1, n = 250)
# hist(res_Lat1)
# plot(res_Lat1)
# 
# 
# Anova(modLat1)
# summary(modLat1)
# emmeans(modLat1, ~Streatment, type = "response")

#r.squaredGLMM(modLat1)

##Model for Latency (without Hour interaction)

modLat2 <- glmmTMB(data = d1 %>% filter(Rem == "1"), Latency1 ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=gaussian)

# modLat3 <- glmmTMB(data = d1 %>% filter(Rem == "1"), Latency1 ~ Btreatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=gaussian)

res_Lat2 <- simulateResiduals(fittedModel = modLat2, n = 250)
hist(res_Lat2)
plot(res_Lat2)

Anova(modLat2)
summary(modLat2)
emmeans(modLat2, ~Streatment, type = "response")

r.squaredGLMM(modLat2) 

bartlett.test(Latency1 ~ Streatment, d1)

##Model for Hour of removal

modHour <- glmmTMB(data = d1 %>% filter(Rem == "1"), Hour1 ~ Streatment + (1|Block.ID) + (1|TimeID) + (1|TimeSiteID), family=gaussian)

res_Hour <- simulateResiduals(fittedModel = modLat2, n = 250)
hist(res_Hour)
plot(res_Hour)

Anova(modHour)
summary(modHour)
emmeans(modHour, ~Streatment, type = "response")

r.squaredGLMM(modHour)

bartlett.test(Hour1 ~ Streatment, d1)

##Make figures##
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##Figure for removal event 
emRem <- emmeans(modRem1, ~Streatment, type="response") %>% as.data.frame()

ggplot(emRem, mapping = aes(x = Streatment, y = prob))

plotRem <- ggplot() + labs(x = "Dung type", y = "Probability of removal") + 
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
  theme(panel.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) + theme(text = element_text(size=20)) +
  theme(legend.position="none")+
  ylim(0,1)


plotRem

# ggsave("05_Figures/ProbRem1.png", height = 5, width = 7)

##Figure for number of scat removed
emRemNo <- emmeans(modRemNo2, ~Streatment, type="response") %>% as.data.frame()

plotRemNo <- ggplot() + labs(x = "Dung type", y = "Count of removed dung") + geom_jitter(data=d1 %>% filter(Streatment=='FRUITY'), aes(x=Streatment, y=Rem),  height=0, width=.25, size=1, alpha=.1, color="#56B4E9") +
  geom_errorbar(data=emRemNo ,aes(x=Streatment, y=response, ymin=(response-SE), ymax=(response+SE), color=Streatment), width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  geom_point(data=emRemNo , aes(x=Streatment, y=response, color=Streatment), size=5, position=position_dodge(width=0.5)) +
  scale_color_manual(values=cbPalette) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +
  theme(legend.position="none") +
  theme(text = element_text(size=20)) +
  ylim(1.5,8)


plotRemNo

ggsave("05_Figures/CountRem1.png", height = 5, width = 7)

##Figure for Latency
emLat <- emmeans(modLat2, ~Streatment, type="response") %>% as.data.frame()

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plotLat <- ggplot(emLat, aes(x = Streatment, y = emmean)) +
  geom_point(aes(color = Streatment), size = 3) + 
  ylim(0,8) +
  scale_color_manual(values = cbPalette) +
  geom_errorbar(aes(x=Streatment, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE), color=Streatment), width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  theme(panel.border = element_rect(color="black", fill=NA, size=2)) +
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +
  theme(text = element_text(size=20)) +
  theme(legend.position="none") +
  theme_bw() +
  labs(x = "Dung type", y = "Latency until removal") 

plotLat

ggsave("05_Figures/Lat1.png", height = 5, width = 7)

##Figure for Hour - do not use for assignment
emH <- emmeans(modHour, ~Streatment, type="response") %>% as.data.frame()

plotHour <- ggplot() + geom_jitter(data=d1 %>% filter(Streatment=='FRUITY'), aes(x=Streatment, y=Rem),  height=0, width=.25, size=1, alpha=.1, color="#56B4E9") +
  geom_errorbar(data=emH ,aes(x=Streatment, y=emmean, ymin=(emmean-SE), ymax=(emmean+SE), color=Streatment), width=.2, lwd=1.25, position=position_dodge(width=0.5)) + 
  geom_point(data=emLat , aes(x=Streatment, y=emmean, color=Streatment), size=5, position=position_dodge(width=0.5)) +
  scale_color_manual(values=cbPalette) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color="black", fill=NA, size=2)) + 
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +
  theme(text = element_text(size=20))
plotHour


