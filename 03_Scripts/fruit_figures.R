#### SET UP WORKSPACE ####

library(tidyverse)
library(cowplot)

#### REMOVAL EVENT ####

rem.ev <- read_csv("02_Clean_Data/remev_mod.csv")

rem.ev$Streatment[rem.ev$Streatment=="FRUIT"]<-"Fruit"
rem.ev$Streatment[rem.ev$Streatment=="CONTROL"]<-"Control"

names(rem.ev)

ggplot(data=rem.ev)+
  geom_point(aes(x = Streatment,
                 y = prob,
                 color = Streatment),
             size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = prob,
                    ymin = (prob - (1.96*SE)), 
                    ymax = (prob + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                size = 2,
                position = position_dodge(width=0.4))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(x = "Diet Treatment", 
       y = "Probability of removal", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.title.y = element_text(size = 18,
                                    face = "bold"),
        axis.title.x = element_text(size = 18,
                                    face="bold"),
        axis.text.x = element_text(color = "black",
                                   size = 12))

# ggsave("05_Figures/ProbRem2.jpg", height = 5, width = 8)

#### REMOVAL AMOUNT ####

remno <- read_csv("02_Clean_Data/remno_mod.csv")

remno$Streatment[remno$Streatment=="FRUIT"]<-"Fruit"
remno$Streatment[remno$Streatment=="CONTROL"]<-"Control"

ggplot(data=remno)+
  geom_point(aes(x = Streatment,
                 y = response,
                 color = Streatment),
             size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = response,
                    ymin = (response - (1.96*SE)), 
                    ymax = (response + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                linewidth = 2,
                position = position_dodge(width=0.4))+
  scale_y_continuous(limits = c(0, 10))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = "Diet treatment", 
       y = "Removal amount", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.text.x = element_text(color = "black",
                                   size = 12),
        axis.title = element_text(size = 18,
                                  face = "bold"))

# ggsave("05_Figures/CountRem2.jpg", height = 5, width = 8)

#### LATENCY ####

lat <- read_csv("02_Clean_Data/lat_mod.csv")

lat$Streatment[lat$Streatment=="FRUIT"]<-"Fruit"
lat$Streatment[lat$Streatment=="CONTROL"]<-"Control"

ggplot(data=lat)+
  geom_point(aes(x = Streatment,
                 y = response,
                 color = Streatment),
             size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = response,
                    ymin = (response - (1.96*SE)), 
                    ymax = (response + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                size = 2, 
                position = position_dodge(width=0.4))+
  scale_y_continuous(limits = c(0, 24))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = "Diet treatment", 
       y = "Time until removal (h)", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.text.x = element_text(color = "black",
                                   size = 12),
        axis.title = element_text(size = 18,
                                  face = "bold"))

# ggsave("05_Figures/Lat.jpg", height = 5, width = 8)

#### GERMINATION RATE ####

germ.rates <- read_csv("02_Clean_Data/model_avg.csv")

germ.rates[germ.rates == "75% burial"] <- "75%"
germ.rates[germ.rates == "50% burial"] <- "50%"
germ.rates[germ.rates == "No burial"] <- "0%"

ggplot(data = germ.rates)+
  geom_point(aes(x=Treatment,
                 y=p,
                 color=Treatment),
             size=4) +
  geom_errorbar(aes(x=Treatment,
                y=p,
                ymin = lcl ,
                ymax = ucl,
                color=Treatment),
                width=0,
                linewidth=1.5)+
  scale_y_continuous(limits = c(0,0.4))+
  labs(x="Burial proportion",
       y="Germination probability",
       color=NULL)+
  scale_color_manual(values=c("black", "#E69F00", "#56B4E9"))+
  theme(legend.position = "none",
        panel.background = element_rect(fill ="white"),
        panel.border = element_rect(color="black", fill = NA),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.text.x = element_text(color = "black",
                                   size = 12),
        panel.grid.major.y = element_line(color="gray",
                                          linetype = "longdash"),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 18,
                                  face = "bold"),
        )

ggsave("05_Figures/bootstrap_burial.jpg", height = 5, width = 5)

#### Cowplot ####

p1 <- ggplot(data=rem.ev)+
  geom_point(aes(x = Streatment,
                 y = prob,
                 color = Streatment),
             size = 5)+
  annotate("text", x = 2.3, y = 1.00, label = "p = 0.014", size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = prob,
                    ymin = (prob - (1.96*SE)), 
                    ymax = (prob + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                size = 2,
                position = position_dodge(width=0.4))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_y_continuous(limits = c(0, 1.0))+
  labs(x = NULL, 
       y = "Probability of removal", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.title.y = element_text(size = 14,
                                    face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p1

#

p2 <- ggplot(data=remno)+
  geom_point(aes(x = Streatment,
                 y = response,
                 color = Streatment),
             size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = response,
                    ymin = (response - (1.96*SE)), 
                    ymax = (response + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                linewidth = 2,
                position = position_dodge(width=0.4))+
  annotate("text", x = 2.3, y = 10.5, label = "p = 0.003", size = 5)+
  scale_y_continuous(limits = c(0, 10.5))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = NULL, 
       y = "Removal amount", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(color = "black",
                                   size = 12),
        axis.title.y = element_text(size = 14,
                                    face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2

#

p3 <- ggplot(data=lat)+
  geom_point(aes(x = Streatment,
                 y = response,
                 color = Streatment),
             size = 5)+
  geom_errorbar(aes(x = Streatment,
                    y = response,
                    ymin = (response - (1.96*SE)), 
                    ymax = (response + (1.96*SE)),
                    color=Streatment,
                    width = .1),
                size = 2, 
                position = position_dodge(width=0.4))+
  annotate("text", x = 2.3, y = 24.00, label = "p = 0.076", size = 5)+
  scale_y_continuous(limits = c(0, 24))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  labs(x = "Diet treatment", 
       y = "Time until removal (h)", 
       color = NULL)+
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color = "black",
                                   size = 12),
        axis.title = element_text(size = 14,
                                    face = "bold"))



p3

## Gridded

g1 <- plot_grid(p1, p2, p3, 
                ncol=1, 
                align="v")
g1


ggsave("test.jpg", height=10, width=5)
