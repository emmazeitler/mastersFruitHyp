library(tidyverse)

burial <- read_csv("02_Clean_Data/burial_germination_rates.csv")
burial$index <- 1:nrow(burial)
burial_adj <- burial %>% 
  pivot_longer(4:5, names_to = "status", values_to = "germrate")


hist(burial_adj$germrate)

## Dotplot with regression visualization

ggplot(burial_adj, aes(x=index, y=germrate, color = status), size = 4)+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Pair ID")+
  ylab("Germination Rate")+
  theme(legend.position = "none") +
  scale_color_manual(values=c("#0072B2", "#8B7F47"))+
  scale_x_discrete(limits=c(1:18)) +
  theme_bw()

##

burial_lollipop <- burial

burial_lollipop <- burial_lollipop[order(burial_lollipop$no_burial),]

burial_lollipop$x <- 1:nrow(burial)

burial_lollipop <- burial_lollipop %>% 
  mutate(x=LETTERS[1:18])


ggplot(burial_lollipop) +
  geom_segment(aes(x=factor(index), xend=factor(index), y=no_burial, yend=burial), color="grey") +
  geom_point(aes(x=factor(index), y=no_burial), color = "#8B7F47", size = 3) +
  geom_point(aes(x=factor(index), y = burial), color = "#0072B2", size = 3) +
  xlab("Pair ID")+
  ylab("Germination Rate")+
  theme_bw()

#### Test 1 ####

test <- burial_lollipop %>% 
  pivot_longer(4:5, names_to = "germtype", values_to = "germrate" )

class(test$germtype)

ggplot() +
  geom_segment(data = burial_lollipop, 
    aes(x=x, 
        xend=x, 
        y=no_burial, 
        yend=burial), 
    color="grey") +
  geom_point(data=filter(test, germtype=="no_burial"), 
    aes(x=x, 
        y=germrate,
        color = germtype), 
    size = 3) +
  geom_point(data=filter(test, germtype=="burial"),
    aes(x=x,
        y = germrate,
        color=germtype), 
    size = 3) + 
  scale_color_manual(values = c("#0072B2", "#8B7F47"),
                     labels = c("buried", "unburied"))+
  scale_x_discrete(labels=c("A" = "Almeida et al. (2022)",
                            "B" = "Alemida et al. (2022)",
                            "C" = "Andresen (2003)",
                            "D" = "Andresen (2001)",
                            "E" = "Andresen (2003)",
                            "F" = "Andresen (2003)",
                            "G" = "Andresen (2003)",
                            "H" = "Koike et al.(2012)",
                            "I" = "Koike et al.(2012)",
                            "J" = "Koike et al.(2012)",
                            "K" = "Koike et al.(2012)",
                            "L" = "Koike et al.(2012)",
                            "M" = "Andresen (2003)",
                            "N" = "Urrea-Galeano et al. (2019)",
                            "O" = "Andresen and Levey (2004)",
                            "P" = "Andresen (2003)",
                            "Q" = "Andresen (2003)",
                            "R" = "Andresen (2003)"))+
  coord_flip()+
 labs(x = "Germination rate",
      y = "Source publication",
      color = NULL)+
  theme_bw()


#### Test 2 ####
test2 <- test

test2$germtype[test2$germtype=="no_burial"]<-"a_no_burial"
test2$germtype[test2$germtype=="burial"]<-"b_burial"

class(test2$germtype)
table(test2$germtype)

ggplot() +
  geom_segment(data = burial_lollipop, 
               aes(x=x, 
                   xend=x, 
                   y=no_burial, 
                   yend=burial), 
               color="darkgrey",
               linewidth = 1.5) +
  geom_point(data=filter(test2, germtype=="a_no_burial"), 
             aes(x=x, 
                 y=germrate,
                 color = germtype), 
             size = 5) +
  geom_point(data=filter(test2, germtype=="b_burial"),
             aes(x=x,
                 y = germrate,
                 color=germtype), 
             size = 5) + 
  scale_color_manual(values = c("#8B7F47", "#0072B2"),
                     labels = c("Unburied", "Buried"))+
  scale_x_discrete(labels=c("A" = "Almeida et al. (2022)",
                            "B" = "Alemida et al. (2022)",
                            "C" = "Andresen (2003)",
                            "D" = "Andresen (2001)",
                            "E" = "Andresen (2003)",
                            "F" = "Andresen (2003)",
                            "G" = "Andresen (2003)",
                            "H" = "Koike et al. (2012)",
                            "I" = "Koike et al. (2012)",
                            "J" = "Koike et al. (2012)",
                            "K" = "Koike et al. (2012)",
                            "L" = "Koike et al. (2012)",
                            "M" = "Andresen (2003)",
                            "N" = "Urrea-Galeano et al. (2019)",
                            "O" = "Andresen and Levey (2004)",
                            "P" = "Andresen (2003)",
                            "Q" = "Andresen (2003)",
                            "R" = "Andresen (2003)"))+
  coord_flip()+
  labs(y = "Germination rate",
       x = "Source publication",
       color = NULL)+
  theme_bw() +
  theme(axis.text = element_text(size = 12,
                                 color = "black"),
        axis.title = element_text(size = 18,
                                  color="black",
                                  face="bold"),
        legend.text = element_text(size=12,
                                   color="black"))

ggsave("05_Figures/burial_v_nonburial_2.png", height=8, width = 9)
