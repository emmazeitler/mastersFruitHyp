library(tidyr)
library(ggplot2)
library(readr)

burial <- read_csv("burial_germination_rates.csv")
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
  geom_segment(aes(x=x, xend=x, y=no_burial, yend=burial), color="grey") +
  geom_point(aes(x=x, y=no_burial), color = "#8B7F47", size = 3) +
  geom_point(aes(x=x, y = burial), color = "#0072B2", size = 3) +
  ylim(0,0.8) +
  scale_x_discrete(limits=c(1:18)) +
  xlab("Pair ID")+
  ylab("Germination Rate")+
  coord_flip()+
  theme_bw()

burial_lollipop <- burial

burial_lollipop$x <- 1:nrow(burial)

burial_lollipop <- burial_lollipop[order(burial_lollipop$no_burial),]

burial_lollipop <- burial_lollipop %>% 
  mutate(x=LETTERS[1:18])




