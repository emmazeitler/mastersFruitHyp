## --------------- HEADER ------------------------------------------------------
## Script name: dung-beetle-sim.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-07-16
## Date Last modified: 2022-07-16
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script takes germination data from the literature
## and simulates germination probability based on different rates of burial
## associated with fruit-based scat derived from experimental data. 

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

## --------------- TEST FOR OUTLIERS  ------------------------------------------

# Test for outliers
library(outliers)
grubbs.test(beetle.burial)
grubbs.test(no.burial)

# Remove outliers and repetitive values from no burial
beetle.burial <- c(0.27, 0.05, 0.2, 0.65, 0.55, 0.59, 0.73, 0.65, 0.35, 0.16, 0.31, 0.3, 0.28, 0.26, 0.68, 0.58, 0.6, 0.11)
no.burial <- c(0.00, 0.00, 0.05, 0.1, 0.1, 0.1, 0.1, 0.1, 0.15, 0.07, 0.08, 0.05, 0.11, 0.02, 0.45, 0.38, 0.44, 0.12)

## --------------- BOOTSTRAP SAMPLING MEANS  -----------------------------------

# Beetle burial
beetle.burial.boot <- vector()
for(i in 1:1000){
	beetle.burial.boot[i] <- mean(sample(beetle.burial, replace = TRUE, 18))
}

hist(beetle.burial.boot)

# No burial
no.burial.boot <- vector()
for(i in 1:1000){
	no.burial.boot[i] <- mean(sample(no.burial, replace = TRUE, 18))
}

hist(no.burial.boot)


## --------------- RUN THE SIMULATION ------------------------------------------

# Control
p.remove.con <- 0.50
p.no.remove.con <- 1-p.remove

# Make vector
control.germ.success <- vector()

for(i in 1:1000){
	draw <- runif(1) # Generate number between 0-1
	if(draw < 0.50){ # If it's less than 50
		p <- sample(no.burial.boot, 1) # Draw from the no burial
	} else { # Otherwise
		p <- sample(beetle.burial.boot, 1) # Draw from beetle burial
	}
	control.germ.success[i] <- p # Stick the value for this loop in the vector
}

hist(control.germ.success)

# Fruit
p.remove.fruit <- 0.69
p.no.remove.fruit <- 1-p.remove.fruit

fruit.germ.success <- vector()

for(i in 1:1000){
	draw <- runif(1)
	if(draw < 0.25){
		p <- sample(no.burial.boot, 1)
	} else {
		p <- sample(beetle.burial.boot, 1)
	}
	fruit.germ.success[i] <- p
}

hist(fruit.germ.success)

## --------------- CREATE DATA FRAMES ------------------------------------------

control.germ.success <- as.data.frame(control.germ.success) # convert to df
names(control.germ.success)[1] <- "Probability" # add column name for values
control.germ.success$Treatment <- "Control scat" # add treatment value

fruit.germ.success <- as.data.frame(fruit.germ.success)
names(fruit.germ.success)[1] <- "Probability"
fruit.germ.success$Treatment <- "Fruit scat"

no.burial.germ.success <- as.data.frame(no.burial.boot)
names(no.burial.germ.success)[1] <- "Probability"
no.burial.germ.success$Treatment <- "No burial"


comb <- rbind(control.germ.success, fruit.germ.success, no.burial.germ.success)
## --------------- CALCULATE MEANS AND CI --------------------------------------

comb.sum <- comb %>% 
	group_by(Treatment) %>% 
	summarize(p = mean(Probability),
						n = n(),
						se = sqrt(p*(1-p)/n),
						margin = qnorm(0.995)*sqrt(p*(1-p)/n),
						lcl = p - margin,
						ucl = p + margin)

## --------------- CONFIDENCE INTERVAL FIGURE ----------------------------------

comb.sum$Treatment <- as_factor(comb.sum$Treatment)
levels(comb.sum$Treatment)[levels(comb.sum$Treatment) =="Control scat"] <- "50% burial"
levels(comb.sum$Treatment)[levels(comb.sum$Treatment) =="Fruit scat"] <- "75% burial"

comb.sum$Treatment <- factor(comb.sum$Treatment, levels=c('No burial', '50% burial',
																											 '75% burial'))

# write_csv(comb.sum, "02_Clean_Data/model_avg.csv")

ggplot(comb.sum, aes(x = Treatment, y = p, fill = Treatment))+
	geom_linerange(aes(ymin = lcl, ymax = ucl), 
								 size = 2,  position = position_dodge(0.25))+
	geom_point(size = 3, color = "black", pch=21, stroke = 3,
						 position = position_dodge(0.25))+
	scale_fill_manual(values = c("white", "grey", "black"))+
	scale_y_continuous(limits = c(0,.5))+
	xlab("")+
	ylab("Germination probability")+
	theme_bw()+
	theme(text = element_text(size = 30))+
	theme(panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				legend.position = 'none',
				legend.title = element_blank(),
				legend.background = element_blank(),
				plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
				axis.title.x = element_text(face = "bold"))+
	ggtitle("Dung beetle bootstrap simulaton")

############################################################################

ggplot(comb.sum, aes(x=Treatment, y=p)) +
  geom_point(aes(color = Treatment), size = 4) +
  geom_linerange(aes(ymin = lcl, ymax = ucl, color = Treatment), 
                 size = 2,  position = position_dodge(0.25))+
  theme(legend.position = "none") +
  ylab("Germination Probability") +
  xlab("Burial Proportion") +
  theme(text = element_text(size = 24)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_manual(values = c("black", "#E69F00", "#56B4E9")) +
  theme_bw()

## --------------- OLD CALCULATE EFFECT SIZE -----------------------------------
library(effsize)

# Check assumptions
# Equal variance among groups

# Fruit vs. control
fruit.con.es <- cohen.d(fruit.germ.success$Probability, control.germ.success$Probability)
fruit.con.es.df <- data_frame("Comparison" = "Fruit.vs.con",
															"Estimate" = fruit.con.es$estimate,
															"lcl" = fruit.con.es$conf.int[1],
															"ucl" = fruit.con.es$conf.int[2])

# Fruit vs. no burial
fruit.nb.es <- cohen.d(fruit.germ.success$Probability, no.burial.germ.success$Probability)
fruit.nb.es.df <- data_frame("Comparison" = "Fruit.vs.nb",
														 "Estimate" = fruit.nb.es$estimate,
														 "lcl" = fruit.nb.es$conf.int[1],
														 "ucl" = fruit.nb.es$conf.int[2])

# Control vs. no burial
con.nb.es <- cohen.d(control.germ.success$Probability, no.burial.germ.success$Probability)
con.nb.es.df <- data_frame("Comparison" = "Con.vs.nb",
													 "Estimate" = con.nb.es$estimate,
													 "lcl" = con.nb.es$conf.int[1],
													 "ucl" = con.nb.es$conf.int[2])

es.df <- rbind(fruit.con.es.df, fruit.nb.es.df, con.nb.es.df)


## --------------- OLD EFFECT SIZE FIGURE --------------------------------------

es.df <- es.df[-1,]

ggplot(es.df, aes(x = Comparison, y = Estimate))+
	geom_linerange(aes(ymin = lcl, ymax = ucl), size = 3)+
	geom_point(size = 2, color = "black", stroke = 3)+
	xlab("Germination probability")+
	ylab("")+
	theme_bw()+
	theme(text = element_text(size = 30))+
	theme(panel.grid.minor.y = element_blank(),
				panel.grid.major.y = element_blank(),
				legend.position = c(0.24, 0.9),
				legend.title = element_blank(),
				legend.background = element_blank(),
				plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
				axis.title.x = element_text(face = "bold"))+
	ggtitle("Effect size")

## --------------- OLD NO BURIAL -----------------------------------------------
no.burial.p <- mean(no.burial.boot)
no.burial.n <- length(no.burial.boot)
no.burial.se <- sqrt(no.burial.p*(1-no.burial.p)/no.burial.p)
no.burial.margin = qnorm(0.975)*sqrt(no.burial.p*(1-no.burial.p)/no.burial.n)
no.burial.lcl = no.burial.p-no.burial.margin
no.burial.ucl = no.burial.p+no.burial.margin

no.burial <- data_frame("Treatment" = "No burial", "p" = no.burial.p, 
												"n" = no.burial.n, "se" = no.burial.se, 
												"margin" = no.burial.margin, "lcl" = no.burial.lcl, 
												"ucl" = no.burial.ucl, "Placeholder" = "yep")

comb.sum <- rbind(comb.sum, no.burial)

no.burial.boot <- as_data_frame(no.burial.boot)
names(no.burial.boot)[1] <- "Probability"
no.burial.boot$Treatment <- "No burial"

no.burial.boot.sum <- no.burial.boot %>% 
	group_by(Treatment) %>% 
	summarize(p = mean(Probability),
						n = n(),
						se = sqrt(p*(1-p)/n),
						margin = qnorm(0.975)*sqrt(p*(1-p)/n),
						lcl = p - margin,
						ucl = p + margin)

## --------------- OLD FIGURES -------------------------------------------------

# Version 2
# Rearrange factor values
comb.sum$Treatment <- factor(comb.sum$Treatment, 
														 levels = c("Control scat", "No burial", "Fruit scat"))

ggplot(comb.sum, aes(x = p, y = Placeholder, fill = Treatment))+
	geom_point(size = 3, color = "black", pch=21, stroke = 3,
						 position = position_dodge(0.25))+
	geom_linerange(aes(xmin = lcl, xmax = ucl), 
								 size = 2,  position = position_dodge(0.25))+
	scale_fill_manual(values = c("white", "gray", "#21918c"))+
	xlab("Germination probability")+
	ylab("")+
	theme_bw()+
	theme(text = element_text(size = 30))+
	theme(panel.grid.minor.y = element_blank(),
				panel.grid.major.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				legend.position = c(0.24, 0.9),
				legend.title = element_blank(),
				legend.background = element_blank(),
				plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
				axis.title.x = element_text(face = "bold"))+
	coord_fixed(1/10)+
	guides(fill = guide_legend(nrow = 1))+
	ggtitle("Dung beetle bootstrap simulaton")

# Version 3 
ggplot(comb.sum, aes(x = p, y = Placeholder, fill = Treatment))+
	geom_linerange(aes(xmin = lcl, xmax = ucl), 
								 size = 1.5,  position = position_dodge(0.25))+
	geom_point(size = 5, color = "black", pch=21, stroke = 1,
						 position = position_dodge(0.25))+
	scale_fill_manual(values = c("gray", "#21918c"))+
	xlab("Germination probability")+
	ylab("")+
	scale_x_continuous(limits = c(0,.4))+
	theme_bw()+
	theme(text = element_text(size = 30))+
	theme(panel.grid.minor.y = element_blank(),
				panel.grid.major.y = element_blank(),
				axis.text.y = element_blank(),
				axis.ticks.y = element_blank(),
				legend.position = c(0.75, 0.9),
				legend.title = element_blank(),
				legend.background = element_blank(),
				plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
				axis.title.x = element_text(face = "bold"))+
	coord_fixed(1/7)+
	guides(fill = guide_legend(nrow = 1))+
	ggtitle("Dung beetle bootstrap simulaton")+
	geom_vline(xintercept = 0.09, linetype = "dashed", color = "red", size = 1)+
	annotate(geom = "text", x = 0.05, y = 1.4, label = "No burial", size = 8)
