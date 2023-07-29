## --------------- HEADER ------------------------------------------------------
## Script name: dung-beetle-conditional-prob.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2023-07-06
## Date Last modified: 2023-07-06
## Copyright (c) David S. Mason, 2023
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script takes germination data from the literature
## and calculates the probability of germination based on different rates of 
## burial associated with fruit-based scat derived from experimental data. 

library(tidyverse)

rm(list=ls())

burial.germ.phat <- mean(c(0.27, 0.05, 0.2, 0.65, 0.55, 0.59, 0.73, 0.65, 0.35,
													 0.16, 0.31, 0.3, 0.28, 0.26, 0.68, 0.58, 0.6, 0.11))
no.burial.germ.phat <- mean(c(0.00, 0.00, 0.05, 0.1, 0.1, 0.1, 0.1, 0.1, 0.15, 
												 0.07, 0.08, 0.05, 0.11, 0.02, 0.45, 0.38, 0.44, 0.12))

fruit.beetle.burial.phat <- 0.75
fruit.bettle.no.burial.phat <- 0.25

control.beetle.burial.phat <- 0.5
control.beetle.no.burial.phat <- 0.5

# Fruity scat

# Fruity scat probability of germination after burial by beetles
fruit.beetle.burial.germ.phat <- fruit.beetle.burial.phat*burial.germ.phat

# Fruity scat probability of germination with no beetle burial
fruit.beetle.no.burial.germ.phat <- fruit.bettle.no.burial.phat*no.burial.germ.phat

# Combined probability of seeds in fruity scat germinating
fruity.scat.phat <- fruit.beetle.burial.germ.phat+fruit.beetle.no.burial.germ.phat

# Control scat

# Control scat probability of germination after burial by beetles
control.beetle.burial.germ.phat <- control.beetle.burial.phat*burial.germ.phat

# Control scat probability of germination with no beetle burial
control.beetle.no.burial.germ.phat <- control.beetle.no.burial.phat*no.burial.germ.phat

# Combined probability of seeds in control scat germinating
control.scat.phat <- control.beetle.burial.germ.phat+control.beetle.no.burial.germ.phat

# Create a dataframe
cond.prob <- tibble(Treatment = c('Fruit scat', 'Control scat', 'No burial'),
						 phat = c(fruity.scat.phat, control.scat.phat, no.burial.germ.phat),
						 n = c(74, 74, 74))

cond.prob <- cond.prob |>
	mutate(se = sqrt(phat*(1-phat)/n),
				 margin = qnorm(0.975)*sqrt(phat*(1-phat)/n),
				 lcl = phat - margin,
				 ucl = phat + margin)

cond.prob$Treatment <- as_factor(cond.prob$Treatment)

# write.csv(cond.prob,"02_Clean_Data/cond_prob.csv")

ggplot(cond.prob, aes(x = Treatment, y = phat, fill = Treatment))+
	geom_linerange(aes(ymin = lcl, ymax = ucl), 
								 size = 2,  position = position_dodge(0.25))+
	geom_point(size = 3, color = "black", pch=21, stroke = 3,
						 position = position_dodge(0.25))+
	scale_fill_manual(values = c("#56B4E9", "#E69F00", "white"))+
	scale_y_continuous(limits = c(0,.5))+
	scale_x_discrete(limits = rev,
									 labels = c("0%", "50%", "75%"))+
	xlab("Burial proportion")+
	ylab("Germination probability")+
	theme(text = element_text(size = 30))+
	theme(panel.grid.minor = element_blank(),
	      panel.background = element_rect(fill="white"),
				panel.grid.major = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.text = element_text(color = "black",
				                           size = 12),
				axis.title = element_text(size = 18,
				                            face = "bold"),
				legend.position = 'none',
				legend.title = element_blank(),
				legend.background = element_blank(),
				axis.title.x = element_text(face = "bold"))
