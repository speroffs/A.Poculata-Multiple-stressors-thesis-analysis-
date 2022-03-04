# Sarah Speroff
# Rotjan Lab 2021

# A. Poculata Multiple stressor thesis analysis 
# X - Rack water quality data 

# set wd
getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/WQ")

# read data
WQ <- read.csv("XRack.water.quality.csv", header=TRUE)

# inspect data
head(WQ)
tail(WQ)
str(WQ)

#load libraries
library(ggplot2)

################################################################################
#
#                                TEMPERATURE
#
################################################################################

# load library
library(data.table)

# calculate means and sd of temps
setDT(WQ)
WQ[ ,list(mean=mean(Temperature), sd=sd(Temperature)), by=Sump] # maybe we'll remove the pre-treatment dates


# order date
WQ$Date <- factor(WQ$Date, levels = c("9/20/21", "9/21/21", "9/22/21", "9/23/21", "9/24/21", "9/27/21",
                                      "9/28/21", "9/29/21","9/30/21", "10/1/21", "10/2/21", "10/3/21", 
                                      "10/4/21", "10/5/21","10/6/21", "10/7/21", "10/8/21", "10/9/21", 
                                      "10/10/21", "10/11/21", "10/12/21", "10/13/21", "10/14/21", 
                                      "10/15/21", "10/16/21", "10/17/21","10/18/21", "10/19/21", 
                                      "10/20/21", "10/21/21", "10/22/21", "10/23/21",
                                      "10/24/21", "10/25/21", "10/26/21", "10/27/21"))

# reorder treaments to match x rack sump colors
WQ$Treatment <- factor(WQ$Treatment, levels = c("PDS, PLS","NDS, NLS","PDF, PLF", "NDF, NLF"))
  
# plot 
ggplot(data=WQ, aes(x=Date, y=Temperature, group = Treatment, color = Treatment)) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("PDS, PLS" = "orange", "NDS, NLS" = "gold", 
                                "PDF, PLF" = "forest green", "NDF, NLF" = "blue")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  #geom_vline(aes(xintercept = "9/27/21"), size = 0.5, color = "orange", linetype = "dashed") +
  #geom_vline(aes(xintercept = "9/29/21"), size = 0.5, color = "gold", linetype = "dashed") +
  #geom_vline(aes(xintercept = "10/1/21"), size = 0.5, color = "forest green", linetype = "dashed") +
  #geom_vline(aes(xintercept = "10/4/21"), size = 0.5, color = "blue", linetype = "dashed") +
  scale_y_continuous(breaks = seq(20,35,2)) 
 
  



  