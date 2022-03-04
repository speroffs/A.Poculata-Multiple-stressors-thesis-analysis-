# Libraries 
library(ggplot2)
library(Rmisc)
library(dplyr)
library(broom)
library(tidyr)

# Working Directory 
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Respiration")

# load the data 
data <- read.csv("Respiration Data compiled - allrates.csv", header = TRUE)

# Rename column headers
names(data)[names(data) == "mass.specific"] <- "rate"

# absolute value rate for better visualization
data$rate = abs(data$rate)

# Change column variable type 
data$B.TAG=as.factor(data$B.TAG)
data$Day = as.factor(data$Day)
data$Sym.state = as.factor(data$Sym.state)
data$Bacteria = as.factor(data$Bacteria)
data$Plastic = as.factor(data$Plastic)
data$Light = as.factor(data$Light)
data$Food = as.factor(data$Food)
data$Temperature = as.factor(data$Temperature)

# Re-level treatments 
data$Treatment <- factor(data$Treatment, levels = c("Control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))
# Re-order days
data$Day <- factor(data$Day, levels = c("8", "15", "22"))

str(data) #final check

###################################################################################################################################
#
#                                                   Summary Statistics and comparisons by Treatment
#
###################################################################################################################################

#  Grouped by treatment:
 
1. Do astrangia response to e.coli in every treatment/what is the effect of bacteria on metabolic rate?
    Includes all treatments: Control, PDS, PLS, NDS, NLS, PDF, PLF, NDF, NLF
lmer(rate ~ Bacteria + (1|B.TAG), data = . )
emmeans(lmer, list(pairwise~Bacteria))

2. Do astrangia respond differently to e.coli over time in every treatment / what is the effect of time on metabolic rate?
    Includes most treatments: PDS, PLS, NDS, NLS, PDF, PLF, NDF, NLF
lmer(rate ~ Day * Bacteria + (1|B.TAG), data = . )
emmeans(lmer, list(pairwise~c(Bacteria, Day)))

3. Do astrangia phenotypes respond differently to e.coli in every treatment / what is the effect of phenotype on metabolic rate?
    Includes all treatments: Control, PDS, PLS, NDS, NLS, PDF, PLF, NDF, NLF
lmer(rate ~ Sym.state * Bacteria + (1|B.TAG), data = . )
emmeans(lmer, list(pairwise~c(Sym.state, Bacteria)))

4. Do astrangia phenotypes repspond differently to e.coli over time in every treatment / what is the effect of phenotype over time on metabolic rate?
    Includes most treatments: PDS, PLS, NDS, NLS, PDF, PLF, NDF, NLF
lmer(rate ~ Sym.state * Day * Bacteria + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise ~ c(Sym.state, Day, Bacteria)))



##
# Question 1: Do astrangia respond to e.coli in every treatment? What is the effect of bacteria on metabolic rate?

# create grouping factor to run 9 linear models for each treatment at once
by_treat <- group_by(data, Treatment)

# Create a df with the parameter coefficients of the linear models  
response.lm <- 
  do(by_treat, 
     tidy(                     
       lm(rate~Bacteria, data= .)))
response.lm
## Almost, there is a significant differential response of corals to e. coli in 8/9 treatments where respiration is significantly faster in the e.coli treatment groups compared to the saltwater control groups except for NDF. Lack of a significant difference in NDF could be due to a decrease in sample size for e.coli data - 2 time points instead of 3

# Summarise to plot
bac.sum <- summarySE(data, measurevar = "rate", groupvars = c("Treatment", "Bacteria"))
bac.sum$Treatment <- factor(bac.sum$Treatment, levels = c("Control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))

# make sig df
sig1 <- tribble(
  ~Treatment, ~Bacteria, ~rate,
  "NLF", "SW", 0.03) # < 0.05 *
sig2 <- tribble(
  ~Treatment, ~Bacteria, ~rate,
  "Control", "SW", 0.03, # < 0.01 **
  "PLS", "SW", 0.03) # < 0.01 **
sig3 <- tribble(
  ~Treatment, ~Bacteria, ~rate,
  "PDS", "SW", 0.03, # < 0.001 ***
  "NDS", "SW", 0.03, # < 0.001 ***
  "NLS", "SW", 0.03, # < 0.001 ***
  "PDF", "SW", 0.03, # < 0.001 ***
  "PLF", "SW", 0.03) # < 0.001 ***

# plot
ggplot(data = bac.sum, 
       aes(x = Bacteria,
           y = rate,
           color = Bacteria)) +
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.1, size = 1) +
  facet_wrap(vars(Treatment)) +
  scale_color_manual(values = c("black", "royalblue3")) +
  labs(x = "", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "Data represent mean +/- SEM; Corals respond significantly more to bacterial exposure in \nall treatments except for NDF." )+
  theme_classic() 
#geom_text(data = sig1, label = "*", size = 6, color = "black") +
#geom_text(data = sig2, label = "**", size = 6, color = "black") +
#geom_text(data = sig3, label = "***", size = 6, color = "black") 


##
# Question 2: Do astrangia respond to differently to e.coli over time in every treatment? What is the effect of time on rate?
# Run 2 way anovas for the effect of bacterial treatment and time on respiration rate for every treatment 

# PDS
pds.data <- filter(data, Treatment == "PDS") # && HERE && (no control)
pds_aov1 <- aov(rate ~ Day * Bacteria, data = pds.data) # && HERE && (no control)
summary(pds_aov1) # yes, day is significant effect on rate

# Run a tukey HSD post hoc test to determine at which time points the rate between saltwater and bacterial treatments are different
pds_tukey1 <- tidy(TukeyHSD(pds_aov1, which = 'Day:Bacteria')) # && HERE && (no control)
pds_tukey1 <- filter(pds_tukey1, adj.p.value <= 0.05) # && HERE && (no control) # filter out significant interactions 
pds_tukey1

# Summarize rate by treatment, Bacterial treatment, and Day
treat.means <- summarySE(data, measurevar = "rate", groupvars = c("Treatment", "Day", "Bacteria"))

# Plot differential response of corals to e.coli vs saltwater over time for each treatment
# Make a df of the significant  interactions / where a * should be plotted
sig <- tribble(
  ~Day, ~Treatment, ~Bacteria, ~rate,
  "22", "PDS", "EC", 0.06, # < 0.001 ***
  "8", "NDS", "EC", 0.06 # < 0.001 ***
)
sig2 <- tribble(
  ~Day, ~Treatment, ~Bacteria, ~rate,
  "8", "PDF", "EC", 0.06, # < 0.01 **
)
# plot
ggplot(data = treat.means, 
       aes(x = Day, 
           y = rate, 
           col = Bacteria, 
           group = Bacteria)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), 
                width=0.2, 
                size = 1) +
  #  geom_text(data = sig, label = "***", size = 6, color = "black") +
  #  geom_text(data = sig2, label = "**", size = 6, color = "black") +
  scale_color_manual(values = c("black", "royalblue3")) +
  facet_wrap(vars(Treatment)) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 0.08)) +
  theme(legend.position = "right",
        legend.title = element_text(size=12),
        axis.title = element_text(size = 14),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "\nDay", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "Data represent mean +/- SEM; Time significantly affected the rate in PDS, NDS, and PDF treatments" )

##
# Question 3: do astrangia phenotypes respond to differently to e.coli in every treatment?

# Ambient Control
ctrl.lm <- lm(rate~Sym.state*Bacteria, data=ctrl.data) #  && HERE &&
summary(ctrl.lm) # Bacteria yes; phenotype yes 
# Determine which comparisons are different
ctrl.aov <- aov(rate~Sym.state*Bacteria, data=ctrl.data) # && HERE &&
ctrl_tukey <- tidy(TukeyHSD(ctrl.aov, which = 'Sym.state:Bacteria'))#  && HERE &&
ctrl_tukey <- filter(ctrl_tukey, adj.p.value <= 0.05) # && HERE && # filter out significant interactions 
ctrl_tukey

# Summarize rate by Treatment, Bacterial condition, and phenotype
phen.means <- summarySE(data, measurevar = "rate", groupvars = c("Treatment", "Bacteria", "Sym.state"))
phen.means
# Plot the effect of phenotype on response to bacterial treatment for all stressors 
ggplot(data = phen.means, 
       aes(x = Bacteria, 
           y = rate, 
           col = Sym.state, 
           group = Sym.state)) +
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.2, size = 1) +
  facet_wrap(vars(Treatment)) +
  labs(x = "\nBacterial Treatment", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM; NLS is the only experimental treatment where \nphenotype affects metabolic response (ANOVA; p=7.8e-8)" ) +
  theme_classic() +
  scale_color_manual(values = c("tan", "coral4")) 

##
# Question 4: do astrangia phenotypes respond to differently to e.coli over time in every treatment?

# Run 3 way anovas for the effect of bacterial treatment, phenotype, and time on respiration rate for every treatment 
# PDS
pds_aov2 <- aov(rate ~ Sym.state * Day * Bacteria, data = pds.data) # && HERE && (no control)
summary(pds_aov2) # Bacteria, day, Bacteria:day significant

# Run a tukey HSD post hoc test to determine at which time points the rate between saltwater and bacterial treatments are different
pds_tukey2 <- tidy(TukeyHSD(pds_aov2, which = 'Sym.state:Day:Bacteria')) # && HERE && (no control)
pds_tukey2 <- filter(pds_tukey2, adj.p.value <= 0.05) # && HERE && (no control) # filter out significant interactions
pds_tukey2

# Summarize rate by Chronic treatment, Bacterial treatment, Day & SYM.STATE
time.phen.means <- summarySE(data, measurevar = "rate", groupvars = c("Treatment", "Day", "Bacteria", "Sym.state"))
# Plot SW/EC & SYM/APO for all treatments
ggplot(data = time.phen.means, 
       aes(x = Day, 
           y = rate, 
           col = Sym.state, 
           group = interaction(Bacteria, Sym.state))) +
  geom_line(aes(color = Sym.state, linetype = Bacteria)) +
  geom_point() +
  facet_wrap(vars(Treatment)) +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.2, size = 1) +
  labs(x = "\nDay", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM; Phenotype did not affect metabolic rate over time for any experimental treatment.")+
  theme_classic() +
  scale_color_manual(values = c("tan", "coral4")) 


###################################################################################################################################
#
#                                                   Summary Statistics and comparisons by Stressor
#
###################################################################################################################################


#  Grouped by stressor, and sym.state

1. Does plastic/food/light/temperature/sym.state affect metabolic rate / what is the effect of the stressor on metabolic rate?
lmer(rate ~ stressor + (1|B.TAG), data = . )
emmeans(lmer, list(pairwise~stressor))

2. Does stressor impact the response to e.coli / what is the effect of the stressor in each bacterial treatment on metabolic rate?
lmer(rate ~ stressor * Bacteria + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise ~ c(Stressor, Bacteria)))

3. Does the stressor impact the response to e.coli over time / what is the effect of the stressor in each bacterial treatment on metabolic rate over time?
lmer(rate ~ stressor * Bacteria * Day + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise~c(Stressor, Bacteria, Day)))

4. Is there a differential response to the stressor by phenotype / what is the effect of phenotype exposed to this stressor on metabolic rate?
lmer(rate ~ Sym.state * stressor + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise~c(Sym.state, stressor)))

5. Is there a differential metabolic response to bacteria by plastic and phenotype / what is the effect of phenotype when exposed to the stressor and bacteria on metabolic rate? 
lmer(rate ~ Sym.state * stressor * bacteria + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise~c(Sym.state, stressor, Bacteria)))

6. Is there a differential metabolic response to bacteria by plastic and phenotype over time / what is the effect of phenotype when exposed to the stressor and bacteria over time on metabolic rate?
lmer(rate ~ Sym.state * stressor * bacteria * Day + (1|B.TAG), data = .)
emmeans(lmer, list(pairwise~c(Sym.state, stressor, Bacteria, Day)))




# Question 1: does plastic treatment affect metabolic rate? 
plastic.lm1 <- lm(rate ~ Plastic, data = data) # && HERE && > plastic/food/light/temperature/sym.state
summary(plastic.lm1) 

# Summarize by plastic
plastic.sum1 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic")) # && HERE && > plastic/food/light/temperature/sym.state

# Visualize
ggplot(data = plastic.sum1, # && HERE && > plastic/food/light/temperature/sym.state
       aes(x = Plastic,
           y = rate)) +
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.1, size = 1) +
  #scale_color_manual(values = c("black", "darkgreen")) +
  ggtitle ("Plastic") +
  theme(legend.position = "right",
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "\n", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM" ) +
  theme_classic()

# Question 2: Is there a differential response to e.coli based on plastic treatment 
plastic.lm2 <- lm(rate ~ Plastic * Bacteria, data = data) # && HERE && > plastic/food/light/temperature/sym.state
summary(plastic.lm2) # Nope!

# Summarize by bacteria
plastic.sum2 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic","Bacteria")) # && HERE && > plastic/food/light/temperature/sym.state

# Visualize
ggplot(data = plastic.sum2, # && HERE && > plastic/food/light/temperature/sym.state
       aes(x = Bacteria,
           y = rate, 
           color = Plastic)) + # && HERE && > plastic/food/light/temperature/sym.state
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.1, size = 1) +
  scale_color_manual(values = c("black", "darkgreen")) +
  ggtitle ("Plastic") +
  theme(legend.position = "right",
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "\n", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM" ) +
  theme_classic()

## 
# Question 3. Is there a differential response to e.coli over time based on plastic treatmet?
# model 
plastic.lm3 <- lm(rate ~ Plastic * Day * Bacteria, data = data)
summary(plastic.lm3) 

# Write an anova to determine significance for the following interactions:
plastic_aov3 <- aov(rate ~ Bacteria * Day * Plastic, data = data)
summary(plastic_aov3)

# Let's see where those interactions might be significant 
plastic_tukey <- tidy(TukeyHSD(plastic_aov, 
                               which = 'Bacteria:Plastic:Day'))
plastic_tukey <- filter(plastic_tukey, adj.p.value <= 0.05)
plastic_tukey 

# Summarize by bacteria and plastic over time to visualize
plastic.sum3 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic", "Day", "Bacteria"))

# Plot 
ggplot(plastic.sum3, 
       aes(x = Day, 
           y = rate, 
           col = Plastic, 
           group = Plastic)) +
  geom_line() +
  geom_point() +
  
  facet_wrap(vars(Bacteria)) +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.2, size = 1) +
  labs(x = "\nDay", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM") +
  scale_color_manual(values = c("black", "darkgreen")) +
  theme_classic()

## 
# Question 4. Is there a differential response to plastic by phenotype?
# nodel
plastic.lm4 <- lm(rate ~ Sym.state * Plastic, data = data)
summary(plastic.lm4) 

# Summarize to visualize
plastic.sum4 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic","Sym.state"))

# Visualize
ggplot(data = plastic.sum4,
       aes(x = Plastic,
           y = rate, 
           col = Sym.state)) +
  geom_point() +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.1, size = 1) +
  scale_color_manual(values = c("tan", "coral4")) +
  ggtitle ("Plastic") +
  theme(legend.position = "right",
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "\n", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM" ) +
  theme_classic()

##
# Question 5: Is there a differential metabolic response to e.coli by plastic & phenotype
# model
plastic.lm5 <- lm(rate ~ Sym.state * Plastic * Bacteria, data = data)
summary(plastic.lm5) 

# Summarize to visualize
plastic.sum5 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic","Sym.state","Bacteria"))

# Visualize
ggplot(data = plastic.sum5,
       aes(x = Plastic,
           y = rate, 
           col = Sym.state)) +
  geom_point() +
  facet_grid(~Bacteria) +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.1, size = 1) +
  scale_color_manual(values = c("tan", "coral4")) +
  ggtitle ("Plastic") +
  theme(legend.position = "right",
        legend.title = element_text(size=12),
        axis.title = element_text(size = 12),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "\n", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM" ) +
  theme_classic()

##
# Question 6: Is there a differential metabolic response to e.coli by plastic & phenotype over time
#
plastic.lm6 <- lm(rate ~ Sym.state * Plastic * Bacteria * Day, data = data)
summary(plastic.lm6) # Bacteria, yes. Day 15, yes. Phenotype @ day 15, yes. 

# Write an anova to determine significance for the following interactions:
plastic_aov6 <- aov(rate ~ Sym.state * Plastic * Bacteria * Day, data = data)
summary(plastic_aov6)

# Determine where the significant interactions are 
plastic_tukey6 <- tidy(TukeyHSD(plastic_aov6, 
                                which = 'Sym.state:Plastic:Day:Bacteria:'))
plastic_tukey6 <- filter(plastic_tukey6, adj.p.value <= 0.05)
plastic_tukey6

# Summarize to visualize
plastic.sum6 <- summarySE(data, measurevar = "rate", groupvars = c("Plastic","Sym.state","Bacteria", "Day"))

# Visualize 
ggplot(plastic.sum6, 
       aes(Day, 
           rate, 
           col = Sym.state, 
           group = Sym.state)) +
  geom_line() +
  geom_point() +
  facet_grid(Plastic~Bacteria) +
  geom_errorbar(aes(ymin=rate-se, ymax=rate+se), width=0.2, size = 1) +
  labs(x = "\nDay", 
       y = expression(paste("Rate O2 Consumption (umol*min"^"-1"~"*g"^"-1"~")")), 
       caption = "data represent mean +/- SEM; no significant differences between phenotype \n in each plastic/bacterial treatment group" ) +
  scale_color_manual(values = c("tan", "coral4")) + 
  theme_classic()


