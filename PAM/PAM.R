# Sarah Speroff
# Multiple Stressor Photosynthetic efficiency data analysis 

#set wd
getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/PAM")

# load the data 
PAM.data <- read.csv("Full_Fv_Fm.data - FvFm.long.format.csv", header = TRUE)

################################################################################
#
#                           Inspect the data
#
################################################################################

names(PAM.data)
head(PAM.data)
summary(PAM.data)
str(PAM.data)
nrow(PAM.data)
length(unique(PAM.data$B.Tag)) # should be 480 
table(PAM.data$Sym.State) #should be even split

# change variables
PAM.data$Day <- as.numeric(PAM.data$Day)
PAM.data$Sump <- as.factor(PAM.data$Sump)
PAM.data$Colony <- as.factor(PAM.data$Colony)
PAM.data$Treatment <- as.factor(PAM.data$Treatment)
PAM.data$Temperature <- as.factor(PAM.data$Temperature)
PAM.data$Plastic <- as.factor(PAM.data$Plastic)
PAM.data$Light <- as.factor(PAM.data$Light)
PAM.data$Food <- as.factor(PAM.data$Food)
PAM.data$Tank <- as.factor(PAM.data$Tank)
PAM.data$Tile <- as.factor(PAM.data$Tile)
PAM.data$Sym.State <- as.factor(PAM.data$Sym.State)
PAM.data$B.Tag <- as.factor(PAM.data$B.Tag)


################################################################################
#
#                         Check for normality 
#
################################################################################

# plot the distribution of the average pam values 
par(mfrow=c(1,1)) 
hist(PAM.data$FvFm.AVG)

# alternatively we can use ggplot
library(ggplot2)
library(ggpubr)
ggdensity(PAM.data$FvFm.AVG, 
          main = "Density plot of Fv.Fm",
          xlab = "Fv.Fm")

# Now let's take a look at our quantiles  
par(mfrow=c(1,2))
hist(PAM.data$FvFm.AVG, main="Fv/Fm")
qqnorm(PAM.data$FvFm.AVG, main="Fv/FM")
qqline(PAM.data$FvFm.AVG) # looks like a decent fit overall despite the stray points near the corners 

# summarise pam values 
summary(PAM.data$FvFm.AVG)

# interquartile range: 3rd - 1st q
0.27 - 0.11  #0.16
# outlier rule of thumb: an outlier is > 1.5x the interquartile range above the 3rd
# or below the 1st quartile 
# for our data, outliers would be less than -0.08 or grater than 0.4
# so according to this, we have some outliers greater than 0.4
################################################################################
#
#                        Univariate Linear Models 
#
################################################################################

# Let's run a simple lm to see if sym.state affects FvFm
sym.lm <- lm(FvFm.AVG~Sym.State, data= PAM.data)
summary(sym.lm)
# plot the results of that lm
par(mfrow=c(2,2))
plot(sym.lm) 
# run by brian to see if the model fit looks okay? 
# does this data need to be transformed?

# what if we added day to our model, does that significantly improve it?
sym.day <- lm(FvFm.AVG ~ Sym.State + Day, data=PAM.data)
anova(sym.lm, sym.day)
# yes, our sym.day model is made significantly better fit than just sym 

# what about if we add treatment?
treat.lm <- lm(FvFm.AVG~ Sym.State + Day + Treatment, data=PAM.data)
anova(sym.day, treat.lm)
# yes again, this model is even better 

summary(treat.lm)

# check model
library(see)
library(qqplotr)
check_model(treat.lm)
check_collinearity(treat.lm)
check_outliers(treat.lm)
check_homogeneity(treat.lm)

########### 
# now we'll run simple, single variable models to see the effects of our individual predictor variables on the FvFm response variable 

# Plastic
plastic.lm <- lm(FvFm.AVG~Plastic, data=PAM.data)
summary(plastic.lm)
par(mfrow=c(2,2))
plot(plastic.lm) 

# plot a histogram of the residuals 
plastic.res <- resid(plastic.lm)
hist(plastic.res)

# Food
food.lm <- lm(FvFm.AVG~Food, data=PAM.data)
summary(food.lm)
par(mfrow=c(2,2))
plot(food.lm)

# Light
light.lm <- lm(FvFm.AVG~Light, data=PAM.data)
summary(light.lm)
par(mfrow=c(2,2))
plot(light.lm)

# Temperature 
temperature.lm <- lm(FvFm.AVG~Temperature, data=PAM.data)
summary(temperature.lm)
par(mfrow=c(2,2))
plot(temperature.lm) # largest cook's distance of all the simple lms

########### seems like all factors affect FvFm independently ########### 

################################################################################
#
#                          Mixed Linear Models
#
################################################################################
 
library(lme4)
library(lmerTest)
library(performance)

# can't have NAs
PAM.data <- PAM.data[complete.cases(PAM.data),]
str(PAM.data)

# taking *repeated measures* into account, null model
m0 <- lmer(FvFm.AVG ~ 1 + (1|B.Tag), data=PAM.data)
summary(m0)

m1 <- lmer(FvFm.AVG ~ Plastic + (1|B.Tag), data = PAM.data)
anova(m0, m1) # log-likelihood test

m2 <- lmer(FvFm.AVG ~ Plastic + Food + (1|B.Tag), data = PAM.data)
anova(m1, m2)

m3 <- lmer(FvFm.AVG ~ Food + (1|B.Tag), data = PAM.data)
anova(m3, m2)

m4 <- lmer(FvFm.AVG ~ Food + Plastic + (1|B.Tag), data = PAM.data)
anova(m2, m4)

m5 <- lmer(FvFm.AVG ~ Plastic + Food + Light + (1|B.Tag), data = PAM.data)
anova(m2, m5)

m6 <- lmer(FvFm.AVG ~ Plastic + Food + Light + Temperature + (1|B.Tag), data = PAM.data)
anova(m5, m6)

m7 <- lmer(FvFm.AVG ~ Plastic + Food + Light + Temperature + Sym.State + (1|B.Tag), data = PAM.data) # did not give singularity error 
anova(m6, m7)

AIC(m1, m2, m3, m4, m5, m6, m7)

# let's take a look at our m7
summary(m7)
# looks like light is a non-significant term: let's remove it and see if our model is better 
m8 <- lmer(FvFm.AVG ~ Plastic + Food + Temperature + Sym.State + (1|B.Tag), data = PAM.data)
summary(m8)
anova(m8, m7)

AIC(m1, m2, m3, m4, m5, m6, m7, m8)

# now let's add more random effects to see if they make anything better 

m9 <- lmer(FvFm.AVG ~ Plastic + Food + Temperature + Sym.State + 
             (1|B.Tag) + (1|Sump), data = PAM.data) # no
anova(m8, m9)

m10 <-lmer(FvFm.AVG ~ Plastic + Food + Temperature + Sym.State + 
             (1|B.Tag) + (1|Tank), data = PAM.data) # no
anova(m8, m10)

m11 <- lmer(FvFm.AVG ~ Plastic + Food + Temperature + Sym.State + 
              (1|B.Tag) + (1|Colony), data = PAM.data)
anova(m8, m11) # yes, better
summary(m11)

m12 <- lmer(FvFm.AVG ~ Plastic + Food + Temperature + Sym.State + 
              (1|B.Tag) + (1|Colony) + (1|Tile), data = PAM.data)
anova(m11, m12) # yes, better
summary(m12)

AIC(m8, m11, m12) # m12 best

mfull <- lmer(FvFm.AVG ~ Plastic + Food + Temperature + Light + Sym.State +
                (1|B.Tag) + (1|Colony) + (1|Tile) + (1|Tank) + (1|Sump) + (1|Day), data = PAM.data)
summary(mfull)
############################################################################################
######### try fitting a BAYESIAN model to fix the issue of singularity in our lmer ######### 

library(rstanarm)
library(bayestestR)
library(insight)

# model
bmod <- stan_glmer(FvFm.AVG ~ Treatment + (1|B.Tag), data = PAM.data)

# extract parameters
post <- describe_posterior(bmod)
print_md(post, digits=2)

#extract parameters (ie. coefficients)
postt <- insight::get_parameters(bmod)
head(postt)
nrow(postt)

# visualize posterior distribution 
ggplot(postt, aes(x=TreatmentNDF)) +
  geom_density(fill="red")

# describe the posteriors - 3 elements needed
# first, a point-estimate (mean, median, mode): a one-value summary similar to beta in freq models
mean(postt$TreatmentNDF)
median(postt$TreatmentNDF)
map_estimate(postt$TreatmentNDF) # MAP (maximum A posteriori) is equivalent to mode (ie the peak of distribution)

# second, we need to describe the uncertainty
range(postt$TreatmentNDF) # includes the extreme highs and lows 
hdi(postt$TreatmentNDF, ci= 0.89) # highest density interval - range containing the 98% most probable effect values

# 



######### not getting error messages but I have no idea what any of this means 


# let's start with a null model that includes all of our random factors 
m0 <- lmer(FvFm.AVG~1 + 
             (1|Sump) +
             (1|Colony) +
             (1|Tank) +
             (1|Tile), 
             data=PAM.data, REML=TRUE) #null
summary(m0)

# full model which includes all fixed and all random factors 
m1 <- lmer(FvFm.AVG ~ Plastic + Light + Food + Temperature + Sym.State +
             (1|Sump) +
             (1|Colony) +
             (1|Tank) +
             (1|Tile), 
           data=PAM.data, REML=TRUE)

summary(m1)
r2(m1)
# Conditional: variance explained by whole model, including random factors 
# Marginal: variance explained by fixed factors
anova(m0, m1)

# remove food 
m2 <- lmer(FvFm.AVG ~ Plastic + Light + Temperature + Sym.State +
                   (1|Sump) +
                   (1|Colony) +
                   (1|Tank) +
                   (1|Tile), 
                 data=PAM.data, REML=TRUE)
summary(m2)
r2(m2)

anova(m0, m1, m2) 
AIC(m0,m1,m2) # highest AIC score: 

##########################

rep.meas <- lmer(FvFm.AVG ~ Treatment + (1|Day), data = PAM.data)
summary(rep.meas)


  

################################################################################
#
#                      Preliminary data visualization 
#
################################################################################

## exploratory plots 
library(plyr)
library(dplyr)
library(tidyr)

# FvFm by Sym.State
ggplot(PAM.data, aes(Sym.State, FvFm.AVG))+
  geom_jitter(width = 0.25, height=0.25, alpha=0.8, aes(color=Treatment))+
  facet_wrap(vars(Treatment)) +
  stat_summary(geom="point", fun="mean", col="black") 

# FvFm with time as x axis
ggplot(PAM.data, aes(Day, FvFm.AVG, color=Treatment)) +
  facet_wrap(vars(Treatment)) + 
  geom_jitter(width = 0.2, height=0.25, alpha=0.8) +
  geom_smooth(se = FALSE, method = lm) + 
  stat_summary(geom="point", fun="mean", col="black")

################## now let's break it down by each treatment ###################

library(ggthemes)

# New df with just control variables 
ctrl.data <- filter(PAM.data, Treatment == "control" )

## Control ##
ggplot(ctrl.data, aes(Sym.State, FvFm.AVG)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  stat_summary(geom="point", fun="mean", col="black") + # WANT TO CONNECT THE DOTS
  ggtitle("Ambient Control") +
  theme_bw() 

ggplot(ctrl.data, aes(Treatment, FvFm.AVG, color=Sym.State))+
  geom_jitter(width=0.25, alpha=0.9) +
  scale_color_brewer(palette="Set1") +
  theme_bw()

################## 

# New df with just pds variables 
pds.data <- filter(PAM.data, Treatment == "PDS" )

## PDS: compare apo and sym over time 
ggplot(pds.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method = loess) +
  facet_wrap(~Sym.State) +
  theme_bw() +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("PDS")

# PDS: compare sym state at each time point 
ggplot(pds.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("PDS") +
  theme_bw()

# an attempt at a paired observation graph
ggplot(pds.data, aes(x=Day, y=FvFm.AVG, group=B.Tag)) +
  geom_point(aes(colour=Sym.State), size=1.5, position=position_dodge(width=0.1)) +
  geom_line(size=0.5, alpha=0.5, position=position_dodge(width=0.1)) +
  xlab('Sym State') +
  ylab('Fv/Fm') +
  facet_grid(vars(Sym.State)) +
scale_colour_manual(values=c("#009E73", "#D55E00"), guide=FALSE) + 
  theme_bw() +
  ggtitle("PDS")

################## 

# New df with just pls variables 
pls.data <- filter(PAM.data, Treatment == "PLS" )

## PLS: compare apo and sym over time 
ggplot(pls.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method = loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("PLS") +
  theme_bw()
  
## PLS: compare sym state at each time point 
ggplot(pls.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("PLS") +
  theme_bw()

################## 

# New df with just nls variables 
nls.data <- filter(PAM.data, Treatment == "NLS" )

## NLS: compare apo and sym over time 
ggplot(nls.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method = loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("NLS")  +
  theme_bw()
  
## NLS: compare sym state at each time point 
ggplot(nls.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("NLS") +
  theme_bw()

################## 

# New df with just nds variables 
nds.data <- filter(PAM.data, Treatment == "NDS" )
  
## NDS: compare apo and sym over time 
ggplot(nds.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method = loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("NDS") +
  theme_bw()

## NDS: compare sym state at each time point 
ggplot(nds.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("NDS") +
  theme_bw()

################## 

# New df with just pdf variables 
pdf.data <- filter(PAM.data, Treatment == "PDF" )
  
## PDF: compare apo and sym over time 
ggplot(pdf.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) + 
  geom_smooth(method=loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("PDF") +
  theme_bw()

## PDF: compare sym state at each time point 
ggplot(pdf.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("PDF") +
  theme_bw()

################## 

# New df with just plf variables 
plf.data <- filter(PAM.data, Treatment == "PLF" )

## PLF: compare apo and sym over time 
ggplot(plf.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method=loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("PLF") + 
  theme_bw()

## PLF: compare sym state at each time point 
ggplot(PAM.data, aes(Sym.State, FvFm.AVG, Treatment==PLF)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(aes(col="grey")) +
  facet_wrap(~Day) +
  stat_summary(geom="point", fun="mean", col="darkgreen")+
  ggtitle("PLF")

################## 

# New df with just ndf variables 
ndf.data <- filter(PAM.data, Treatment == "NDF" )

## NDF: compare apo and sym over time 
ggplot(ndf.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method=loess)+
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("NDF") + 
  theme_bw()

## NDF: compare sym state at each time point 
ggplot(ndf.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("NDF") +
  theme_bw()

################## 

# New df with just nlf variables 
nlf.data <- filter(PAM.data, Treatment == "NLF" )

## NLF: compare apo and sym over time 
ggplot(nlf.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  geom_smooth(method=loess) +
  facet_wrap(~Sym.State) +
  stat_summary(geom="point", fun="mean", col="black") +
  ggtitle("NLF: Elevated control") +
  theme_bw()

## NLF: compare sym state at each time point 
ggplot(nlf.data, aes(Sym.State, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(~Day) +
  stat_summary(geom="point", fun="mean", col="black")+
  ggtitle("NLF: Elevated control") +
  theme_bw()

################## now let's break it down by each variable ###################

###
# FvFm by plastic exposure :
ggplot(PAM.data, aes(Plastic, FvFm.AVG, color=Plastic)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8)+
  stat_summary(geom="point", fun="mean", col="black") +
  facet_wrap(vars(Sym.State)) +
  theme_bw()

# FvFm by plastic exposure over time 
ggplot(PAM.data, aes(Day, FvFm.AVG, color=Plastic)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(vars(Plastic), vars(Sym.State)) +
  stat_summary(geom="point", fun="mean", col="black") +
  theme_bw()

###
# FvFm by light exposure :
ggplot(PAM.data, aes(Light, FvFm.AVG, color=Light)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8)+
  stat_summary(geom="point", fun="mean", col="black") +
  facet_wrap(vars(Sym.State)) +
  theme_bw()

# FvFm by light exposure over time 
ggplot(PAM.data, aes(Day, FvFm.AVG, color=Light)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(vars(Light), vars(Sym.State)) +
  stat_summary(geom="point", fun="mean", col="black") +
  theme_bw()

##
# FvFm by food treatment :
ggplot(PAM.data, aes(Food, FvFm.AVG, color=Food)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8)+
  stat_summary(geom="point", fun="mean", col="black") +
  facet_wrap(vars(Sym.State)) +
  theme_bw()

# FvFm by food treatment over time 
ggplot(PAM.data, aes(Day, FvFm.AVG, color=Food)) +
  geom_jitter(width = 0.25, height=0.25, alpha=0.8) +
  facet_grid(vars(Food), vars(Sym.State)) +
  stat_summary(geom="point", fun="mean", col="black") +
  theme_bw()

##### All treatments over by time 
day.plot <- 
  ggplot(PAM.data, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_jitter(width=0.3, height=0, alpha=0.8) +
  facet_wrap(vars(Treatment)) +
# stat_summary(geom="point", fun="mean", col="black") +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x = "Day", y = "Photosynthetic Efficiency (Fv/Fm)",
       color = "Symbiont State") +
  theme(legend.position = "top") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

day.plot + scale_color_brewer(palette="Set1")

#### All treatments over time (not separated by sym state)
all.plot <- 
  ggplot(PAM.data, aes(Day, FvFm.AVG, color=Treatment)) +
  geom_jitter(width=0.3, height=0, alpha=0.9) +
  facet_wrap(vars(Treatment)) +
  # stat_summary(geom="point", fun="mean", col="black") +
  geom_smooth(method=loess) +
  theme_bw() +
  labs(x = "Day", y = "Photosynthetic Efficiency (Fv/Fm)",
       color = "State") +
  theme(legend.position = "top") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

all.plot + scale_color_brewer(palette="Spectral")


#### more fun color options 
library(paletteer) 
paletteer_d("awtools::mpalette")

all.plot + scale_color_paletteer_d("awtools::mpalette")
all.plot + scale_color_paletteer_d("DresdenColor::paired")
all.plot + scale_color_paletteer_d("ggsci::default_jco")

################################################################################
#
#                     Calculating stats 
#
################################################################################

# calculate 
library(dplyr)
pamstats <- PAM.data %>% 
  group_by(Treatment, Sym.State, Day) %>% 
  summarize( Min = min(FvFm.AVG),
             Max = max(FvFm.AVG),
             Mean = mean(FvFm.AVG),
             SD = sd(FvFm.AVG), 
             N = length(FvFm.AVG)
  )
pamstats

# plot them
stat.plot <- 
  ggplot(pamstats, aes(Day, Mean, color=Sym.State)) +
  geom_line(aes(linetype=Sym.State, color=Sym.State)) +
  geom_point(aes(color=Sym.State)) +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), 
                width=0.2, position=position_dodge(0.05)) +
  facet_wrap(vars(Treatment)) +
  labs(x = "Day", y = "Photosynthetic Efficiency (Fv/Fm)", color="Sym State") +
  theme(legend.position = "top") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  theme_bw()

stat.plot + scale_color_manual(values=c("burlywood3", "coral4"))


################################################################################
#
#                     Code from H. Aichelman (from Caroline) 
#
################################################################################

#Loading pam Data
pam_all.data <- read.csv("pam_data_clean.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
pam_all.data$Day<- pam_all.data$ï..Day

#Releveling data to compare everything to the control
pes_all.data$Treatment <- factor(pes_all.data$Treatment, levels=c("NO_N_25", "N_25", "NO_N_RAMP","N_RAMP"))
pam_treatmeans <- summarySE(pam_all.data, measurevar="avg", groupvars=c("Treatment","Day")) #This summarySE function is from the package Rmisc

#pam_treatmeans$time <- as.numeric(as.character(pam_treatmeans$time))

#plot, treatment x axis colored by site data figure
ggplot(pam_treatmeans,aes(x = Day, y = avg, color = Treatment))+
  theme_bw()+
  geom_point(size = 3, position = position_dodge(width=0.3))+
  #geom_smooth(aes(group=sitename), position = position_dodge(width=0.3), size = 0.5, method='lm', se = FALSE)+
  geom_line(aes(group = Treatment), size = 0.5, linetype="dashed", position = position_dodge(width=0.3))+
  #geom_errorbar(aes(x = time, ymax = pam+se, ymin = pam-se),
  #width = .2, position = position_dodge(width=0.3)) +
  scale_color_manual(name = "Treatment",
                     labels = c("NO_N_25", "N_25", "NO_N_RAMP","N_RAMP"),
                     values = c("lightskyblue3", "gray42", "lightgoldenrod2", "brown1"))+
  ylab("Fv/Fm")+
  xlab("Day") +
  ggtitle("Photosynthetic efficiency per day")
#ylim(0.4,0.7) +
theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))


######### now use that code but fill in my data 

# PAM.data has already been loaded and changed to factors 

# relevel to compare all to control 
PAM.data$Treatment <- factor(PAM.data$Treatment, levels = c("control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))

# now summarise
library(Rmisc)
pam_treatmeans <- summarySE(PAM.data, measurevar="FvFm.AVG", groupvars=c("Treatment", "Day")) #This summarySE function is from the package Rmisc
pam_treatmeans

# plot
ggplot(pam_treatmeans,aes(x = Day, y = FvFm.AVG, color = Treatment))+
  theme_bw()+
  geom_point(size = 3, position = position_dodge(width=0.3))+
  #geom_smooth(aes(group=sitename), position = position_dodge(width=0.3), size = 0.5, method='lm', se = FALSE)+
  geom_line(aes(group = Treatment), size = 0.5, linetype="dashed", position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymax = FvFm.AVG+se, ymin = FvFm.AVG-se),
                width = .2, position = position_dodge(width=0.3)) +
  scale_color_manual(name = "Treatment",
                     labels = c("control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"),
                     values = c("black", "orange", "orange2", "lightgoldenrod2", "gold", "green", "darkgreen", "lightblue", "darkblue" ))+
  ylab("Fv/Fm")+
  xlab("Day") +
  ggtitle("Photosynthetic efficiency per day") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))




# summarise
library(Rmisc)
pam_treatmeans <- summarySE(PAM.data, measurevar="FvFm.AVG", groupvars=c("Treatment", "Day", "Sym.State")) 
pam_treatmeans

# plot it 
mean.stat.plot <- 
  ggplot(pam_treatmeans, aes(Day, FvFm.AVG, color=Sym.State)) +
  geom_line(aes(linetype=Sym.State, color=Sym.State)) +
  geom_point(aes(color=Sym.State)) +
  geom_errorbar(aes(ymin=FvFm.AVG-se, ymax=FvFm.AVG+se), 
                width=0.2, position=position_dodge(0.05)) +
  facet_wrap(vars(Treatment)) +
  labs(x = "Day", y = "Photosynthetic Efficiency (Fv/Fm)", color="Sym State") +
  theme(legend.position = "top") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  theme_bw()

mean.stat.plot + scale_color_manual(values=c("burlywood3", "coral4"))












