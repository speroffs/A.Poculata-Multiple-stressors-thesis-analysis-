# Sarah Speroff
#Multiple stressor survival analysis 

# set wd
getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Survival")

# load library
library(survival);
library(survminer)

################################################################################
#
# We'll start off by looking at the simple Kaplan-Meier survival analysis which 
# is a non-parametric analysis that assesses the probability that an individual
# will survive past a certain time point (T) given that the individual is alive
# right before (T)
#
# the Kaplan-Meier probability survival curves are formally paired with a log-
# rank test whose H0 is: the survival curves of ____ conditions are identical 
# over time. The log-rank test compares the entire curve over time rather than 
# the survival probability at a specific time-point. This test assesses the 
# statistical significance between curves but does not estimate the size of the
# effect (ie the magnitude of the difference between groups) and is therefore 
# only useful for univariate analysis 
#
################################################################################

# Load our data 
surv.dat <- read.csv("Full_ daily mortality tracker - Censored.CoxPH.w.controls.csv", 
                     header = TRUE) 
str(surv.dat) 
names(surv.dat)

#convert to factors
surv.dat$Plastic <- as.factor(surv.dat$Plastic)
surv.dat$Light <- as.factor(surv.dat$Light)
surv.dat$Food <- as.factor(surv.dat$Food)
surv.dat$Treatment <- as.factor(surv.dat$Treatment)
surv.dat$Sym.State <- as.factor(surv.dat$Sym.State)

# relevel to compare all to control 
surv.dat$Treatment <- factor(surv.dat$Treatment, levels = c("amb.ctrl", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))


####

# Let's look at each variable separately 

# We'll start with Temperature 
temp <- survfit(Surv(Time, Death) ~ Temperature, data=surv.dat)
summary(temp)

# plot it 
plot(temp, mark.time = T, 
     col=c("blue","red"),
     ylim=c(0.7,1.0),
     xlab = "Days",
     ylab = "Proportion Alive",
     main = "Temperature",
     las=1, lwd = 2)
legend(0,0.8, legend=c("19 C", "30 C"), 
       lty=1, lwd=1.5, cex=0.8,col=c("blue","red"))

# Log-rank test to determine statistical significance between the curves
survdiff(Surv(Time, Death) ~ Temperature, data = surv.dat) 
#chi2: 31.3, 1 df, p: 2e-8

####

# Now let's look at Plastic
plastic <- survfit(Surv(Time, Death) ~ Plastic, data = surv.dat)
summary(plastic)

# plot
plot(plastic, mark.time = T, 
     col=c("darkgrey","forestgreen"),
     ylim=c(0.7,1.0),
     xlab = "Days",
     ylab = "Proportion Alive",
     main = "Plastic",
     las=1, lwd = 2)
legend(0,0.8, legend=c("(-) Plastic", "(+) Plastic"), 
       lty=1, lwd=1.5, cex=0.8,col=c("darkgrey","forestgreen"))

# long rank
survdiff(Surv(Time, Death) ~ Plastic, data = surv.dat) 
# chi2: 19.7, 1 df, p: 9e-6 

####

# Now let's look at Light
light <- survfit(Surv(Time, Death) ~ Light, data = surv.dat)
summary(light)

# plot
plot(light, mark.time = T, 
     col=c("darkgrey","goldenrod2"),
     ylim=c(0.7,1.0),
     xlab = "Days",
     ylab = "Proportion Alive",
     main = "Light",
     las=1, lwd = 2)
legend(0,0.8, legend=c("Dark", "Light"), 
       lty=1, lwd=1.5, cex=0.8,col=c("darkgrey","goldenrod2"))

# long rank
survdiff(Surv(Time, Death) ~ Light, data = surv.dat) 
# chi2: 6.4, 1 df, p: 0.01

####

# Now let's look at Food
food <- survfit(Surv(Time, Death) ~ Food, data = surv.dat)
summary(food)

# plot
plot(food, mark.time = T, 
     col=c("magenta","darkgrey"),
     ylim=c(0.7,1.0),
     xlab = "Days",
     ylab = "Proportion Alive",
     main = "Food",
     las=1, lwd = 2)
legend(0,0.8, legend=c("Fed", "Starved"), 
       lty=1, lwd=1.5, cex=0.8,col=c("magenta","darkgrey"))

# long rank
survdiff(Surv(Time, Death) ~ Food, data = surv.dat) 
# chi2: 0.2, 1 df, p:0.7

####

# Now let's look at Sym State
symstate <- survfit(Surv(Time, Death) ~ Sym.State, data = surv.dat)
summary(symstate)

# plot
plot(symstate, mark.time = T, 
     col=c("darkgrey","brown"),
     ylim=c(0.7,1.0),
     xlab = "Days",
     ylab = "Proportion Alive",
     main = "Sym State",
     las=1, lwd = 2)
legend(0,0.8, legend=c("Apo", "Sym"), 
       lty=1, lwd=1.5, cex=0.8,col=c("darkgrey","brown"))

# long rank
survdiff(Surv(Time, Death) ~ Sym.State, data = surv.dat) 
# chi2: 8.8, 1 df, p:0.003

#####
#
# According to the log-rank tests for each variable * independent of others *
# temperature, plastic, light, and sym.state all have statistically different 
# survival probability functions. Feeding regimen does not
#
####


# Even though we cannot estimate the log-rank stastical significance for the difference 
# between treatment curves, let's go ahead and plot it's kaplan-meier survival
# functions anyway to get a visual idea of what the treatments look like 

# fit a Kaplan-meier model for survival by treatment 
km1 <- survfit(Surv (Time, Death) ~ Treatment, data=surv.dat, type = "kaplan-meier") 

#see the model output 
km1
summary(km1)

# Now let's plot all of our survival curves
treat.col <- c("black", "firebrick4", "red2", "darkgoldenrod3", "goldenrod1", "darkgreen", "green3", "blue4", "royalblue2")
new.pal <- c("#131513", "#E02D00", "#FF7E33", "#D6A70A", "#ECDD09", 
             "#075F3C", "#7BB12F", "#35516E", "#3F88C5")

plot(km1, conf.int=FALSE, 
     xlab = "Time (days)", 
     ylab = "Proportion Alive",
     mark.time=T, 
     ylim = c(0.6,1.0), 
     las=1, lwd = 2,
     col=new.pal)
legend(0,0.85, legend=c("Amb.Ctrl", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"),
       lty=1, lwd=1.5, cex=0.8,col=treat.col)


################################################################################
#
# Now let's take a look at Cox  Proportional Hazard analysis.This takes into 
# account any co-variates that might impact the survival of our individuals. 
# We can use this model to determine what the hazard ratio #is between two or 
# more treatments, i.e. how are the individuals affected by #treatment A vs 
# treatment B (not predictive, cannot predict or estimate future survival)
#
################################################################################

# Let's fit our null model first 
cox.0 <- coxph(Surv(Time, Death)~1, data=surv.dat)

# now fit a cox ph model that includes all of our predictor variables
cox.mod <- coxph(Surv(Time, Death) ~ Temperature + Plastic + Food + Light + Sym.State,
                 data = surv.dat)
cox.mod
summary(cox.mod)

##
# a note from the developer about the warning: 
#
#    "Loglik converged before variable  1 ; coefficient may be infinite"
#
# When one of the coefficients goes to infinity in a Cox model, the Wald 
# test of significance beta/se(beta) breaks down, and is no longer reliable.  
# The LR test however is still valid.  Hence routines like stepAIC are ok.
# So are predicted values, residuals, etc etc.  In fact it is pretty much only the 
# Wald test that needs to be ignored: it is based on a Taylor series that simply 
# doesn't work that far from zero.  Oops -- confidence intervals based on the se 
# are also useless.
##

# Let's try and test the proportional hazards assumptions will our cox.mod
# the PH assumption can be checked with statistical tests and graphical shoenfeld residuals 
# schoenfeld residuals are indepenent of time: a plot that shoes a non-random pattern against time
# is evidence that the PH assumption has been violated 

# the coxzph() function tests the assumption of proportional hazards for each covariate included in a model
# for each covariate, the function correlaes the scaled schoenfeld residuals with time to test for independence 
# between the residuals and time. it also perfoms a global test for the whole model 

# the proportional hazard assumption is supported by a non-significant relationship between the residuals and time:
# if the relationship is significant, the assumptions of proportional hazards have been violated

# this is the model we'll test 
cox.mod <- coxph(Surv(Time, Death) ~ Temperature + Plastic + Food + Light + Sym.State,
                 data = surv.dat)

# run the assumptions test 
test.ph <- cox.zph(cox.mod)
test.ph

# RESULTS:
#                chisq df    p
#Temperature 4.41e-09  1 0.9999
#Plastic     4.35e-01  1 0.5096
#Food        1.66e+00  1 0.1980
#Light       5.52e-01  1 0.4574
#Sym.State   7.96e+00  1 0.0048
#GLOBAL      1.15e+01  5 0.0418

# based on that test: there is a significant association between the residuals of sym.state and time, and is therefore 
# violating the ph assumptions, while all other covariates do not. The global test (for the whole model) does violate
# the assumptions though

# we can also show this test graphically:
ggcoxzph(test.ph)

##
# a note about reading schoenfeld's test
#
# systematic departures from a horizontal line are indicative of non-proportional 
# hazards, since proportional hazards assumes that estimates 𝛽1,𝛽2,𝛽3 do not 
# vary much over time. A violations of proportional hazards assumption can be resolved by:
# Adding covariate*time interaction, Stratification
##

# Test for influential observations / outliers 
ggcoxdiagnostics(cox.mod, 
                 type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw())

# deviance residuals is a normalized transformation of the martingale residual
# should be roughly symmetrically distributed around 0 with a sd of +/- 1
ggcoxdiagnostics(cox.mod, 
                 type = "deviance",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw())
# + values correspond to individuals that "died too soon" compared to expected
# survival times while - values correspond to those that "lived to long"
# very large or small values are outliers and are poorly predicted by the model 

# let's write another model without sym.state to see if our assumptions and 
# residuals improve

mod2 <- coxph(Surv(Time, Death) ~ Temperature + Plastic + Food + Light,
                         data = surv.dat)
# run the assumptions test 
test.mod2 <- cox.zph(mod2)
test.mod2
# results 
#              chisq df    p
#Temperature 3.77e-09  1 1.00
#Plastic     4.72e-01  1 0.49
#Food        1.76e+00  1 0.18
#Light       5.77e-01  1 0.45
#GLOBAL      3.34e+00  4 0.50

# graphically
ggcoxzph(test.mod2)

# Test for influential observations / outliers 
ggcoxdiagnostics(mod2, 
                 type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw())
# deviance
ggcoxdiagnostics(mod2, 
                 type = "deviance",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw())








# let's write a model that does not include temperature 
cox.mod2 <- coxph(Surv(Time, Death) ~ Plastic + Food + Light + Sym.State,
                  data= surv.dat)
cox.mod2
summary(cox.mod2)

# lets look at this model output visually using a forest plot 
ggforest(cox.mod2, data=surv.dat)

# let's write another model that drops the least significant variable 
cox.mod3 <- coxph(Surv(Time,Death)~ Plastic + Food + Sym.State, data=surv.dat)

# now we can use the anova function to compare these two models to see the 
# log-likehood scores (we like to see high LL scores - model fits the data)
anova(cox.0, cox.mod2, cox.mod3)

# we can also extract the partial log likelihood 
logLik(cox.mod2)
logLik(cox.mod3)

# and therefore can call AIC to determine which model is better
AIC(cox.0, cox.mod, cox.mod2,cox.mod3)
# model 3 is better but just by a fraction 

# let's go ahead and remove the next least significant variable 
cox.mod4 <- coxph(Surv(Time,Death) ~ Plastic + Sym.State, data=surv.dat)

# compare all  models 
anova(cox.mod2, cox.mod3, cox.mod4)
AIC(cox.0, cox.mod, cox.mod2, cox.mod3, cox.mod4)

# this last model (plastic + sym.state is the best but just barely)

####

# Now let's go ahead and test our model assumptions 

# we'll start with our full model 
full.hazard <- cox.zph(cox.mod)
print(full.hazard)

par(mfrow=c(3,2)) 
plot(full.hazard)

par(mfrow=c(3,2)) 
ggcoxzph(full.hazard)

# same plot just in ggplot 
ggcoxdiagnostics(cox.mod, type="schoenfeld", 
                 linear.predictions = FALSE, 
                 ggtheme = theme_bw())


