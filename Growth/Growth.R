# Sarah Speroff
# Multiple Stressor growth by mass data analysis 

# MASS IS RECORDED IN G

#set wd
getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Growth")

# load the data 
mass.dat <- read.csv("Full_ ID Metadata - *Frag metadata (xrack&control).csv", header = TRUE)

################################################################################
#
#                          Data Prep
#
################################################################################

names(mass.dat)
head(mass.dat)
summary(mass.dat)
str(mass.dat)
nrow(mass.dat)
length(unique(mass.dat$B.TAG)) # should be 480 
table(mass.dat$Sym.State) #should be even split


# select rows to bring into new df
data <- subset(mass.dat, select = c("B.TAG", "Sym.State","Treat.ID", "Temperature", "Plastic", "Light", "Food",
                                    "Initial.Mass..g.", "Final.Mass..g.", "Days.In.Treatment"))

# change column names 
names(data)[names(data) == "Initial.Mass..g."] <- "Initial" 
names(data)[names(data) == "Final.Mass..g."] <- "Final" 
names(data)[names(data) == "Days.In.Treatment"] <- "Time" 
names(data)[names(data) == "Treat.ID"] <- "Treatment" 

# relevel to compare all to control 
data$Treatment <- factor(data$Treatment, levels = c("control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))

# filter out NAs 
data <- data[complete.cases(data),]

################################################################################
#
#            Calculate average change in mass from initial to final weight
#
################################################################################

# create a new column
data$Change = NA

# Now calculate the change in mass over time 
data$Change = ((data$Final - data$Initial))*1000/data$Time # converting initial and final g to mg and normalizing to time
# units are change in miligrams over time 
data$Change <- round(data$Change, 2)

#summarise the change in mass  
library(Rmisc)
change_means <- summarySE(data, measurevar="Change", groupvars=c("Treatment","Sym.State")) 
change_means

# now plot the change stats
library(ggplot2)
ggplot(change_means, aes(Treatment, Change, col=Sym.State)) +
  geom_point() +
  geom_hline(yintercept = 0, lty=2) +
  geom_segment(aes(xend=Treatment, yend=0))+
  ylab("Net Change Mass (mg/day)")+
  xlab("Treatment") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  theme_bw()

# flip the axes
ggplot(change_means, aes(Change, Treatment, col=Sym.State)) +
  geom_point() +
  #geom_vline(xintercept = 0, lty=2) + 
  geom_vline(xintercept = -0.83430380, col="tan", lty=2) +
  geom_vline(xintercept = -0.69474359, col="black", lty=2) +
  geom_segment(aes(xend=0, yend=Treatment)) +
  ylab("Treatment")+
  xlab("Net Change Mass (mg/day)") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  scale_color_manual(name = "Sym State",
                     labels = c("Apo", "Sym"),
                     values = c("tan","black")) +
  theme_bw()

# Let's look at change in mass not separate by sym state
# summarise
allchange <- summarySE(data, measurevar="Change", groupvars=c("Treatment")) 
allchange
#and plot
ggplot(allchange, aes(Change, Treatment)) +
  geom_point() +
  geom_vline(xintercept = -0.7649682, col="black", lty=2) +
  geom_segment(aes(xend=0, yend=Treatment)) +
  ylab("Treatment")+
  xlab("Net Change Mass (mg/day)") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  theme_bw()


################################################################################
#
#                         Summarise each time point
#
################################################################################

#summarise the initial mass
init_means <- summarySE(data, measurevar = "Initial", groupvars = c("Treatment", "Sym.State"))
init_means
names(init_means)[names(init_means) == "Initial"] <- "Mass" 
init_means$Day <- "Initial"

# summarise final mass
fin_mass <- summarySE(data, measurevar = "Final", groupvars = c("Treatment", "Sym.State"))
fin_mass
names(fin_mass)[names(fin_mass) == "Final"] <- "Mass" 
fin_mass$Day <- "Final"

means_dat <- rbind(init_means, fin_mass)
means_dat$Day <- factor(means_dat$Day, levels = c("Initial", "Final"))


############# plot the means and standard error of the mean for each time !!!!!!!!!!!!!!

mass.p <- ggplot(means_dat, aes(Day, Mass, col = Sym.State)) +
  geom_point() + 
  geom_line(aes(group=Sym.State))+
  geom_errorbar(aes(ymin=Mass-se, ymax=Mass+se), width=0.2) +
  facet_wrap(vars(Treatment)) +
  labs(x = "", y = "Mass (mg)", color="Sym State") +
  theme(legend.position = "top") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  theme_bw()

mass.p + scale_color_manual(values=c("burlywood3", "coral4"))

############# !!!!!!!!!!!!!!



################################################################################

# bring in long formatted data frame to plot change in mass by time points: Initial and Final
long.dat <- read.csv("growth - growthdata.csv")
names(long.dat)

# change column names 
names(long.dat)[names(long.dat) == "Days.In.Treatment"] <- "Time" 
names(long.dat)[names(long.dat) == "Treat.ID"] <- "Treatment" 

# relevel
long.dat$Treatment <- factor(long.dat$Treatment, levels = c("control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))
long.dat$Day <- factor(long.dat$Day, levels = c("Initial", "Final"))

# remove NAs
long.dat <- long.dat[complete.cases(long.dat),]

# keep only those that lasted 22 days
long.dat <- filter(long.dat, Time == "22" )

# plot the initial and final mass 
ggplot(long.dat, aes(Day, Mass, col=Sym.State))+
  geom_jitter() +
  facet_wrap(vars(Treatment))

# can we sumamrise that as well?
means <- summarySE(long.dat, measurevar = "Mass", groupvars = c("Treatment", "Day", "Sym.State"))

## plot, for only fragments that lasted the full 22 days 
ggplot(means, aes(Day, Mass, color = Sym.State)) +
  geom_point() +
  geom_line(aes(group=Sym.State)) +
  facet_wrap(vars(Treatment)) +
  geom_errorbar(aes(ymax = Mass+se, ymin = Mass-se), width = .1) +
  ylab("Mass (g)")+
  xlab("") +
  scale_color_manual(name = "Sym State",
                     labels = c("Apo", "Sym"),
                     values = c("tan","black")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  theme_bw()


################################################################################

# bring in long formatted data frame with mass at ALL TIME POINTS

# load data
decaydat <- read.csv("growth - growthtime.csv")
names(decaydat)

# change column names 
names(decaydat)[names(decaydat) == "Treat.ID"] <- "Treatment" 

# relevel
decaydat$Treatment <- factor(decaydat$Treatment, levels = c("Control", "PDS", "PLS", "NDS", "NLS", "PDF", "PLF", "NDF", "NLF"))
decaydat$Day <- factor(decaydat$Day, levels = c("0", "8", "15", "22"))

# remove NAs
decaydat <- decaydat[complete.cases(decaydat),]

# convert g to mg 
#decaydat$Mass <- decaydat$Mass*1000

# summarise the mass at each interval by treatment and sym state
allsum <- summarySE(decaydat, measurevar = "Mass", groupvars = c("Treatment", "Day", "Sym.State"))
allsum

# plot 
ggplot(allsum, aes(Day, Mass, color = Sym.State)) +
  geom_point() +
  geom_line(aes(group=Sym.State)) +
  facet_wrap(vars(Treatment)) +
  geom_errorbar(aes(ymax = Mass+se, ymin = Mass-se), width = .1) +
  ylab("Mass (g)")+
  xlab("Day") +
  scale_color_manual(name = "Sym State",
                     labels = c("Apo", "Sym"),
                     values = c("tan","black")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  theme_bw()





























###### layer a plot with different data sets?!?

#ggp <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
#  geom_point(data = data1, col = "red") +
#  geom_line(data = data2, col = "blue")
#ggp                                 # Draw plot
