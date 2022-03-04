###########################################################
#
#                 Libraries
#
###########################################################

library(ggplot2)
library(patchwork)
library(Rmisc)

getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Respiration")
###########################################################
#
#               Ctrl SALTWATER DATA
#
###########################################################


#### SW data management ####

# load APO Ctrl SW data
apo.dat <- read.csv("Apo_Ctrl_SW_rate.csv", header=TRUE)
# load SYM Ctrl SW data
sym.dat <- read.csv("Sym_Ctrl_SW_rate.csv", header=TRUE)

# Rename the Vials to correspond with the sym state
apo.dat$vial[apo.dat$vial == "A2"] <- "A.A2"
apo.dat$vial[apo.dat$vial == "A3"] <- "A.A3"
apo.dat$vial[apo.dat$vial == "A4"] <- "A.A4"
apo.dat$vial[apo.dat$vial == "A5"] <- "A.A5"
apo.dat$vial[apo.dat$vial == "A6"] <- "A.A6"

apo.dat$vial[apo.dat$vial == "B2"] <- "A.B2"
apo.dat$vial[apo.dat$vial == "B3"] <- "A.B3"
apo.dat$vial[apo.dat$vial == "B4"] <- "A.B4"
apo.dat$vial[apo.dat$vial == "B5"] <- "A.B5"
apo.dat$vial[apo.dat$vial == "B6"] <- "A.B6"

apo.dat$vial[apo.dat$vial == "C2"] <- "A.C2"
apo.dat$vial[apo.dat$vial == "C3"] <- "A.C3"
apo.dat$vial[apo.dat$vial == "C4"] <- "A.C4"
apo.dat$vial[apo.dat$vial == "C5"] <- "A.C5"
apo.dat$vial[apo.dat$vial == "C6"] <- "A.C6"

apo.dat$vial[apo.dat$vial == "D2"] <- "A.D2"
apo.dat$vial[apo.dat$vial == "D3"] <- "A.D3"
apo.dat$vial[apo.dat$vial == "D4"] <- "A.D4"
apo.dat$vial[apo.dat$vial == "D5"] <- "A.D5"
apo.dat$vial[apo.dat$vial == "D6"] <- "A.D6"

# SYM 
sym.dat$vial[sym.dat$vial == "A2"] <- "S.A2"
sym.dat$vial[sym.dat$vial == "A3"] <- "S.A3"
sym.dat$vial[sym.dat$vial == "A4"] <- "S.A4"
sym.dat$vial[sym.dat$vial == "A5"] <- "S.A5"
sym.dat$vial[sym.dat$vial == "A6"] <- "S.A6"

sym.dat$vial[sym.dat$vial == "B2"] <- "S.B2"
sym.dat$vial[sym.dat$vial == "B3"] <- "S.B3"
sym.dat$vial[sym.dat$vial == "B4"] <- "S.B4"
sym.dat$vial[sym.dat$vial == "B5"] <- "S.B5"
sym.dat$vial[sym.dat$vial == "B6"] <- "S.B6"

sym.dat$vial[sym.dat$vial == "C2"] <- "S.C2"
sym.dat$vial[sym.dat$vial == "C3"] <- "S.C3"
sym.dat$vial[sym.dat$vial == "C4"] <- "S.C4"
sym.dat$vial[sym.dat$vial == "C5"] <- "S.C5"
sym.dat$vial[sym.dat$vial == "C6"] <- "S.C6"

sym.dat$vial[sym.dat$vial == "D2"] <- "S.D2"
sym.dat$vial[sym.dat$vial == "D3"] <- "S.D3"
sym.dat$vial[sym.dat$vial == "D4"] <- "S.D4"
sym.dat$vial[sym.dat$vial == "D5"] <- "S.D5"
sym.dat$vial[sym.dat$vial == "D6"] <- "S.D6"

# List the two dfs to merge
ctrl.SW.list <- list(sym.dat, apo.dat)
lapply(ctrl.SW.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
ctrl.SW.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  ctrl.SW.list
)

#### SW data plotting ####

# Visualize
g1 <- ggplot(ctrl.SW.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.3, size = 0.8, show.legend = FALSE) +
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("Control, T3, Saltwater") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  theme_classic() 
g1

# visualize by sym state
g2 <- ggplot(ctrl.SW.dat, aes(Time.Elapsed.Min, value, col = sym.state)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("Control, T3, Saltwater") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  scale_color_manual(values = c("coral4", "gray10")) +
  theme_classic() 
g2

(g1+g2)

#### SW data summary stats ####

# Load MO2_DF files for both phenotypes 
apo.sw.MO2 <- read.csv("Apo_Ctrl_3_SW_allrate.csv", header=TRUE)
sym.sw.MO2 <- read.csv("Sym_Ctrl_3_SW_allrate.csv", header=TRUE) 

# join apo and sym data frames
MO2_df <- rbind(apo.sw.MO2, sym.sw.MO2)
MO2_df

# Summarize mass.specific rate by Phenotype
resp.avg <- summarySE(MO2_df, measurevar = "mass.specific", groupvars = "Sym.state")

# add experimental identifiers
resp.avg$Day <- "22"
resp.avg$Bacteria <- "SW"
resp.avg$Treatment <- "Control"
View(resp.avg)

# summaraize mass.specific rate for all 
sumrate <- summarySE(MO2_df, measurevar = "mass.specific")
# add experimental identifiers
sumrate$Day <- "22"
sumrate$Bacteria <- "SW"
sumrate$Treatment <- "Control"
View(sumrate)


###########################################################
#
#               Ctrl E.COLI DATA
#
###########################################################

#### EC data management ####

# load APO Ctrl EC data
apo.ECdat <- read.csv("Apo_Ctrl_EC_rate.csv", header=TRUE)
# load SYM Ctrl EC data
sym.ECdat <- read.csv("Sym_Ctrl_EC_rate.csv", header=TRUE)

# Rename the Vials to correspond with the sym state
apo.ECdat$vial[apo.ECdat$vial == "A2"] <- "A.A2"
apo.ECdat$vial[apo.ECdat$vial == "A3"] <- "A.A3"
apo.ECdat$vial[apo.ECdat$vial == "A4"] <- "A.A4"
apo.ECdat$vial[apo.ECdat$vial == "A5"] <- "A.A5"
apo.ECdat$vial[apo.ECdat$vial == "A6"] <- "A.A6"

apo.ECdat$vial[apo.ECdat$vial == "B2"] <- "A.B2"
apo.ECdat$vial[apo.ECdat$vial == "B3"] <- "A.B3"
apo.ECdat$vial[apo.ECdat$vial == "B4"] <- "A.B4"
apo.ECdat$vial[apo.ECdat$vial == "B5"] <- "A.B5"
apo.ECdat$vial[apo.ECdat$vial == "B6"] <- "A.B6"

apo.ECdat$vial[apo.ECdat$vial == "C2"] <- "A.C2"
apo.ECdat$vial[apo.ECdat$vial == "C3"] <- "A.C3"
apo.ECdat$vial[apo.ECdat$vial == "C4"] <- "A.C4"
apo.ECdat$vial[apo.ECdat$vial == "C5"] <- "A.C5"
apo.ECdat$vial[apo.ECdat$vial == "C6"] <- "A.C6"

apo.ECdat$vial[apo.ECdat$vial == "D2"] <- "A.D2"
apo.ECdat$vial[apo.ECdat$vial == "D3"] <- "A.D3"
apo.ECdat$vial[apo.ECdat$vial == "D4"] <- "A.D4"
apo.ECdat$vial[apo.ECdat$vial == "D5"] <- "A.D5"
apo.ECdat$vial[apo.ECdat$vial == "D6"] <- "A.D6"

# SYM 
sym.ECdat$vial[sym.ECdat$vial == "A2"] <- "S.A2"
sym.ECdat$vial[sym.ECdat$vial == "A3"] <- "S.A3"
sym.ECdat$vial[sym.ECdat$vial == "A4"] <- "S.A4"
sym.ECdat$vial[sym.ECdat$vial == "A5"] <- "S.A5"
sym.ECdat$vial[sym.ECdat$vial == "A6"] <- "S.A6"

sym.ECdat$vial[sym.ECdat$vial == "B2"] <- "S.B2"
sym.ECdat$vial[sym.ECdat$vial == "B3"] <- "S.B3"
sym.ECdat$vial[sym.ECdat$vial == "B4"] <- "S.B4"
sym.ECdat$vial[sym.ECdat$vial == "B5"] <- "S.B5"
sym.ECdat$vial[sym.ECdat$vial == "B6"] <- "S.B6"

sym.ECdat$vial[sym.ECdat$vial == "C2"] <- "S.C2"
sym.ECdat$vial[sym.ECdat$vial == "C3"] <- "S.C3"
sym.ECdat$vial[sym.ECdat$vial == "C4"] <- "S.C4"
sym.ECdat$vial[sym.ECdat$vial == "C5"] <- "S.C5"
sym.ECdat$vial[sym.ECdat$vial == "C6"] <- "S.C6"

sym.ECdat$vial[sym.ECdat$vial == "D2"] <- "S.D2"
sym.ECdat$vial[sym.ECdat$vial == "D3"] <- "S.D3"
sym.ECdat$vial[sym.ECdat$vial == "D4"] <- "S.D4"
sym.ECdat$vial[sym.ECdat$vial == "D5"] <- "S.D5"
sym.ECdat$vial[sym.ECdat$vial == "D6"] <- "S.D6"

# List the two dfs to merge
ctrl.EC.list <- list(sym.ECdat, apo.ECdat)
lapply(ctrl.EC.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
ctrl.EC.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  ctrl.EC.list
)

#### SW data plotting ####

# Visualize
g1 <- ggplot(ctrl.EC.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.3, size = 0.8, show.legend = FALSE) +
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("Control, T3, E.Coli") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  theme_classic() 
g1

# visualize by sym state
g2 <- ggplot(ctrl.EC.dat, aes(Time.Elapsed.Min, value, col = sym.state)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("Control, T3, E.Coli") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  scale_color_brewer(palette = "Set1") +
  theme_classic() 
g2

(g1+g2)

#### SW data summary stats ####

# Load MO2_DF files for both phenotypes 
apo.ec.MO2 <- read.csv("Apo_Ctrl_3_EC_allrate.csv", header=TRUE)
sym.ec.MO2 <- read.csv("Sym_Ctrl_3_EC_allrate.csv", header=TRUE)

# join apo and sym data frames
MO2_ec_df <- rbind(apo.ec.MO2, sym.ec.MO2)
MO2_ec_df

# Summarize mass.specific rate by Phenotype
ec.resp.avg <- summarySE(MO2_ec_df, measurevar = "mass.specific", groupvars = "Sym.state")

# add experimental identifiers
ec.resp.avg$Day <- "22"
ec.resp.avg$Bacteria <- "EC"
ec.resp.avg$Treatment <- "Control"
View(ec.resp.avg)

# summaraize mass.specific rate for all 
ec.sumrate <- summarySE(MO2_ec_df, measurevar = "mass.specific")
# add experimental identifiers
ec.sumrate$Day <- "22"
ec.sumrate$Bacteria <- "EC"
ec.sumrate$Treatment <- "Control"
View(ec.sumrate)

