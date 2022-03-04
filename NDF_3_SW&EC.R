# Rotjan Lab Respirometry data analysis STANDARD FORMAT
# Script Authors: Caroline Fleming and Sarah Speroff
# Date script last edited: 06/01/2021

##########################################################
#
#       EXPERIMENTAL INFORMATION
#
#       Date: 10/25/2021
#       Experiment name NDF_3_10.25.21_ecoli_and_sw:
#       Investigator name: S. Speroff
#
##########################################################


##########################################################
#
#       Set working directory + install packages  
#
##########################################################
# set directory
# getwd()
setwd("/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Respiration")

# libraries
library(respR)

##########################################################
#
#                Import + clean data -- CSV
#
##########################################################

# data import
data <- read.csv("NDF_3_10.25.21_ecoli_and_sw.xlsx - formated.csv", header=TRUE)
str(data)

# remove unecesary columns
data <- subset(data, select = -c(X, 
                                 Tm...C., 
                                 p..mbar., 
                                 Salinity..g.1000g.,
                                 T_internal...C., 
                                 Error))
# rename headers
names(data)[names(data) == "Time.Min."] <- "Minutes" 

# convert all experimental chambers to numeric
# note that if this is a large dataset over a long period of time, you will likely introduce NAs by coercion (that's okay)
data$A1=as.numeric(data$A1)
data$A2=as.numeric(data$A2)
data$A3=as.numeric(data$A3)
data$A4=as.numeric(data$A4)
data$A5=as.numeric(data$A5)
data$A6=as.numeric(data$A6)
data$B1=as.numeric(data$B1)
data$B2=as.numeric(data$B2)
data$B3=as.numeric(data$B3)
data$B4=as.numeric(data$B4)
data$B5=as.numeric(data$B5)
data$B6=as.numeric(data$B6)
data$C1=as.numeric(data$C1)
data$C2=as.numeric(data$C2)
data$C3=as.numeric(data$C3)
data$C4=as.numeric(data$C4)
data$C5=as.numeric(data$C5)
data$C6=as.numeric(data$C6)
data$D1=as.numeric(data$D1)
data$D2=as.numeric(data$D2)
data$D3=as.numeric(data$D3)
data$D4=as.numeric(data$D4)
data$D5=as.numeric(data$D5)
data$D6=as.numeric(data$D6)

# new time elapsed (sec) interval column
for (i in 1:nrow(data)){ 
  x <- nrow(data)
  s <- seq(from = 0, by = 15, length.out = x)
  data$Time.Elapsed.Sec <- (rep(s))
}

# new time elapsed (min) interval column 
for (i in 1:nrow(data)){
  data$Time.Elapsed.Min <- data$Time.Elapsed.Sec/60
}

# final data check
str(data)

###########################################################
#
#                Experiment Identifiers
#
###########################################################

#KEY
# SW = SW control
# SYM, APO = coral
# EC = E. Coli
# SYM_EC, APO_EC = coral and E. Coli
# SW_EC = sea water and E. Coli

# Vial Setup - 
# A1, B1: saltwater controls (column #3, 4)
# C1: saltwater + E. coli controls (column #5)
# D1: Negative O2 control (column #6)
# A2 - A6 & B2 - B6: SYM fragments
# C2 - C6 & D2 - D6: APO fragments

###########################################################
#
#         scale -O2 control to re-zero baseline
#
###########################################################

# calculate the mean value of our negative control in a new df
neg <- data[,3:26] - mean(data[,6])

# create new columns in neg to match the time values in data
library(tibble)
neg <- neg %>%
  add_column(Minutes = data$Minutes,
             .before = "A1") %>%
  add_column(Date.Time = data$Date.Time,
             .before = "Minutes") %>%
  add_column(Time.Elapsed.Sec = data$Time.Elapsed.Sec,
             .after = "D6")  %>%
  add_column(Time.Elapsed.Min = data$Time.Elapsed.Min,
             .after = "Time.Elapsed.Sec")

# change name back to data
data <- neg

###########################################################
#
#                Saltwater Controls 
#
###########################################################

# Make a SW control dataframe using columns 3 & 4
SW.data <- data[c(28, 3, 4)] 

# SW inspect object, with all oxygen values pooled
SW_insp <- inspect(SW.data,
                   time = 1, 
                   oxygen = c(2, 3),
                   plot = TRUE)

# check assumptions of inspect object 
print(SW_insp)

# Calc background rate of sw subset data 
SW.rate <- calc_rate.bg(SW.data, 
                        time = 1,
                        oxygen = c(2, 3))

# print calculated rates & average rate
print(SW.rate)

###########################################################
#
#                Saltwater + E. Coli Controls 
#
###########################################################

# Make a SW+EC control dataframe using column 5
SW.EC.data <- data[c(28, 5, 10)] 

# Inspect 
SW_EC_insp <- inspect(SW.EC.data,
                      time = 1, 
                      oxygen = c(2,3),
                      plot = TRUE)

# check assumptions of inspect object 
print(SW_EC_insp)

# Subset the data to remove NAs at the end of the time series 
SW.EC.data <- SW.EC.data[1:934,]

# Re Inspect 
SW_EC_insp <- inspect(SW.EC.data,
                      time = 1, 
                      oxygen = c(2,3),
                      plot = TRUE)


# Calc background rate of subset data 
SW.EC.rate <- calc_rate.bg(SW.EC.data, 
                           time = 1,
                           oxygen = c(2, 3))

# print calculated rates & average rate
print(SW.EC.rate)

###########################################################
#
#               Negative Oxygen Controls 
#
###########################################################

# Make a -O2 control dataframe using column 6
NEG.O2.data <- data[c(28, 6)]

# Inspect 
NEG_O2_insp <- inspect(NEG.O2.data,
                       time = 1, 
                       oxygen = 2,
                       plot = TRUE)
# Print
print(NEG_O2_insp)

# Calc background rate of sw subset data 
NEG.O2.rate <- calc_rate.bg(NEG.O2.data, 
                            time = 1,
                            oxygen = 2)

# print calculated rates & average rate
print(NEG.O2.rate)

###########################################################
#
#                   SW CHALLENGE 
#                  Vials A2 - A5
#
###########################################################

########### A2 ###########

# New DF
A2.data <- data[c(28, 7)]

# Inspect
A2_insp <- inspect(A2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

print(A2_insp)

# Calculate Rate
A2_rate <- calc_rate(A2_insp, 
                     from = 1,
                     to = nrow(A2.data),
                     by = "row")

# Adjust rate to SW bg
A2_adjusted_rate <- adjust_rate(A2_rate, SW.rate)

# Calculate mass-specific rate 
MO2_A2 <- convert_rate(A2_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.073,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_A2)

########### A3 ###########

# New DF
A3.data <- data[c(28, 11)]

# Inspect
A3_insp <- inspect(A3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(A3_insp)

# Subset the data to remove NAs at the end of the time series 
A3.data <- A3.data[1:953,]

# Re-inspect
A3_insp <- inspect(A3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
A3_rate <- calc_rate(A3_insp, 
                     from = 1,
                     to = nrow(A3.data),
                     by = "row")

# Adjust rate to SW bg
A3_adjusted_rate <- adjust_rate(A3_rate, SW.rate)

# Calculate mass-specific rate 
MO2_A3 <- convert_rate(A3_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.147,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_A2)

########### A4 ###########

# New DF
A4.data <- data[c(28, 15)]

# Inspect
A4_insp <- inspect(A4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(A4_insp)

# Subset the data to remove NAs at the end of the time series 
A4.data <- A4.data[1:951,]

# Re-inspect
A4_insp <- inspect(A4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
A4_rate <- calc_rate(A4_insp, 
                     from = 1,
                     to = nrow(A4.data),
                     by = "row")

# Adjust rate to SW bg
A4_adjusted_rate <- adjust_rate(A4_rate, SW.rate)

# Calculate mass-specific rate 
MO2_A4 <- convert_rate(A4_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.263,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_A4)

########### A5 ###########

# New DF
A5.data <- data[c(28, 19)]

# Inspect
A5_insp <- inspect(A5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(A5_insp)

# Subset the data to remove NAs at the end of the time series 
A5.data <- A5.data[1:949,]

# Re-inspect
A5_insp <- inspect(A5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
A5_rate <- calc_rate(A5_insp, 
                     from = 1,
                     to = nrow(A5.data),
                     by = "row")

# Adjust rate to SW bg
A5_adjusted_rate <- adjust_rate(A5_rate, SW.rate)

# Calculate mass-specific rate 
MO2_A5 <- convert_rate(A5_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.113,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_A5)

########### A6 ###########

# New DF
#A6.data <- data[c(28, 23)]

# Inspect
#A6_insp <- inspect(A6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)
# Print
#print(A6_insp)

# Subset the data to remove NAs at the end of the time series 
#A6.data <- A6.data[1:950,]

# Re-inspect
#A6_insp <- inspect(A6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)

# Calculate Rate
#A6_rate <- calc_rate(A6_insp, 
#                     from = 1,
#                     to = nrow(A6.data),
#                     by = "row")

# Adjust rate to SW bg
#A6_adjusted_rate <- adjust_rate(A6_rate, SW.rate)

# Calculate mass-specific rate 
#MO2_A6 <- convert_rate(A6_adjusted_rate,
#                       o2.unit = "%",            # original O2 units
#                       time.unit = "min",        # time units 
#                       output.unit = "umol/min/g",     # desired rate units 
#                       volume = 1.9,             # chamber liquid volume
#                       mass = 0.125,              # mass in GRAMS
#                       t = 19,                 # temperature
#                       S = 35,                   # salinity 
#                       P = 1006.5/1013)               # atmospheric pressure

#summary(MO2_A6)


###########################################################
#
#                   EC CHALLENGE
#                 Vials C2 - C5
#
###########################################################

########### C2 ###########

# New DF
C2.data <- data[c(28, 9)]

# Inspect
C2_insp <- inspect(C2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(C2_insp)

# Subset the data to remove NAs at the end of the time series 
C2.data <- C2.data[1:947,]

# Re-inspect
C2_insp <- inspect(C2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
C2_rate <- calc_rate(C2_insp, 
                     from = 1,
                     to = nrow(C2.data),
                     by = "row")

# Adjust rate to SW bg
C2_adjusted_rate <- adjust_rate(C2_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_C2 <- convert_rate(C2_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.16,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_C2)

########### C3 ###########

# New DF
C3.data <- data[c(28, 13)]

# Inspect
C3_insp <- inspect(C3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(C3_insp)

# Subset the data to remove NAs at the end of the time series 
C3.data <- C3.data[1:203,]

# Re-inspect
C3_insp <- inspect(C3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
C3_rate <- calc_rate(C3_insp, 
                     from = 1,
                     to = nrow(C3.data),
                     by = "row")

# Adjust rate to SW bg
C3_adjusted_rate <- adjust_rate(C3_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_C3 <- convert_rate(C3_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.282,              # mass in GRAMS
                       t = 19.8,                 # temperature
                       S = 35,                   # salinity 
                       P = 1017.7/1013)               # atmospheric pressure

summary(MO2_C3)

########### C4 ###########

# New DF
C4.data <- data[c(28, 17)]

# Inspect
C4_insp <- inspect(C4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(C4_insp)

# Subset the data to remove NAs at the end of the time series 
C4.data <- C4.data[1:942,]

# Re-inspect
C4_insp <- inspect(C4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
C4_rate <- calc_rate(C4_insp, 
                     from = 1,
                     to = nrow(C4.data),
                     by = "row")

# Adjust rate to SW bg
C4_adjusted_rate <- adjust_rate(C4_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_C4<- convert_rate(C4_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.106,              # mass in GRAMS
                      t = 19.8,                 # temperature
                      S = 35,                   # salinity 
                      P = 1017.7/1013)               # atmospheric pressure

summary(MO2_C4)

########### C5 ###########

# New DF
C5.data <- data[c(28, 21)]

# Inspect
C5_insp <- inspect(C5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(C5_insp)

# Subset the data to remove NAs at the end of the time series 
C5.data <- C5.data[1:939,]

# Re-inspect
C5_insp <- inspect(C5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
C5_rate <- calc_rate(C5_insp, 
                     from = 1,
                     to = nrow(C5.data),
                     by = "row")

# Adjust rate to SW bg
C5_adjusted_rate <- adjust_rate(C5_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_C5<- convert_rate(C5_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.188,              # mass in GRAMS
                      t = 19.8,                 # temperature
                      S = 35,                   # salinity 
                      P = 1017.7/1013)               # atmospheric pressure

summary(MO2_C5)

########### C6 ###########

# New DF
#C6.data <- data[c(28, 25)]

# Inspect
#C6_insp <- inspect(C6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)
# Print
#print(C6_insp)

# Subset the data to remove NAs at the end of the time series 
#C6.data <- C6.data[1:916,]

# Re-inspect
#C6_insp <- inspect(C6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)

# Calculate Rate
#C6_rate <- calc_rate(C6_insp, 
#                     from = 1,
#                     to = nrow(C6.data),
#                     by = "row")

# Adjust rate to SW bg
#C6_adjusted_rate <- adjust_rate(C6_rate, SW.rate)

# Calculate mass-specific rate 
#MO2_C6<- convert_rate(C6_adjusted_rate,
#                      o2.unit = "%",            # original O2 units
#                      time.unit = "min",        # time units 
#                      output.unit = "umol/min/g",     # desired rate units 
#                      volume = 1.9,             # chamber liquid volume
#                      mass = 0.21,              # mass in GRAMS
#                      t = 19,                 # temperature
#                      S = 35,                   # salinity 
#                      P = 1006.5/1013)               # atmospheric pressure

#summary(MO2_C6)


###########################################################
#
#           Compile Rate Data for EACH challenge
#
###########################################################

#gather all rates for SW
MO2_list_SW <-list (MO2_A2, MO2_A3, MO2_A4, MO2_A5)
# and EC
MO2_list_EC <-list (MO2_C2, MO2_C3, MO2_C4, MO2_C5)

# change to df
MO2_SW_df <- data.frame(matrix(unlist(MO2_list_SW), nrow=length(MO2_list_SW), byrow=TRUE))
MO2_EC_df <- data.frame(matrix(unlist(MO2_list_EC), nrow=length(MO2_list_EC), byrow=TRUE))

# change names
names(MO2_SW_df)[names(MO2_SW_df) == "X1"] <- "X1"
names(MO2_SW_df)[names(MO2_SW_df) == "X2"] <- "X2"
names(MO2_SW_df)[names(MO2_SW_df) == "X3"] <- "input.rate"
names(MO2_SW_df)[names(MO2_SW_df) == "X4"] <- "converted.rate"
names(MO2_SW_df)[names(MO2_SW_df) == "X5"] <- "volumetric"
names(MO2_SW_df)[names(MO2_SW_df) == "X6"] <- "mass.specific"
names(MO2_SW_df)[names(MO2_SW_df) == "X7"] <- "input.o2.unit"
names(MO2_SW_df)[names(MO2_SW_df) == "X8"] <- "input.time.unit"
names(MO2_SW_df)[names(MO2_SW_df) == "X9"] <- "output.unit"

# change names
names(MO2_EC_df)[names(MO2_EC_df) == "X1"] <- "X1"
names(MO2_EC_df)[names(MO2_EC_df) == "X2"] <- "X2"
names(MO2_EC_df)[names(MO2_EC_df) == "X3"] <- "input.rate"
names(MO2_EC_df)[names(MO2_EC_df) == "X4"] <- "converted.rate"
names(MO2_EC_df)[names(MO2_EC_df) == "X5"] <- "volumetric"
names(MO2_EC_df)[names(MO2_EC_df) == "X6"] <- "mass.specific"
names(MO2_EC_df)[names(MO2_EC_df) == "X7"] <- "input.o2.unit"
names(MO2_EC_df)[names(MO2_EC_df) == "X8"] <- "input.time.unit"
names(MO2_EC_df)[names(MO2_EC_df) == "X9"] <- "output.unit"

# Remove duplicate columns x1 and x2 
MO2_SW_df <- subset(MO2_SW_df, select = -c(X1, X2))
MO2_EC_df <- subset(MO2_EC_df, select = -c(X1, X2))

# Add column for SW vials
MO2_SW_df$vial <- (c("A2", "A3", "A4", "A5"))
# Add column for EC vials
MO2_EC_df$vial <- (c("C2", "C3", "C4", "C5"))

# Add B.TAG for individuals in those respective vials
MO2_SW_df$B.TAG <- (c("W48", "W52", "Y48", "B7"))
MO2_EC_df$B.TAG <- (c("G52", "W51", "O7398", "Y1"))

## Add "day"
MO2_SW_df$Day <- "22"
MO2_EC_df$Day <- "22"

# add sym state
MO2_SW_df$Sym.state <- c(rep("sym", times = 3), rep("apo", times = 1))
MO2_EC_df$Sym.state <- c(rep("sym", times = 2), rep("apo", times = 2))


# Add bacterial treatment
MO2_SW_df$Bacteria <- "SW"
MO2_EC_df$Bacteria <- "EC"

View(MO2_SW_df)
View(MO2_EC_df)

path <- "/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Respiration/AllRateFiles"
write.csv(MO2_df, file.path(path, "NDF_3_SW_allrate.csv"))
write.csv(MO2_df, file.path(path, "NDF_3_EC_allrate.csv"))

###########################################################
#
#                     Visualize
# 
###########################################################


########### SALTWATER ###########

# melt df to long format
library(reshape2)
melt.data.SW <- melt(SW.data, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# plot it 
library(ggplot2)
ggplot(melt.data.SW, aes(Time.Elapsed.Min, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("SW rate") +
  ylab("Oxygen Saturation (%)")+
  xlab("Time (m)") +
  theme_bw()

########### SALTWATER + ECOLI ###########

# melt df to long format
melt.data.EC <- melt(SW.EC.data, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# plot
ggplot(melt.data.EC, aes(Time.Elapsed.Min, value)) +
  geom_point()+
  geom_smooth(method="lm") +
  ggtitle("SE + EC rate") +
  ylab("Oxygen Saturation (%)")+
  xlab("Time (m)") +
  theme_bw()

########### NEGATIVE O2 ###########

# plot
ggplot(NEG.O2.data, aes(Time.Elapsed.Min, D1)) +
  geom_point()+
  geom_smooth(method="lm") +
  ggtitle("SE + EC rate") +
  ylab("Oxygen Saturation (%)")+
  xlab("Time (m)") +
  theme_bw()

########### SYM FRAGMENTS in SW ###########

# Start by making a list of the sym dfs
SW.sym.list <- list(A2.data, A3.data)
lapply(SW.sym.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
SW.sym.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  SW.sym.list
)

# now melt the df so that all Vials are in one row
SW.sym.dat <- melt(SW.sym.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
SW.sym.dat$sym.state <- "sym"

# plot
library(RColorBrewer)
ggplot(SW.sym.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("SYM (sw) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() 

########### SYM FRAGMENTS in EC ###########

# Start by making a list of the sym dfs
EC.sym.list <- list(C2.data, C3.data)
lapply(EC.sym.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
EC.sym.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  EC.sym.list
)

# now melt the df so that all Vials are in one row
EC.sym.dat <- melt(EC.sym.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
EC.sym.dat$sym.state <- "sym"

# plot
library(RColorBrewer)
ggplot(EC.sym.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("SYM (ec) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw()

########### APO FRAGMENTS in SW ###########

# Start by making a list of the apo dfs
SW.apo.list <- list(A4.data, A5.data)
lapply(SW.apo.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
SW.apo.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  SW.apo.list
)

# now melt the df so that all Vials are in one row
SW.apo.dat <- melt(SW.apo.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
SW.apo.dat$sym.state <- "apo"

#plot
ggplot(SW.apo.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("APO (sw) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_bw() 

########### APO FRAGMENTS in EC ###########

# Start by making a list of the apo dfs
EC.apo.list <- list(C4.data, C5.data)
lapply(EC.apo.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
EC.apo.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  EC.apo.list
)

# now melt the df so that all Vials are in one row
EC.apo.dat <- melt(EC.apo.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
EC.apo.dat$sym.state <- "apo"

#plot
ggplot(EC.apo.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("APO (sw) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_bw() 

# join apo and sym data frames
SW.frag.dat <- rbind(SW.apo.dat, SW.sym.dat)
SW.frag.dat

# join apo and sym data frames
EC.frag.dat <- rbind(EC.apo.dat, EC.sym.dat)
EC.frag.dat

# VisualizE

#### SW ####
g1 <- ggplot(SW.frag.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.3, size = 0.8, show.legend = FALSE) +
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NDF, T3, SALTWATER") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  theme_classic() 
g1

# visualize by sym state
g2 <- ggplot(SW.frag.dat, aes(Time.Elapsed.Min, value, col = sym.state)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NDF, T2, SALTWATER") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  scale_color_manual(values = c("coral4", "gray10")) +
  theme_classic() 
g2

library(patchwork)
(g1+g2)

#### EC ####

g3 <- ggplot(EC.frag.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.3, size = 0.8, show.legend = FALSE) +
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NDF, T3, E.COLI") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  theme_classic() 
g3

# visualize by sym state
g4 <- ggplot(EC.frag.dat, aes(Time.Elapsed.Min, value, col = sym.state)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NDF, T2, E.COLI") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  scale_color_manual(values = c("coral4", "gray10")) +
  theme_classic() 
g4

(g3+g4)

###########################################################
#
#                 Summarize Rate data
#
###########################################################

# linear model to average rate of all sym fragments 
SW.apo.lm <- lm(value~Time.Elapsed.Min, data=SW.apo.dat)
summary(SW.apo.lm)

# linear model to average rate of all sym fragments 
EC.apo.lm <- lm(value~Time.Elapsed.Min, data=EC.apo.dat)
summary(EC.apo.lm)

# linear model to average rate of all sym fragments 
SW.sym.lm <- lm(value~Time.Elapsed.Min, data=SW.sym.dat)
summary(SW.sym.lm)

# linear model to average rate of all sym fragments 
EC.sym.lm <- lm(value~Time.Elapsed.Min, data=EC.sym.dat)
summary(EC.sym.lm)


# Summarize mass.specific rate by Phenotype
library(Rmisc)

MO2_SW_df$mass.specific = as.numeric(MO2_SW_df$mass.specific)
MO2_EC_df$mass.specific = as.numeric(MO2_EC_df$mass.specific)


SW.resp.avg <- summarySE(MO2_SW_df, measurevar = "mass.specific", groupvars = "Sym.state")
EC.resp.avg <- summarySE(MO2_EC_df, measurevar = "mass.specific", groupvars = "Sym.state")

# add experimental identifiers
SW.resp.avg$Treatment <- "NDF"
SW.resp.avg$Day <- "22"
SW.resp.avg$Bacteria <- "SW"
View(SW.resp.avg)

# add experimental identifiers
EC.resp.avg$Treatment <- "NDF"
EC.resp.avg$Day <- "22"
EC.resp.avg$Bacteria <- "EC"
View(EC.resp.avg)

# summaraize mass.specific rate for all 
SW.sumrate <- summarySE(MO2_SW_df, measurevar = "mass.specific")
# add experimental identifiers
SW.sumrate$Treatment <- "NDF"
SW.sumrate$Day <- "22"
SW.sumrate$Bacteria <- "SW"
View(SW.sumrate)

# summaraize mass.specific rate for all 
EC.sumrate <- summarySE(MO2_EC_df, measurevar = "mass.specific")
# add experimental identifiers
EC.sumrate$Treatment <- "NDF"
EC.sumrate$Day <- "22"
EC.sumrate$Bacteria <- "EC"
View(EC.sumrate)
