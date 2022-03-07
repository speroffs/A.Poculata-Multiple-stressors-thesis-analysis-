# Rotjan Lab Respirometry data analysis STANDARD FORMAT
# Script Authors: Caroline Fleming and Sarah Speroff
# Date script last edited: 06/01/2021

##########################################################
#
#       EXPERIMENTAL INFORMATION
#
#       Date: 10/21/2021
#       Experiment name: NLS_3_10.21.21_e.coli
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
data <- read.csv("NLS_3_10.21.21_e.coli.xlsx - formated.csv", header=TRUE)
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
# A1: saltwater controls (column #3)
# B1, C1: saltwater + E. coli controls (column #4, 5)
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
SW.data <- data[c(28, 3)] 

# SW inspect object, with all oxygen values pooled
SW_insp <- inspect(SW.data,
                   time = 1, 
                   oxygen = 2, 
                   plot = TRUE)

# check assumptions of inspect object 
print(SW_insp)

# Calc background rate of sw subset data 
SW.rate <- calc_rate.bg(SW.data, 
                        time = 1,
                        oxygen = 2)

# print calculated rates & average rate
print(SW.rate)

###########################################################
#
#                Saltwater + E. Coli Controls 
#
###########################################################

# Make a SW+EC control dataframe using column 5
SW.EC.data <- data[c(28, 4, 5)]

# Inspect 
SW_EC_insp <- inspect(SW.EC.data,
                      time = 1, 
                      oxygen = c(2,3),
                      plot = TRUE)

# check assumptions of inspect object 
print(SW_EC_insp)

# Subset the data to remove NAs at the end of the time series 
#SW.EC.data <- SW.EC.data[1:874,]

# Re Inspect 
#SW_EC_insp <- inspect(SW.EC.data,
#                      time = 1, 
#                      oxygen = c(2,3),
#                      plot = TRUE)

# Calc background rate of subset data 
SW.EC.rate <- calc_rate.bg(SW.EC.data, 
                           time = 1,
                           oxygen = c(2,3))

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
#                   SYM FRAGMENTS 
#               Vials A2 - A6 & B2 - B6
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

#print(A2_insp)

# Subset the data to remove NAs at the end of the time series 
A2.data <- A2.data[1:61,]

# Re Inspect
A2_insp <- inspect(A2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
A2_rate <- calc_rate(A2_insp, 
                     from = 1,
                     to = nrow(A2.data),
                     by = "row")

# Adjust rate to SW bg
A2_adjusted_rate <- adjust_rate(A2_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_A2 <- convert_rate(A2_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.8,             # chamber liquid volume
                       mass = 0.347,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

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
#print(A3_insp)

# Subset the data to remove NAs at the end of the time series 
A3.data <- A3.data[1:64,]

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
A3_adjusted_rate <- adjust_rate(A3_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_A3 <- convert_rate(A3_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.175,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_A3)

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
A4.data <- A4.data[1:935,]

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
A4_adjusted_rate <- adjust_rate(A4_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_A4 <- convert_rate(A4_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.052,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

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
A5.data <- A5.data[1:255,]

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
A5_adjusted_rate <- adjust_rate(A5_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_A5 <- convert_rate(A5_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.139,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_A5)

########### A6 ###########

# New DF
A6.data <- data[c(28, 23)]

# Inspect
A6_insp <- inspect(A6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(A6_insp)

# Subset the data to remove NAs at the end of the time series 
A6.data <- A6.data[1:132,]

# Re-inspect
A6_insp <- inspect(A6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
A6_rate <- calc_rate(A6_insp, 
                     from = 1,
                     to = nrow(A6.data),
                     by = "row")

# Adjust rate to SW bg
A6_adjusted_rate <- adjust_rate(A6_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_A6 <- convert_rate(A6_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.8,             # chamber liquid volume
                       mass = 0.309,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_A6)

########### B2 ###########

# New DF
B2.data <- data[c(28, 8)]

# Inspect
B2_insp <- inspect(B2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

#print(B2_insp)

# Subset the data to remove NAs at the end of the time series 
B2.data <- B2.data[1:71,]

# Re-inspect
B2_insp <- inspect(B2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
print(B2_insp)

# Calculate Rate
B2_rate <- calc_rate(B2_insp, 
                     from = 1,
                     to = nrow(B2.data),
                     by = "row")

# Adjust rate to SW bg
B2_adjusted_rate <- adjust_rate(B2_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_B2 <- convert_rate(B2_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.115,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_B2)

########### B3 ###########

# New DF
B3.data <- data[c(28, 12)]

# Inspect
B3_insp <- inspect(B3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

print(B3_insp)

# Subset the data to remove NAs at the end of the time series 
B3.data <- B3.data[1:451,]

# Re-inspect
B3_insp <- inspect(B3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
print(B3_insp)

# Calculate Rate
B3_rate <- calc_rate(B3_insp, 
                     from = 1,
                     to = nrow(B3.data),
                     by = "row")

# Adjust rate to SW bg
B3_adjusted_rate <- adjust_rate(B3_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_B3 <- convert_rate(B3_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",     # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.121,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_B3)

########### B4 ###########

# New DF
B4.data <- data[c(28, 16)]

# Inspect
B4_insp <- inspect(B4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

print(B4_insp)

# Subset the data to remove NAs at the end of the time series 
B4.data <- B4.data[1:447,]

# Re-inspect
B4_insp <- inspect(B4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
print(B4_insp)

# Calculate Rate
B4_rate <- calc_rate(B4_insp, 
                     from = 1,
                     to = nrow(B4.data),
                     by = "row")

# Adjust rate to SW bg
B4_adjusted_rate <- adjust_rate(B4_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_B4 <- convert_rate(B4_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",  # desired rate units 
                       volume = 1.8,             # chamber liquid volume
                       mass = 0.194,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_B4)

########### B5 ###########

# New DF
B5.data <- data[c(28, 20)]

# Inspect
B5_insp <- inspect(B5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

print(B5_insp)

# Subset the data to remove NAs at the end of the time series 
B5.data <- B5.data[1:106,]

# Re-inspect
B5_insp <- inspect(B5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
print(B5_insp)

# Calculate Rate
B5_rate <- calc_rate(B5_insp, 
                     from = 1,
                     to = nrow(B5.data),
                     by = "row")

# Adjust rate to SW bg
B5_adjusted_rate <- adjust_rate(B5_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_B5 <- convert_rate(B5_adjusted_rate,
                       o2.unit = "%",            # original O2 units
                       time.unit = "min",        # time units 
                       output.unit = "umol/min/g",  # desired rate units 
                       volume = 1.9,             # chamber liquid volume
                       mass = 0.27,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

summary(MO2_B5)

########### B6 ###########

# New DF
#B6.data <- data[c(28, 24)]

# Inspect
#B6_insp <- inspect(B6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)

#print(B6_insp)

# Subset the data to remove NAs at the end of the time series 
#B6.data <- B6.data[1:86,]

# Re-inspect
#B6_insp <- inspect(B6.data,
#                   time = 1,
#                   oxygen = 2,
#                   plot = TRUE)
#print(B6_insp)

# Calculate Rate
#B6_rate <- calc_rate(B6_insp, 
#                     from = 1,
#                     to = nrow(B6.data),
#                     by = "row")

# Adjust rate to SW bg
#B6_adjusted_rate <- adjust_rate(B6_rate, SW.EC.rate)

# Calculate mass-specific rate 
#MO2_B6 <- convert_rate(B6_adjusted_rate,
#                       o2.unit = "%",            # original O2 units
#                       time.unit = "min",        # time units 
#                       output.unit = "umol/min/g",  # desired rate units 
# #                      volume = 1.9,             # chamber liquid volume
##                       mass = 0.287,              # mass in GRAMS
#                       t = 20,                 # temperature
#                       S = 35,                   # salinity 
#                       P = 1015.3/1013)               # atmospheric pressure

#summary(MO2_B6)

###########################################################
#
#                   APO FRAGMENTS 
#               Vials C2 - C6 & D2 - D6
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
C2.data <- C2.data[1:177,]

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
                       mass = 0.115,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

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
#print(C3_insp)

# Subset the data to remove NAs at the end of the time series 
C3.data <- C3.data[1:72,]

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
                       mass = 0.2,              # mass in GRAMS
                       t = 20,                 # temperature
                       S = 35,                   # salinity 
                       P = 1015.3/1013)               # atmospheric pressure

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
#print(C4_insp)

# Subset the data to remove NAs at the end of the time series 
C4.data <- C4.data[1:43,]

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
                      mass = 0.14,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

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
#print(C5_insp)

# Subset the data to remove NAs at the end of the time series 
C5.data <- C5.data[1:42,]

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
                      mass = 0.198,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_C5)

########### C6 ###########

# New DF
C6.data <- data[c(28, 25)]

# Inspect
C6_insp <- inspect(C6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(C6_insp)

# Subset the data to remove NAs at the end of the time series 
C6.data <- C6.data[1:140,]

# Re-inspect
C6_insp <- inspect(C6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
C6_rate <- calc_rate(C6.data, 
                     from = 1,
                     to = nrow(C6.data),
                     by = "row")

# Adjust rate to SW bg
C6_adjusted_rate <- adjust_rate(C6_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_C6<- convert_rate(C6_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.201,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_C6)

########### D2 ###########

# New DF
D2.data <- data[c(28, 10)]

# Inspect
D2_insp <- inspect(D2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
#print(D2_insp)

# Subset the data to remove NAs at the end of the time series 
D2.data <- D2.data[1:78,]

# Re-inspect
D2_insp <- inspect(D2.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
D2_rate <- calc_rate(D2_insp, 
                     from = 1,
                     to = nrow(D2.data),
                     by = "row")

# Adjust rate to SW bg
D2_adjusted_rate <- adjust_rate(D2_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_D2<- convert_rate(D2_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.225,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_D2)

########### D3 ###########

# New DF
D3.data <- data[c(28, 14)]

# Inspect
D3_insp <- inspect(D3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
#print(D3_insp)

# Subset the data to remove NAs at the end of the time series 
D3.data <- D3.data[1:59,]

# Re-inspect
D3_insp <- inspect(D3.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
D3_rate <- calc_rate(D3_insp, 
                     from = 1,
                     to = nrow(D3.data),
                     by = "row")

# Adjust rate to SW bg
D3_adjusted_rate <- adjust_rate(D3_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_D3<- convert_rate(D2_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.126,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_D3)

########### D4 ###########

# New DF
D4.data <- data[c(28, 18)]

# Inspect
D4_insp <- inspect(D4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
#print(D4_insp)

# Subset the data to remove NAs at the end of the time series 
D4.data <- D4.data[1:56,]

# Re-inspect
D4_insp <- inspect(D4.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
D4_rate <- calc_rate(D4_insp, 
                     from = 1,
                     to = nrow(D4.data),
                     by = "row")

# Adjust rate to SW bg
D4_adjusted_rate <- adjust_rate(D4_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_D4<- convert_rate(D4_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.151,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_D4)

########### D5 ###########

# New DF
D5.data <- data[c(28, 22)]

# Inspect
D5_insp <- inspect(D5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
print(D5_insp)

# Subset the data to remove NAs at the end of the time series 
D5.data <- D5.data[1:265,]

# Re-inspect
D5_insp <- inspect(D5.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
D5_rate <- calc_rate(D5_insp, 
                     from = 1,
                     to = nrow(D5.data),
                     by = "row")

# Adjust rate to SW bg
D5_adjusted_rate <- adjust_rate(D5_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_D5<- convert_rate(D5_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.182,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_D5)

########### D6 ###########

# New DF
D6.data <- data[c(28, 26)]

# Inspect
D6_insp <- inspect(D6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)
# Print
#print(D6_insp)

# Subset the data to remove NAs at the end of the time series 
D6.data <- D6.data[1:61,]

# Re-inspect
D6_insp <- inspect(D6.data,
                   time = 1,
                   oxygen = 2,
                   plot = TRUE)

# Calculate Rate
D6_rate <- calc_rate(D6_insp, 
                     from = 1,
                     to = nrow(D6.data),
                     by = "row")

# Adjust rate to SW bg
D6_adjusted_rate <- adjust_rate(D6_rate, SW.EC.rate)

# Calculate mass-specific rate 
MO2_D6<- convert_rate(D6_adjusted_rate,
                      o2.unit = "%",            # original O2 units
                      time.unit = "min",        # time units 
                      output.unit = "umol/min/g",     # desired rate units 
                      volume = 1.9,             # chamber liquid volume
                      mass = 0.247,              # mass in GRAMS
                      t = 20,                 # temperature
                      S = 35,                   # salinity 
                      P = 1015.3/1013)               # atmospheric pressure

summary(MO2_D6)

###########################################################
#
#                   Compile Rate Data
#
###########################################################

#gather all rates 
MO2_list <-list (MO2_A2, MO2_A3, MO2_A4, MO2_A5, MO2_A6,
                 MO2_B2, MO2_B3, MO2_B4, MO2_B5,  
                 MO2_C2, MO2_C3, MO2_C4, MO2_C5, MO2_C6, 
                 MO2_D2, MO2_D3, MO2_D4, MO2_D5, MO2_D6)

# change to df
MO2_df <- data.frame(matrix(unlist(MO2_list), nrow=length(MO2_list), byrow=TRUE))

# change names
names(MO2_df)[names(MO2_df) == "X1"] <- "X1"
names(MO2_df)[names(MO2_df) == "X2"] <- "X2"
names(MO2_df)[names(MO2_df) == "X3"] <- "input.rate"
names(MO2_df)[names(MO2_df) == "X4"] <- "converted.rate"
names(MO2_df)[names(MO2_df) == "X5"] <- "volumetric"
names(MO2_df)[names(MO2_df) == "X6"] <- "mass.specific"
names(MO2_df)[names(MO2_df) == "X7"] <- "input.o2.unit"
names(MO2_df)[names(MO2_df) == "X8"] <- "input.time.unit"
names(MO2_df)[names(MO2_df) == "X9"] <- "output.unit"

# Remove duplicate columns x1 and x2 
MO2_df <- subset(MO2_df, select = -c(X1, X2))

# Add colum for resp vials
MO2_df$vial <- (c("A2", "A3", "A4", "A5", "A6",
                  "B2", "B3", "B4", "B5",
                  "C2", "C3", "C4", "C5", "C6",
                  "D2", "D3", "D4", "D5", "D6"))

# Add B.TAG for individuals in those respective vials
MO2_df$B.TAG <- (c("G40", "G42", "B37", "B39", "B41", 
                   "W34", "W36", "Y32", "Y34",
                   "G80", "G82", "B82", "B84", "B87",
                   "W78", "W80", "Y74", "Y76", "Y78"))
## Add "day"
MO2_df$Day <- "22"

# add sym state
MO2_df$Sym.state <- c(rep("sym", times = 9), rep("apo", times = 10))

# Add bacterial treatment
MO2_df$Bacteria <- "EC"

# Add stress treatment
MO2_df$Treatment <- "NLS"

View(MO2_df)

#path <- "/Users/speroffs/Desktop/A.Poculata-Multiple-stressors-thesis-analysis-/Respiration/AllRateFiles"
#write.csv(MO2_df, file.path(path, "NLS_3_EC_allrate.csv"))

###########################################################
#
#                     Visualize
# 
###########################################################


########### SALTWATER ###########

# plot it 
library(ggplot2)
ggplot(SW.data, aes(Time.Elapsed.Min, A1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("SW rate") +
  ylab("Oxygen Saturation (%)")+
  xlab("Time (m)") +
  theme_bw()

########### SALTWATER + ECOLI ###########

# melt df to long format
library(reshape2)
melt.data <- melt(SW.EC.data, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# plot
ggplot(melt.data, aes(Time.Elapsed.Min, value)) +
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
  ggtitle("Neg rate") +
  ylab("Oxygen Saturation (%)")+
  xlab("Time (m)") +
  theme_bw()

########### SYM FRAGMENTS ###########

# Start by making a list of the sym dfs
sym.list <- list(A2.data, A3.data, A4.data, A5.data, A6.data, 
                 B2.data, B3.data, B4.data, B5.data)
lapply(sym.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
sym.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  sym.list
)

# now melt the df so that all Vials are in one row
sym.dat <- melt(sym.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
sym.dat$sym.state <- "sym"

# plot
library(RColorBrewer)
ggplot(sym.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("SYM (ec) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() 

########### APO FRAGMENTS ###########

# Start by making a list of the apo dfs
apo.list <- list(C2.data, C3.data, C4.data, C5.data, C6.data, 
                 D2.data, D3.data, D4.data, D5.data, D6.data)
lapply(apo.list, function(x) c(toString(dim(x)), toString(names(x))))

# Use Reduce to merge all sym dfs into one
apo.dat <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  apo.list
)

# now melt the df so that all Vials are in one row
apo.dat <- melt(apo.dat, id.vars = "Time.Elapsed.Min", variable.name = "vial")

# add new column for phenotype
apo.dat$sym.state <- "apo"

#plot
ggplot(apo.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("APO (ec) rates") +
  ylab("cO2 (% Air Saturation)")+
  xlab("Time (m)") +
  scale_color_brewer(palette = "BrBG") +
  theme_bw() 

# join apo and sym data frames
allfrag.dat <- rbind(apo.dat, sym.dat)
allfrag.dat

# Visualize
g1 <- ggplot(allfrag.dat, aes(Time.Elapsed.Min, value, col = vial)) +
  geom_point(alpha = 0.3, size = 0.8, show.legend = FALSE) +
  geom_smooth(method="lm", se=FALSE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NLS, T3, E.COLI") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  theme_classic() 
g1

# visualize by sym state
g2 <- ggplot(allfrag.dat, aes(Time.Elapsed.Min, value, col = sym.state)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  #scale_x_continuous(limits = c(0, 240)) +
  #scale_y_continuous(limits = c(60, 100)) +
  ggtitle("NLS, T3, E.COLI") +
  ylab("O2 (% Air Saturation)") +
  xlab("Time (min)") +
  scale_color_manual(values = c("coral4", "gray10")) +
  theme_classic() 

g2

library(patchwork)
(g1 + g2)

###########################################################
#
#                 Summarize Rate data
#
###########################################################

# linear model to average rate of all sym fragments 
apo.lm <- lm(value~Time.Elapsed.Min, data=apo.dat)
summary(apo.lm)

# linear model to average rate of all sym fragments 
sym.lm <- lm(value~Time.Elapsed.Min, data=sym.dat)
summary(sym.lm)

# Summarize mass.specific rate by Phenotype
library(Rmisc)
MO2_df$mass.specific = as.numeric(MO2_df$mass.specific)

resp.avg <- summarySE(MO2_df, measurevar = "mass.specific", groupvars = "Sym.state")

# add experimental identifiers
resp.avg$Day <- "22"
resp.avg$Bacteria <- "EC"
resp.avg$Treatment <- "NLS"

View(resp.avg)

# summaraize mass.specific rate for all 
sumrate <- summarySE(MO2_df, measurevar = "mass.specific")
# add experimental identifiers
sumrate$Day <- "22"
sumrate$Bacteria <- "EC"
sumrate$Treatment <- "NLS"
View(sumrate)

