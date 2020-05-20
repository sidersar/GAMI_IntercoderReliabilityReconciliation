# GAMI Quality Control Coder Reliability
# ARSiders (siders@udel.edu)
# Spring 2020


# ===  set working directory

# ========= IMPORT DATA 
# exported by user answer from SysRev & compiled by hand
data <- read.csv("GAMI_MasterData_5.18.20.csv")
source("GAMI_QualityControlFunctions.R")

# === clean data
data <- cleandata(data)

# === define teams
teams.table <- read.csv("Teams.csv")
teams<-teams.vector(teams.table)

# ========= QUALITY CONTROL 

# === Completeness

# check that coders completed the coding and did not leave too many questions unanswered
# calcluate % blank answers to important questions 
blanks.table<-blanks(data,10) # number sets cutoff point: % blanks considered "unreliable"
completeness2<-formtblanks(blanks.table,10) # number is cutoff point for "unreliable"

# === Follows Instructions

# check whether coders over- or under-include 
# compare exclusion and inclusion rates to team averages (of coders who coded more than 20 papers)
rates <- ratescomparison(data,completeness2,teams,10) # number is % difference from average considered "unreliable"

# === Designate "Unreliable"
unrelcoders <- unreliablecoders(rates)
unrelcoders
length(unrelcoders)

# === Write results
csvFileName <- paste("CoderReliability",Sys.Date(),".csv",sep="")
write.csv(rates, file=csvFileName)
