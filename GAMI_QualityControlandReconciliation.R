# GAMI Quality Control Coder Reliability & Intercoder Reconciliation
# ARSiders (siders@udel.edu)
# Spring 2020

# ===== set working directory

# =============== IMPORT DATA 

# exported by user answer from SysRev & compiled by hand
data <- read.csv("GAMI_MasterData_5.18.20.csv")
teams.table <- read.csv("Teams.csv")
source("GAMI_QualityControlFunctions.R")

# ===== clean data
data <- cleandata(data)

# =============== QUALITY CONTROL 

#  1.Completeness
  # check that coders completed the coding and did not leave too many questions unanswered
  # calcluate % blank answers to important questions 

#  2.Follows Instructions
  # check whether coders over- or under-include 
  # compare exclusion and inclusion rates to team averages (of coders who coded more than 20 papers)

rates <- ratescomparison(data,teams.table,cutoff.bl=10,cutoff.inc=10) 
# first number is % blanks accepted; second is difference from average considered "unreliable"

# ===== Designate "Unreliable"
unrelcoders <- unreliablecoders(rates)

# ===== Export results
csvFileName <- paste("CoderReliability",Sys.Date(),".csv",sep="")
write.csv(rates, file=csvFileName)

# =============== INTERCODE RECONCILIATION 

source("GAMI_IntercodeReconciliationFunctions.R")

# =====>Identify cases that need human review
# 1 - articles coded by two unreliable coders and at least one says include and sufficient
#     need human to verify that article should be included
# 2 - articles coded at least one reliable coder but no answer given for Implementation 
#     need human to provide an Implementation answer 

# articles in need of reviewing
  # articles2review<-arts2rev(data,unrelcoders)
  # included in reoncile update function (but if no articles have been reviewed, used this)

# articles already reviewed 
reccode <- read.csv("Articles_NeedReview2020-04-14_Reconciled.csv")

# ===== Identify articles that still need human review 
articles4review<-reconcileupdate(data,unrelcoders,reccode)

# export results 
csvFileName2 <- paste("Articles4HumanReview",Sys.Date(),".csv",sep="")
write.csv(articles4review, file=csvFileName2)

# ===== Update data with human reviewed entries 
data2<-reconciledata(data,unrelcoders,reccode)

# ===== Reconcile intercoder responses
reconcile <-reconciliation(data2,unrelcoders,reccode)

# == export results
csvFileName3 <- paste("ReconciledCodes",Sys.Date(),".csv",sep="")
write.csv(reconcile, file=csvFileName3)

