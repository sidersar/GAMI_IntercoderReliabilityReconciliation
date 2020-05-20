# GAMI Quality Control Coder Reliability
# Functions to be Sourced
# ARSiders (siders@udel.edu)
# Spring 2020

library(dplyr)
library(pracma)

# ========= DATA 

# number of coders and unique coder names
coders <-as.vector(unique(data[,2], incomparables = FALSE))

# unique article labels
articles <-as.vector(unique(data[,1], incomparables = FALSE))


# ========= ASSIGN TEAMS

teams.vector<-function(teams.table){
  teamAfrica <- as.vector(teams.table[,1])
  teamAfrica <- teamAfrica[-which(teamAfrica=="")]
  
  teamAsia <-  as.vector(teams.table[,2])
  teamAsia <- teamAsia[-which(teamAsia=="")]
  
  teamCentral <- as.vector(teams.table[,3])
  teamCentral <- teamCentral[-which(teamCentral=="")]
  
  teamCities <- as.vector(teams.table[,11])
  teamCities <- teamCities[-which(teamCities=="")]
  
  teamEuro <- as.vector(teams.table[,5])
  teamEuro <- teamEuro[-which(teamEuro=="")]
  
  teamFood <- as.vector(teams.table[,10])
  teamFood <- teamFood[-which(teamFood=="")]
  
  teamHealth <- as.vector(teams.table[,13])
  teamHealth <- teamHealth[-which(teamHealth=="")]
  
  teamNorthAm <- as.vector(teams.table[,4])
  teamNorthAm <- teamNorthAm[-which(teamNorthAm=="")]
  
  teamOcean <- as.vector(teams.table[,8])
  teamOcean <- teamOcean[-which(teamOcean=="")]
  
  teamPov <- as.vector(teams.table[,12])
  teamPov <- teamPov[-which(teamPov=="")]
  
  teamTerr <- as.vector(teams.table[,7])
  teamTerr <- teamTerr[-which(teamTerr=="")]
  
  teamWater <- as.vector(teams.table[,9])
  teamWater <- teamWater[-which(teamWater=="")]
  
  teamSmall <- as.vector(teams.table[,6])
  teamSmall <- teamSmall[-which(teamSmall=="")]
  
  teams <- list(teamAfrica, teamAsia, teamCentral, teamCities, teamEuro, teamFood, teamHealth, teamNorthAm, teamOcean, teamPov, teamTerr, teamWater, teamSmall)

  namest <-c("Africa","Asia","Central","Cities","Euro","Food","Health","NorthAm","Ocean","Pov","Terr","Water","Small")
  names(teams)<-namest
  
  return(teams)
}

# ========= CLEAN DATA

cleandata <- function(data){
  data[is.na(data)]=""
  data[,8] <- as.character(data[,8])
  data[,9] <- as.character(data[,9])
  data[,10] <- as.character(data[,10])
  data[,11] <- as.character(data[,11]) 
  data[,14] <- as.character(data[,14])
  data[,17] <- as.character(data[,17])
  data[,20] <- as.character(data[,20])
  data[,23] <- as.character(data[,23])
  data[,27] <- as.character(data[,27])
  data[,30] <- as.character(data[,30])
  data[,35] <- as.character(data[,35])
  data[,35] <- trimws(data[,35])
  data[,36] <- as.numeric(data[,36])
  
  #correcting some imports by l.berrangford & alexandra.lesnikowski
  if ((data[,2]=="l.berrangford") || (data[,2]=="alexandra.lesnikowski")){
    if (data[,5]=="NS"){
      data[,2] <- "nick.simpson"
    }
    else if (data[,5]=="ARS"){
      data[,2] <- "siders"
    }
  }
  
  # cleaning data to have only one implementation stage
  for (i in 1:nrow(data)){
    # if a coder assigned multiple implementation stages
    allstages <- unlist(strsplit(data[i,35],split='|||', fixed=TRUE))
    allstages <- trimws(allstages)
    if (length(allstages)>1) {
      if ("Implementation widespread" %in% allstages){
        data[i,35] <- "Implementation widespread"
      }    
      else if ("Implementation expanding" %in% allstages){
        data[i,35] <- "Implementation expanding"
      }
      else if ("Adaptation planning & early implementation" %in% allstages){
        data[i,35] <- "Adaptation planning & early implementation"
      }
      else if ("Vulnerability assessment and/or early planning" %in% allstages){
        data[i,35] <- "Vulnerability assessment and/or early planning"
      }
      else if ("Recognition" %in% allstages){
        data[i,35] <- "Vulnerability assessment and/or early planning"
      }
      else data[i,35] <- "Evidence of risk reduction associated with adaptation efforts"
    }
    
    else { # one answer per coder
      if (strcmp(data[i,35],"Recognition")){
        data[i,35] <- "Vulnerability assessment and/or early planning"
      }
      else if (data[i,35] == ""){
        data[i,36] <- 0
      }
      else if (strcmp(data[i,35],"Vulnerability assessment and/or early planning")){
        data[i,36] <- 1
      }
      else if (strcmp(data[i,35],"Adaptation planning & early implementation")){
        data[i,36] <- 2  
      }  
      else if (strcmp(data[i,35], "Implementation expanding")){
        data[i,36] <- 3
      }
      else if (strcmp(data[i,35], "Implementation widespread")){
        data[i,36] <- 4
      }
      else if (strcmp(data[i,35],"Evidence of risk reduction associated with adaptation efforts")){
        data[i,36] <- 5
      }
    }
  }
  
  # second for loop -- now that we've cleaned up the text, assign numeric score
  for (i in 1:nrow(data)){      
    if (strcmp(data[i,35], "Vulnerability assessment and/or early planning")){
      data[i,36] <- 1
    }
    else if (strcmp(data[i,35],"Adaptation planning & early implementation")){
      data[i,36] <- 2  
    }  
    else if (strcmp(data[i,35], "Implementation expanding")){
      data[i,36] <- 3
    }
    else if (strcmp(data[i,35],"Implementation widespread")){
      data[i,36] <- 4
    }
    else if (strcmp(data[i,35], "Evidence of risk reduction associated with adaptation efforts")){
      data[i,36] <- 5
    }
  }
  data[,36] <- as.numeric(data[,36])
  return(data)  
}




# ========= CODER RELIABILITY CHECK  

# ==> COMPLETENESS 

# check that coders completed the coding and did not leave too many questions unanswered

# calcluate % blank answers to important questions 

blanks <- function(data,cutoff){
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  table <- as.data.frame(matrix(nrow=length(coders), ncol=7))
  colnames(table) <- c("User","Articles","ImpBlanks","%Total","%Imp","TotalSig","ImpSig")
  # cycle through all coders  
  for (j in 1:length(coders)){
    # how many articles coded
    num.coded <-0
    count <- 0    # set blank count to 0
    fullcode <- 0
    # cycle through all articles
    for (i in 1:(nrow(data))){
      lncount <- 0
      if (data[i,2]==coders[j]){
        num.coded<-num.coded+1
      }
      # user tagged article include and sufficient = true
      if ((data[i,2]==coders[j]) & (data[i,4]==TRUE) & data[i,7]==TRUE){
        # count blanks in important questions
        # geography
        if (trimws(data[i,8])=="") {
          lncount <- lncount + 1   }
        # sector 
        if (trimws(data[i,10])==""){
          lncount <- lncount + 1   }   
        # indigenous knowledge 
        if (is.na(trimws(data[i,12])) | (trimws(data[i,12])=="") | (is.null(trimws(data[i,12])))){
          lncount <- lncount + 1   }   
        # local knowledge 
        if (is.na(trimws(data[i,13])) | (trimws(data[i,13])=="") | (is.null(trimws(data[i,13])))){
          lncount <- lncount + 1   }   
        # actors / institutions 
        if (trimws(data[i,14])==""){
          lncount <- lncount + 1   }   
        # type of response 
        if (is.na(data[i,23]) | (data[i,23]=="") | (is.null(data[i,23]))){
          lncount <- lncount + 1   }   
        # implementation tools
        if (is.na(data[i,25]) | (data[i,25]=="") | (is.null(data[i,25]))){
          lncount <- lncount + 1   }   
        # hazards 
        if (is.na(data[i,27]) | (data[i,27]=="") | (is.null(data[i,27]))){
          lncount <- lncount + 1   }   
        # exposure 
        if (is.na(data[i,30]) | (data[i,30]=="") | (is.null(data[i,30]))){
          lncount <- lncount + 1   }   
        # implementation 
        if (is.na(data[i,35]) | (data[i,35]=="") | (is.null(data[i,35]))){
          lncount <- lncount + 1   }   
        # depth 
        if (is.na(data[i,40]) | (data[i,39]=="") | (is.null(data[i,40]))){
          lncount <- lncount + 1   }   
        # scope 
        if (is.na(data[i,42]) | (data[i,41]=="") | (is.null(data[i,42]))){
          lncount <- lncount + 1   }   
        # speed 
        if (is.na(data[i,44]) | (data[i,43]=="") | (is.null(data[i,44]))){
          lncount <- lncount + 1   }   
        # reduce risk 
        if (is.na(data[i,46]) | (data[i,45]=="") | (is.null(data[i,46]))){
          lncount <- lncount + 1   }   
        # indicators of success
        if (is.na(data[i,48]) | (data[i,47]=="") | (is.null(data[i,48]))){
          lncount <- lncount + 1   }   
        # limits
        if (is.na(data[i,54]) | (data[i,53]=="") | (is.null(data[i,54]))){
          lncount <- lncount + 1   }          
        count <- count + lncount 
        fullcode <- fullcode + 1
      }
      table[j,2] <- num.coded
      table[j,3] <- count
      ful <- as.numeric(count/(fullcode*64))*100
      im <- as.numeric(count/(fullcode*18))*100
      table[j,4] <- as.numeric(format(ful,digits=2))
      table[j,5] <- as.numeric(format(im,digits=2))
    }
  }
  return(table)  
}

# label coders where % blanks > % (cutoff)
formtblanks <- function(table, cutoff){
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  table[,1] <- coders
  for (j in 1:length(coders)){
    if (table[j,4]>cutoff){
      table[j,6] <- 's'
    }
    else table[j,6] <- ''
    if (table[j,5]>cutoff){
      table[j,7] <- 's'
    }
    else table[j,7] <- ''
  }
  return(table)
}



# ==> FOLLOWS INSTRUCTIONS

# Check whether coders over- or under-include 
# compare exclusion and inclusion rates to team averages (of coders who coded more than 20 papers)

ratescomparison <- function(data,completeness,teams,cutoff){
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  # add columns to table 
  table <- as.data.frame(matrix(nrow=length(coders), ncol=11))
  colnames(table) <- c("User","Articles","#ImpBlanks","%ImpBlanks","BlanksSig","Team","TeamExcRate","IndivExclRate","TeamInclRate","IndivInclRate","Deviation")
  table[,1] <- completeness[,1]
  table[,2] <- completeness[,2]
  table[,3] <- completeness[,3]
  table[,4] <- completeness[,5]
  table[,5] <- completeness[,7]
  
  #read in team assignments
  teamAfrica<-teams[[1]]
  teamAsia<-teams[[2]]
  teamCentral<-teams[[3]]
  teamCities<-teams[[4]]
  teamEuro<-teams[[5]]
  teamFood<-teams[[6]]
  teamHealth<-teams[[7]]
  teamNorthAm<-teams[[8]]
  teamOcean<-teams[[9]]
  teamPov<-teams[[10]]
  teamTerr<-teams[[11]]
  teamWater<-teams[[12]]
  teamSmall<-teams[[13]]

  team.names <- c("Africa","AsiaAustra","CentralAm","Cities","Europe","Food","Health","NorthAm","Oceans","Poverty","Terrestrial","Water","SmallIsl")
  # label coders according to their team 
  table[table$User %in% teamAfrica, 6] <- team.names[1]
  table[table$User %in% teamAsia,6] <- team.names[2]
  table[table$User %in% teamCentral,6] <- team.names[3]
  table[table$User %in% teamCities,6] <- team.names[4]
  table[table$User %in% teamEuro,6] <- team.names[5]
  table[table$User %in% teamFood,6] <- team.names[6]
  table[table$User %in% teamHealth,6] <- team.names[7]
  table[table$User %in% teamNorthAm,6] <- team.names[8]
  table[table$User %in% teamOcean,6] <- team.names[9]
  table[table$User %in% teamPov,6] <- team.names[10]
  table[table$User %in% teamTerr,6] <- team.names[11]
  table[table$User %in% teamWater,6] <- team.names[12]
  table[table$User %in% teamSmall,6] <- team.names[13] 
  
  #calculate individual inclusion and exclusion rates 
  # loop through each coder
  for (i in 1:length(coders)){
    #subset data
    indiv <- data[data[,2]==coders[i],]
    numcoded <- nrow(indiv)
    excluded <- nrow(indiv[indiv$Include==FALSE,])
    included <- nrow(indiv[indiv$Include==TRUE,])
    incrate <- signif((included/numcoded)*100,digits=2)
    exclrate <- signif((excluded/numcoded)*100,digits=2)
    table[table[,1]==coders[i],10] <- incrate
    table[table[,1]==coders[i],8] <- exclrate
  }
  
  #calculate team inclusion and exclusion rates
  table <- teamstats(table,coders)
  
  # compare team and indiv incl & excl 
  for (j in 1:length(coders)){
    #subset data for each person
    p <- table[table$User==coders[j],]  
    # compare team excl rate + excl rate if exclude too many
    Edif <- (p$IndivExclRate-p$TeamExcRate)
    # compare team inclusion rate + inc rate if include too many
    Idif <- (p$IndivInclRate-p$TeamInclRate) 
    # label deviations
    if ((is.na(p$IndivExclRate)) || (is.na(p$IndivInclRate))){
      table[table$User==coders[j],11] <- "na"
    }
    else if ((is.na(Edif)) || (is.na(Idif))) {
      table[table$User==coders[j],11] <- "na"
    }
    else if (Edif > cutoff) {
      table[table$User==coders[j],11] <- "OverExcl"
    }
    else if (Idif > cutoff) {
      table[table$User==coders[j],11] <- "OverIncl"
    }
    else table[table$User==coders[j],11] <- ""
  }
  return(table)  
}



teamstats <- function(table,coders,teams){
  team.names <- c("Africa","AsiaAustra","CentralAm","Cities","Europe","Food","NorthAm","Water","Poverty","Health","Europe","Terrestrial","Oceans")
  prolific <- coders[table$Articles > 20]
  
  # calculate average inclusion and exclusion rates for each team 
  # use only prolific coders to calculate average (if only code a few papers might not be representative sample)
  
  for (h in 1:length(team.names)){
    # subset data for team 
    tab.team<-table[table$Team %in% team.names[h],]
    prol.coders<-tab.team[tab.team$User %in% prolific,]
    
    # average team inclusion rate
#    tmincl<-signif(mean(prol.coders[,10]), digits=2)
    tmincl<-median(prol.coders[,10])
    table[table$Team %in% team.names[h],9]<-tmincl
    # average team exclusion rate
#    tmexcl<-signif(mean(prol.coders[,8]), digits=2)
    tmexcl<-median(prol.coders[,8])
    table[table$Team %in% team.names[h],7]<-tmexcl
  }
  return(table)
}
    

# ==> VECTOR OF UNRELIABLE CODERS 

unreliablecoders <- function(table){
  overex <- table[table$Deviation=="OverExcl",]
  overex.coders <- as.vector(unique(overex$User, incomparables = FALSE))

  overin <- table[table$Deviation=="OverIncl",]
  overin.coders <- as.vector(unique(overin$User, incomparables = FALSE))
  
  empty <- table[table$BlanksSig == "s",]
  empty.coders <- as.vector(unique(empty$User, incomparables = FALSE))
  
  longlist <- c(overex.coders, overin.coders, empty.coders)
  unrelcoders <- unique(longlist, incomparables = FALSE)

  return(unrelcoders)
}


