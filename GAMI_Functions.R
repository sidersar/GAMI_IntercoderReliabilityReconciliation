# GAMI Data Cleaning
# ARSiders
# Spring 2020

library(dplyr)
library(pracma)

# ======= IMPORT DATA 
# exported by user answer from SysRev & compiled by hand

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
  
  if ((data[,2]=="l.berrangford") || (data[,2]=="alexandra.lesnikowski")){
    if (data[,5]=="NS"){
      data[,2] <- "nick.simpson"
    }
    else if (data[,5]=="ARS"){
      data[,2] <- "siders"
    }
  }
  
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
#      else 
#      {
#        imp <- unlist(strsplit(data[i,35],split='|||', fixed=TRUE))
#        imp <- imp[!imp %in% c("Evidence of risk reduction associated with adaptation efforts", "Recognition")]
#        data[i,35] <- paste(imp, collapse = "|||")
#      }
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

cdata <- cleandata(data)
#head(cdata)

#data[1:40,35]
#cdata[1:40,c(35,36)]


# articles with a) more than one coder; b) at least one reliable coder; 
# c) at least one reliable coder says include and says d) sufficient
easya <- function(data,unrelcoders) {
  # find article IDs for articles with more than one coder
  ct <- as.data.frame(table(data$Article.ID))
  mults <- as.vector(ct[ct$Freq>1,1])
  print(c("Number multi-coded articles",length(mults)))
  # find article IDs for aritlces with at least one reliable coder
  coders <- as.vector(unique(data[,2], incomparables = FALSE))
  relcoders <- setdiff(coders,unrelcoders)
  # cycle through aticles with more than one coder
  relarts <- vector()
  for (i  in 1:(length(mults))){
    # subset each article
    d <- data[data$Article.ID==mults[i],]
    # check for a reliable coder
    if (any(d[,2] %in% relcoders)){
      # check if a rel coder says to include 
      c <- as.vector(d[,2])
      for (k in 1:length(c)){
        if ((c[k] %in% relcoders) & (d[k,4] == TRUE)){
          if (d[k,7]==TRUE){
            relarts <- c(relarts,d[k,1])
          } 
        }
      }
    }
  }
  relarts <- as.vector(unique(relarts, incomparables = FALSE))
  #print(length(relarts))
  return(relarts)
}
t <- easya(data,unrelcoders)
# returns a vector of article IDs




# articles with single coder response; reliable coder who says include and sufficient
sing <- function(data,unrelcoders){
  coders <- as.vector(unique(data[,2], incomparables = FALSE))
  relcoders <- setdiff(coders,unrelcoders)
  # articles by single coder
  ct <- as.data.frame(table(data$Article.ID))
  sing <- as.vector(ct[ct$Freq==1,1])
  # cycle through articles in just one coder
  relarts <- vector()
  for (i  in 1:(length(sing))){
    # subset each article
    d <- data[data$Article.ID==sing[i],]   # fix ins[i] to mults[i] in GAMI functions (and remove Ins)
    # check if coder is reliable coder
    if (d[i,2] %in% relcoders){
      # check if says to include 
      if (d[i,4] == TRUE){
        # and if says sufficient
        if (d[i,7] == TRUE){
          relarts <- c(relarts,d[i,1])
        }
      }
    }
  }
  relarts <- as.vector(unique(relarts, incomparables = FALSE))
  #print(length(relarts))
  return(relarts)
}
#sings <- sing(data,unrelcoders)
# returns a vector of article IDs


# articles coded by two unreliable coders; at least one says include and sufficient 
dualunrel <- function(data,unrelcoders) {
  # find article IDs for articles with more than one coder
  ct <- as.data.frame(table(data$Article.ID))
  mults <- as.vector(ct[ct$Freq>1,1])
  # find article IDs for aritlces with both unreliable coders
  coders <- as.vector(unique(data[,2], incomparables = FALSE))
  # cycle through aticles with more than one coder
  unrelarts <- vector()
  for (i  in 1:(length(mults))){
    # subset each article
    d <- data[data$Article.ID==mults[i],]
    # if all coders are unreliable
    if (all(d$zommers %in% unrelcoders)) {
       # if at least one says include and sufficient 
      c <- as.vector(d[,2])
      for (k in 1:length(c)){
        if ((d[k,4]==TRUE) & (d[k,7] ==TRUE)){
          unrelarts <- c(unrelarts,d[k,1])
        }
      }
    }
  }
  unrelartd <- data[data$Article.ID %in% unrelarts,] 
  return(unrelartd)
}

Process_Imp <- function(d, shelldf, j) {
  if (dim(table(d[,35])) == 1) {
    shelldf[j,36] <- d[1,36]
    shelldf[j,35] <- d[1,35]
    # if they agree because both entries are blank, set to review
    if (d[1,35]==""){
      shelldf[j,35] <- "BLANK: REVIEW NEEDED"
    }
  }
  else if (dim(table(d[,35])) > 1) {
    # if there are multiple entries
    # if one is Evidence, remove that row
    if (isTRUE(any(d[,35] == "Evidence of risk reduction associated with adaptation efforts"))){
      d <- d[(d[,35]!="Evidence of risk reduction associated with adaptation efforts"),]
      
      # if that gets us down to only one row, take that row's answer
      if (dim(table(d[,35])) == 1){
        shelldf[j,35] <- d[,35]
      }
      # if still have multiple entries, do the count to reconcile
      else {
        # how far apart entries are
        g <- (max(d[,36]) - min(d[,36]))
        # if conflict codes one apart, take the higher
        if (g==1) {
          ma <- max(d[,36])
          hig <- d[(d[,36]==ma),]
          shelldf[j,35] <- hig[,35]
        }
        else if (g > 1) {   # if conflict farther apart, take average (round up)
          av <- mean(d[,36])
          av <- ceiling(av) # round up
          if (av == 1){
            shelldf[j,36] <- 1
            shelldf[j,35] <- "Vulnerability assessment and/or early planning"
          }
          else if (av==2){
            shelldf[j,36] <- 2
            shelldf[j,35] <- "Adaptation planning & early implementation"
          }
          else if (av==3){
            shelldf[j,36] <- 3
            shelldf[j,35] <- "Implementation expanding"
          }
          else if (av==4){
            shelldf[j,36] <- 4
            shelldf[j,35] <- "Implementation widespread"
          }
          else if (av==5){
            shelldf[j,36] <- 5
            shelldf[j,35] <- " Evidence of risk reduction associated with adaptation efforts"
          }
          else {
            print("REVIEW NEEDED",i)
          }
        }
        if (shelldf[j,35]==""){
          shelldf[j,35] <- "BLANK: REVIEW NEEDED"
        }
      }
    }
    # else if none of the multiple entries are Evidence:
    else {
      # how far apart entries are
      g <- (max(d[,36]) - min(d[,36]))
      # if conflict codes one apart, take the higher
      if (g==1){
        ma <- max(d[,36])
        hig <- d[(d[,36]==ma),]
        shelldf[j,35] <- hig[1,35]
        if (shelldf[j,35]==""){
          shelldf[j,35] <- "BLANK: REVIEW NEEDED"
        }
      }
      else if (g > 1) {   # if conflict farther apart, take average (round up)
        av <- mean(d[,36])
        av <- ceiling(av) # round up
        if (av == 1){
          shelldf[j,36] <- 1
          shelldf[j,35] <- "Vulnerability assessment and/or early planning"
        }
        else if (av==2){
          shelldf[j,36] <- 2
          shelldf[j,35] <- "Adaptation planning & early implementation"
        }
        else if (av==3){
          shelldf[j,36] <-3
          shelldf[j,35] <- "Implementation expanding"
        }
        else if (av==4){
          shelldf[j,36] <- 4
          shelldf[j,35] <- "Implementation widespread"
        }
        else if (av==5){
          shelldf[j,36] <-5
          shelldf[j,35] <- " Evidence of risk reduction associated with adaptation efforts"
        }
        else {
          print("REVIEW NEEDED",i)
        }
        
        # if (shelldf[j,35]==""){
        #   shelldf[j,35] <- "BLANK: REVIEW NEEDED"
        # }
      }
    }
  }
  else {
    #data[j,35] <- paste(d[,35], "REVIEW NEEDED", collapse = "|||")
  }
  
  if (shelldf[j,35]=="") {
    shelldf[j,35] <- "BLANK: REVIEW NEEDED"
  }
  return(shelldf)
}

unrel.articles <- dualunrel(data,unrelcoders)   
# returns a database of articles that need to be reviewed

csvFileName2 <- paste("Articles_ReviewInclusion",Sys.Date(),".csv",sep="")
write.csv(unrel.articles, file=csvFileName2)
