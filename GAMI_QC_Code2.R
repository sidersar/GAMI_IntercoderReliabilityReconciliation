# GAMI Quality Check
# ARSiders
# Spring 2020

#setwd("C:/Users/AR Siders/OneDrive - UD College of Arts and Sciences/Projects/1 - Adapt Collab Coding - Ford/QC Reconciliation")
#library(plyr)

# ====== IMPORT DATA 
# exported by user answer from SysRev & compiled by hand

#data <- read.csv("QCMasterList_3.20.20.csv")
#names(data)
#dim(data)

#source("GAMI_Functions.R")

# ====== QUALITY CONTROL 

# === Descriptive stats

data <- cleandata(data)

# number of coders and unique coder names
coders <-as.vector(unique(data[,2], incomparables = FALSE))
length(coders)

# unique article labels
articles <-as.vector(unique(data[,1], incomparables = FALSE))
length(articles)


# === Completeness

# for each coder, calculate % blank answers to 18 important questions
blanks <- function(data){
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  table <- as.data.frame(matrix(nrow=length(coders), ncol=7))
  colnames(table) <- c("User","Articles","ImpBlanks","%Total","%Imp","TotalSig","ImpSig")
  # cycle through all coders  
  for (j in 1:length(coders)){
    count <- 0    # set blank count to 0
    fullcode <- 0
      # cycle through all articles
      for (i in 1:(nrow(data))){
           lncount <- 0
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
             if (is.na(data[i,40]) | (data[i,40]=="") | (is.null(data[i,40]))){
             lncount <- lncount + 1   }   
              # scope 
             if (is.na(data[i,42]) | (data[i,42]=="") | (is.null(data[i,42]))){
             lncount <- lncount + 1   }   
              # speed 
             if (is.na(data[i,44]) | (data[i,44]=="") | (is.null(data[i,44]))){
             lncount <- lncount + 1   }   
              # reduce risk 
             if (is.na(data[i,46]) | (data[i,46]=="") | (is.null(data[i,46]))){
             lncount <- lncount + 1   }   
              # indicators of success
             if (is.na(data[i,48]) | (data[i,48]=="") | (is.null(data[i,48]))){
             lncount <- lncount + 1   }   
              # limits
             if (is.na(data[i,54]) | (data[i,54]=="") | (is.null(data[i,54]))){
             lncount <- lncount + 1   }          
           count <- count + lncount 
           fullcode <- fullcode + 1
           }
      table[j,2] <- fullcode
      table[j,3] <- count
      ful <- as.numeric(count/(fullcode*64))*100
      im <- as.numeric(count/(fullcode*18))*100
      table[j,4] <- as.numeric(format(ful,digits=2))
      table[j,5] <- as.numeric(format(im,digits=2))
    }
  }
  return(table)  
}
blanktab <- blanks(data)


# label coders where % blanks > 10%
fort <- function(table){
    coders <-as.vector(unique(data[,2], incomparables = FALSE))
    table[,1] <- coders
    for (j in 1:length(coders)){
      if (table[j,4]>10){
        table[j,6] <- 's'
      }
      else table[j,6] <- ''
      if (table[j,5]>10){
        table[j,7] <- 's'
      }
      else table[j,7] <- ''
    }
  return(table)
}
fort(blanktab)
completeness <- fort(blanktab)


# === INCLUSION RELIABILITY

# for each user, compare exclusion and inclusion rates to team averages (of those who code >20 papers)
ratescomparison <- function(data,completeness){
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  # add columns to table 
  table <- as.data.frame(matrix(nrow=length(coders), ncol=12))
  colnames(table) <- c("User","Articles","ImpBlanks","%Imp","ImpSig","Team","TeamExc","TeamIncl","InclRate","ExclRate","Dev","TmSD")
  table[,1] <- completeness[,1]
  table[,2] <- completeness[,2]
  table[,3] <- completeness[,3]
  table[,4] <- completeness[,5]
  table[,5] <- completeness[,7]

# create teams -- annoyingly by Hand
  teamAfrica <- c("adadeposh","alcadese","philiantwi","edmond.totin","megan.lukas","nick.simpson","northm","ofoegbu.c","philantwi","so2326","starowolo")
    table[table$User %in% teamAfrica, 6] <- "Africa"
  teamAsia <- c("92pneha","a.mukherji","eranga.galappaththi","krishadave8","lamhuynh6493","mahantiashwina","mohammadaminurrahman.shah","pkb911","shukla","tara.tchen")
    table[table$User %in% teamAsia,6] <- "AsiaAustra"
  teamCentral <- c("carolzavaleta2009","christopher.alarconr20","eeiear","julia.pelaez","mariellasina")
    table[table$User %in% teamCentral,6] <- "CentralAm"
  teamCities <- c("deepal.doshi","f.i.hoefsloot","malcolm.araos")
    table[table$User %in% teamCities,6] <- "CentralCities"
  teamEuro <- c("jan.petzold","T.L.Leiter","vanesachala")
    table[table$User %in% teamEuro,6] <- "Europe"
  teamFood <- c("aidan.farrell","aph82","brian.pentz","cgrady","christa.anderson","custodio.matavel","ddie","delphine.deryng","eab365","id_bhatt","jmu393","maskell","pokharelpratik1921","rbeznerkerr","shinnythakur","sjdvais","st3267","zommers","sarah.dhaen")
    table[table$User %in% teamFood,6] <- "Food"
  teamHealth <- c("alyssagattipcc","amnunbog","eegs","harrison","kai.zhu")
    table[table$User %in% teamHealth,6] <- "Health"
  teamNorthAm <- c("cenquist","dorothy.heinrich","elisabeth.gilmore")
    table[table$User %in% teamCentral,6] <- "NorthAm"
  teamOcean <- c("raquel.ruiz103","jajibade")
    table[table$User %in% teamOcean,6] <- "Oceans"
  teamPov <- c("cmcomber","gwongpar","kebr","kripajagan","skoller","ss18arm")
    table[table$User %in% teamPov,6] <- "Poverty"
  teamTerr <- c("siders","stephanie.barr")
    table[table$User %in% teamTerr,6] <- "Terrestrial"
  teamWater <- c("christine.kirchhoff","cristina.mullin","Nicole.vanmaanen","tzabu")
    table[table$User %in% teamWater,6] <- "Water"
  teams <- list(teamAfrica, teamAsia, teamCentral, teamCities, teamEuro, teamFood, teamHealth, teamNorthAm, teamOcean, teamPov, teamTerr, teamWater)  
  #calculate individual inclusion and exclusion rates 
  # run through each coder
  for (i in 1:length(coders)){
    #subset data
    indiv <- data[data$zommers==coders[i],]
    numcoded <- nrow(indiv)
    excluded <- nrow(indiv[indiv$Include==FALSE,])
    included <- nrow(indiv[indiv$Include==TRUE,])
    incrate <- (included/numcoded)*100
    exclrate <- (excluded/numcoded)*100
    table[table[,1]==coders[i],9] <- signif(incrate, digits=2)
    table[table[,1]==coders[i],10] <- signif(exclrate, digits=2)
  }
  #calculate team inclusion and exclusion rates
  prolific <- coders[table$Articles > 20]
  #subset data for team
  for (k in 1:12){
    tm <- data[data$zommers %in% teams[[k]], ]
    # coders who code more than 20 articles
    tm <- tm[tm$zommers %in% prolific,]
    tm2 <- as.vector(unique(tm[,2], incomparables = FALSE))
    # team incl    
    tmincl <- mean(table[table$User %in% tm2,9])
    tmsd <- sd(table[table$User %in% tm2,9])
    tmincl <- signif(tmincl, digits=2)
    table[table$User %in% teams[[k]],8] <- tmincl
    table[table$User %in% teams[[k]],12] <- signif(tmsd, digits=2)
    # team excl
    tmexcl <-  mean(table[table$User %in% tm2,10])
    tmexcl <- signif(tmexcl, digits=2)
    table[table$User %in% teams[[k]],7] <- tmexcl
  }
  # compare team and indiv incl & excl 
  # run through each coder
    for (i in 1:length(coders)){
      #subset data for each person
      p <- table[table$User==coders[i],]  
      # compare team excl rate + excl rate if exclude too many
      Edif <- (p$ExclRate-p$TeamExc)
      # compare team inclusion rate + inc rate if include too many
      Idif <- (p$InclRate-p$TeamIncl) 
      # label deviations
      if ((is.na(p$ExclRate)) || (is.na(p$InclRate)) || (is.na(p$TmSD))){
        table[table$User==coders[i],11] <- "na"
      }
      else if ((is.na(Edif)) || (is.na(Idif))) {
        table[table$User==coders[i],11] <- "na"
      }
      else if (Edif > p$TmSD) {
        table[table$User==coders[i],11] <- "OverExcl"
      }
      else if (Idif > p$TmSD) {
        table[table$User==coders[i],11] <- "OverIncl"
      }
      else table[table$User==coders[i],11] <- ""
    }
  return(table)  
}
rates <- ratescomparison(data,completeness)

unreliablecoders <- function(rates){
  u1 <- rates[rates$Dev=="OverExcl",]
  u2 <- rates[rates$Dev=="OverIncl",]
  u3 <- rates[rates$ImpSig == "s",]
  uc1 <- as.vector(unique(u1[,1], incomparables = FALSE))
  uc2 <- as.vector(unique(u2[,1], incomparables = FALSE))
  uc3 <- as.vector(unique(u3[,1], incomparables = FALSE))
  unrelcoders <- c(uc1,uc2,uc3)
  return(unrelcoders)
}
unrelcoders <- unreliablecoders(rates)

csvFileName <- paste("CoderReliability",Sys.Date(),".csv",sep="")
write.csv(rates, file=csvFileName)
