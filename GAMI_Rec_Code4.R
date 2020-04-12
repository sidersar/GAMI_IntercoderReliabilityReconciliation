# GAMI Reconciliation
# ARSiders
# Spring 2020
# this is a test
setwd("C:/Users/AR Siders/OneDrive - UD College of Arts and Sciences/Projects/1 - Adapt Collab Coding - Ford/QC Reconciliation")

# ======= IMPORT DATA 
# exported by user answer from SysRev & compiled by hand

data <- read.csv("GAMI_MasterData_4.11.20.csv")
names(data)
dim(data)

source("GAMI_QC_Code2.R")
source("GAMI_Functions.R")

# ======= RECONCILIATION

reconcil <- function(data,unrelcoders){
  # create empty data frame to hold results
  shelldf <- as.data.frame(matrix(ncol=66))
  colnames(shelldf) <- colnames(data)
  # set the row where write results
  j <- 1
  # clean data
  data <- cleandata(data)
  data[,64] <- as.character(data[,64])
  data[,65] <- as.character(data[,65])
  data[,66] <- as.character(data[,66])
  data[,8] <- as.character(data[,8])
  data[,9] <- as.character(data[,9])
  data[,10] <- as.character(data[,10])
  data[,11] <- as.character(data[,11])
  data[,14] <- as.character(data[,14])
  data[,15] <- as.character(data[,15])
  data[,16] <- as.character(data[,16])
  data[,17] <- as.character(data[,17])
  data[,18] <- as.character(data[,18])
  data[,19] <- as.character(data[,19])
  data[,20] <- as.character(data[,20])
  
  data[is.na(data)]=""
  coders <-as.vector(unique(data[,2], incomparables = FALSE))
  print(c("Number of coders",length(coders)))
  print(c("Number of unreliable coders",length(unrelcoders)))
  relcoders <- setdiff(coders,unrelcoders)
  
  # CASE 1: multiple coders; at least one reliable coder says include and sufficient 
    easy.case <- easya(data,unrelcoders)
    print(c("Number of multi-coded papers with >=1 reliable include + sufficient",length(easy.case)))
    
    #  edata <- data[data[,1] %in% easy.case,] 
  
    # cycle through the "easy" cases
    for (i in 1:length(easy.case)) {
      # SUBSET CODES FOR A SINGLE ARTICLE
      d <- data[data$Article.ID==easy.case[i],]

      # set article citation info
      shelldf[j,1] <- d[1,1] # article ID
      shelldf[j,64] <- d[1,64] # title
      shelldf[j,65] <- d[1,65] # journal
      shelldf[j,66] <- d[1,66] # authors
      # combine coder names 
      shelldf[j,2] <- paste(d[,2], collapse = "|||")
      # combine summaries
      shelldf[j,6] <- paste(d[,6], collapse = "|||")  
      # set include and suficient to TRUE 
      shelldf[j,4] <- TRUE
      shelldf[j,7] <- TRUE
      
      # geography -- if agree, take that answer, else take both
      if (dim(table(d[,8])) == 1){
        shelldf[j,8] <- d[1,8]
      }
      else {
        d[,8] <- as.character(d[,8])
        s8 <- unlist(strsplit(d[,8],split='|||', fixed=TRUE))
        s8 <- unique(s8, incomparables = FALSE)
        shelldf[j,8] <- paste(s8, collapse="|||") 
      }
      # geography country -- if agree, take that answer, else take both
      if (dim(table(d[,9])) == 1){
        shelldf[j,9] <- d[1,9]
      }
      else {
        d[,9] <- as.character(d[,9])
        s9 <-unlist(strsplit(d[,9],split='|||', fixed=TRUE))
        s9 <- unique(s9, incomparables = FALSE)
        shelldf[j,9] <- paste(s9, collapse="|||")
      } 
      # sector -- if agree, take that answer else take both
      if (dim(table(d[,10])) == 1){
        shelldf[j,10] <- d[1,10]
      }
      else {
        d[,10] <- as.character(d[,10])
        s8 <-unlist(strsplit(d[,10],split='|||', fixed=TRUE))
        s8 <- unique(s8, incomparables = FALSE)
        shelldf[j,10] <- paste(s8, collapse="|||")
      } 
      # cross cutting topics -- if agree, take that answer, else take both
      if (dim(table(d[,11])) == 1){
        shelldf[j,11] <- d[1,11]
      }
      else {
        d[,11] <- as.character(d[,11])
        s11 <-unlist(strsplit(d[,11],split='|||', fixed=TRUE))
        s11 <- unique(s11, incomparables = FALSE)
        shelldf[j,11] <- paste(s11, collapse="|||")
      } 
      # indigenous knowledge -- if either one is TRUE, take TRUE 
      d[,12] <- as.logical(d[,12])
      if (isTRUE(any(d[,12]))){
        shelldf[j,12] <-"TRUE"
      }    
      else shelldf[j,12]<-"FALSE"
      # local knowledge -- if either one is TRUE, take TRUE 
      d[,13] <- as.logical(d[,12])
      if (isTRUE(any(d[,13]))){
        shelldf[j,13]<-"TRUE"
      }
      else shelldf[j,13]<-"FALSE"        
      # actors / institutions-- if agree, take that answer, else all
      if (dim(table(d[,14])) == 1){
        shelldf[j,14] <- d[1,14]
      }
      else  {
        d[,14] <- as.character(d[,14])
        s8 <-unlist(strsplit(d[,14],split='|||', fixed=TRUE))
        s8 <- unique(s8, incomparables = FALSE)
        shelldf[j,14] <- paste(s8, collapse="|||")
      }                
      # actors, other -- take all 
      shelldf[j,15] <- paste(d[,15], collapse="|||")                 
      # actors, quotes -- take all 
      shelldf[j,16] <- paste(d[,16], collapse="|||")    
      # equity planning -- if same, take that one, else all 
      if (dim(table(d[,17])) == 1){
        shelldf[j,17] <- d[1,17]
      }
      else  {
        d[,17] <- as.character(d[,17])
        s17 <-unlist(strsplit(d[,17],split='|||', fixed=TRUE))
        s17 <- unique(s17, incomparables = FALSE)
        shelldf[j,17] <- paste(s17, collapse="|||")
      }
      # equity planning other -- take all 
      shelldf[j,18] <- paste(d[,18], collapse="|||")                  
      # equity quotes - take all 
      shelldf[j,19] <- paste(d[,19], collapse="|||")                   
      # equity targeting -- if same, take that one, else all
      if (dim(table(d[,20])) == 1){
        shelldf[j,20] <- d[1,20]
      }
      else  {         
        d[,20] <- as.character(d[,20])
        s20 <-unlist(strsplit(d[,20],split='|||', fixed=TRUE))
        s20 <- unique(s20, incomparables = FALSE)
        shelldf[j,20] <- paste(s20, collapse="|||")
      }
      # equity targeting other -- take all
      shelldf[j,21] <- paste(d[,21], collapse="|||")                   
      # equity targeting quotes - take all 
      shelldf[j,22] <- paste(d[,22], collapse="|||")                   
      # type of response -- if agree, take that answer, else take all
      if (dim(table(d[,23])) == 1){
        shelldf[j,23] <- d[1,23]
      }
      else {
        d[,23] <- as.character(d[,23])
        s23 <- unlist(strsplit(d[,23],split='|||', fixed=TRUE))
        s23 <- unique(s23, incomparables = FALSE)
        shelldf[j,23] <- paste(s23, collapse="|||")
      }
      # response quotes - take all 
      shelldf[j,24] <- paste(d[,24], collapse="|||")             
      # implementation tools - take all 
      shelldf[j,25] <- paste(d[,25], collapse="|||")
      # implementation quotes - take all 
      shelldf[j,26] <- paste(d[,26], collapse="|||") 
      # hazards -- if agree, take that answer, else take all
      if (dim(table(d[,27])) == 1){
        shelldf[j,27] <- d[1,27]
      }
      else {
        d[,27] <- as.character(d[,27])
        s27 <- unlist(strsplit(d[,27],split='|||', fixed=TRUE))
        s27 <- unique(s27, incomparables = FALSE)
        shelldf[j,27] <- paste(s27, collapse="|||")  
      }
      # hazards other - take all 
      shelldf[j,28] <- paste(d[,28], collapse = "|||")  
      # hazards quotes - take all 
      shelldf[j,29] <- paste(d[,29], collapse = "|||")    
      # exposure - take all 
      if (dim(table(d[,30])) == 1) {
        shelldf[j,30] <- d[1,30]
      }
      else {
        d[,30] <- as.character(d[,30])
        s30 <- unlist(strsplit(d[,30],split='|||', fixed=TRUE))
        s302 <- unique(s30, incomparables = FALSE)
        shelldf[j,30] <- paste(s302, collapse = "|||")
      }
      # exposure other - take all 
      shelldf[j,31] <- paste(d[,31], collapse = "|||")    
      # exposure quotes - take all 
      shelldf[j,32] <- paste(d[,32], collapse = "|||")  
      # links to risk - take all 
      shelldf[j,33] <- paste(d[,33], collapse = "|||")  
      # links to risk quotes - take all 
      shelldf[j,34] <- paste(d[,34], collapse = "|||")  
      
      
      # implementation quotes - take all 
      shelldf[j,37] <- paste(d[,37], collapse = "|||")  
      # adaptation finance -- if either one is TRUE, take TRUE 
      d[,38] <- as.logical(d[,38])
      if (isTRUE(any(d[,38]))){
        shelldf[j,38]<-"TRUE"
      }
      else shelldf[j,38]<-"FALSE" 
      # finance costs -- if agree, take that answer, else take both
      if (dim(table(d[,39])) == 1){
        shelldf[j,39] <- d[1,39]
      }
      else {
        d[,39] <- as.character(d[,39])
        s39 <- unlist(strsplit(d[,39],split='|||', fixed=TRUE))
        s39 <- unique(s39, incomparables = FALSE)
        shelldf[j,39] <- paste(s39, collapse="|||")  
      }
      # depth 
      shelldf[j,40] <- paste(d[,40], collapse = "|||")  
      # depth quotes -- take all
      shelldf[j,41] <- paste(d[,44], collapse = "|||")  
      # scope 
      shelldf[j,42] <- paste(d[,42], collapse = "|||")  
      # scope quotes -- take all
      shelldf[j,43] <- paste(d[,43], collapse = "|||")  
      # speed
      shelldf[j,44] <- paste(d[,44], collapse = "|||")  
      # speed quotes -- take all
      shelldf[j,45] <- paste(d[,45], collapse = "|||")  
      # reduced risk -- if either one is TRUE, take TRUE
      d[,46] <- as.logical(d[,46])
      if (isTRUE(any(d[,46]))){
        shelldf[j,46]<-"TRUE"
      }
      else shelldf[j,46]<-"FALSE"                    
      # reduced risk quotes -- take all
      shelldf[j,47] <- paste(d[,47], collapse = "|||")  
      # indicators  -- if either one is TRUE, take TRUE 
      d[,48] <- as.logical(d[,48])
      if (isTRUE(any(d[,48]))){
        shelldf[j,48] <- "TRUE"
      }
      else shelldf[j,48] <- "FALSE"                  
      # indicators quotes -- take all 
      shelldf[j,49] <- paste(d[,49], collapse = "|||")  
      # maladaptation 
      shelldf[j,50] <- paste(d[,50], collapse = "|||")  
      # maladpatation quotes -- take all  
      shelldf[j,51] <- paste(d[,51], collapse = "|||")  
      # co benefits 
      shelldf[j,52] <- paste(d[,52], collapse = "|||")  
      # co benefits -- take all
      shelldf[j,53] <- paste(d[,53], collapse = "|||")  
      # limits -- if either says true, take true
      d[,54] <- as.logical(d[,54])
      if (isTRUE(any(d[,54]))){
        shelldf[j,54] <- "TRUE"
      }
      else shelldf[j,54] <- "FALSE" 
      # limits describe -- take all 
      shelldf[j,55] <- paste(d[,55], collapse = "|||")  
      # hard, soft -- if same, take that, else take all 
      shelldf[j,56] <- paste(d[,56], collapse = "|||")  
      # approach limits -- if same, take that, else take all   
      if (dim(table(d[,57])) == 1){
        shelldf[j,57] <- d[1,57]
      }
      else {
        d[,57] <- as.character(d[,57])
        s57 <- unlist(strsplit(d[,57],split='|||', fixed=TRUE))
        s57 <- unique(s57, incomparables = FALSE)
        shelldf[j,57] <- paste(s57, collapse="|||")  
      }                 
      # approach limits quotes -- take all 
      shelldf[j,58] <- paste(d[,58], collapse = "|||")  
      # methods -- take all 
      shelldf[j,59] <- paste(d[,59], collapse = "|||")  
      # coherence -- take all 
      shelldf[j,60] <- paste(d[,60], collapse = "|||")  
      # adequacy -- take all 
      shelldf[j,61] <- paste(d[,61], collapse = "|||")  
      # relevance -- take all 
      shelldf[j,62] <- paste(d[,62], collapse = "|||")  
      # user note -- take all 
      shelldf[j,63] <- paste(d[,63], collapse = "|||")  

      # implementation
      # all agree, take that answer
      d[,35] <- as.character(d[,35])
      shelldf <- Process_Imp(d, shelldf, j)

      j <- j + 1
      #print(j)
    } 
    
    # CASE 2: Single reliable coder says both include and sufficient
    
    k <- j + 1
    singles <- sing(data,unrelcoders)
    # SUBSET data for single codes
    sd <- data[data$Article.ID %in% singles,]
    #sd[1,1:11] <- as.character(sd[1:11])
    # for (i in 1:nrow(sd)) {
    #   for(z in 1:ncol(sd)){
    #     shelldf[k,z] <- as.character(sd[i,z])
    #   }
    # }
    
    for (i in 1:nrow(sd)) {
      shelldf[k,1] <- as.character(sd[i,1])
      shelldf[k,2] <- as.character(sd[i,2])
      shelldf[k,3] <- as.character(sd[i,3])
      shelldf[k,4] <- as.logical(sd[i,4])
      shelldf[k,5] <- as.character(sd[i,5])
      shelldf[k,6] <- as.character(sd[i,6])
      shelldf[k,7] <- as.logical(sd[i,7])
      shelldf[k,8] <- as.character(sd[i,8])
      shelldf[k,9] <- as.character(sd[i,9])
      shelldf[k,10] <- as.character(sd[i,10])
      shelldf[k,11] <- as.character(sd[i,11])
      shelldf[k,12] <- as.logical(sd[i,12])
      shelldf[k,13] <- as.logical(sd[i,13])
      shelldf[k,14] <- as.character(sd[i,14])
      shelldf[k,15] <- as.character(sd[i,15])
      shelldf[k,16] <- as.character(sd[i,16])
      shelldf[k,17] <- as.character(sd[i,17])
      shelldf[k,18] <- as.character(sd[i,18])
      shelldf[k,19] <- as.character(sd[i,19])
      shelldf[k,20] <- as.character(sd[i,20])
      shelldf[k,21] <- as.character(sd[i,21])
      shelldf[k,22] <- as.character(sd[i,22])
      shelldf[k,23] <- as.character(sd[i,23])
      shelldf[k,24] <- as.character(sd[i,24])
      shelldf[k,25] <- as.character(sd[i,25])
      shelldf[k,26] <- as.character(sd[i,26])
      shelldf[k,27] <- as.character(sd[i,27])
      shelldf[k,28] <- as.character(sd[i,28])
      shelldf[k,29] <- as.character(sd[i,29])
      shelldf[k,30] <- as.character(sd[i,30])
      shelldf[k,31] <- as.character(sd[i,31])
      shelldf[k,32] <- as.character(sd[i,32])
      shelldf[k,33] <- as.character(sd[i,33])
      shelldf[k,34] <- as.character(sd[i,34])
      shelldf[k,35] <- as.character(sd[i,35])
      shelldf[k,36] <- sd[i,36]
      shelldf[k,37] <- as.character(sd[i,37])
      shelldf[k,38] <- as.logical(sd[i,38])
      shelldf[k,39] <- as.character(sd[i,39])
      shelldf[k,40] <- as.character(sd[i,40])
      shelldf[k,41] <- as.character(sd[i,41])
      shelldf[k,42] <- as.character(sd[i,42])
      shelldf[k,43] <- as.character(sd[i,43])
      shelldf[k,44] <- as.character(sd[i,44])
      shelldf[k,45] <- as.character(sd[i,45])
      shelldf[k,46] <- as.logical(sd[i,46])
      shelldf[k,47] <- as.character(sd[i,47])
      shelldf[k,48] <- as.logical(sd[i,48])
      shelldf[k,49] <- as.character(sd[i,49])
      shelldf[k,50] <- as.character(sd[i,50])
      shelldf[k,51] <- as.character(sd[i,51])
      shelldf[k,52] <- as.character(sd[i,52])
      shelldf[k,53] <- as.character(sd[i,53])
      shelldf[k,54] <- as.logical(sd[i,54])
      shelldf[k,55] <- as.character(sd[i,55])
      shelldf[k,56] <- as.character(sd[i,56])
      shelldf[k,57] <- as.character(sd[i,57])
      shelldf[k,58] <- as.character(sd[i,58])
      shelldf[k,59] <- as.character(sd[i,59])
      shelldf[k,60] <- as.character(sd[i,60])
      shelldf[k,61] <- as.character(sd[i,61])
      shelldf[k,62] <- as.character(sd[i,62])
      shelldf[k,63] <- as.character(sd[i,63])
      shelldf[k,64] <- as.character(sd[i,64])
      shelldf[k,65] <- as.character(sd[i,65])
      shelldf[k,66] <- as.character(sd[i,66])
      if (shelldf[k,35] == "") {
        shelldf[k,35] <- "BLANK:REVIEW NEEDED"
      }
      k <- k+1
    }
    
    singles<-sing(data,unrelcoders)
    print(c("Number of single-coded papers by reliable coder who says Include & Suff",length(singles)))
    unrelarts <- dualunrel(data,unrelcoders)   
    
    print(c("Number articles coded by multi unreliable coders",length(unrelarts)))
  
  return(shelldf)
}

recdual <- reconcil(data,unrelcoders)

csvFileName2 <- paste("GAMI_Reonciliation",Sys.Date(),".csv",sep="")
write.csv(recdual, file=csvFileName2)


# CASE 3: multiple coders; both unreliable; at least one says include and sufficient
# require human review -- then become either easy or sing 

# CASE 4: one coder; unreliable (IGNORE - treat as uncoded)

