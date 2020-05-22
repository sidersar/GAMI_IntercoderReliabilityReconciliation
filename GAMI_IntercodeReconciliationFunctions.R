# GAMI Reconciling Differences in Coder Responses
# ARSiders (siders@udel.edu)
# Spring 2020


# =============== DATA ON CODING

# ===== number of coders and unique coder names
coders <-as.vector(unique(data[,2], incomparables = FALSE))
relcoders <- setdiff(coders,unrelcoders)

# ===== unique article labels
articles <-as.vector(unique(data[,1], incomparables = FALSE))

# ===== articles with more than one coder
cot <- as.data.frame(table(data$Article.ID))
multiples <- as.vector(cot[cot$Freq>1,1])

# ===== Single-coded articles

singla <- function(data,unrelcoders){
   coders <- as.vector(unique(data[,2], incomparables = FALSE))
   relcoders <- setdiff(coders,unrelcoders)
   # articles by single coder
   ct <- as.data.frame(table(data$Article.ID))
   sing <- as.vector(ct[ct$Freq==1,1])
   print(c("Number single-coded articles",length(sing)))
   # cycle through articles with just one coder
   relart.sing.inc <- vector()
   relart.sing.exc <- vector()
   unrel.sing.inc <- vector()
   unrel.sing.exc <- vector()
   for (i  in 1:(length(sing))){
     # subset each article
     d <- data[data$Article.ID==sing[i],]   
     # check if coder is reliable coder
     if (d[1,2] %in% relcoders){
       if ((d[1,4] == TRUE) & (d[1,7] == TRUE)){
       relart.sing.inc <- c(relart.sing.inc,d$Article.ID)
       }
       else 
       relart.sing.exc <- c(relart.sing.exc, d$Article.ID)
     }
     else if (d[1,2] %in% unrelcoders){
       if ((d[1,4] == TRUE) & (d[1,7] == TRUE)){
         unrel.sing.inc <- c(unrel.sing.inc,d$Article.ID)
       }
       else 
         unrel.sing.exc <- c(unrel.sing.exc, d$Article.ID)      
     }
   }
   lengths<-(length(relart.sing.inc)+length(relart.sing.exc)+length(unrel.sing.inc)+length(unrel.sing.exc))
   print(c("Checking addition",lengths))
   print(c("Single reliable included",length(relart.sing.inc)))   
   print(c("Single reliable excluded",length(relart.sing.exc)))   
   print(c("Single unreliable included",length(unrel.sing.inc)))   
   print(c("Single unreliable excluded",length(unrel.sing.exc)))   

   # no return: just provides counts 
}

# ===== Single-coded articles by UNreliable coder who says INC+SUFF
singlun <- function(data,unrelcoders){
  # articles by single coder
  ct <- as.data.frame(table(data$Article.ID))
  sing <- as.vector(ct[ct$Freq==1,1])
  # cycle through articles with just one coder
  unrel.sing.inc <- vector()
  unrel.sing.exc <- vector()
  for (i  in 1:(length(sing))){
    # subset each article
    d <- data[data$Article.ID==sing[i],]   
    # check if coder is reliable coder
    if (d[1,2] %in% unrelcoders){
      if ((d[1,4] == TRUE) & (d[1,7] == TRUE)){
        unrel.sing.inc <- c(unrel.sing.inc,d$Article.ID)
      }
      else 
        unrel.sing.exc <- c(unrel.sing.exc, d$Article.ID)      
    }
  }
  return(unrel.sing.inc)
}

# ===== two unreliable coders
twounrelcoded <- function(data,unrelcoders) {
  unrelv <- vector()
  # find article IDs for articles with more than one coder
  ct <- as.data.frame(table(data$Article.ID))
  mults <- as.vector(ct[ct$Freq>1,1])
  # cycle through aticles with more than one coder
  for (i  in 1:(length(mults))){
    # subset each article
    d <- data[data$Article.ID==mults[i],]
    # if all coders are unreliable
    if (all(d$User %in% unrelcoders)) {
      unrelv <-c(unrelv,d[1,1])
    }
  }
  unrelartsdb <- data[data$Article.ID %in% unrelv,] 
  return(unrelartsdb)
}


# =============== IDENTIFY PAPERS THAT NEED HUMAN REVIEW 

# ===== IDENTIFY ARTICLES BY TWO UNRELIABLE CODERS: AT LEAST ONE SAYS INCLUDE & SUFFICIENT 

dualunrel <- function(data,unrelcoders) {
  unrelartv <- vector()
  # find article IDs for articles with more than one coder
  ct <- as.data.frame(table(data$Article.ID))
  mults <- as.vector(ct[ct$Freq>1,1])
  # cycle through aticles with more than one coder
  for (i  in 1:(length(mults))){
    # subset each article
    d <- data[data$Article.ID==mults[i],]
    # if all coders are unreliable
    if (all(d$User %in% unrelcoders)) {
      # if at least one says include and sufficient 
        if (any(d[,4]==TRUE) & (any(d[,7]==TRUE))){
          unrelartv <- c(unrelartv,d[1,1])
        }
    }
  }
  unrelartd <- data[data$Article.ID %in% unrelartv,] 
  return(unrelartd)
}

# ===== IDENTIFY ARTICLES WITH NO IMPLEMENTATION RESPONSE 

blank_imp <- function(data){
  # at least one reliable coder says INC&SUFF
  coders <- as.vector(unique(data[,2], incomparables = FALSE))
  relcoders <- setdiff(coders,unrelcoders)
  blank_impsv <-vector()
  # loop though each article
  for (j in 1:length(articles)){
    d <- data[data$Article.ID==articles[j],]
    if (any(d[,2] %in% relcoders)){
      if (any(d[,4]==TRUE) & (any(d[,7]==TRUE)) & (all(d[,35]==""))){
        blank_impsv <-c(blank_impsv,articles[j])
      }
    }
  }
  blank.articles <- data[data$Article.ID %in% blank_impsv,]
  return(blank.articles)
}


# ===== Combine and run the code to identify articles in need of human review
arts2rev <-function(data,unrelcoders){
  twounrel <-dualunrel(data,unrelcoders)
  noimplement <-blank_imp(data)
  arts2review <-rbind(twounrel,noimplement)
  return(arts2review)
}


# ===== single reliable coder
singre <- function(data,relcoders,sing){
  # cycle through articles with just one coder
  relsing <- vector()
  for (i  in 1:(length(sing))){
    # subset each article
    sd <- data[data$Article.ID==sing[i],]   
    # check if coder is reliable coder
    if (sd[1,2] %in% relcoders){
      relsing<-c(relsing,sd[1,1])
    }
  }
  return(relsing)
}



# ======= RETURN ARTICLES ONCE REVIEWED

# remove articles already reviewed to return dataframe of articles that still need review
reconcileupdate <- function(data,unrelcoders,reccode){
  reccode<-cleandata(reccode)
  colnames(reccode)<-colnames(data)
  # articles flagged as needing human review
  articles2review<-arts2rev(data,unrelcoders)
  flagged.arts <- as.vector(unique(articles2review$Article.ID, incomparables = FALSE)) 
  # articles already reviewed by human
  recd.arts <- as.vector(unique(reccode$Article.ID, incomparables = FALSE))
  print(c("Articles already reviewed",length(recd.arts)))
  # articles that still need human review 
  arts.stillreview <- setdiff(flagged.arts,recd.arts)
  print(c("Articles still needing review",length(arts.stillreview)))
  # database just of those that still need review
  articles4review <- articles2review[articles2review$Article.ID %in% arts.stillreview,]
  return(articles4review)  
}

# replace reviewed data in overall data frame to reconcile 
reconciledata <- function(data,unrelcoders,reccode){
  reccode2<-cleandata(reccode)
  data<-cleandata(data)
  print(c("Ncol reccode",ncol(reccode2)))
  colnames(reccode2)<-colnames(data)
  print(c("Dim of data",dim(data)))
  # articles flagged as needing human review
  articles2review<-arts2rev(data,unrelcoders)
  flagged.arts <- as.vector(unique(articles2review$Article.ID, incomparables = FALSE)) 
  # articles already reviewed by human
  recd.arts <- as.vector(unique(reccode2$Article.ID, incomparables = FALSE))
  print(c("Articles already reviewed",length(recd.arts)))
  # articles that still need human review 
  arts.stillreview <- setdiff(flagged.arts,recd.arts)
  # database just of those that still need review
  articles4review <- articles2review[articles2review$Article.ID %in% arts.stillreview,]
  # replace data with reviewed articles 
  # remove data for reviewed articles 
  a<-which(data$Article.ID %in% recd.arts)
  d<- data[-a,]
  # add on reviewed article information 
  d<-rbind(d,reccode2)
  print(c("Should equal dim data at end",dim(d)))
  return(d)
}


# =============== RECONCILE CODER DIFFERENCES 


6205494 # should be FALSE 



reconciliation <-function(data,unrelcoders,reccode){
  data <- cleandata(data)
  reccode2<-cleandata(reccode)
  colnames(reccode2)<-colnames(data)
  
  ct <- as.data.frame(table(data$Article.ID))
  mults <- as.vector(ct[ct$Freq>1,1])
  sing <- as.vector(ct[ct$Freq==1,1])

  shelldf <- as.data.frame(matrix(ncol=66))
  colnames(shelldf) <- colnames(data)

  # deciding between cases 1,3
  singlesre<-singre(data,relcoders,sing)    
  singlesunre<-setdiff(sing,singlesre)
  a<-length(singlesre)
  b<-length(singlesunre)
  
  print(c("Number of articles coded",length(articles)))
  print(c("How many coded by multiple people",length(mults)))
  print(c("How many coded by one person",length(sing)))
  
  # deciding between Cases 2,4,5
      # loop through articles with more than 1 coder
      case2<-vector()
      case4<-vector()
      case5<-vector()
      for (h in 1:length(mults)){
        rd <- data[data$Article.ID==mults[h],]
        # check if coders reliable
        if (all(rd[,2] %in% relcoders)){
          case4<-c(case4,rd[1,1])
        } else if (all(rd[,2] %in% unrelcoders)){
          case2<-c(case2,rd[1,1])
        } else {
          case5<-c(case5,rd[1,1])
        }
      } 
      # creates a vector of article IDs for each case
      
      # articles already reviewed by human
      recd.arts <- as.vector(unique(reccode2$Article.ID, incomparables = FALSE))
      # which are the same 
      reviewedunrels <-intersect(case2,recd.arts)
        # articles coded by two unreliable reviewers, a third human says include
        # remove these from case 2
        case2<-case2[!case2 %in% reviewedunrels]
        # add them to case 5
        case5<-c(case5,reviewedunrels)
        
      print(c("Case1: One unreliable coder (not reconciled)",length(singlesunre)))
      print(c("Case2: Two unreliable coders (not yet reviewed) (not reconciled)",length(case2)))
      print(c("Case3: One reliable coder (recorded)",length(singlesre)))
      print(c("Case4: Two reliable coders (reconciled)",length(case4)))
      print(c("Case5: Two coders mix re/unrel or reviewed unrel (reconciled)",length(case5)))
      
      summing<-(a+length(case2)+b+length(case4)+length(case5))
      print(c("Sum check",summing))
      print(c("Num articles",length(articles)))
      proper<-(a+length(case4)+length(case5))

  # case 1: single unreliable coder - treat as uncoded; do not include 
  # case 2: two unreliable coders: treat as uncoded; do not include 
  
  # case 3: single reliable coder: take answers into reconciled table 
  print("Recording single codes")
  single.reliable.coder <-sing.rel.cod(shelldf,data,singlesre)

  # case 4: two reliable coders: reconcile 
  print("Reconciling multiple codes")
  mult.reliable.coder <- multreliables(shelldf,data,case4,case5)
  # for the moment, case 4 and 5 are both treated the same
  
  # case 5: two coders, one reliable one unreliable: reconcile weighting reliable
#  mult.unrel.coders <- multunrels(shelldf,data,case5)
  
  #combine results in shell dataframe
  shelldf<-rbind(single.reliable.coder,mult.reliable.coder) #,mult.unrel.coders)
  
  # remove rows of NA
  shelldf2<-shelldf[-(which(is.na(shelldf[,1]))),]
  print(c("Num cases 3+4+5 should equal csv rows",proper))
  print(c("Number csv rows",nrow(shelldf2)))
  
  return(shelldf2)
}


# # ===== FINDING EMPTY CODES AFTER RECONCILIATION
# 
# emptycodes <-function(shelldf){
#   shelldfempty<-  as.data.frame(matrix(ncol=66))
#   # loop through each row of the reconciled data
#   row<-1
#   for (i in 1:nrow(shelldf)){
#     shelldf[i,4]<-as.logical(shelldf[i,4])
#     shelldf[i,7]<-as.logical(shelldf[i,7])
#     # reset count to 0
#     ctblanks<-0
#     if (isTRUE(shelldf[i,4])) {
#       if (isTRUE(shelldf[i,7])){
#       # calculate how many entries are empty 
#         if (is.na(shelldf[i,8]) ||is.null(shelldf[i,8]) || (shelldf[i,8]=="") || (shelldf[i,8]=="|||")){
#           ctblanks<-(ctblanks+1)
#         }
#         # if (is.na(shelldf[i,11]) ||is.null(shelldf[i,8]) || (shelldf[i,10]=="") || (shelldf[i,10]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,12]) ||is.null(shelldf[i,8]) || (shelldf[i,12]=="") || (shelldf[i,12]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,13]) ||is.null(shelldf[i,8]) || (shelldf[i,13]=="") || (shelldf[i,13]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,14]) ||is.null(shelldf[i,8]) || (shelldf[i,14]=="") || (shelldf[i,14]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,23]) ||is.null(shelldf[i,8]) || (shelldf[i,23]=="") || (shelldf[i,23]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,25]) ||is.null(shelldf[i,8]) || (shelldf[i,25]=="") || (shelldf[i,25]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,27]) ||is.null(shelldf[i,8]) || (shelldf[i,27]=="") || (shelldf[i,27]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,48]) ||is.null(shelldf[i,8]) || (shelldf[i,48]=="") || (shelldf[i,48]=="|||")){
#         #   ctblanks<-(ctblanks+1)
#         # }
#         # if (is.na(shelldf[i,54]) ||is.null(shelldf[i,54]) || (shelldf[i,54]=="") || (shelldf[i,54]=="|||")){
#         #   ctblanks<-(ctblanks+1) 
#         # }
#      if (ctblanks>0){
#       shelldfempty[row,]<-shelldf[i,]
#       row<-(row+1)
#      }
#     }
#    }
#   }
#   print(c("Codes to include with empty answers",nrow(shelldfempty)))
#   return(shelldfempty)
# }



# ===== CASE 3: SINGLE RELIABLE CODER - WRITE INTO SHELL DF

sing.rel.cod <- function(shelldf,data,sing){
  # SUBSET data for single codes
  k <- (nrow(shelldf) + 1)
  sd <- data[data$Article.ID %in% sing,]
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
     shelldf[k,36] <- as.character(sd[i,36])
     shelldf[k,37] <- as.logical(sd[i,37])
     shelldf[k,38] <- as.character(sd[i,38])
     shelldf[k,39] <- as.character(sd[i,39])
     shelldf[k,40] <- as.character(sd[i,40])
     shelldf[k,41] <- as.character(sd[i,41])
     shelldf[k,42] <- as.character(sd[i,42])
     shelldf[k,43] <- as.character(sd[i,43])
     shelldf[k,44] <- as.character(sd[i,44])
     shelldf[k,45] <- as.logical(sd[i,45])
     shelldf[k,46] <- as.character(sd[i,46])
     shelldf[k,47] <- as.logical(sd[i,47])
     shelldf[k,48] <- as.character(sd[i,48])
     shelldf[k,49] <- as.character(sd[i,49])
     shelldf[k,50] <- as.character(sd[i,50])
     shelldf[k,51] <- as.character(sd[i,51])
     shelldf[k,52] <- as.character(sd[i,52])
     shelldf[k,53] <- as.logical(sd[i,53])
     shelldf[k,54] <- as.character(sd[i,54])
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
     shelldf[k,66] <- as.numeric(sd[i,66])
     if ((shelldf[k,35]=="") || (is.na(shelldf[k,35])) || (is.null(shelldf[k,35]))) {
       shelldf[k,35] <- "BLANK: REVIEW NEEDED"
     }
     k <- k+1
   }
   return(shelldf)
}


# ===== CASE 4: MULTIPLE RELIABLE CODERS - RECONCILE INTO SHELLDF 

multreliables <-function(shelldf,data,case4,case5){
  case4<-c(case4,case5)
  j <- (nrow(shelldf) + 1)
  # subset data for multiple reliable coders
  md <- data[data$Article.ID %in% case4,]
  # cycle through the articles
    for (m in 1:nrow(md)){
      d <- md[md$Article.ID==case4[m],]
      # set article citation info
      shelldf[j,1] <- as.character(d[1,1]) # article ID
      shelldf[j,63] <- as.character(d[1,63]) # title
      shelldf[j,64] <- as.character(d[1,64]) # journal
      shelldf[j,65] <- as.character(d[1,65]) # authors
      # combine coder names 
      shelldf[j,2] <- paste(d[,2], collapse = "|||")
      # combine summaries
      shelldf[j,6] <- paste(d[,6], collapse = "|||")  

      # Include -- if either one is TRUE, take TRUE 
      d[,4] <- as.logical(d[,4])
      if (isTRUE(any(d[,4]))){
        shelldf[j,4] <-"TRUE"
      }    
      else shelldf[j,4]<-"FALSE"
      # Include -- if either one is TRUE, take TRUE 
      d[,7] <- as.logical(d[,7])
      if (isTRUE(any(d[,7]))){
        shelldf[j,7] <-"TRUE"
      }    
      else shelldf[j,7]<-"FALSE"
    
      # only continue if at least one says Include & Sufficient 
      if (isTRUE(any(d[,4])) & isTRUE(any(d[,7]))){
        
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
        shelldf[j,36] <- paste(d[,36], collapse = "|||")  
        # adaptation finance -- if either one is TRUE, take TRUE 
        d[,37] <- as.logical(d[,37])
        if (isTRUE(any(d[,37]))){
          shelldf[j,37]<-"TRUE"
        }
        else shelldf[j,37]<-"FALSE" 
        # finance costs -- if agree, take that answer, else take both
        if (dim(table(d[,38])) == 1){
          shelldf[j,38] <- d[1,38]
        }
        else {
          d[,38] <- as.character(d[,38])
          s38 <- unlist(strsplit(d[,38],split='|||', fixed=TRUE))
          s38 <- unique(s38, incomparables = FALSE)
          shelldf[j,38] <- paste(s38, collapse="|||")  
        }
        # depth 
        shelldf[j,39] <- paste(d[,39], collapse = "|||")  
        # depth quotes -- take all
        shelldf[j,40] <- paste(d[,40], collapse = "|||")  
        # scope 
        shelldf[j,41] <- paste(d[,41], collapse = "|||")  
        # scope quotes -- take all
        shelldf[j,42] <- paste(d[,42], collapse = "|||")  
        # speed
        shelldf[j,43] <- paste(d[,43], collapse = "|||")  
        # speed quotes -- take all
        shelldf[j,44] <- paste(d[,44], collapse = "|||")  
        # reduced risk -- if either one is TRUE, take TRUE
        d[,45] <- as.logical(d[,45])
        if (isTRUE(any(d[,45]))){
          shelldf[j,45]<-"TRUE"
        }
        else shelldf[j,45]<-"FALSE"                    
        # reduced risk quotes -- take all
        shelldf[j,46] <- paste(d[,46], collapse = "|||")  
        # indicators  -- if either one is TRUE, take TRUE 
        d[,47] <- as.logical(d[,47])
        if (isTRUE(any(d[,47]))){
          shelldf[j,47] <- "TRUE"
        }
        else shelldf[j,47] <- "FALSE"                  
        # indicators quotes -- take all 
        shelldf[j,48] <- paste(d[,48], collapse = "|||")  
        # maladaptation 
        shelldf[j,49] <- paste(d[,49], collapse = "|||")  
        # maladpatation quotes -- take all  
        shelldf[j,50] <- paste(d[,50], collapse = "|||")  
        # co benefits 
        shelldf[j,51] <- paste(d[,51], collapse = "|||")  
        # co benefits -- take all
        shelldf[j,52] <- paste(d[,52], collapse = "|||")  
        # limits -- if either says true, take true
        d[,53] <- as.logical(d[,53])
        if (isTRUE(any(d[,53]))){
          shelldf[j,53] <- "TRUE"
        }
        else shelldf[j,53] <- "FALSE" 
        # limits describe -- take all 
        shelldf[j,54] <- paste(d[,54], collapse = "|||")  
        # hard, soft -- if same, take that, else take all 
        shelldf[j,55] <- paste(d[,55], collapse = "|||")  
        # approach limits -- if same, take that, else take all   
        if (dim(table(d[,56])) == 1){
          shelldf[j,56] <- d[1,56]
        }
        else {
          d[,56] <- as.character(d[,56])
          s56 <- unlist(strsplit(d[,56],split='|||', fixed=TRUE))
          s56 <- unique(s56, incomparables = FALSE)
          shelldf[j,56] <- paste(s56, collapse="|||")  
        }                 
        # approach limits quotes -- take all 
        shelldf[j,57] <- paste(d[,57], collapse = "|||")  
        # methods -- take all 
        shelldf[j,58] <- paste(d[,58], collapse = "|||")  
        # coherence -- take all 
        shelldf[j,59] <- paste(d[,59], collapse = "|||")  
        # adequacy -- take all 
        shelldf[j,60] <- paste(d[,60], collapse = "|||")  
        # relevance -- take all 
        shelldf[j,61] <- paste(d[,61], collapse = "|||")  
        # user note -- take all 
        shelldf[j,62] <- paste(d[,62], collapse = "|||")  
        
        # implementation
        # all agree, take that answer
        d[,35] <- as.character(d[,35])
        shelldf <- Process_Imp(d, shelldf, j)
      }
      
    j <- j + 1
  }
  return(shelldf)
}


# ===== Code to process the Implementation Scores

Process_Imp <- function(d, shelldf, j) {
  if (dim(table(d[,35])) == 1) {
    shelldf[j,66] <- d[1,66]
    shelldf[j,35] <- d[1,35]
    shelldf[j,35] <- as.character(shelldf[j,35])
    # if they agree because both entries are blank, set to review
    if ((shelldf[j,35]=="") || (is.na(shelldf[j,35])) || (is.null(shelldf[j,35]))) {
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
        g <- (max(d[,66]) - min(d[,66]))
        # if conflict codes one apart, take the higher
        if (g==1) {
          ma <- max(d[,66])
          hig <- d[(d[,66]==ma),]
          shelldf[j,35] <- hig[,35]
        }
        else if (g > 1) {   # if conflict farther apart, take average (round up)
          av <- mean(d[,66])
          av <- ceiling(av) # round up
          if (av == 1){
            shelldf[j,66] <- 1
            shelldf[j,35] <- "Vulnerability assessment and/or early planning"
          }
          else if (av==2){
            shelldf[j,66] <- 2
            shelldf[j,35] <- "Adaptation planning & early implementation"
          }
          else if (av==3){
            shelldf[j,66] <- 3
            shelldf[j,35] <- "Implementation expanding"
          }
          else if (av==4){
            shelldf[j,66] <- 4
            shelldf[j,35] <- "Implementation widespread"
          }
          else if (av==5){
            shelldf[j,66] <- 5
            shelldf[j,35] <- " Evidence of risk reduction associated with adaptation efforts"
          }
          else {
            print("REVIEW NEEDED",i)
          }
        }
      }
    }
    # else if none of the multiple entries are Evidence:
    else {
      # how far apart entries are
      g <- (max(d[,66]) - min(d[,66]))
      # if conflict codes one apart, take the higher
      if (g==1){
        ma <- max(d[,66])
        hig <- d[(d[,66]==ma),]
        shelldf[j,35] <- hig[1,35]
      }
      else if (g > 1) {   # if conflict farther apart, take average (round up)
        av <- mean(d[,66])
        av <- ceiling(av) # round up
        if (av == 1){
          shelldf[j,66] <- 1
          shelldf[j,35] <- "Vulnerability assessment and/or early planning"
        }
        else if (av==2){
          shelldf[j,66] <- 2
          shelldf[j,35] <- "Adaptation planning & early implementation"
        }
        else if (av==3){
          shelldf[j,66] <-3
          shelldf[j,35] <- "Implementation expanding"
        }
        else if (av==4){
          shelldf[j,66] <- 4
          shelldf[j,35] <- "Implementation widespread"
        }
        else if (av==5){
          shelldf[j,66] <-5
          shelldf[j,35] <- " Evidence of risk reduction associated with adaptation efforts"
        }
      }
    }
  }
  if ((shelldf[j,35]=="") || (is.na(shelldf[j,35])) || (is.null(shelldf[j,35]))) {
    shelldf[j,35] <- "BLANK: REVIEW NEEDED"
  }
  return(shelldf)
}


# ===== Case 5: Two coders (one rel, one unreliable) 

# prioritizing reliable coders ... work in progress 
# 
# multunrels <-function(shelldf,data,case5){
#   j <- (nrow(shelldf) + 1)
#   # subset data for multiple reliable coders
#   mud <- data[data$Article.ID %in% case5,]
#   # cycle through the articles
#   for (m in 1:nrow(mud)){
#     d <- mud[mud$Article.ID==case5[m],]
#     ar<- as.vector(d[d$User %in% relcoders,2])
#     a<-which(d[,2]==ar)
#     # set article citation info
#     shelldf[j,1] <- d[1,1] # article ID
#     shelldf[j,63] <- d[1,63] # title
#     shelldf[j,64] <- d[1,64] # journal
#     shelldf[j,65] <- d[1,65] # authors
#     # combine coder names 
#     shelldf[j,2] <- paste(d[,2], collapse = "|||")
#     # combine summaries
#     shelldf[j,6] <- paste(d[,6], collapse = "|||")  
#     # set include and suficient to TRUE 
#     shelldf[j,4] <- TRUE
#     shelldf[j,7] <- TRUE  
#   
#     # geography -- if agree, take that answer, else take reliable
#     shelldf[j,8] <- d[a,8]
#     # geography country -- if agree, take that answer, else take both
#     if (dim(table(d[,9])) == 1){
#       shelldf[j,9] <- d[1,9]
#     }
#     else {
#       d[,9] <- as.character(d[,9])
#       s9 <-unlist(strsplit(d[,9],split='|||', fixed=TRUE))
#       s9 <- unique(s9, incomparables = FALSE)
#       shelldf[j,9] <- paste(s9, collapse="|||")
#     } 
#     # sector -- if agree, take that answer else take reliable
#     shelldf[j,10] <- d[a,10]
#     
#     # cross cutting topics -- if agree, take that answer, else take reliable
#     shelldf[j,11] <- d[a,11]
#     
#     # indigenous knowledge -- take reliable answer 
#     shelldf[j,12] <-d[a,12]
#     # local knowledge -- take reliable answer
#     shelldf[j,12] <-d[a,12]
#     
#     # actors / institutions-- if agree, take that answer, else all
#     if (dim(table(d[,14])) == 1){
#       shelldf[j,14] <- d[1,14]
#     }
#     else  {
#       d[,14] <- as.character(d[,14])
#       s8 <-unlist(strsplit(d[,14],split='|||', fixed=TRUE))
#       s8 <- unique(s8, incomparables = FALSE)
#       shelldf[j,14] <- paste(s8, collapse="|||")
#     }                
#     # actors, other -- take all 
#     shelldf[j,15] <- paste(d[,15], collapse="|||")                 
#     # actors, quotes -- take all 
#     shelldf[j,16] <- paste(d[,16], collapse="|||")    
#     # equity planning -- if same, take that one, else all 
#     if (dim(table(d[,17])) == 1){
#       shelldf[j,17] <- d[1,17]
#     }
#     else  {
#       d[,17] <- as.character(d[,17])
#       s17 <-unlist(strsplit(d[,17],split='|||', fixed=TRUE))
#       s17 <- unique(s17, incomparables = FALSE)
#       shelldf[j,17] <- paste(s17, collapse="|||")
#     }
#     # equity planning other -- take all 
#     shelldf[j,18] <- paste(d[,18], collapse="|||")                  
#     # equity quotes - take all 
#     shelldf[j,19] <- paste(d[,19], collapse="|||")                   
#     # equity targeting -- if same, take that one, else all
#     if (dim(table(d[,20])) == 1){
#       shelldf[j,20] <- d[1,20]
#     }
#     else  {         
#       d[,20] <- as.character(d[,20])
#       s20 <-unlist(strsplit(d[,20],split='|||', fixed=TRUE))
#       s20 <- unique(s20, incomparables = FALSE)
#       shelldf[j,20] <- paste(s20, collapse="|||")
#     }
#     # equity targeting other -- take all
#     shelldf[j,21] <- paste(d[,21], collapse="|||")                   
#     # equity targeting quotes - take all 
#     shelldf[j,22] <- paste(d[,22], collapse="|||")                   
#     # type of response -- if agree, take that answer, else take all
#     if (dim(table(d[,23])) == 1){
#       shelldf[j,23] <- d[1,23]
#     }
#     else {
#       d[,23] <- as.character(d[,23])
#       s23 <- unlist(strsplit(d[,23],split='|||', fixed=TRUE))
#       s23 <- unique(s23, incomparables = FALSE)
#       shelldf[j,23] <- paste(s23, collapse="|||")
#     }
#     # response quotes - take all 
#     shelldf[j,24] <- paste(d[,24], collapse="|||")             
#     # implementation tools - take all 
#     shelldf[j,25] <- paste(d[,25], collapse="|||")
#     # implementation quotes - take all 
#     shelldf[j,26] <- paste(d[,26], collapse="|||") 
#     # hazards -- if agree, take that answer, else take all
#     if (dim(table(d[,27])) == 1){
#       shelldf[j,27] <- d[1,27]
#     }
#     else {
#       d[,27] <- as.character(d[,27])
#       s27 <- unlist(strsplit(d[,27],split='|||', fixed=TRUE))
#       s27 <- unique(s27, incomparables = FALSE)
#       shelldf[j,27] <- paste(s27, collapse="|||")  
#     }
#     # hazards other - take all 
#     shelldf[j,28] <- paste(d[,28], collapse = "|||")  
#     # hazards quotes - take all 
#     shelldf[j,29] <- paste(d[,29], collapse = "|||")    
#     # exposure - take all 
#     if (dim(table(d[,30])) == 1) {
#       shelldf[j,30] <- d[1,30]
#     }
#     else {
#       d[,30] <- as.character(d[,30])
#       s30 <- unlist(strsplit(d[,30],split='|||', fixed=TRUE))
#       s302 <- unique(s30, incomparables = FALSE)
#       shelldf[j,30] <- paste(s302, collapse = "|||")
#     }
#     # exposure other - take all 
#     shelldf[j,31] <- paste(d[,31], collapse = "|||")    
#     # exposure quotes - take all 
#     shelldf[j,32] <- paste(d[,32], collapse = "|||")  
#     # links to risk - take all 
#     shelldf[j,33] <- paste(d[,33], collapse = "|||")  
#     # links to risk quotes - take all 
#     shelldf[j,34] <- paste(d[,34], collapse = "|||")  
#     
#     # implementation quotes - take all 
#     shelldf[j,36] <- paste(d[,36], collapse = "|||")  
#     # adaptation finance -- if either one is TRUE, take TRUE 
#     d[,37] <- as.logical(d[,37])
#     if (isTRUE(any(d[,37]))){
#       shelldf[j,37]<-"TRUE"
#     }
#     else shelldf[j,37]<-"FALSE" 
#     # finance costs -- if agree, take that answer, else take both
#     if (dim(table(d[,38])) == 1){
#       shelldf[j,38] <- d[1,38]
#     }
#     else {
#       d[,38] <- as.character(d[,38])
#       s38 <- unlist(strsplit(d[,38],split='|||', fixed=TRUE))
#       s38 <- unique(s38, incomparables = FALSE)
#       shelldf[j,38] <- paste(s38, collapse="|||")  
#     }
#     # depth 
#     shelldf[j,39] <- paste(d[,39], collapse = "|||")  
#     # depth quotes -- take all
#     shelldf[j,40] <- paste(d[,40], collapse = "|||")  
#     # scope 
#     shelldf[j,41] <- paste(d[,41], collapse = "|||")  
#     # scope quotes -- take all
#     shelldf[j,42] <- paste(d[,42], collapse = "|||")  
#     # speed
#     shelldf[j,43] <- paste(d[,43], collapse = "|||")  
#     # speed quotes -- take all
#     shelldf[j,44] <- paste(d[,44], collapse = "|||")  
#     # reduced risk -- if either one is TRUE, take TRUE
#     d[,45] <- as.logical(d[,45])
#     if (isTRUE(any(d[,45]))){
#       shelldf[j,45]<-"TRUE"
#     }
#     else shelldf[j,45]<-"FALSE"                    
#     # reduced risk quotes -- take all
#     shelldf[j,46] <- paste(d[,46], collapse = "|||")  
#     # indicators  -- if either one is TRUE, take TRUE 
#     d[,47] <- as.logical(d[,47])
#     if (isTRUE(any(d[,47]))){
#       shelldf[j,47] <- "TRUE"
#     }
#     else shelldf[j,47] <- "FALSE"                  
#     # indicators quotes -- take all 
#     shelldf[j,48] <- paste(d[,48], collapse = "|||")  
#     # maladaptation 
#     shelldf[j,49] <- paste(d[,49], collapse = "|||")  
#     # maladpatation quotes -- take all  
#     shelldf[j,50] <- paste(d[,50], collapse = "|||")  
#     # co benefits 
#     shelldf[j,51] <- paste(d[,51], collapse = "|||")  
#     # co benefits -- take all
#     shelldf[j,52] <- paste(d[,52], collapse = "|||")  
#     # limits -- if either says true, take true
#     d[,53] <- as.logical(d[,53])
#     if (isTRUE(any(d[,53]))){
#       shelldf[j,53] <- "TRUE"
#     }
#     else shelldf[j,53] <- "FALSE" 
#     # limits describe -- take all 
#     shelldf[j,54] <- paste(d[,54], collapse = "|||")  
#     # hard, soft -- if same, take that, else take all 
#     shelldf[j,55] <- paste(d[,55], collapse = "|||")  
#     # approach limits -- if same, take that, else take all   
#     if (dim(table(d[,56])) == 1){
#       shelldf[j,56] <- d[1,56]
#     }
#     else {
#       d[,56] <- as.character(d[,56])
#       s56 <- unlist(strsplit(d[,56],split='|||', fixed=TRUE))
#       s56 <- unique(s56, incomparables = FALSE)
#       shelldf[j,56] <- paste(s56, collapse="|||")  
#     }                 
#     # approach limits quotes -- take all 
#     shelldf[j,57] <- paste(d[,57], collapse = "|||")  
#     # methods -- take all 
#     shelldf[j,58] <- paste(d[,58], collapse = "|||")  
#     # coherence -- take all 
#     shelldf[j,59] <- paste(d[,59], collapse = "|||")  
#     # adequacy -- take all 
#     shelldf[j,60] <- paste(d[,60], collapse = "|||")  
#     # relevance -- take all 
#     shelldf[j,61] <- paste(d[,61], collapse = "|||")  
#     # user note -- take all 
#     shelldf[j,62] <- paste(d[,62], collapse = "|||")  
#     
#     # implementation -- take reliable coder 
#     br<- d[d$User %in% unrelcoders,2]
#     b<-which(d[,2]==br)
#     d[,35] <- as.character(d[,35])
#     shelldf[j,35] <- d[a,35]
#       # unless relcoder is blank  
#         if ((shelldf[j,35]=="") || (is.na(shelldf[j,35])) || (is.null(shelldf[j,35]))) {
#           shelldf[j,35] <- d[b,35]
#         } 
#     if ((shelldf[j,35]=="") || (is.na(shelldf[j,35])) || (is.null(shelldf[j,35]))) {
#       shelldf[j,35] <- "BLANK: REVIEW NEEDED"
#     }
#     
#     j <- j + 1
#   }
#   return(shelldf)
# }
# 
# test<-multunrels(shelldf,data,case5)

