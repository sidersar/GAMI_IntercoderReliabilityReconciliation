# GAMI
# Post-reconciliation cleaning 
# August 2020
# arsiders

# read in reconciled data

#csvFileName <- paste("GAMI_Reconciled_Cleaned",Sys.Date(),".csv",sep="")
#write.csv(data4, file=csvFileName)


# ====== REMOVE BOOK CHAPTERS (post-reconciliation)
# if summary includes the word "book" or "chapter" remove from database 

bookchap <- function(data){
  chapters <- NA
  for (i in 1:nrow(data)){
    summary <- unlist(strsplit(data[i,"Summarize"],split=' ', fixed=TRUE))  
    if (("book" %in% summary) || ("chapter" %in% summary)){
      chapters <- c(chapters,data[i,1])   
    }
  }
  chapters<-chapters[-1]
  chapters<- as.vector(unique(chapters, incomparables = FALSE))
  print(c("Number book chapters IDs",length(chapters)))
  chaps <- data[data$Article.ID %in% chapters,]
  data2 <- setdiff(data,chaps)
  return(data2)
}


# ====== Remove entries with "False" for Include or Sufficient 

includesuff <- function(data){
  removeID <- NA
  data[,7] <- as.logical(data[,7])
  data[,4] <- as.logical(data[,4])
  for (i in 1:nrow(data)){
    if ((data[i,7] == FALSE) || (data[i,4] == FALSE)||(is.na(data[i,4]))){
      removeID <- c(removeID, data[i,1])
    }
  }
  removeID <- removeID[-1]
  removeID <- as.vector(unique(removeID, incomparables = FALSE))
  notsuff <- data[data$Article.ID %in% removeID,]
  print(c("Number insufficient IDs",length(removeID)))
  data2 <- setdiff(data,notsuff)
  return(data2)
}



# ====== Fixing the HEALTH/CITIES typo (post-reconciliation)

# Problem: Decide which "Cities settlements & key infrastructure; Health" labels refer to cities and which to health 

healthcities <- function(data){
  library(stringr)
  library(dplyr)
  
  # category A: cities code, no well-being  ---> A&B: search for health or city/infrastructure-related words to decide labels
  # category B: cities code and well-being
  # category C: well-being code, no cities ---> assume articles relate to health; no modifications necessary

  healthwords <- c("health","morbidity", "sanitation", "healthy","disease","mortality")
  citywords <- c("cities","city","urban","metropolitan","towns","Taiwan",
                 "transport","transportation","infrastructure","building","buildings","construction",
                 "protection","water","coastal","defense","built","physical","flood","flooding")
            # used most frequent words from city-labeled Summaries, Response Types, and Implementation Tools  
            # to create the citywords list

  #subset out Cities-coded articles 
  d <- data %>% filter(str_detect(data[,10],"Cities settlements & key infrastructure"))
  data2 <- setdiff(data,d)
  print(c("Cities tagged articles",nrow(d)))
  
  #subset those that include well-being
#  d.well <- d %>% filter(str_detect(d[,10],"well-being & communities")) # category B
#  print(c("Cities and well-being tagged articles",nrow(d.well)))
  #take the difference - those only cities
#  d.cities<-setdiff(d,d.well) # category A
#  print(c("Cities only tagged articles",nrow(d.cities)))
#i=12
  for (i in 1:nrow(d)){
    # identify all sectors
    sectors <- unlist(strsplit(d[i,"1.2.Sector"],split='|||', fixed=TRUE))
    sectors <- trimws(sectors)
    # identify exposure-vulnerabilities
    exposures <- unlist(strsplit(d[i,"3.4.1.Exposure.vulnerability"],split='|||', fixed=TRUE))
    exposures <- as.factor(trimws(exposures))
    # identify cross-cutting topics 
    topics <- unlist(strsplit(d[i,"1.3.Cross.cutting.topics"],split='|||', fixed=TRUE))
    exposures <- as.factor(trimws(topics))
    
    # break out Summary words
    summ <- unlist(strsplit(d[i,"Summarize"],split=' ', fixed=TRUE))  

    # if Exposure includes "Health & well-being" OR if Summary includes health words,
    # add well-being to sectors (doesn't remove well-being if already there)
    if ((any(exposures=="Health & wellbeing")) || (any(healthwords %in% summ))){
      sectors <- c(sectors,"well-being & communities")
      # remove duplicate if introduced
      sectors <- as.vector(unique(sectors, incomparables = FALSE)) 
      d[i,"1.2.Sector"] <- paste(sectors, collapse = "|||")  
    }
    
    # if Exposure includes "Work and economic growth" 
    # OR if Summary includes city or infrastructure words
    # OR if cross-cutting topics mention cities by the sea
    # keep City label: else remove  
    
    if ((any(exposures=="Work and economic growth")) || (any(citywords %in% summ))
        || (any(topics=="Cities and settlements by the sea"))){
      sectors <- c(sectors,"Cities settlements & key infrastructure")
      # remove duplicate if introduced
      sectors <- as.vector(unique(sectors, incomparables = FALSE)) 
      d[i,"1.2.Sector"] <- paste(sectors, collapse = "|||")
    } else {
      city<-("Cities settlements & key infrastructure; Health")
      sectors<-(sectors[-(which(sectors==city))])
      d[i,"1.2.Sector"] <- paste(sectors, collapse = "|||")
    }
  }  
  
  # replace labels in Excel 
  #  d2<-gsub("well-being & communities","Health, well-being & communities",d)
  #  d3<-gsub("Cities settlements & key infrastructure; Health","Cities settlements & key infrastructure",d2)
  
  data2<- rbind(data2,d)
  return(data2)
}


# ===== MOST FREQUENT WORDS IN SUMMARY, RESPONSE, AND IMPLEMENTATION TO DECIDE ON "CITY WORDS"

# library(tm)
# 
# Corpus = Corpus(VectorSource(d$Summarize))
# Corpus
# params = list(stopwords=T, wordLengths=c(3,20))
# dtm = DocumentTermMatrix(Corpus,control=params)
# matrix = as.matrix(dtm)
# dim(matrix)
# freq <- colSums(matrix)
# freq <- sort(freq, decreasing=TRUE)
# 
# library(wordcloud)
# words <- names(freq)
# wordcloud(words[1:200], freq[1:200])
# 
# 
# RCorpus = Corpus(VectorSource(d$`3.1.2.Response-QUOTES`))
# Corpus
# params = list(stopwords=T, wordLengths=c(3,20))
# dtm = DocumentTermMatrix(RCorpus,control=params)
# matrix = as.matrix(dtm)
# dim(matrix)
# freq <- colSums(matrix)
# freq <- sort(freq, decreasing=TRUE)
# words <- names(freq)
# wordcloud(words[1:200], freq[1:200])
# 
# 
# ICorpus = Corpus(VectorSource(d$`3.2.2.ImpTools-QUOTES`))
# ICorpus
# params = list(stopwords=T, wordLengths=c(3,20))
# dtm = DocumentTermMatrix(ICorpus,control=params)
# matrix = as.matrix(dtm)
# dim(matrix)
# freq <- colSums(matrix)
# freq <- sort(freq, decreasing=TRUE)
# words <- names(freq)
# wordcloud(words[1:200], freq[1:200])
