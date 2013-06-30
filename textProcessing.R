textProcessing <- function(listofReviews, desiredDimensions = 1000, commentChunk = 1000){
  #Import Libraries 
  library(class)
  library(rJava)
  library(RWeka)
  library(Snowball)
  library(openNLP)
  library(openNLPmodels.en)
  library(tm)
  
  for (i in 1:commentChunk){
    
  textProcessed <- unlist(listofReviews[i], use.names = FALSE)
  #delete non alphanumeric characters
   
  textProcessed <- Corpus(VectorSource(textProcessed))
  textProcessed <- tm_map(textProcessed, tolower)
  textProcessed <- tm_map(textProcessed, removeWords, stopwords("english")) 
  textProcessed <- tm_map(textProcessed, stemDocument) 
  textProcessed <- DocumentTermMatrix(textProcessed)
  #compactMatrix <- DocumentTermMatrix(textProcessed)
  wordsFrecuency <- colSums(inspect(textProcessed))
  wordsFrecuency <- order(wordsFrecuency, decreasing = TRUE)
  wordsFrecuency <- wordsFrecuency[1:desiredDimensions]
  textProcessed <- inspect(textProcessed)[ , wordsFrecuency]
  return(textProcessed)
  #return(textProcessed, compactMatrix)
  }
}