createDictionary <- function(inputText){

  library(tm)
  
  if (length(inputText) <50000){
    indeces <- sample(length(inputText), length(inputText))
  }else{
    indeces <- sample(length(inputText), 50000)
  }
inputText <- Corpus(VectorSource(inputText[indeces]))

inputText <- tm_map(inputText, tolower)
stopwords <- stopwords("english")
inputText <- tm_map(inputText, removeWords, stopwords)
inputText <- tm_map(inputText, removePunctuation)
inputText <- tm_map(inputText, removeNumbers)
lematrix <- TermDocumentMatrix(inputText)

dictio1 <- findFreqTerms(lematrix, lowfreq=800)
#dictio2 <- removeSparseTerms(lematrix, 0.95)
#dictio2 <- rownames(as.data.frame(inspect(dictio2)))
#dictio2 <- "arf"

return(dictio1)  
#return(list(dictio1, dictio2))
}