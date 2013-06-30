extractContent <- function(inputText, dictionary){
  
require(tm)

dictionary <- Corpus(VectorSource(dictionary))  
dictionary <- tm_map(dictionary, tolower)
stopwords <- stopwords("english")
dictionary <- tm_map(dictionary, removeWords, stopwords)
dictionary <- tm_map(dictionary, removePunctuation)
dictionary <- tm_map(dictionary, removeNumbers)  
dictionary <- as.character(inspect(dictionary))
dictionary <- Dictionary(dictionary)  

inputText <- Corpus(VectorSource(inputText))
inputText <- tm_map(inputText, tolower)
stopwords <- stopwords("english")
inputText <- tm_map(inputText, removeWords, stopwords)
inputText <- tm_map(inputText, removePunctuation)
inputText <- tm_map(inputText, removeNumbers)
lematrix <- inspect(DocumentTermMatrix(inputText, list(dictionary = dictionary)))

}
