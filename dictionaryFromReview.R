dictionaryFromReview <- function(listOfStrings){
  #Import Libraries 
  library(class)
  library(rJava)
  library(RWeka)
  library(Snowball)
  library(openNLP)
  library(openNLPmodels.en)
  library(tm)
  
    newEntry <- unlist(listOfStrings, use.names = FALSE)
    #lower case
    newEntry <- tolower(newEntry)
    #delete non alphanumeric characters
    newEntry <- gsub("[^[:alnum:] ]"," ",newEntry)

    #Tokenize Strings
    newEntry <- tokenize(newEntry, language = "en")
    newEntry <- removeWords(newEntry, stopwords(kind = "en"))
        
    dictionary <- unique(newEntry)

}