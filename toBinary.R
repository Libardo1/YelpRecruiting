toBinary <- function(listToBinary){
  #List Length
  m <- length(listToBinary)
  
  #Dictionary Creation
  dictionary <- unique(as.character(listToBinary))
  
  #Create an empty matrix with the desired dimensions
  processedMarix <- sapply(as.character(listToBinary), lookInDictionary <- function(inputString){
    as.numeric(inputString == dictionary)
  })
  processedMarix <- t(processedMarix)
}