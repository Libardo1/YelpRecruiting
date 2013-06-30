toBinaryFromParagraph <- function(listToBinary){
    
  #Dictionary Creation
  dictionary <- unique(unlist(listToBinary, use.names = FALSE))

  #Create a matrix with the desired dimensions
  
  processedMatrix <- sapply(listToBinary, lookIntoDictionary <- function(inputString){
    
    for (i in 1:length(listToBinary)){
      text <- rep(0, length(dictionary))
      text[which(inputString[i] == dictionary)] = 1
    }
    return(text)
  })
  processedMatrix <- t(processedMatrix)
  
}