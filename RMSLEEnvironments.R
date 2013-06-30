RMSLEEnvironments <- function(predictions, y){
  
  m <- length(y)
  RMSLEcost = sqrt(sum((log(predictions + 1) - log(y + 1))^2) / m)
  
}
