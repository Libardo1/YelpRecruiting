Normalize <- function(vectorToNormalize){
  
  mu <- mean(vectorToNormalize)
  sigma <- sd(vectorToNormalize)
  
  sapply(vectorToNormalize, anonfun <- function(anonnumber){
    return((anonnumber - mu) / sigma)
  })
}