stochasticLinearGradient <- function(usersKeys, businessKeys, stars, date, y, theta, alpha, users.env, business.env, checkin.env, reviewContent = NULL, reviewSentiment = NULL, lambda){
  
  m <- length(y)
  
  stars <- as.numeric(stars)
  date <- as.numeric(date)
  
  
  for (iter in 1:m){  
    
    X1 <- try(get(as.character(usersKeys[iter]), envir = users.env), silent = TRUE)
    if (class(X1) == "try-error"){X1 <- get("mean", envir = users.env)}
    X2 <- try(get(as.character(businessKeys[iter]), envir = business.env), silent = TRUE)
    if (class(X2) == "try-error"){X2 <- get("mean", envir = business.env)}
    X3 <- try(get(as.character(businessKeys[iter]), envir = checkin.env), silent = TRUE)
    if (class(X3) == "try-error"){X3 <- 0 } #if the key is not found in the environment it is assumend that there is zero check-ins
    
    X <- c(1, X1, X2, X3, stars[iter], date[iter], reviewContent[iter, ], reviewSentiment[iter])    
    
    theta[1] <- theta[1] - ((alpha / m)  %*% (X %*% theta - y[iter]))
    theta[2:length(theta)] <- theta[2:length(theta)]  - ((alpha / m) * (X[2:length(theta)] %*% (X %*% theta - y[iter])) + (lambda/m* theta[2:length(theta)]))
    
    print(paste(iter, "/" , m, "Iteration" ))
    
    
  }
  
  return(theta)
}