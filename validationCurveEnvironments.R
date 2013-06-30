validationCurveEnvironments <- function(usersKeys, businessKeys, stars, date, y, alpha, users.env, business.env, checkin.env, reviewContent = NULL, reviewSentiment = NULL, trainIndices, valIndices){
  
  stars <- as.numeric(stars)
  date <- as.numeric(date)
  
  source('D:/Wacax/Kaggle Data Analysis/Yelp/predictFromEnvironments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp/stochasticLinearGradientEnvrionments.R')
  source('D:/Wacax/Kaggle Data Analysis/Yelp/RMSLEEnvironments.R')
    
  lambdaVec = c(300, 100, 30, 10, 3, 1, 0, 0,1, 0.3, 0.01, 0.03)
  
  errorTrain <- rep(0, length(lambdaVec))
  errorVal <- rep(0, length(lambdaVec))
  
  for (i in 1:length(lambdaVec)){
    lambda <- lambdaVec[i]
    model <- stochasticLinearGradient(usersKeys[trainIndices], businessKeys[trainIndices], stars[trainIndices], date[trainIndices],
                                      y[trainIndices], theta, alpha, users.env, business.env, checkin.env,
                                      reviewContent[trainIndices, ], reviewSentiment[trainIndices], lambda)

    errorTrain[i] <- RMSLEEnvironments(predictFromEnvironments(usersKeys[trainIndices], businessKeys[trainIndices], 
                                                               stars[trainIndices], date[trainIndices],
                                                               model, users.env, business.env, checkin.env,
                                                               reviewContent[trainIndices, ], reviewSentiment[trainIndices]), y[trainIndices])
   
    errorVal[i] <- RMSLEEnvironments(predictFromEnvironments(usersKeys[valIndices], businessKeys[valIndices], 
                                                             stars[valIndices], date[valIndices],
                                                             model, users.env, business.env, checkin.env, 
                                                             reviewContent[valIndices, ], reviewSentiment[valIndices]), y[valIndices])
        
  }
  return(list(errorTrain, errorVal))  
  
}