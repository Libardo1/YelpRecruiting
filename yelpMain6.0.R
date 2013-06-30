#Yelp Challenge 
#Even if I win I won't be taken by them, I cannot go to a better place

#####################################
#Init
rm(list = ls())
ls()

setwd("D:/Wacax/Kaggle Data Analysis/Yelp")

#####################################
#Reading Training Set

library(RJSONIO)
Lines <- readLines("yelp_training_set_business.json")
business <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_user.json")
users <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_checkin.json")
checkin <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_training_set_review.json")
review <- as.data.frame(t(sapply(Lines, fromJSON)))

#########################################
#Read Test Set
Lines <- readLines("yelp_test_set_business.json")
businessTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_user.json")
usersTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_checkin.json")
checkinTest <- as.data.frame(t(sapply(Lines, fromJSON)))
Lines <- readLines("yelp_test_set_review.json")
reviewTest <- as.data.frame(t(sapply(Lines, fromJSON)))
rm(Lines) #Delete element called "Lines"

source('D:/Wacax/Kaggle Data Analysis/Yelp/Normalize.R')

dateReviewed <- substr(as.character(review$date), 1, 10)
dateReviewed <- strptime(dateReviewed, "%Y-%m-%d")
dateReviewed <- Normalize(as.numeric(dateReviewed))

dateReviewedTest <- substr(as.character(reviewTest$date), 1, 10)
dateReviewedTest <- strptime(dateReviewedTest, "%Y-%m-%d")
dateReviewedTest <- Normalize(as.numeric(dateReviewedTest))

#############################################
#Define targets
y <- as.numeric((as.data.frame(review$votes))[2,])

#############################################
#Checkin preprocesing

checkinOrderedTrain <- log(as.numeric(sapply(checkin$checkin_info, sum, USE.NAMES = FALSE)))
checkinOrderedTest <- log(as.numeric(sapply(checkinTest$checkin_info, sum, USE.NAMES = FALSE)))

#############################################
#Preprocess business columns
source('D:/Wacax/Kaggle Data Analysis/Yelp/extractContent.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/toBinary.R')

businessCategories <- extractContent(as.character(c(business$categories, businessTest$categories), use.names=FALSE),
                                     unique(unlist(c(business$categories, businessTest$categories), use.names=FALSE)))

BusinessMatrix <- cbind(businessCategories,
                        toBinary(c(business$city, businessTest$city)), 
                        toBinary(c(business$state, businessTest$state)), 
                        as.numeric(c(business$open, businessTest$open)), 
                        log(as.numeric(c(business$review_count, businessTest$review_count))), 
                        as.numeric(c(business$stars, businessTest$stars)), 
                        Normalize(as.numeric(c(business$latitude, businessTest$latitude))), 
                        Normalize(as.numeric(c(business$longitude, businessTest$longitude))) 
)

#############################################
#Preprocess users columns

meanUsersVotes <- apply(as.data.frame(users$votes), 1, mean)
meanUsersVotes <- matrix(data = rep(meanUsersVotes, nrow(usersTest)), nrow = nrow(usersTest) , ncol = 3, byrow = TRUE)

UsersMatrix <- cbind(rbind(t(as.data.frame(users$votes)), meanUsersVotes),
                                   log(as.numeric(c(users$review_count, usersTest$review_count))), 
                                   as.numeric(c(users$average_stars, usersTest$average_stars))
)

UsersMatrix[ ,1] <- Normalize(UsersMatrix[ ,1])
UsersMatrix[ ,2] <- Normalize(UsersMatrix[ ,2])
UsersMatrix[ ,3] <- Normalize(UsersMatrix[ ,3])

#################################################
#Alternaive Users Preprocessing

OtherUsersMatrix <- cbind(log(as.numeric(c(users$review_count, usersTest$review_count))), 
                     as.numeric(c(users$average_stars, usersTest$average_stars))
)

#The Kmeans is my poor attempt to detect fake users
KmeansUsers <- kmeans(cbind(t(as.data.frame(users$votes)), log(as.numeric(users$review_count)), 
                            as.numeric(users$average_stars)
), 2, iter.max = 100)

OtherUsersMatrix <- cbind(OtherUsersMatrix, c(KmeansUsers$cluster, rep(2, nrow(usersTest))))

########################################################
#Assign values to environments

source('D:/Wacax/Kaggle Data Analysis/Yelp/assignToEnvironment.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/getVectors.R')

#Assign business values environment
business.env <- new.env()
assignToEnvironment(as.character(c(business$business_id, businessTest$business_id)), BusinessMatrix, business.env)

#Assign users values environment
users.env <- new.env()
assignToEnvironment(as.character(c(users$user_id, usersTest$user_id)), UsersMatrix, users.env)

#Assign other users values environment
users2.env <- new.env()
assignToEnvironment(as.character(c(users$user_id, usersTest$user_id)), OtherUsersMatrix, users2.env)

#Assign checkin values to environment
checkin.env <- new.env()
assignToEnvironment(as.character(c(checkin$business_id, checkinTest$business_id)), c(checkinOrderedTrain, checkinOrderedTest), checkin.env)

#Calculate Means
meanBusiness <- apply(BusinessMatrix, 2, mean)
meanUsers <- apply(UsersMatrix, 2, mean)
otherMeanUsers <- apply(OtherUsersMatrix, 2, mean)
meanCheckin <- mean(c(checkinOrderedTrain, checkinOrderedTest))

#Assign means to the respective environments
assign("mean", meanBusiness, envir = business.env)
assign("mean", meanUsers, envir = users.env)
assign("mean", otherMeanUsers, envir = users2.env)
assign("mean", meanCheckin, envir = checkin.env)

#################################################
#Rewiew Text Processing
#Import Libraries 
library(class)
library(rJava)
library(RWeka)
library(Snowball)
library(openNLP)
library(openNLPmodels.en)
library(tm)
source('D:/Wacax/Kaggle Data Analysis/Yelp/createDictionary.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/extractContent.R')
source('D:/Wacax/Kaggle Data Analysis/Yelp/score.sentiment.R')

reviewPlusReviewtest <- c(review$text, reviewTest$text)
names(reviewPlusReviewtest) <- NULL

dictionary <- createDictionary(reviewPlusReviewtest)
reviewsContent <- sapply(reviewPlusReviewtest, extractContent, dictionary)

#Sentiment Analysis
inquirer <- read.csv('inquirerbasic.csv')
negativeIndices <- inquirer[, 4] %in% 'Negativ'
positiveIndices <- inquirer[, 3] %in% 'Positiv'
negativeWords <- tolower(inquirer[negativeIndices, 1])
positiveWords <- tolower(inquirer[positiveIndices, 1])
reviewsSentiment <- sapply(reviewPlusReviewtest, score.sentiment, positiveWords, negativeWords)
reviewsSentiment <- unlist(reviewsSentiment[1,])

#end of preprocessing
##################################################
#Machine Learning
##################################################
#Validation Curves
#Sampling the data
source('D:/Wacax/Kaggle Data Analysis/Yelp/RMSLEEnvironments.R')

alpha <- 0.03
fullDataShuffled <- sample(length(y))
trainIndices <- sample(fullDataShuffled, floor(length(fullDataShuffled)*0.6))
valIndices <- sample(which(!(fullDataShuffled %in% trainIndices)), floor(sum(!(fullDataShuffled %in% trainIndices))*0.5))
testIndices <- which(!(fullDataShuffled %in% trainIndices))[!(which(!(fullDataShuffled %in% trainIndices)) %in% valIndices)]

errorsAlphas <- validationCurveEnvironments(usersIdTrain, businessIdTrain, starsTrain,
                                      dateReviewed, y, alpha, users.env,
                                      business.env, checkin.env,
                                      reviewsContent[1:229907, ], 
                                      reviewsSentiment[1:229907], trainIndices, valIndices)

errorTest <-  RMSLEEnvironments(predictFromEnvironments(usersIdTrain[testIndices], businessIdTrain[testIndices], 
                                                   starsTrain[testIndices], dateReviewed[testIndices],
                                                   model, users.env, business.env, checkin.env, 
                                                   reviewsContent[testIndices, ], reviewsSentiment[testIndices]), y[testIndices])


######################################################
#Stochastic Gradient Descent from Environments
source('D:/Wacax/Kaggle Data Analysis/Yelp/stochasticLinearGradientEnvrionments.R')

thetaLength <- ncol(reviewsContent) +
  length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 4 #4for the extra ones column, sentiment scores, the stars column and the date column

theta <- rep(0, thetaLength)
alpha <- 0.3
lambda <- 30

indicesModel <- sample(1:length(usersIdTrain), length(usersIdTrain))

modelStochastic <- stochasticLinearGradient(usersIdTrain[indicesModel], businessIdTrain[indicesModel], starsTrain[indicesModel], dateReviewed[indicesModel], y[indicesModel], theta, alpha, users.env, business.env, checkin.env, reviewsContent[indicesModel, ], reviewsSentiment[indicesModel], lambda)

####################################################
#Prediction
source('D:/Wacax/Kaggle Data Analysis/Yelp/predictFromEnvironments.R')
prediction <- predictFromEnvironments(usersIdTest, businessIdTest, starsTest, dateReviewedTest, modelStochastic, users.env, business.env, checkin.env, reviewsContent[229908:nrow(reviewsContent), ], reviewsSentiment[229908:length(reviewsSentiment)])

####################################################
#Ensemble of linears second try
source('D:/Wacax/Kaggle Data Analysis/Yelp/stochasticLinearGradientEnvrionments.R')

thetaLength <- ncol(reviewsContent) +
  length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 4 #3for the sentiment scores, the stars column and the date column

theta <- rep(0, thetaLength)
alpha <- 0.3
lambda <- 3

numberOfShuffles <- 30
ensembleStochastics <- matrix(data = rep(0, thetaLength * numberOfShuffles), nrow = numberOfShuffles, ncol = thetaLength)

for(ii in 1:numberOfShuffles){

indicesModel <- sample(1:length(usersIdTrain), length(usersIdTrain))
modelStochastic <- stochasticLinearGradient(usersIdTrain[indicesModel], businessIdTrain[indicesModel], starsTrain[indicesModel], dateReviewed[indicesModel], y[indicesModel], theta, alpha, users.env, business.env, checkin.env, reviewsContent[indicesModel, ], reviewsSentiment[indicesModel], lambda)

ensembleStochastics[ii, ] <- t(modelStochastic)

print(paste(ii, "/" , numberOfShuffles, "Model" ))


}
########################################
#Prediction for the Ensemble

source('D:/Wacax/Kaggle Data Analysis/Yelp/predictFromEnvironments.R')

predictionsBlock <- matrix(data = rep(0, length(usersIdTest) * numberOfShuffles), nrow = length(usersIdTest), ncol = numberOfShuffles)

for (i in 1:nrow(ensembleStochastics)){
predictionsBlock[ ,i] <- predictFromEnvironments(usersIdTest, businessIdTest, starsTest, dateReviewedTest, ensembleStochastics[i, ], users.env, business.env, checkin.env, reviewsContent[229908:nrow(reviewsContent), ], reviewsSentiment[229908:length(reviewsSentiment)])

print(paste(i, "/" , nrow(ensembleStochastics), "Prediction" ))

}

votes <- apply(floor(predictionsBlock), 1, mean)

id <- as.character(reviewTest$review_id)
write.csv(cbind(id, votes), file = "prediction.csv", row.names = FALSE)
##################################################
#Ensemble of linear models

NumEnsembles <- 4

EnsembleLength <- thetaLength <- ncol(reviewsContent) +
  length(get("mean", envir = users.env)) + 
  length(get("mean", envir = business.env)) +
  length(get("mean", envir = checkin.env)) + 3 #3for the sentiment scores, the stars column and the date column

alphaVec <- c(1, 0.3, 0.01, 0.03)
Ensemble <- matrix(data = rep(0, EnsembleLength *  length(alphaVec) * NumEnsembles), 
                   nrow = (length(alphaVec) * NumEnsembles), ncol = EnsembleLength)

for (i in 1:NumEnsembles*length(alphaVec)){
  
  alphas <- rep(alphaVec, NumEnsembles)
  Ensemble[i, ] <- t(stochasticLinearGradient(usersIdTrain, businessIdTrain, starsTrain, dateReviewed, y, theta, alphas[i], users.env, business.env, checkin.env, reviewsContent[1:229907, ], reviewsSentiment[1:229907], lambda))
    
}

########
#extra calculation

predictionsBlock <- matrix(data = rep(0, length(usersIdTrain[testIndices]) * numberOfShuffles), nrow = length(usersIdTrain[testIndices]), ncol = numberOfShuffles)

for (i in 1:nrow(ensembleStochastics)){
  predictionsBlock[ ,i] <- predictFromEnvironments(usersIdTrain[testIndices], businessIdTrain[testIndices], 
  starsTrain[testIndices], dateReviewed[testIndices],
  ensembleStochastics[i, ], users2.env, business.env, checkin.env, 
  reviewsContent[testIndices, ], reviewsSentiment[testIndices])
}

votes1 <- apply(floor(predictionsBlock), 1, mean)
votes1[votes1 < 0] <- 0
error1 <- RMSLEEnvironments(votes1, y[testIndices])

votes2 <- apply(floor(predictionsBlock2), 1, mean)
votes2[votes2 < 0] <- 0
error2 <- RMSLEEnvironments(votes2, y[testIndices])

votes3 <- apply(predictionsBlock, 1, mean)
votes3[votes3 < 0] <- 0
error3 <- RMSLEEnvironments(votes3, y[testIndices])

votes4 <- apply(predictionsBlock2, 1, mean)
votes4[votes4 < 0] <- 0
error4 <- RMSLEEnvironments(votes4, y[testIndices])

votes5 <- apply(round(predictionsBlock), 1, mean)
votes5[votes5 < 0] <- 0
error5 <- RMSLEEnvironments(votes5, y[testIndices])

votes6 <- apply(round(predictionsBlock2), 1, mean)
votes6[votes6 < 0] <- 0
error6 <- RMSLEEnvironments(votes2, y[testIndices])


votes7 <- apply(floor(predictionsBlock), 1, median)
votes7[votes7 < 0] <- 0
error7 <- RMSLEEnvironments(votes7, y[testIndices])

votes8 <- apply(floor(predictionsBlock2), 1, median)
votes8[votes8 < 0] <- 0
error8 <- RMSLEEnvironments(votes8, y[testIndices])

votes9 <- apply(predictionsBlock, 1, median)
votes9[votes9 < 0] <- 0
error9 <- RMSLEEnvironments(votes9, y[testIndices])

votes10 <- apply(predictionsBlock2, 1, median)
votes10[votes10 < 0] <- 0
error10 <- RMSLEEnvironments(votes10, y[testIndices])

votes11 <- apply(round(predictionsBlock), 1, median)
votes11[votes11 < 0] <- 0
error11 <- RMSLEEnvironments(votes11, y[testIndices])

votes12 <- apply(round(predictionsBlock2), 1, median)
votes12[votes12 < 0] <- 0
error12 <- RMSLEEnvironments(votes12, y[testIndices])

votes13 <- floor(apply(floor(predictionsBlock), 1, mean))
votes13[votes13 < 0] <- 0
error13 <- RMSLEEnvironments(votes13, y[testIndices])

votes14 <- floor(apply(floor(predictionsBlock2), 1, mean))
votes14[votes14 < 0] <- 0
error14 <- RMSLEEnvironments(votes14, y[testIndices])

votes15 <- floor(apply(predictionsBlock, 1, mean))
votes15[votes15 < 0] <- 0
error15 <- RMSLEEnvironments(votes15, y[testIndices])

votes16 <- floor(apply(predictionsBlock2, 1, mean))
votes16[votes16 < 0] <- 0
error16 <- RMSLEEnvironments(votes16, y[testIndices])

votes17 <- floor(apply(floor(predictionsBlock), 1, mean))
votes17[votes17 < 0] <- 0
error17 <- RMSLEEnvironments(votes17, y[testIndices])

votes18 <- floor(apply(floor(predictionsBlock2), 1, mean))
votes18[votes18 < 0] <- 0
error18 <- RMSLEEnvironments(votes18, y[testIndices])

votes19 <- round(apply(predictionsBlock, 1, mean))
votes19[votes19 < 0] <- 0
error19 <- RMSLEEnvironments(votes19, y[testIndices])

votes20 <- round(apply(predictionsBlock2, 1, mean))
votes20[votes20 < 0] <- 0
error20 <- RMSLEEnvironments(votes20, y[testIndices])

votes21 <- ceiling(apply(predictionsBlock, 1, mean))
votes21[votes21 < 0] <- 0
error21 <- RMSLEEnvironments(votes21, y[testIndices])

votes21 <- ceiling(apply(predictionsBlock2, 1, mean))
votes21[votes21 < 0] <- 0
error21 <- RMSLEEnvironments(votes21, y[testIndices])
