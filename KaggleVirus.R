#load in data
Virus <- read.csv("train.csv")

#change species to not have levels
Virus$Species <- as.character(Virus$Species)

#change WnvPresent to a factor so that it's a classification model
Virus$WnvPresent <- as.factor(Virus$WnvPresent)

#loop to give numerical values to the species
for (n in 1:10506){
  if(grepl("PIPIENS/RESTUANS", Virus$Species[n])){
    Virus$Species[n] <- 1
  }else if(grepl("RESTUANS", Virus$Species[n])){
    Virus$Species[n] <- 2
  }else if(grepl("PIPIENS", Virus$Species[n])){
    Virus$Species[n] <- 3
  }else if(grepl("SALINARIUS", Virus$Species[n])){
    Virus$Species[n] <- 4
  }else if(grepl("TERRITANS", Virus$Species[n])){
    Virus$Species[n] <- 5
  }else if(grepl("ERRATICUS", Virus$Species[n])){
    Virus$Species[n] <- 6
  }else if(grepl("TARSALIS", Virus$Species[n])){
    Virus$Species[n] <- 7
  }else{
  }
}

#get rid of variables that won't be used for testing
Virus <- Virus[c(3:4,8:10,12)]

#check that Virus data is in correct format
print(str(Virus))
#'data.frame':  10506 obs. of  6 variables:
#$ Species        : chr  "1" "2" "2" "1" ...
#$ Block          : int  41 41 62 79 79 15 25 11 11 11 ...
#$ Latitude       : num  42 42 42 42 42 ...
#$ Longitude      : num  -87.8 -87.8 -87.8 -87.8 -87.8 ...
#$ AddressAccuracy: int  9 9 9 8 8 8 8 8 8 8 ...
#$ WnvPresent     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...

#use data splitting
install.packages("caret")
library(caret)

DataSplit1 <- createDataPartition(y=Virus$WnvPresent, p=0.75, list=FALSE)

train1 <- Virus[DataSplit1,]
test1 <- Virus[-DataSplit1]

Virus1 <- train(WnvPresent~., data = Virus, method = "glm")

#examine results
summary(Virus1)

#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     -1.628e+01  2.287e+00  -7.116 1.18e-12 ***
#  Species2        -3.850e-02  5.300e-03  -7.264 4.02e-13 ***
#  Species3         3.731e-02  5.395e-03   6.917 4.88e-12 ***
#  Species4        -5.341e-02  2.403e-02  -2.223  0.02625 *  
#  Species5        -4.760e-02  1.520e-02  -3.132  0.00174 ** 
#  Species6        -8.529e-02  2.208e-01  -0.386  0.69935    
#  Species7        -4.749e-02  9.019e-02  -0.527  0.59849    
#  Block            6.924e-05  9.085e-05   0.762  0.44599    
#  Latitude        -2.281e-02  2.742e-02  -0.832  0.40552    
#  Longitude       -1.972e-01  3.220e-02  -6.125 9.37e-10 ***
#  AddressAccuracy -1.590e-03  1.742e-03  -0.913  0.36140

#the different species seem to be the only non-reliable factors due to the high
#p-values of species 6 through 7, we will test this anyways and remove the species 
#category for the next trial, we can also try getting rid of block and latitude
#depending on how removing species affects the accuracy

#load data to be tested
test <- read.csv("test.csv")

#put test data in same format to be tested

table(test$Species)
table(Virus$Species)
#1 is the most common for both categories, so treat unknown as species 1

test$Species <- as.character(test$Species)
for (n in 1:116293){
  if(grepl("PIPIENS/RESTUANS", test$Species[n])){
    test$Species[n] <- 1
  }else if(grepl("RESTUANS", test$Species[n])){
    test$Species[n] <- 2
  }else if(grepl("PIPIENS", test$Species[n])){
    test$Species[n] <- 3
  }else if(grepl("SALINARIUS", test$Species[n])){
    test$Species[n] <- 4
  }else if(grepl("TERRITANS", test$Species[n])){
    test$Species[n] <- 5
  }else if(grepl("ERRATICUS", test$Species[n])){
    test$Species[n] <- 6
  }else if(grepl("TARSALIS", test$Species[n])){
    test$Species[n] <- 7
  }else{
    test$Species[n] <- 1
  }
}

#predict
Virus_Predictions1 <- predict(Virus1, newdata = test)

#put in correct format
Virus_1 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions1)
write.csv(Virus_1, "Virus_Predict1.csv", row.names = FALSE)

#SCORE: 0.65519

#
#
#
#
#
#
#
#
#
#

#install necessary packages
install.packages("caret")
library(caret)

#use a gbm model
set.seed(123)
Virus2_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)
gbmGrid2 <-  expand.grid(interaction.depth = c(1, 2), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05), n.minobsinnode=1)
gbm_fit2 <- train(WnvPresent~., data = Virus, method = "gbm", trControl = Virus2_control, verbose = FALSE, tuneGrid = gbmGrid2)

#predict
Virus_Predictions2 <- predict(gbm_fit2, newdata = test)

#put in correct format
Virus_2 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions2)
write.csv(Virus_2, "Virus_Predict2.csv", row.names = FALSE)

#SCORE: 0.65916

#
#
#
#
#
#
#
#
#
#

#Try getting rid of AddressAccuracy to see what happens, since it has a high p-value,
#Depending on the p-values of the other factors, we may get rid of more.
VirusCut <- Virus[c(1:4, 6)]

install.packages("gbm")
library(gbm)
VirusCut$Species = as.factor(VirusCut$Species)
gbm3 <- gbm(WnvPresent~., data=VirusCut, distribution="bernoulli", n.trees=1000, shrinkage=0.05, interaction.depth=2)#, n.minobsinnode=10, cv.folds=5)
#gbm did not work, gave NAs

#use data splitting to examine p-values
library(caret)
DataSplit3 <- createDataPartition(y=VirusCut$WnvPresent, p=0.75, list=FALSE)
train3 <- VirusCut[DataSplit3,]
test3 <- VirusCut[-DataSplit3]

Virus3 <- train(WnvPresent~., data = VirusCut, method = "glm")

#examine results
summary(Virus3)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#  (Intercept) -3.106e+02  4.230e+01  -7.342 2.11e-13 ***
#  Species2    -1.193e+00  1.578e-01  -7.561 4.01e-14 ***
#  Species3     5.952e-01  9.425e-02   6.315 2.71e-10 ***
#  Species4    -1.469e+01  4.227e+02  -0.035    0.972    
#  Species5    -1.457e+01  2.637e+02  -0.055    0.956    
#  Species6    -1.527e+01  3.956e+03  -0.004    0.997    
#  Species7    -1.455e+01  1.606e+03  -0.009    0.993    
#  Block        1.297e-03  1.798e-03   0.721    0.471    
#  Latitude    -6.385e-01  5.625e-01  -1.135    0.256    
#  Longitude   -3.812e+00  6.350e-01  -6.003 1.93e-09 ***

#It seems as though Latitude and Block still have a high p-value. Although some of
#the species also have a high p-value, we'll keep them since not all of them do.
VirusCut <- VirusCut[c(1,4:5)]
VirusCut$WnvPresent <- as.factor(VirusCut$WnvPresent)

#Use same method for analyzing
DataSplit3 <- createDataPartition(y=VirusCut$WnvPresent, p=0.75, list=FALSE)
train3 <- VirusCut[DataSplit3,]
test3 <- VirusCut[-DataSplit3]

Virus3 <- train(WnvPresent~., data = VirusCut, method = "glm")

#predict
Virus_Predictions3 <- predict(Virus3, newdata = test)

#put in correct format
Virus_3 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions3)
write.csv(Virus_3, "Virus_Predict3.csv", row.names = FALSE)
#would only give 0s, not probabilities

#try glm model
Virus3 = glm(WnvPresent~., data=VirusCut, family = "binomial")
#examine results
summary(Virus3)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#  (Intercept) -293.04286   39.02440  -7.509 5.95e-14 ***
#  Species2      -1.19501    0.15781  -7.572 3.67e-14 ***
#  Species3       0.59978    0.09392   6.386 1.70e-10 ***
#  Species4     -14.68740  422.66742  -0.035    0.972    
#  Species5     -14.55455  263.91349  -0.055    0.956    
#  Species6     -15.29252 3956.18033  -0.004    0.997    
#  Species7     -14.56321 1606.01617  -0.009    0.993    
#  Longitude     -3.30831    0.44481  -7.438 1.03e-13 ***
#only some species have a significant p value, so we'll try and see what happens
#when we get rid of species

#predict
Virus_Predictions3 <- predict(Virus3, test, type = "response")

#put in correct format
Virus_3 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions3)
write.csv(Virus_3, "Virus_Predict3.csv", row.names = FALSE)

#SCORE: 0.64128

#
#
#
#
#
#
#
#
#
#

#try this using caret gbm model

#use a gbm model
set.seed(123)
Virus4_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)
gbmGrid4 <-  expand.grid(interaction.depth = c(1, 2), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05), n.minobsinnode=1)
gbm_fit4 <- train(WnvPresent~., data = VirusCut, method = "gbm", trControl = Virus4_control, verbose = FALSE, tuneGrid = gbmGrid4)

#predict
Virus_Predictions4 <- predict(gbm_fit4, newdata = test)

#put in correct format
Virus_4 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions4)
write.csv(Virus_4, "Virus_Predict4.csv", row.names = FALSE)
#outputs all zeros, won't give probabilities like before

#
#
#
#
#
#
#
#
#
#

#try glm model without species
VirusCut2 <- Virus[c(2:6)]
Virus4 = glm(WnvPresent~., data=VirusCut2, family = "binomial")
summary(Virus4)
#Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -2.926e+02  4.332e+01  -6.755 1.43e-11 ***
#  Block            1.126e-03  1.812e-03   0.621   0.5346    
#  Latitude        -8.547e-01  5.623e-01  -1.520   0.1285    
#  Longitude       -3.716e+00  6.275e-01  -5.921 3.20e-09 ***
#  AddressAccuracy -5.989e-02  3.445e-02  -1.738   0.0821 

#as before, Longitude continues having a low p value regardless of the other
#variables being tested

#predict
Virus_Predictions4 <- predict(Virus4, test, type = "response")

#put in correct format
Virus_4 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions4)
write.csv(Virus_4, "Virus_Predict4.csv", row.names = FALSE)

#SCORE: 0.64422

#
#
#
#
#
#
#
#
#
#

#try testing it on a gbm model
gbm5 <- gbm(WnvPresent~., data=VirusCut2, distribution="bernoulli", n.trees=1000, shrinkage=0.05, interaction.depth=2, cv.folds=3)
best.iter <- gbm.perf(gbm5,method="cv")
Virus_Predictions5 <- predict(gbm5, test, best.iter)
#if cv.fold was written, it would crash the system, but when I took it out, I couldn't
#run the second line
#when I ignored the cv.folds, the predictions would all be NAs

#
#
#
#
#
#
#
#
#
#

#try with a caret gbm model
library(caret)
set.seed(123)
Virus6_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)
gbmGrid6 <-  expand.grid(interaction.depth = c(1, 2), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05), n.minobsinnode=1)
gbm_fit6 <- train(WnvPresent~., data = Virus, method = "gbm", trControl = Virus6_control, verbose = FALSE, tuneGrid = gbmGrid6)

#predict
Virus_Predictions6 <- predict(gbm_fit6, newdata = test)
#again prints out all zeros

#
#
#
#
#
#
#
#
#
#

#try glm model with only Longitude and AddressAccuracy
VirusCut3 <- VirusCut2[c(3:5)]
Virus7 = glm(WnvPresent~., data=VirusCut3, family = "binomial")
summary(Virus7)
#Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -272.23718   41.27158  -6.596 4.22e-11 ***
#  Longitude         -3.07667    0.47166  -6.523 6.89e-11 ***
#  AddressAccuracy   -0.06569    0.03308  -1.985   0.0471 * 

#significant p values for everything

#predict
Virus_Predictions7 <- predict(Virus7, test, type = "response")

#put in correct format
Virus_7 <- data.frame("Id" = test$Id, "WnvPresent" = Virus_Predictions7)
write.csv(Virus_7, "Virus_Predict7.csv", row.names = FALSE)

#SCORE: 0.65809

#
#
#
#
#
#
#
#
#
#

VirusCut4 <- Virus[c(4:6)]
#use a gbm model
set.seed(123)
Virus8_control <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = TRUE)
gbmGrid8 <-  expand.grid(interaction.depth = c(1, 2), n.trees = seq(200,10000, by=200), shrinkage = c(0.1,.05), n.minobsinnode=1)
gbm_fit8 <- train(WnvPresent~., data = VirusCut, method = "gbm", trControl = Virus8_control, verbose = FALSE, tuneGrid = gbmGrid8)

#predict
Virus_Predictions8 <- predict(gbm_fit8, newdata = test)
#outputs all zeros
#this had a pretty high score for the glm model, I think if this would have ran with
#the caret gbm like the first two did it would have been the best score
