#Data Exploration and Data Cleaning
str(completesurvey)
summary(completesurvey)
sum(is.na(completesurvey))

#Convert categorical variables into factor
completesurvey$car <- as.factor(completesurvey$car)
completesurvey$brand <- as.factor(completesurvey$brand)
completesurvey$zipcode <- as.factor(completesurvey$zipcode)
completesurvey$elevel <- as.ordered(completesurvey$elevel)

#Create a Decision Tree C5.0 model

#load library and set seed
library(caret) 

#create a 20% sample of the data
set.seed(998) 
completesurveysample<- completesurvey[sample(1:nrow(completesurvey), 2000,replace=FALSE),] 

#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(completesurveysample$brand, p = .75, list = FALSE)
training <- completesurveysample[inTraining,]
testing <- completesurveysample[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train C 5.0 with a tuneLenght = 1 
C5.0 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1)

#training results
C5.0

#Evaluate the importance of each varaible
importanceC5.0 <- varImp(C5.0,scale=FALSE) #estimate variable importance of the model
print(importanceC5.0) 

#plot importance
plot(importanceC5.0) 

#Random forest with Manual Grid
#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
completesurveysample <- completesurvey[sample(1:nrow(completesurvey), 2000,replace=FALSE),]

#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(completesurveysample$brand, p = .75, list = FALSE)
training <- completesurveysample[inTraining,]
testing <- completesurveysample[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#train Random Forest Regression model
system.time(rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))

#training results
rfFitm1

#estimate variable importance of the model
importancefFitm1 <- varImp(rfFitm1,scale=FALSE)

#summarize importance
print(importancefFitm1) 
plot(importancefFitm1)

#Creates predictions with the best performing model
predictions <- predict(C5.0, incompletesurvey)
summary(predictions)

#Add column to dataframe
#Creates a column with the value of the
predictionscolumn <- matrix(predictions) predictions
#Creates the column with NAs

#Adds the values from predictions to the dataframe
incompletesurvey["brandpredictions"] <- NA incompletesurvey$brandpredictions <- predictions 

#Creates an excel file
write.csv(incompletesurveypredictions, file = "incompletesurveypredictions.csv")


     
