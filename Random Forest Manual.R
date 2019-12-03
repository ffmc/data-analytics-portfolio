#caret model - Manual Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
completesurveysample <- completesurvey[sample(1:nrow(completesurvey), 2000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(completesurveysample$brand, p = .75, list = FALSE)
training <- completesurveysample[inTraining,]
testing <- completesurveysample[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))


#training results
rfFitm1


