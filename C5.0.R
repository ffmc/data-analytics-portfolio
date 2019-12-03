#caret model - Automatic Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = completesurvey
#Y Value = Brand

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
completesurveysample<- completesurvey[sample(1:nrow(completesurvey), 2000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(completesurveysample$brand, p = .75, list = FALSE)
training <- completesurveysample[inTraining,]
testing <- completesurveysample[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train C 5.0 with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
C5.0 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1)

#training results
C5.0

git config --global user.name 'Francisco Cardoso'

gitconfig --global user.email "ffmcardoso@gmail.com"
