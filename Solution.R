
# This script trains a Random Forest model based on the data,
# saves a sample submission
# Author : Pintu Prasad Gupta
# Email Id : pintugcoder@gmail.com
# Date oF Submisstin : 28-08-2016

# library for randomForest tool to include.
library(randomForest)

set.seed(1)
# store all data in train
train <- read.csv("titanic_train.csv", stringsAsFactors=FALSE)
# store all data in test
test  <- read.csv("titanic_test.csv",  stringsAsFactors=FALSE)

# function to extract Features 
extractFeatures <- function(data) {
  features <- c(
				"Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- mean(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}
# apply randome Forest function to get reference data using Survived variable
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=10000, importance=TRUE)
#make a submission frame 
submission <- data.frame(PassengerId = test$PassengerId)
#predict result using reference data for test data
submission$Survived <- predict(rf, extractFeatures(test))
# write the predict result to titanic_sample_submission file.
write.csv(submission, file = "titanic_sample_submission.csv",row.names=FALSE)