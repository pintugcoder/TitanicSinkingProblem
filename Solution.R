
# This script trains a Random Forest model based on the data,
# saves a sample submission
# Author : Pintu Prasad Gupta
# Email Id : pintugcoder@gmail.com
# Date oF Submisstin : 04-08-2016


library(randomForest)

set.seed(1)
train <- read.csv("titanic_train.csv", stringsAsFactors=FALSE)
test  <- read.csv("titanic_test.csv",  stringsAsFactors=FALSE)

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

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=10000, importance=TRUE)

submission <- data.frame(PassengerId = test$PassengerId)

submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "titanic_sample_submission.csv",row.names=FALSE)