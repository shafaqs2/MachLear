


# One thing that people regularly do is quantify how much of a particular activity they do, 
# but they rarely quantify how well they do it
rm(list=ls())
library(caret)
library(randomForest)
# training<- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
# testing<- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

training<- read.csv("D:/R/8_PREDMACHLEARN/Week 4/pml-training.csv")
testing<- read.csv("D:/R/8_PREDMACHLEARN/Week 4/pml-testing.csv")

# First exclude all variable that does not have much variability
# in them. See notes.
nzv <-nearZeroVar(training)
training<- training[ -nzv]
testing<- testing[ -nzv]
a<- NULL
for(i in 1:dim(training)[2]){
        a[i]<- sum(is.na(training[, i]))
}
a<-which(a==0)
# training<-training[a]
# training<- training[-1]
# testing<-testing[a]
# testing<- testing[-c(1, 59)]
# testing <- rbind(training[2, -59] , testing)
# testing <- testing[-1,]

training<-training[a]
training<- training[-1]
testing<-testing[a]
testing<- testing[-c(1, 59)]

training$cvtd_timestamp<- as.Date(training$cvtd_timestamp)
testing$cvtd_timestamp<- as.Date(testing$cvtd_timestamp)

training_Total <- createDataPartition(y=training$classe,p=0.6, list=FALSE)
training_Train <- training[training_Total,]
training_Test <-  training[-training_Total,]

set.seed(112233)
modFit <- randomForest(classe ~ ., data=training_Train)
prediction <- predict(modFit, training_Test, type = "class")
confusionMatrix(prediction, training_Test$classe)
# cmrf

predict(modFit, testing, type = "class")

# testing$magnet_dumbbell_z<- as.numeric(testing$magnet_dumbbell_z)
# testing$magnet_forearm_y<- as.numeric(testing$magnet_forearm_y)
# testing$magnet_forearm_z<- as.numeric(testing$magnet_forearm_z)


# modFit <- train(classe~ .,data=training_Train, method="rf",ntree=250, do.trace=FALSE, prox=FALSE, trControl = trainControl(allowParallel = TRUE))
# confusionMatrix(training_Test$classe, predict(modFit,training_Test))
# predict(modFit,testing)
# predict(modFit,testing[, -59])
######################################################################################


# Preprocess using preProcess(data, method="knnImpute") = impute/estimate the missing data using k nearest
# neighbors (knn) imputation
# table(is.na(training))
# train<- training[c(1:30, 6000:6030, 9600:9630, 13000:13030, 16000:16030), ]
# test<-testing

# modFit<- gbm(classe~ ., data=train, n.trees=25)
# 
# gbmTrainPredictions = predict(object = modFit, newdata = test, n.trees = 25, type = "response")
# gbm <- train(classe~ ., method="gbm", data=train, ntree=25, verbose=F)

# modFit <- train(classe~ .,data=train, method="rf",ntree=25, do.trace=FALSE, prox=FALSE, trControl = trainControl(allowParallel = TRUE))
# pp<-preProcess(training[,-59],method="pca",thresh=0.9)
# trainPC <- predict(pp,training[,-59])
# testPC <- predict(pp,testing[,-59])





# gbm <- train(classe~ ., method="gbm", data=train, verbose=F)

# modFit<- train(classe~ ., method="bag", data=train)


