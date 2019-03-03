install.packages("MASS")
library(MASS)
data(Boston)
head(Boston)
?Boston
install.packages('caret')
library(caret)
install.packages("klaR")
library(klaR)
########################################################
####Reading the dataset and renaming the columns, part a
########################################################

diabetes<-read.table(file.choose())
names(diabetes) <- c("NA","NA","NA", "observation_number","glucose.area", "insulin.area", "SSPG", "relative.weight","fasting.plasma.glucose", "class_number")
pairs(diabetes[,5:10], col=sapply(diabetes$class_number,switch,"1"="blue","2"="red", "3" = "darkgreen"))

##################################################
####Splitting the dataset into test and train
##################################################
set.seed(12345)
train = sample(1:nrow(diabetes), nrow(diabetes)*.66)
diabetes_train = diabetes[train,]
diabetes_test = diabetes[-train,]
dim(diabetes_train)
dim(diabetes_test)

##################################################
####Performing LDA and QDA
##################################################



lda.train <- lda(class_number ~ glucose.area + insulin.area + SSPG+relative.weight+fasting.plasma.glucose, data = diabetes_train, type = "response")
lda.pred1 <- predict(lda.train, newdata = diabetes_train)

lda.test <- lda(class_number ~ glucose.area + insulin.area + SSPG+relative.weight+fasting.plasma.glucose, data = diabetes_test, type = "response")
lda.pred2 <- predict(lda.test, newdata = diabetes_test,type = "response")

qda.train <- qda(class_number ~ glucose.area + insulin.area + SSPG+relative.weight+fasting.plasma.glucose, data = diabetes_train, type = "response")
qda.pred1 <- predict(qda.train, newdata = diabetes_train, type = "response")

qda.test <- qda(class_number ~ glucose.area + insulin.area + SSPG+relative.weight+fasting.plasma.glucose, data = diabetes_test, type = "response")
qda.pred2 <- predict(qda.test, newdata = diabetes_test, type = "response")

##################################################
####Computing errors by confusion matrix
##################################################

library(e1071)
confldatrain <- confusionMatrix(as.factor(lda.pred1$class), as.factor(diabetes_train$class_number))
confldatest <- confusionMatrix(as.factor(lda.pred2$class), as.factor(diabetes_test$class_number))
confqdatrain <- confusionMatrix(as.factor(qda.pred1$class), as.factor(diabetes_train$class_number))
confqdatest <- confusionMatrix(as.factor(qda.pred2$class), as.factor(diabetes_test$class_number))
confldatrain
confldatest
confqdatrain
confqdatest

##################################################
####Part c
##################################################

glucose.area <- 0.98
insulin.area <- 122
SSPG <- 544
relative.weight <- 186
fasting.plasma.glucose <- 184
temp <- data.frame(glucose.area,insulin.area,SSPG,relative.weight,fasting.plasma.glucose)

fitlda_test <- predict(lda.test, newdata = temp, type = "response")
fitlda_test$class

fitqda_test <- predict(qda.test, newdata = temp, type = "response")
fitlda_test$class
