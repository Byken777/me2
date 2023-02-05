library(caret)
library(rpart.plot)
irisdata<-datasets::iris
head(iris)
table(iris$Species)
summary(irisdata)
str(irisdata)
set.seed(3033)
intrain <- createDataPartition(y = irisdata$Species, p= 0.7, list = FALSE)
training <- irisdata[intrain,]
testing <- irisdata[-intrain,]
dim(training);dim(testing)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit_info <- train(Species ~., data = training, method = "rpart", parms = list(split = "information"), trControl=trctrl, tuneLength = 10)
prp(dtree_fit_info$finalModel, box.palette="Reds", tweak=1.2)
test_pred_info<-predict(dtree_fit_info,newdata = testing)
confusionMatrix(test_pred_info,testing$Species)
set.seed(3333)
dtree_fit_gini <- train(Species ~., data = training, method = "rpart", parms = list(split = "gini"), trControl=trctrl, tuneLength = 10)
prp(dtree_fit_gini$finalModel,box.palette = "Blues", tweak = 1.2)

test_pred_gini<-predict(dtree_fit_gini,newdata = testing)
confusionMatrix(test_pred_gini,testing$Species)

