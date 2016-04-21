library(MASS)
library(caret)
library(randomForest)
library(ROCR)
library(miscTools)

#load Boston Housing data 
#this will be regression trees since the target variable is continuous (median value)
data(Boston)
head(Boston)

dim(Boston)

#Use createDataPartition from the caret package to create train and test sets
set.seed(123)
split <- createDataPartition(y=Boston$medv, p = 0.7, list=FALSE)
train <- Boston[split,]
test<- Boston[-split,]

#A note about 'createDataPartition; createResample can be used to make a simple bootstrap and createFolds for 
#cross-validation groupings.

#OK, training and test sets have been created. Now we will run randomForest on the training data.

#The randomForest algorithm below includes target variable (medv) '.' which is all the predictors,
#the data is BostonTrain, number of trees to create using 'bagging' method is 100 and what makes the 
#random forest algorithm set apart from boosting; number of predictors randomly chosen from full set of
#predictors is 5. In regression trees, the recommended number for mtry is the total number of predictors
#divided by three. In classification (an example to follow) the recommended number is the square root of
#predictors. 

rf <- randomForest(medv~., data=train, mtry=6, importance = TRUE)
yhat <- predict(rf, test)

#The MSE:
mean((yhat - test$medv)^2)

#Look at variable importance; looking at increase in MSE by looking at
#mean decrease in accuracy in predictions on out
#of bag samples, when the given variable is excluded from the model and
#increase in node purity by looking at the total decrease in node purity 
#resulting from the given variable, averaged over all trees.
importance(rf)

#Now plot it:
varImpPlot(rf)

##adaBoost
data(iris)
head(iris)

library(adabag)
set.seed(123)
split <- createDataPartition(y=iris$Species, p = 0.7, list=FALSE)
train <- iris[split,]
test<- iris[-split,]
train$Species <- factor(train$Species)
adaboost<-boosting(Species ~ . , data=train, boos=TRUE, mfinal=20, coeflearn='Breiman')
summary(adaboost)
# Above we use the Adaboost algorithm. M-final is the number of times the boosting algorithm is run. Breiman
# as the 'coeflearn' suggests that we are using the M1 algorithm proposed by Breiman. The M1 algorithm
# is a classification algorithm where each class can attain a weight of no more than 1/2.
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,test)
predictions <- predict(adaboost,test)
predictions$class
predictions$confusion
predictions$error
#Let's compare AdaBoost to RandomForest. To do this, I will quickly run randomforest on the iris dataset.
rf_iris <- randomForest(Species ~ ., data = train)
rf_iris
yhat_iris <- predict(rf_iris, test)
#Random forest Confusion Matrix:
table(yhat_iris, test$Species)
#AdaBoost Confusion Matrix:
predictions$confusion
```



  