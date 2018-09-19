# randomForest: randomForest, predict, plot, summary, importance
# gbm: gbm, summary, predict
# tree: tree, plot, predict

# Bagged tree: Boston data
library(MASS)
library(ISLR)
library(tree)
library(gbm)
library(randomForest)

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
# Setting mtry=13 uses all predictors and thus makes this bagging

# Test error
boston.test <- Boston[-train,]
boston.pred <- predict(bag.boston, newdata=boston.test)
mean((boston.test$medv-boston.pred)^2)
# MSE = 13.4 Almost half that of a single pruned tree


# Random forest
# Just remove mtry argument.
rf.boston <- randomForest(medv~., data=Boston, subset=train, improtance=TRUE)

rf.boston.pred <- predict(rf.boston, boston.test)
mean((rf.boston.pred-boston.test$medv)^2)
# 11.64

# View each X's importance
importance(rf.boston)
varImpPlot(rf.boston)


# Boosting
# gbm(..., distribution = "gaussian") for regression
# gbm(..., distribution = "bernoulli") for classification
set.seed(1)
boost.boston <- gbm(medv~., 
                    data=Boston[train,], 
                    distribution="gaussian",
                    n.trees=5000,
                    interaction.depth=4)
summary(boost.boston)
plot(boost.boston, i="lstat")

boost.boston.pred <- predict(boost.boston, boston.test, n.trees=5000)
mean((boost.boston.pred-boston.test$medv)^2)

# Play with shrinkage parameter
boost.boston2 <- gbm(medv~.,
                     data=Boston[train,],
                     distribution="gaussian",
                     n.trees=5000,
                     interaction.depth=4,
                     shrinkage=0.2)
boost.boston.pred2 <- predict(boost.boston2, boston.test, n.trees=5000)
mean((boost.boston.pred2-boston.test$medv)^2)