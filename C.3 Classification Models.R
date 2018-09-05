# defaults: glm
# class: kNN
# MASS: lda, qda
# logistf: logistf, summary.logistf

# Stock market data
library(ISLR)
library(tidyverse)

# Logistic
# Predict direction using lags and volume
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
               data=Smarket, family=binomial)
# Get predicted probability of going up
glm.probs <- predict(glm.fit, type="response")
# Turn these into "Up" or "Down" predictions
glm.pred <- rep("Down", length(Smarket$Direction))
glm.pred[glm.probs>0.5] <- "Up"
# Create confusion matrix
table(glm.pred, Smarket$Direction)
# Training error rate
1-((145+507)/1250)
# 47.8%

# Train/test split: test set is year 2005
train <- Smarket %>% filter(Year<2005)
test <- Smarket %>% filter(Year==2005)
glm.fit2 <- update(glm.fit, data=train)
glm.probs <- predict(glm.fit2, test, type="response")
glm.pred <- rep("Down", length(test$Direction))
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, test$Direction)
# Test error rate
1-((77+44)/length(test$Direction))
# Mean function calculates accuracy; 1-mean calculates error rate
1-mean(glm.pred==test$Direction)
# 52.0%

# LDA
library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=train)
lda.pred <- predict(lda.fit, test)
# Three components: class (predictions), 
#                   posterior (posterior probs)
#                   x (linear discriminants)
table(lda.pred$class, test$Direction)
mean(lda.pred$class==test$Direction)
# 44.0% test error rate
# Change cutoff threshold
sum(lda.pred$posterior[,1]>.9)

# QDA
qda.fit <- qda(Direction~Lag1+Lag2, data=train)
qda.pred <- predict(qda.fit, test)
table(qda.pred$class, test$Direction)
mean(qda.pred$class==test$Direction)
# 60.0% accuracy; 40.0% test error rate!

# KNN
library(class)
knntrain.x <- cbind(train$Lag1, train$Lag2)
knntest.x <- cbind(test$Lag1, test$Lag2)
knntrain.y <- train$Direction
set.seed(1)
# Try k=1
knn.pred1 <- knn(knntrain.x, knntest.x, knntrain.y, 1)
table(knn.pred1, test$Direction)
mean(knn.pred1==test$Direction)
# Only 50% accurate

# Try k=3
knn.pred3 <- knn(knntrain.x, knntest.x, knntrain.y, 3)
mean(knn.pred3==test$Direction)
# Reduces test error to 46.4%

# QDA has the lowest test error rate
