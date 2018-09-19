# tree: whole package, including prune.tree, plot.tree, predict.tree

# Classification tree: carseat sales data
library(ISLR)
library(tree)

# Make binary
Carseats$High <- as.factor(ifelse(Carseats$Sales<=8, "No", "Yes"))

# Build tree
tree.carseats <- tree(High ~ . - Sales, data=Carseats)
summary(tree.carseats)
# Returns training error rate
plot(tree.carseats)
text(tree.carseats, pretty=0)

# Train/test
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]

tree.carseats <- tree(High~.-Sales, Carseats, subset=train)

tree.pred <- predict(tree.carseats, newdata=Carseats.test, type="class")
table(tree.pred, Carseats.test$High)
(86+57)/200
# 71.5%

# Cross validation
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
# 9 terminal nodes minimizes misclassification error rate.

# Plot error rate as a function of size and k (tuning parameter, alpha)
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# Make new tree using the ideal size, 9
prune.carseats <- prune.misclass(tree.carseats, best=9)

# What is the new test error rate?
tree.pred <- predict(prune.carseats, newdata=Carseats.test, type="class")
table(tree.pred, Carseats.test$High)
(94+60)/200
# 77%



# Regression tree: Boston data
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)

# Test error (MSE)
boston.pred <- predict(tree.boston, Boston[-train,])
boston.test <- Boston[-train, "medv"]
mean((boston.test - boston.pred)^2)
# 25.05
# sqrt(MSE) is 5, so predictions are within $5k of the true median home value

# Prune the tree
cv.boston <- cv.tree(tree.boston)
# No pruning is best