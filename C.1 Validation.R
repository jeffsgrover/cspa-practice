# boot::cv.glm

# ISL 5.3
# Lab: Cross-Validation
library(ISLR)
data(Auto)

# 5.3.1 The validation set approach ----
# Split train/test 50/50
set.seed(1)
train <- sample(392, 196)

# Linear model: MPG on horsepower
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
# Calculate test MSE
yhat <- predict(lm.fit, Auto)
mean((yhat-Auto$mpg)[-train]^2) 
# 26.14

# Polynomial model
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((predict(lm.fit2, Auto)-Auto$mpg)[-train]^2) # 19.82
mean((predict(lm.fit3, Auto)-Auto$mpg)[-train]^2) # 19.78
# Quadratic lowers MSE, but cubic doesn't as much.

# Try different subset
set.seed(2)
train2 <- sample(392, 196)
lm.fit.train2 <- lm(mpg~horsepower, data=Auto, subset=train2)
mean((predict(lm.fit.train2, Auto)-Auto$mpg)[-train2]^2) # 23.30
lm.fit2.train2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train2)
lm.fit3.train2 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train2)
mean((predict(lm.fit2.train2, Auto)-Auto$mpg)[-train2]^2) # 18.90
mean((predict(lm.fit3.train2, Auto)-Auto$mpg)[-train2]^2) # 19.26
# Same conclusions.

# 5.3.2 LOO Cross-Validation ----
# Same linear model, but using glm()
library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
names(cv.err) # "call", "K", "delta", "seed"
  # K:      Number of folds. Default is K=n; 392 here.
  # delta:  Test MSE. Identical here: = 24.23. Second is bias-corrected

# Let's run a list of increasingly complex polynomials and see how test MSE changes
cv.error <- rep(0,5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
# Quadratic improves over linear, but no great improvement beyond that.

# 5.3.3 k-fold Cross Validation ----
# Do the same thing as above, but with 10 folds instead of 392
set.seed(17)
cv.error <- rep(0,10)
for(i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error
# Still no improvement over quadratic
