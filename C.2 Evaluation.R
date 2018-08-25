# caret:  createDataPartition, defaultSummary, 
#         train, trainControl, 
#         resamples, resampleHist, resampleSummary

# ISL 5.3 ----
# Lab: Bootstrap

# 5.3.4: The bootstrap
library(ISLR)
library(boot)
data(Portfolio)

# Estimate the accuracy of a statistic of interest (not a linear model) ----
# Example: choosing optimal asset allocation to minimize volatility
# alpha = % of portfolio in asset X
# 1-alpha = % in asset Y

# Function to estimate alpha:
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  alpha <- (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
  return(alpha)
}

# Estimate without bootstrapping
alpha.fn(Portfolio, index=1:100) # index identifies which observations to use. Here, use all 100
# 0.576

# Now estimate using bootstrap.
# Build bootstrap sample of 100 observations, randomly selected with replacement from original data 
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=TRUE))
# 0.596

# boot function lets us do this multiple times
boot(Portfolio, alpha.fn, R=1000) # 1,000 estimates
# estimate = 0.576, SE = 0.0886

# Estimate the accuracy of a linear model ----
# Again, start with function to estimate parameters of interest
boot.fn <- function(data, index) {
  model <- lm(mpg~horsepower, data=data, subset=index)
  return(coef(model))
}

# Run on full data
boot.fn(Auto, 1:392)
# b0 = 39.94, b1 = -0.158
# Run on one bootstrapped sample
boot.fn(Auto, sample(392, 392, replace=T))
# b0 = 39.44, b1 = -0.154

# Run on 1,000 bootstrapped samples
boot(Auto, boot.fn, R=1000)
# b0 = 39.94,     b1 = -0.158
# SE(b0) = 0.869  SE(b1) = 0.007
# Different standard errors than non-bootstrapped model
summary(lm(mpg~horsepower, data=Auto))$coef
# Non-bootstrapped model relies on assumptions
# Bootstrap does not, so these are likely more accurate SEs











# ESL 7: Model Assessment and Selection ----
# No exercises in book, so here are the functions they want us to know
library(caret)

# createDataPartition
# Splits data into train and test.
library(ISLR)
data(Auto)
createDataPartition(Auto[,1], 2)
