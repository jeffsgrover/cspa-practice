# default: smooth.spline, loess
# gam: gam

# Wage data
library(ISLR)

# Polynomial
fit <- lm(wage ~ poly(age, 4, raw=TRUE), data=Wage)
age.grid <- seq(min(Wage$age), max(Wage$age))
preds <- predict(fit, newdata=list(age=age.grid))
# Select degree of polynomial
# Fit nested models
fit1 <- lm(wage ~ age, data=Wage)
fit2 <- lm(wage ~ age + I(age^2), data=Wage)
fit3 <- lm(wage ~ age + I(age^2) + I(age^3), data=Wage)
fit4 <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
fit5 <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data=Wage)
anova(fit1, fit2, fit3, fit4, fit5)
# 3rd and 4th degree are better than quadratic or linear, 
# but 5th degree is unnecessary

# Step function
fit.step <- lm(wage~cut(age, 4), data=Wage)

# Splines
library(splines)
# Use bs() function to create basis functions
# Creates cubic splines by default
fit.spline <- lm(wage ~ bs(age, knots=c(25,40,60)), data=Wage)
pred.spline <- predict(fit.spline, newdata=list(age=age.grid))

# Natural splines
fit.ns <- lm(wage ~ ns(age, 4), data=Wage)
preds.ns <- predict(fit.ns, newdata=list(age=age.grid))
preds.ns <- data.frame(age.grid, preds.ns)
ggplot(aes(x=age, y=wage), data=Wage) + 
  geom_point() +
  geom_line(data=preds.ns, aes(x=age.grid, y=preds.ns), col="red")

# Smoothing splines
fit.smooth.df <- smooth.spline(Wage$age, Wage$wage, df=16)
fit.smooth.cv <- smooth.spline(Wage$age, Wage$wage, cv=TRUE)

# Local regression
fit.local <- loess(wage ~ age, span=0.2, data=Wage)

# GAMs
# Natural splines
fit.gam1 <- lm(wage ~ ns(age, 4) + ns(year, 4) + education, data=Wage)
# Smoothing splines
fit.gam2 <- gam(wage ~ s(age, 4) + s(year, 4) + education, data=Wage)

