# All functions mentioned in the vignette for the lme4 package

library(lme4)
data(sleepstudy)

# Fit a model: reaction as a function of days of sleep deprivation, by subject
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)

# Diagnostic plots
## Fitted vs residuals
plot(fm1, type=c("p", "smooth"))
## Scale-location
plot(fm1, sqrt(abs(resid(.))) ~ fitted(.), type=c("p","smooth"))
## QQ plot
library(lattice)
qqmath(fm1, id=0.05)
