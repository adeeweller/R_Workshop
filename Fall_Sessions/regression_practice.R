# practice interpreting regressions

head(cars)

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data

summary(linearMod)


library(stargazer)

stargazer(linearMod)