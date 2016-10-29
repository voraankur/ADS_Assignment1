library(MASS)
library(ISLR)

lm.fit=lm(medv~lstat, data=Boston)
summary(lm.fit)

lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)

library(car)

lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)


lm.fit3=lm(medv~lstat+lstat2, data=Boston)
summary(lm.fit3)
