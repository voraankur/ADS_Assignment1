library(MASS)
library(ISLR)
data(Boston)
#75% of the sample size
smp_size <- floor(0.75 * nrow(Boston))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(Boston)), size = smp_size)

#Split the data into training and testing
train <- Boston[train_ind, ]
test <- Boston[-train_ind, ]

#Fit a linear regression model 
lm.fit = lm(medv ~ ., data = train)

#Summary of the fit
summary(lm.fit)

#Measures of predictive accuracy
library(forecast)
pred = predict(lm.fit, test)
accuracy(pred, train$medv)
