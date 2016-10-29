data(Affairs, package="AER")
#Transform affairs into a dichotomous factor
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

#75% of the sample size
smp_size <- floor(0.75 * nrow(Affairs))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(Affairs)), size = smp_size)

#Split the data into training and testing
train <- Affairs[train_ind, ]
test <- Affairs[-train_ind, ]

#Fit a logistic regression model 
fit <- glm(ynaffair ~ age + yearsmarried + religiousness + rating,
            data=train, family=binomial(link="logit"))
summary(fit)

#Run the model on the test set
test.probs <- predict(fit, test, type='response')
pred <- rep("No",length(test.probs))

#Set the cutoff value =0.5
pred[test.probs>=0.5] <- "Yes"

#Classification matrix
library(caret)
confusionMatrix(test$ynaffair, pred)

#ROC curve
library(ROCR)
prediction <- prediction(test.probs, test$ynaffair)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
test$probs=test.probs
test$prob=sort(test$probs,decreasing = T)
lift <- lift(ynaffair ~ prob, data = test)
lift
xyplot(lift,plot = "gain")

