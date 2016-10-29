#Some descriptive statistics of Affairs
data(Affairs, package="AER")
summary(Affairs)

#Transform affairs into a dichotomous factor
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

#Construct a logistic regression model for factor ynaffairs using all other variables 
fit1 <- glm(ynaffair ~ gender + age + yearsmarried + children +
                  religiousness + education + occupation + rating,
                data=Affairs, family=binomial(link="logit"))
summary(fit1)

#Construct a logistic regression model for factor ynaffairs 
#Using age, yearsmarried, religiousness and rating
fit2 <- glm(ynaffair ~ age + yearsmarried + religiousness + rating,
            data=Affairs, family=binomial(link="logit"))
summary(fit2)

#Parameters of fit2
coef(fit2)

#Predict the probability of the outcome
newdata <- data.frame(rating=mean(Affairs$rating),
                       age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
newdata
prob <- predict(fit2, newdata=newdata, type="response")
prob

#Export dataset
data(Affairs, package="AER")
write.csv(Affairs,"Affairs.csv")


