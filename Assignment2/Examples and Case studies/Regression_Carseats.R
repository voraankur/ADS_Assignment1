### Regression (Subset selection)
### Needed package and datasets
library(ISLR)
attach(Carseats)
Carseats=na.omit(Carseats) # Get rid of NAs
install.packages("leaps")
library(leaps)

##### Searching all subset models up to size 8 by default
regfit.full=regsubsets(Sales~.,data=Carseats)
summary(regfit.full)

##### Searching all subset models up to size number of variables
regfit.full=regsubsets (Sales~.,data=Carseats ,nvmax=11) 
reg.summary =summary (regfit.full)
names(reg.summary)
reg.summary$rss
reg.summary$adjr2

## Plotting and choosing the subset
par(mfrow=c(2,2)) 
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l") 
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
coef(regfit.full ,6)


#### Forward selection
regfit.fwd=regsubsets(Sales~.,data=Carseats ,nvmax=11, method="forward") 
F=summary(regfit.fwd)
names(F)
F
F$rss
F$adjr2
coef(regfit.fwd,6)

#### Backward selection
regfit.bwd=regsubsets(Sales~.,data=Carseats ,nvmax=11, method="backward") 
B=summary(regfit.bwd)
names(B)
B
B$rss
B$adjr2
coef(regfit.bwd,6)

