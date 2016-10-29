library(data.table)
library(forecast)

install.packages("broom")
library(broom)

#Read Source File
cleanData<-read.csv("C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\cleanData.csv", stringsAsFactors = FALSE)

#Regression Techniques
#Backward Regression
bkwrd.lm= step(lm(kWh~hour + month +year+ day+Day.of.Week+Weekday+Peakhour+Temperature+Dew_PointF+Humidity+Sea_Level_PressureIn+VisibilityMPH+Wind_Direction+Wind_SpeedMPH+WindDirDegrees+Conditions, data = cleanData), direction = 'backward')
#Co-efficient
lm(formula = kWh~as.factor(hour) + month + day + Day.of.Week + Weekday + Peakhour + Temperature + Dew_PointF + Humidity + Wind_Direction + Conditions, data = cleanData)
#Regression
lm.fit1= lm(kWh ~ hour + month + day + Day.of.Week + Weekday + Peakhour + Temperature + Dew_PointF + Humidity + Wind_Direction + Conditions, data = cleanData)
summary(lm.fit1)

#Forward Regression
fwd.lm= step(lm(kWh~1, data = cleanData), direction = "forward", scope = ~hour + month +year+ day+Day.of.Week+Weekday+Peakhour+Temperature+Dew_PointF+Humidity+Sea_Level_PressureIn+VisibilityMPH+Wind_Direction+Wind_SpeedMPH+WindDirDegrees+Conditions)
#Co-efficient
lm(formula = kWh~Peakhour + Weekday + Temperature + month + Day.of.Week + Conditions + Wind_Direction + day + Humidity + Dew_PointF + hour, data = cleanData)
#Regression
lm.fit2= lm(kWh~Peakhour + Weekday + Temperature + month + Day.of.Week + Conditions + Wind_Direction + day + Humidity + Dew_PointF + hour, data = cleanData)
summary(lm.fit2)

#Stepwise Regression
stepWise.lm <- step(lm(kWh~hour + month +year+ day+Day.of.Week+Weekday+Peakhour+Temperature+Dew_PointF+Humidity+Sea_Level_PressureIn+VisibilityMPH+Wind_Direction+Wind_SpeedMPH+WindDirDegrees+Conditions,data=cleanData),direction="both")
#Co-efficient
lm(formula = kWh~hour + month + day + Day.of.Week + Weekday + Peakhour + Temperature + Dew_PointF + Humidity + Wind_Direction + Conditions, data = cleanData)
#Regression
lm.fit3= lm(kWh~hour + month + day + Day.of.Week + Weekday + Peakhour + Temperature + Dew_PointF + Humidity + Wind_Direction + Conditions, data = cleanData)
summary(lm.fit3)

#Split the data in training and testing data
smp_size<-floor(0.75 * nrow(cleanData))
set.seed(123)
train_ind<-sample(seq_len(nrow(cleanData)), size = smp_size)
train<-cleanData[train_ind, ]
test<-cleanData[-train_ind, ]

#Train the model With train data
lm.fit4= lm(kWh~as.factor(VisibilityMPH)+Sea_Level_PressureIn+WindDirDegrees+Wind_SpeedMPH+as.factor(hour)+as.factor(Temperature)+day+Dew_PointF+Wind_Direction+as.factor(Humidity)+as.factor(month)+Conditions+Day.of.Week+Peakhour, data = train)
summary(lm.fit4)

#Predict the data with test data
pred=predict(lm.fit4, test)

#Performance Metrics
accuracy(pred, train$kWh)

test$kWh <- pred
dim(test)

#Writing the o/p of test data
#write.csv(test, "C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\forecastOutputSample.csv")

#DOWNLOAD Coefficients
tidy_lmfit <- tidy(lm.fit4)
RegressionOutput <- data.frame(tidy_lmfit$term, tidy_lmfit$estimate, stringsAsFactors=F)
RegressionOutput[1, 1] = "Constant"
setnames(RegressionOutput, old = c('tidy_lmfit.term','tidy_lmfit.estimate'), new = c('Variables','Coefficient'))

#write the Regression O/p
write.csv(RegressionOutput, "C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\RegressionOutputs.csv", row.names=FALSE)

#DOWNLOAD METRICS
acc <- accuracy(pred, train$kWh)
acc<-as.data.frame(acc)
acc["Account"]<-NA
acc$Account<-train$Account[1]

#write the o/p of performance metrics
write.csv(t(acc), "C:\\Users\\Ankur\\Desktop\\ADS\\ADS_Assignments\\Assignment2\\Part1\\PerformanceMetrics.csv", row.names=TRUE)