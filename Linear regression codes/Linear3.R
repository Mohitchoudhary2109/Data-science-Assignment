library(readr)
emp <-read.csv(file.choose())
attach(emp)
View(emp)
salary <-emp$Salary_hike
churn <-emp$Churn_out_rate
plot(salary,churn)
boxplot(salary,churn)
summary(emp)
cor(salary,churn)
#  SIMPLE LINEAR MODEL
reg <-lm(churn~ salary)
summary(reg)
pred <-predict(reg)
pred
reg$residuals
sqrt(sum(reg$residuals^2)/nrow(emp))
confint(reg,level = 0.95)
predict(reg,interval = "predict")
 ###LOGARITHMIC MODEL
reg_log <-lm(churn~ log(salary))
summary(reg_log)
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")
###EXPONENTIAL MODEL
reg_exp <-lm(log(churn)~ salary)
summary(reg_exp)
confint(reg_exp, level = 0.95)
predict(reg_exp,interval = "predict")
## POLYNOMIAL WITH 2 DEGREE
reg2deg <-lm(log(churn)~ salary+I(salary*salary))
summary(reg2deg)
confint(reg2deg, level = 0.95)
predict(reg2deg, interval = "predict")
poly <-predict(reg2deg)
poly1<-exp(poly)
poly1
error <-emp$Churn_out_rate -poly1
error
sqrt(error^2)/nrow(emp)
plot(reg2deg)




## Applying transformation is increasing Multiple R Squared Value
## So polynomial model with 2 degree has the best, Multiple R-squared