library(readr)
salary_hike <-read.csv(file.choose())
View(salary_hike)
attach(salary_hike)
summary(salary_hike)
var(salary_hike$Salary)
sd(salary_hike$Salary)
var(salary_hike$YearsExperience)
sd(salary_hike$YearsExperience)
##LINEAR MODEL
reg <-lm(Salary ~YearsExperience, data=salary_hike)
summary(reg)
pred <-predict(reg)
pred
reg$residuals
sqrt(sum(reg$residuals^2)/nrow(salary_hike))
confint(reg,level = 0.95)
predict(reg,interval = "confidence")
plot(reg)
##LOGORATHMIC MODEL
reg_log <-lm(Salary ~YearsExperience, data = salary_hike)
summary(reg_log)  
confint(reg_log,level = 0.95)
predict(reg_)log,interval = "confidence")
plot(reg_log)
########
#####MULTIPLE R SQUARED VALUE IS 0.957 WHICH MEANS THE MODEL WILL PREDICT 95% TIME CORRECT