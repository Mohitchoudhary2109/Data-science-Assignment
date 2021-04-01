library(readr)
cc <-read.csv(file.choose())
View(cc)
summary(cc)
plot(cc$Weight.gained..grams., cc$Calories.Consumed)
attach(cc)
cor(Weight.gained..grams.,Calories.Consumed)
#SIMPLE LINEAR REGRESSION MODEL
cc1 <-lm(Weight.gained..grams.~Calories.Consumed)
summary(cc1)
pred <-predict(cc1)
pred
cc1$residuals
sum(cc1$residuals)
mean(cc1$residuals)
sqrt(sum(cc1$residuals^2)/nrow(cc))
sqrt(mean(cc1$residuals^2))
confint(cc1,level=0.95)
predict(cc1,interval = "predict")

library(ggplot2)
ggplot(data = cc, aes(x= Calories.Consumed, y=Weight.gained..grams.))+
  geom_point(color='blue')+
  geom_line(color='red' ,data = cc ,aes(x=Calories.Consumed ,y= pred))
#LOGARITHM MODEL
plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed),Weight.gained..grams.)
cc_log<-lm(Weight.gained..grams.~log(Calories.Consumed))
summary(cc_log)
pred<-predict(cc_log)
pred
cc_log$residuals
sqrt(sum(cc_log$residuals^2)/nrow(cc))
confint(cc_log,interval=0.95)
predict(cc_log,interval = "predict")
##EXPONENTIAL MODEL
plot(Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed,log(Weight.gained..grams.))
cc_exp <-lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(cc_exp)
cc_exp$residuals
sqrt(sum(cc_exp$residuals^2))
logat <-predict(cc_exp)
logat
at <-exp(logat)
error =cc$Weight.gained..grams.-Calories.Consumed
error
sqrt(sum(error^2)/nrow(cc))
confint(cc_exp,interval=0.95)
predict(cc_exp,interval = "predict")
#POLYNOMIAL MODEL WITH 2 DEGREE
plot(Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed,Weight.gained..grams.)
cor(Calories.Consumed*Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed,log(Weight.gained..grams.))
cc2deg <-lm(log(Weight.gained..grams.)~Calories.Consumed+I(Calories.Consumed*Calories.Consumed))
summary(cc2deg)
logpol<-predict(cc2deg)
expy<-exp(logpol)
expy
err=cc$Weight.gained..grams.-expy
err
sqrt(sum(err^2)/nrow(cc))
confint(cc2deg,interval=0.95)
predict(cc2deg,interval = "predict")

  
## Applying transformation is decreasing Multiple R Squared Value
## So model doesnot need further transformation, Multiple R-squared:  0.8968