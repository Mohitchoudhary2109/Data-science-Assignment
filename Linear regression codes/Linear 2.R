library(readr)
DT <-read.csv(file.choose())
View(DT)
attach(DT)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)
#  SIMPLE LINEAR MODEL
reg <-lm(Delivery.Time~Sorting.Time)
summary(reg)
pred <-predict(reg)
pred
reg$residuals
sqrt(sum(reg$residuals^2)/nrow(DT))
confint(reg, interval=0.95)
predict(reg,interval = "predict")


library(ggplot2)
ggplot(data = DT, aes(x= Sorting.Time, y=Delivery.Time))+
  geom_point(color='blue')+
  geom_line(color='red' ,data = DT,aes(x=Sorting.Time ,y= pred))
#  LOGORATHIM MODEL
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
reg_log<-lm(Delivery.Time ~ log(Sorting.Time))
summary(reg_log)
pred<-predict(reg_log)
pred
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(DT))
confint(reg_log,interval=0.95)
predict(reg_log,interval = "predict")
#EXPONENTIAL MODEL
plot(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
reg_exp <-lm(log(Delivery.Time)~ Sorting.Time)
summary(reg_exp)
reg_exp$residuals
sqrt(sum(reg_exp$residuals^2))
logat <-predict(reg_exp)
logat
at <-exp(logat)
at
error = DT$Delivery.Time -at
error
sqrt(sum(error^2)/nrow(DT))
confint(reg_exp,interval=0.95)
predict(reg_exp,interval = "predict")
#POLYNOMIAL  MODEL WITH 2 DEGREE
plot(Sorting.Time,Delivery.Time)
plot(Sorting.Time*Sorting.Time,Delivery.Time)
cor(Sorting.Time*Sorting.Time,Delivery.Time)
plot(Sorting.Time*Sorting.Time, log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time*Sorting.Time,log(Delivery.Time))
reg2deg<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time))
summary(reg2deg)
logply<-predict(reg2deg)
expoly<-exp(logply)
expoly
err=DT$Delivery.Time-expoly
err
sqrt(sum(err^2)/nrow(DT))
confint(reg2deg,interval=0.95)
predict(reg2deg,interval = "predict")

# visualization
ggplot(data = DT, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=Sorting.Time+I(Delivery.Time^2), y=logply))


##  POLYNOMIAL MODEL WITH 3 DEGREE
reg3deg<-lm(log(Delivery.Time)~Sorting.Time + I(Sorting.Time*Sorting.Time) + I(Sorting.Time*Sorting.Time*Sorting.Time))
summary(reg3deg)
logpoly3<-predict(reg3deg)
expoly3<-exp(logpoly3)
expoly3


# visualization
ggplot(data = DT, aes(x = Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3), y =Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DT, aes(x=Sorting.Time+I(Delivery.Time^2)+I(Delivery.Time^3), y=expoly3))


## Applying transformation is decreasing Multiple R Squared Value
## So polynomial model with 3 degree ha the best, Multiple R-squared:0.7819