
emp <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Simple Linear Regression\\emp_data.csv')
emp <- emp_data

CR <- emp$Churn_out_rate
SH <- emp$Salary_hike

boxplot(CR, col="dodgerblue4")
boxplot(SH,col="dodgerblue4")
plot(SH,CR,main="Scatter Plot",
     col="Dodgerblue4",
     col.main="Dodgerblue4",
     col.lab="Dodgerblue4",
     xlab="Churn Out Rate",
     ylab="Salary Hike", pch=20)


reg.model<-lm(CR~SH, data=emp)
summary(reg.model)

plot(SH,CR,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.model, col="red")
res <- signif(residuals(reg.model))
pre <- predict(reg.model)
segments(SH, CR, SH, pre)    

predict(reg.model,newdata= data.frame(SH=c(2000,2200)))

###### Squared Data Transformation ###########

SHsqr <- SH * SH
empsqr <- cbind(emp,SHsqr)

reg.modelsqr<-lm(CR~SHsqr, data=empsqr)
summary(reg.modelsqr)

plot(SHsqr,CR,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.modelsqr, col="red")
res <- signif(residuals(reg.modelsqr))
pre <- predict(reg.modelsqr)
segments(SHsqr, CR, SHsqr, pre)

###### Square-Root Data Transformation ###########

SHsqrt <- sqrt(SH)
empsqrt <- cbind(emp,SHsqrt)

reg.modelsqrt<-lm(CR~SHsqrt, data=empsqrt)
summary(reg.modelsqrt)

plot(SHsqrt,CR,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.modelsqrt, col="red")
res <- signif(residuals(reg.modelsqrt))
pre <- predict(reg.modelsqrt)
segments(SHsqrt, CR, SHsqrt, pre)

###### Log Data Transformation ###########

SHlog <- log(SH)
emplog <- cbind(emp,SHlog)

reg.model.log<-lm(CR~SHlog, data=emplog)
summary(reg.model.log)

plot(SHlog,CR,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.model.log, col="red")
res <- signif(residuals(reg.model.log))
pre <- predict(reg.model.log)
segments(SHlog, CR, SHlog, pre)



