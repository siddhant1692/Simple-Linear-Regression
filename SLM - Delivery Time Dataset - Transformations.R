
delivery <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Simple Linear Regression\\delivery_time.csv')
delivery <- delivery_time

DT <- delivery$Delivery.Time
ST <- delivery$Sorting.Time

boxplot(DT, col="dodgerblue4")
boxplot(ST,col="dodgerblue4")
plot(ST,DT,main="Scatter Plot",
     col="Dodgerblue4",
     col.main="Dodgerblue4",
     col.lab="Dodgerblue4",
     xlab="Delivery Time",
     ylab="Sorting Time", pch=20)


reg.model<-lm(DT~ST, data=delivery)
summary(reg.model)

plot(ST,DT,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")
abline(reg.model, col="red")
res <- signif(residuals(reg.model))
pre <- predict(reg.model)
segments(ST, DT, ST, pre) 

predict(reg.model,newdata= data.frame(ST=c(15,20)))

###### Squared Data Transformation ###########

STsqr <- ST * ST
deliverysqr <- cbind(delivery,STsqr)

reg.modelsqr<-lm(DT~STsqr, data=deliverysqr)
summary(reg.modelsqr)

plot(STsqr,DT,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")
abline(reg.modelsqr, col="red")
res <- signif(residuals(reg.modelsqr))
pre <- predict(reg.modelsqr)
segments(STsqr, DT, STsqr, pre) 

###### Square-Root Data Transformation ###########

STsqrt <- sqrt(ST)
deliverysqrt <- cbind(delivery,STsqrt)

reg.modelsqrt<-lm(DT~STsqrt, data=deliverysqrt)
summary(reg.modelsqrt)

plot(STsqrt,DT,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")
abline(reg.modelsqrt, col="red")
res <- signif(residuals(reg.modelsqrt))
pre <- predict(reg.modelsqrt)
segments(STsqrt, DT, STsqrt, pre) 

###### Log Data Transformation ###########

STlog <- log(ST)
deliverylog <- cbind(delivery,STlog)

reg.model.log<-lm(DT~STlog, data=deliverylog)
summary(reg.model.log)

plot(STlog,DT,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")
abline(reg.model.log, col="red")
res <- signif(residuals(reg.model.log))
pre <- predict(reg.model.log)
segments(STlog, DT, STlog, pre)






