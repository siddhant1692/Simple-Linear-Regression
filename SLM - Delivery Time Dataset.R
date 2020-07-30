
delivery <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Simple Linear Regression\\delivery_time.csv')

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
