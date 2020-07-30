
calories <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Simple Linear Regression\\calories_consumed.csv')

CC <- calories$Calories.Consumed
WG <- calories$Weight.gained..grams.

boxplot(CC, col="dodgerblue4")
boxplot(WG,col="dodgerblue4")
plot(WG,CC,main="Scatter Plot",
     col="Dodgerblue4",
     col.main="Dodgerblue4",
     col.lab="Dodgerblue4",
     xlab="Weight Gained in grams",
     ylab="Calories COnsumed", pch=20)


reg.model<-lm(WG~CC, data=calories)
summary(reg.model)

plot(CC,WG,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.model, col="red")
res <- signif(residuals(reg.model))
pre <- predict(reg.model)
segments(CC, WG, CC, pre) 

predict(reg.model,newdata= data.frame(CC=c(2800,2900)))
