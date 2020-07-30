
Sal <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Simple Linear Regression\\Salary_Data.csv')

Exp <- Sal$YearsExperience
SH <- Sal$Salary

boxplot(Exp, col="dodgerblue4")
boxplot(SH,col="dodgerblue4")
plot(Exp,SH,main="Scatter Plot",
     col="Dodgerblue4",
     col.main="Dodgerblue4",
     col.lab="Dodgerblue4",
     xlab="Years of Experience",
     ylab="Salary", pch=20)


reg.model<-lm(SH~Exp, data=Sal)
summary(reg.model)

plot(Exp,SH,main="Line of Best Fit",col="Dodgerblue4", col.main="Dodgerblue4")  
abline(reg.model, col="red")
res <- signif(residuals(reg.model))
pre <- predict(reg.model)
segments(Exp,SH,Exp,predict(reg.model))    

predict(reg.model,newdata= data.frame(Exp=c(10,11)))
