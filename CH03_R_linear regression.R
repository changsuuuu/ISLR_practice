# Chapter 3 Lab: Linear Regression
# install.packages("MASS")
# install.packages("ISLR")
# install.packages("car")
library(MASS)
library(ISLR)
library(car)


# Simple Linear Regression
head(Boston)
names(Boston)
attach(Boston)
lm.fit=lm(medv~lstat,data=Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

plot(lstat,medv)
# abline(lm.fit)
# abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
# plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # index 반환

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5)) # 지정된 차수 만큼 지정
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

head(Carseats)
lm.fit=lm(Sales~.+Income*Advertising+Price*Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) # one hot incoding 느낌인듯
lm.fit1=lm(Sales~.,data=Carseats)
summary(lm.fit1)

lm.fit=lm(Sales~. - Carseats[,9],data=Carseats)
summary(lm.fit)

# Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
 library(ISLR)
 library(MASS)
 print("The libraries have been loaded.")
 }
LoadLibraries
LoadLibraries()
