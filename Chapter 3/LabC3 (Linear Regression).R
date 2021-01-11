library(ISLR)
library(MASS)

fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)

#names() for other info stored in x
names(lm.fit)

#coefficient estimates obtained by confint()
confint(lm.fit)

#predict() for confidence/prediction intervals, interval arg can be set to either
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="confidence")

#abline() to draw a line 
#abline(a, b) to draw line with intercept a and slope b
#abline(lwd=x) to increase width by factor of x
#plot(pch) to change plotting symbols
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

#par() to split screen and view simultaneously generated plots
par(mfrow=c(2,2))
plot(lm.fit)

#residuals() for residual computation
#rstudent() for studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#hatvalues() for leverage statistics
plot(hatvalues(lm.fit))
#which.max() identifies index of a vector's largest element
#applying which.max() to hatvalues returns largest leverage stat
which.max(hatvalues(lm.fit))

#lm(y~x1+x2+x3+...+xi) to fit multiple linear regression
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
#alternatively, lm(y~., data=a) to perform regression with all predictors from a
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)

#summary(lm.fit)$x gives us component x of the model
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

#vif() used to calculate variance inflation factors; part of car library
library(car)
vif(lm.fit)

#lm(medv~.-x, data=...) to perform regression excluding variable x
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)
#or just use update(lm.fit, ~.-x) to exclude x
lm.fit1=update(lm.fit, ~.-age)

#To include interaction terms using lm(), use x:y to include an interaction term between the two, or x*y to include both AND the interaction term
summary(lm(medv~lstat*age, data=Boston))

#Non-linear transformations can be done on variable x using I(X^2)
#I() needed for standard usage of "^" (exponents)
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
#anova() to quantify how superior a higher power model is
lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2)
#to save time, use poly() within lm() like below:
lm.fit5=lm(medv~poly(lstat, 5))
summary(lm.fit5)
#other transformations work as well, such as logarithmic or even trigonometric ones
summary(lm(medv~log(rm), data=Boston))

#contrasts() to return indicators for dummy variables
#ex: in Carseats, ShelveLoc=1 if location is good, otherwise ShelveLoc=0
contrasts(ShelveLoc)

#Function Creation Example
LoadLibraries=function(){
library(ISLR)
library(MASS)
print("The libraries have been loaded.")
}