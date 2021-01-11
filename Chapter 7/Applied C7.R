library(ISLR)
library(boot)
#6 (Wage)
#6a (polynomial)
attach(Wage)
errors=rep(0, 10)
for (i in 1:10){
  glm.fit=glm(wage~poly(age, i, raw=T), data=Wage)
  errors[i]=cv.glm(Wage, glm.fit, K=10)$delta[1]
}
which.min(errors)
#According to 10-fold cross-validation, the optimal degree for the polynomial fit here is a 6th degree one.
fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age, 2), data=Wage)
fit.3=lm(wage~poly(age, 3), data=Wage)
fit.4=lm(wage~poly(age, 4), data=Wage)
fit.5=lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#ANOVA testing, however, states that a cubic or quartic fit is sufficient
#A quintic fit appears to be unnecessary due to its high p-value
#Therefore, it may be the case that a 7th degree fit is unnecessary and a 3rd or 4th degree fit is preferable

plot(wage~age, data=Wage)
agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])
fit1=lm(wage~poly(age, 6, raw=T))
preds1=predict(fit1, newdata=list(age=age.grid), se=TRUE)
lines(age.grid, preds1$fit, lwd=3, col="red")

#6b (step)
cuts=rep(NA, 10)
#NOTE: For some reason, assigning Wage$age.cut must be used for cross-validation with step functions
#Simply using wage~cut(age, i) will return an error about mismatched levels
for (i in 2:10){
  Wage$age.cut <- cut(Wage$age, i)
  glm.fit=glm(wage~age.cut, data=Wage)
  cuts[i]=cv.glm(Wage, glm.fit, K=10)$delta[1]
}
which.min(cuts)
#Optimal number of cuts chosen by cross-validation is 8
fit2=lm(wage~cut(age, 8))
preds2=predict(fit2, newdata=list(age=age.grid), se=TRUE)
lines(age.grid, preds2$fit, lwd=3, col="blue")


#7 (other variables in Wage)
par(mfrow=c(2, 1))
plot(maritl, wage)
plot(jobclass, wage)
plot(race, wage)
library(gam)
fit1=gam(wage~lo(age, span=0.5)+s(year, 4)+education, data=Wage)
fit2=gam(wage~lo(age, span=0.5)+s(year, 4)+education+maritl, data=Wage)
fit3=gam(wage~lo(age, span=0.5)+s(year, 4)+education+jobclass, data=Wage)
fit4=gam(wage~lo(age, span=0.5)+s(year, 4)+education+race, data=Wage)
fit5=gam(wage~lo(age, span=0.5)+s(year, 4)+education+maritl+jobclass, data=Wage)
fit6=gam(wage~lo(age, span=0.5)+s(year, 4)+education+jobclass+race, data=Wage)
fit7=gam(wage~lo(age, span=0.5)+s(year, 4)+education+race+maritl, data=Wage)
fit8=gam(wage~lo(age, span=0.5)+s(year, 4)+education+maritl+jobclass+race, data=Wage)
anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)

#8 (Auto)
attach(Auto)
names(Auto)
Auto=na.omit(Auto)
plot(displacement, mpg)
plot(weight, mpg)
plot(horsepower, mpg)

#Displacement
#Non-linear fit recommended by ANOVA, quadratic is sufficient
dis.fit1=lm(mpg~displacement)
dis.fit2=lm(mpg~poly(displacement, 2, raw=T))
dis.fit3=lm(mpg~poly(displacement, 3, raw=T))
dis.fit4=lm(mpg~poly(displacement, 4, raw=T))
dis.fit5=lm(mpg~poly(displacement, 5, raw=T))
anova(dis.fit1, dis.fit2, dis.fit3, dis.fit4, dis.fit5)

plot(mpg~displacement)
lims=range(displacement)
grid=seq(from=lims[1], to=lims[2])
preds=predict(dis.fit2, newdata=list(displacement=grid), se=TRUE)
lines(grid, preds$fit, lwd=3, col="red")

#Weight
library(splines)
#Using regression splines on the weight fit, a quadratic spline is deemed as sufficient by ANOVA
w.fit1=lm(mpg~weight)
w.fit2=lm(mpg~bs(weight, degree=1))
w.fit3=lm(mpg~bs(weight, degree=2))
w.fit4=lm(mpg~bs(weight, degree=3))
w.fit5=lm(mpg~bs(weight, degree=4))
anova(w.fit1, w.fit2, w.fit3, w.fit4, w.fit5)

plot(mpg~weight)
lims=range(weight)
grid=seq(from=lims[1], to=lims[2])
preds=predict(w.fit3, newdata=list(weight=grid), se=TRUE)
lines(grid, preds$fit, lwd=3, col="red")
lines(grid, preds$fit+2*preds$se, lty="dashed")
lines(grid, preds$fit-2*preds$se, lty="dashed")

#Horsepower
h.fit1=lm(mpg~horsepower)
h.fit2=loess(mpg~horsepower, span=0.3, degree=1)
h.fit3=loess(mpg~horsepower, span=0.7, degree=1)
h.fit4=loess(mpg~horsepower, span=0.3, degree=2)
h.fit5=loess(mpg~horsepower, span=0.7, degree=2)

MSE=rep(0, 5)
MSE[1]=sum((h.fit1$residuals)^2)/dim(Auto)[1]
MSE[2]=sum((h.fit2$residuals)^2)/dim(Auto)[1]
MSE[3]=sum((h.fit3$residuals)^2)/dim(Auto)[1]
MSE[4]=sum((h.fit4$residuals)^2)/dim(Auto)[1]
MSE[5]=sum((h.fit5$residuals)^2)/dim(Auto)[1]
MSE

plot(mpg~horsepower)
lims=range(horsepower)
grid=seq(from=lims[1], to=lims[2])
preds=predict(h.fit4, newdata=list(weight=grid), se=TRUE)
lines(grid, preds$fit, lwd=3, col="red")
lines(grid, preds$fit+2*preds$se, lty="dashed")
lines(grid, preds$fit-2*preds$se, lty="dashed")

plot(1:5, MSE, xlab="Fit number", type="l")
min=which.min(MSE)
points(min, MSE[min], col="orange", cex=3, pch=20)
#Based on the MSEs, the local regressions have varying degrees of performance
#The 2nd degree local regressions perform better than the 1st degree ones
#However, all of them do significantly better than the standard least squares fit
#This certainly shows a non-linear relationship between horsepower and mpg, as a linear model does not fit the data well

#9 (Boston)
library(MASS)
attach(Boston)
#9a (cubic)
nox.fit=lm(nox~poly(dis, 3, raw=T))
summary(nox.fit)

plot(dis, nox)
lims=range(dis)
grid=seq(from=lims[1], to=lims[2])
preds=predict(nox.fit, newdata=list(dis=grid), se=TRUE)
lines(grid, preds$fit, lwd=3, col="red")

#9b (degrees)
residuals=rep(0, 10)
par(mfrow=c(2, 5))
for (i in 1:10){
  fit=lm(nox~poly(dis, i, raw=T))
  residuals[i]=sum((fit$residuals)^2)
  plot(dis, nox, title=paste("Fit of Degree", toString(i)))
  pred=predict(fit, newdata=list(dis=grid), se=TRUE)
  lines(grid, pred$fit, lwd=3, col="red")
}
residuals

#9c (best polynomial)
#ANOVA
nox.fit1=lm(nox~poly(dis, 1, raw=T))
nox.fit2=lm(nox~poly(dis, 2, raw=T))
nox.fit3=lm(nox~poly(dis, 3, raw=T))
nox.fit4=lm(nox~poly(dis, 4, raw=T))
nox.fit5=lm(nox~poly(dis, 5, raw=T))
anova(nox.fit1, nox.fit2, nox.fit3, nox.fit4, nox.fit5)

#9d (spline)
nox.spline=lm(nox~bs(dis, df=4))
summary(nox.spline)
par(mfrow=c(1,1))
plot(dis, nox)
lims=range(dis)
grid=seq(from=lims[1], to=lims[2])
preds=predict(nox.spline, newdata=list(dis=grid), se=TRUE)
lines(grid, preds$fit, lwd=3, col="red")

#9e (degrees of freedom)
residuals=rep(0, 10)
par(mfrow=c(2, 5))
for (i in 1:10){
  fit=lm(nox~bs(dis, df=i+3))
  residuals[i]=sum((fit$residuals)^2)
  plot(dis, nox, title=paste(toString(i), "Degrees of Freedom"))
  pred=predict(fit, newdata=list(dis=grid), se=TRUE)
  lines(grid, pred$fit, lwd=3, col="red")
}
residuals

#9f (best spline)
#Cross-validation
errors=rep(0, 10)
for (i in 1:10){
  glm.fit=glm(nox~bs(dis, df=i+3), data=Wage)
  errors[i]=cv.glm(Boston, glm.fit, K=10)$delta[1]
}
which.min(errors)

#10 (College)
attach(College)

#10a (selection)
library(leaps)
set.seed(1)
train=sample(c(TRUE, FALSE), dim(College)[1], rep=TRUE)
test=!train
regfit.fwd=regsubsets(Outstate~., data=College[train,], nvmax=5, method="forward")
summary(regfit.fwd)
#Variables Private, Room.Board, PhD, perc.alumni, and Expend were chosen.

#10b (GAM)
library(gam)
gam.m1=gam(Outstate~Private+Room.Board+PhD+perc.alumni+Expend)
gam.m5=gam(Outstate~Private+s(Room.Board, 4)+s(PhD, 5)+s(perc.alumni, 4)+s(Expend, 5))
summary(gam.m1)
summary(gam.m5)
par(mfrow=c(1,5))
plot(gam.m5, se=TRUE, col="orange")

#10c (test)
preds=predict(gam.m1, College[test,])
RSS=sum((College[test,]$Outstate-preds)^2)
RSS
TSS=sum((College[test,]$Outstate-mean(College[test,]$Outstate))^2)
R2=1-RSS/sum((College[test,]$Outstate-mean(College[test,]$Outstate))^2)
R2

#10d (non-linearity)
summary(gam.m1)
summary(gam.m5)

#11 Backfitting

#11a (Generation)
set.seed(1)
y=rnorm(100, sd=2)
x1=rnorm(100, sd=1)
x2=rnorm(100)

#11b (Coef1)
b1=100000

#11c (Fixing b1)
a=y-b1*x1
b2=lm(a~x2)$coef[2]
b2

#11d (Fixing b2)
a=y-b2*x2
b1=lm(a~x1)$coef[2]
b1

#11e (Simulated backfitting)
par(mfrow=c(1,1))
plot(1:1000, type="n", ylim=c(0, 0.3))
line0=rep(0, 1000)
line1=rep(0, 1000)
line2=rep(0, 1000)
for (i in 1:1000){
  a=y-b1*x1
  b2=lm(a~x2)$coef[2]
  line2[i]=b2
  a=y-b2*x2
  b1=lm(a~x1)$coef[2]
  line1[i]=b1
  line0[i]=lm(a~x1)$coef[1]
}
line0
line1
line2
lines(line0, lwd=3, col="red")
lines(line1, lwd=3, col="blue")
lines(line2, lwd=3, col="yellow")

#11f (comparison to multiple linear regression)
model=lm(y~x1+x2)
coef(model)
abline(a=coef(model)[1], b=0, col="green")
abline(a=coef(model)[2], b=0, col="orange")
abline(a=coef(model)[3], b=0, col="purple")

#12 (Backfitting but larger)
set.seed(1)
for (i in 1:100){
  if (i==1){
    data=rnorm(100)
  }
  else{
    data=data.frame(data, rnorm(100))
  }
}

y=rnorm(100)

for (i in 1:100){
  colnames(data)[i]=paste("x", toString(i))
}

estimates=rep(0, 1000)
for (i in 1:100){
    estimates=data.frame(estimates, rep(0, 1000))
}

for (i in 1:101){
  colnames(estimates)[i]=paste("b", toString(i-1))
}

coefs=sample(10000, 100, rep=T)

#Note, the actual backfitting loop below takes a long time to fully run.
for (i in 1:1000){
  for (j in 1:100){
    current=coefs[-j]
    x=data[,-j]
    a=y
    for (k in 1:99){
      a=a-current[k]*x[, k]
    }
    coefs[j]=lm(a~data[, j])$coef[2]
  }
  estimates[i,1]=lm(a~data[, j])$coef[1]
  estimates[i,2:101]=coefs
}
estimates
plot(1:1000, type="n", ylim=c(-1000, 1000), ylab="Coefficient estimate", xlab="Backfitting Iteration No.")
lines(estimates[,1], lwd=3, col="red")
lines(estimates[,2], lwd=3, col="blue")
lines(estimates[,4], lwd=3, col="yellow")

data.full=data.frame(y, data)
model=lm(y~., data=data.full)
coef(model)
abline(a=coef(model)[1], b=0, col="green")
abline(a=coef(model)[2], b=0, col="orange")
abline(a=coef(model)[3], b=0, col="purple")
