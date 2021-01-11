#5
library(ISLR)
attach(Default)
glm.fits=glm(default~income+balance, family="binomial")
summary(glm.fits)
dim(Default)
set.seed(1)
Default=data.frame(Default)
t=sample(10000, 9000)
train=rep(FALSE, 10000)
train[t]=TRUE
Default.test=Default[!train,]
default.test=default[!train]
glm.fits=glm(default~income+balance, family="binomial", subset=train)
glm.probs=predict(glm.fits, Default.test, type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>0.5]="Yes"
mean(glm.pred==default.test)
table(glm.pred, default.test)

#With student dummy
t=sample(10000, 9000)
train=rep(FALSE, 10000)
train[t]=TRUE
Default.test=Default[!train,]
default.test=default[!train]
glm.fits=glm(default~income+balance+student, family="binomial", subset=train)
glm.probs=predict(glm.fits, Default.test, type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>0.5]="Yes"
mean(glm.pred==default.test)
table(glm.pred, default.test)

#6
set.seed(1)
glm.fits=glm(default~income+balance, family="binomial")
summary(glm.fits)
boot.fn=function(data, index)
coefficients(glm(default~income+balance, data=data, family="binomial", subset=index))

boot.fn(Default, sample(10000, 10000, replace=T))
library(boot)
boot(Default, boot.fn, 1000)
summary(glm.fits)

#7
attach(Weekly)
glm.fits=glm(Direction~Lag1+Lag2, family="binomial")
train=rep(TRUE, 1089)
train[1]=FALSE
Direction.test=Direction[1]
glm.fits=glm(Direction~Lag1+Lag2, family="binomial", subset=train)
glm.probs=predict(glm.fits, Direction.test, type="response")
glm.pred=rep("Down", 1)
if (glm.probs[1]>0.5){
  glm.pred[1]="Up"
}
glm.pred==Direction[1]

#LOOCV using for loop
means=rep(0, 1089)
for (i in 1:1089){
  train=rep(TRUE, 1089)
  train[i]=FALSE
  Direction.test=Direction[i]
  glm.fits=glm(Direction~Lag1+Lag2, family="binomial", subset=train)
  glm.probs=predict(glm.fits, Direction.test, type="response")
  glm.pred="Down"
  if (glm.probs[i]>0.5){
    glm.pred="Up"
  }
  if (glm.pred!=Direction[i]){
    means[i]=1
  }
}
mean(means)
#the mean of the means is the test error rate, as it is comprised of only 0s and 1s, with 1s representing errors. Therefore, it shows the proprotion of erroneous predictions to all predictions.

#8
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x, y)

#LOOCV with varying regression degrees
set.seed(5)
f=data.frame(x, y)
verify=rep(0, 4)
for (n in 1:4){
  pred=rep(0, 100)
  for (i in 1:100){
    train=rep(TRUE, 100)
    train[i]=FALSE
    lm.fit=lm(y~poly(x, n), data=f, subset=train)
    pred[i]=(y[i]-predict(lm.fit, f[!train,]))^2
  }
  verify[n]=mean(pred)
}
verify

#Least Squares Models
lm.fits1=lm(y~poly(x, 1), data=f)
summary(lm.fits1)
lm.fits2=lm(y~poly(x, 2), data=f)
summary(lm.fits2)
lm.fits3=lm(y~poly(x, 3), data=f)
summary(lm.fits3)
lm.fits4=lm(y~poly(x, 4), data=f)
summary(lm.fits4)

#9
#Mean estimation
library(MASS)
attach(Boston)
m=mean(medv)
s=sd(medv)
stderr=s/sqrt(dim(Boston)[1])

#Bootstrap for mean
set.seed(1)
boot.fn=function(data, index){
  x=medv[index]
  return(mean(x))
}
boot(Boston, boot.fn, 1000)

m=22.53281
stderr=0.4106622
conf=c(m-2*stderr, m+2*stderr)

#Median
median(medv)

#Median bootstrap
set.seed(1)
med.fn=function(data, index){
  x=medv[index]
  return(median(x))
}
boot(Boston, med.fn, 1000)

#Tenth percentile
q=quantile(medv, probs=seq(0, 1, 0.1))

#Percentile bootstrap
set.seed(1)
tenth.fn=function(data, index){
  x=medv[index]
  return(quantile(x, probs=seq(0, 1, 0.1))[2])
}
boot(Boston, tenth.fn, 1000)
