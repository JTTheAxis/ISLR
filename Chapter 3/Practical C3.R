library(ISLR)
library(MASS)
Auto=read.csv("Auto.csv", header=TRUE, na.strings="?")
Auto=na.omit(Auto)
fix(Auto)
names(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
attach(Auto)
lm.fit=lm(mpg~horsepower)

names(lm.fit)
confint(lm.fit)
plot(mpg)
summary(lm.fit)
coef(lm.fit)
predict(lm.fit, data.frame(horsepower=98), interval="confidence")

plot(horsepower, mpg)
abline(lm.fit, lwd=3, col="orange")

par(mfrow=c(2,2))
plot(lm.fit)

#Matrix of correlations for Auto (less "names")
plot(Auto)
type(Auto)
x=Auto[1:8]
cor(x, x)

lm.fit=lm(mpg~.-name, data=Auto)
summary(lm.fit)
plot(lm.fit)

Auto_log <- data.frame(Auto[1], apply(Auto[2:8], 2, log))
lm.fitlog=lm(mpg~., data=Auto_log)
summary(lm.fitlog)
plot(lm.fitlog)
anova(lm.fit, lm.fitlog)

lm.fit1=lm(mpg~weight)
summary(lm.fit1)
plot(lm.fit1)

lm.fit2=lm(mpg~weight+I(weight^2))
summary(lm.fit2)
plot(lm.fit2)

lm.fit05=lm(mpg~weight+I(weight^0.5))
summary(lm.fit05)
plot(lm.fit05)

anova(lm.fit1, lm.fit2)
anova(lm.fit1, lm.fit05)

#Carseats
carseats=ISLR::Carseats
carset=lm(Sales~Price+Urban+US, data=carseats)

#Only significant predictors
carrev=lm(Sales~Price+US, data=carseats)
summary(carrev)

#Confidence intervals
confint(carrev)

plot(carrev)

#Investigation: t-statistic of null hypothesis
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

#y on x (to perform regression with no intercept, simply add a "0" to the regression call in the predictors section)
yx=lm(y~x+0)
summary(yx)

#x on y
xy=lm(x~y+0)
summary(xy)

#with intercepts
yx1=lm(y~x)
summary(yx1)

xy1=lm(x~y)
summary(xy1)

x=y
yx=lm(y~x+0)
summary(yx)

xy=lm(x~y+0)
summary(yx)

#Experimentations
set.seed(1)
x=rnorm(100, mean=0, sd=1)
sd(x)
var(x)

eps=rnorm(100, mean=0, sd=sqrt(0.25))
sd(eps)
var(eps)

y=-1+0.5*x+eps
length(y)

lsq=lm(y~x)
summary(lsq)
plot(x, y)
abline(lsq, lwd=3, col="orange")
abline(a=-1, b=0.5, lwd=3, col="black")
legend("bottomleft", legend=c("lsq-fit", "Y-fit"), col=c("orange", "black"), lty=1, lwd=2, cex=.8)
confint(lsq)

#Fit with polynomial
psq=lm(y~x+I(x^2))
summary(psq)
anova(lsq, psq)

#Less noise
eps=rnorm(100, mean=0, sd=sqrt(0.1))
sd(eps)
var(eps)

y=-1+0.5*x+eps
length(y)

lsq=lm(y~x)
summary(lsq)
plot(x, y)
abline(lsq, lwd=3, col="orange")
abline(a=-1, b=0.5, lwd=3, col="black")
legend("bottomleft", legend=c("lsq-fit", "Y-fit"), col=c("orange", "black"), lty=1, lwd=2, cex=.8)
confint(lsq)

#More noise
eps=rnorm(100, mean=0, sd=sqrt(0.5))
sd(eps)
var(eps)

y=-1+0.5*x+eps
length(y)

lsq=lm(y~x)
summary(lsq)
plot(x, y)
abline(lsq, lwd=3, col="orange")
abline(a=-1, b=0.5, lwd=3, col="black")
legend("bottomleft", legend=c("lsq-fit", "Y-fit"), col=c("orange", "black"), lty=1, lwd=2, cex=.8)
confint(lsq)

#Collinearity
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
plot(x1, x2)
reg=lm(y~x1+x2)
summary(reg)

#only x1
y1=lm(y~x1)
summary(y1)

#only x2
y2=lm(y~x2)
summary(y2)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y, 6)
reg=lm(y~x1+x2)
par(mfrow=c(2,2))
plot(reg)
summary(reg)

y1=lm(y~x1)
plot(y1)
summary(y1)

y2=lm(y~x2)
plot(y2)
summary(y2)

#Boston
boston=MASS::Boston
attach(boston)
a=lm(crim~age)
summary(a)
b=lm(crim~black)
summary(b)
c=lm(crim~chas)
summary(c)
d=lm(crim~dis)
summary(d)
e=lm(crim~indus)
summary(e)
f=lm(crim~lstat)
summary(f)
g=lm(crim~medv)
summary(g)
h=lm(crim~nox)
summary(h)
i=lm(crim~ptratio)
summary(i)
j=lm(crim~rad)
summary(j)
k=lm(crim~rm)
summary(k)
l=lm(crim~tax)
summary(l)
m=lm(crim~zn)
summary(m)

#multiple regression with all variables
total=lm(crim~., data=boston)
summary(total)

#Single VS Multi coefficient estimates
single=c(coef(a)[2], coef(b)[2],coef(c)[2],coef(d)[2],coef(e)[2],coef(f)[2],coef(g)[2],coef(h)[2],coef(i)[2],coef(j)[2],coef(k)[2],coef(l)[2], coef(m)[2])
multi=coef(total)[2:14]
par(mfrow=c(1,1))
plot(single)
plot(multi)
plot(single, multi)

#non-linearity tests
par(mfrow=c(2,2))
a1=lm(crim~poly(age, 3))
b1=lm(crim~poly(black,3))
d1=lm(crim~poly(dis, 3))
e1=lm(crim~poly(indus,3))
f1=lm(crim~poly(lstat,3))
g1=lm(crim~poly(medv,3))
h1=lm(crim~poly(nox, 3))
i1=lm(crim~poly(ptratio,3))
j1=lm(crim~poly(rad,3))
k1=lm(crim~poly(rm,3))
l1=lm(crim~poly(tax,3))
m1=lm(crim~poly(zn,3))
anova(a, a1)
anova(b, b1)
anova(d, d1)
anova(e, e1)
anova(f, f1)
anova(g, g1)
anova(h, h1)
anova(i, i1)
anova(j, j1)
anova(k, k1)
anova(l, l1)
anova(m, m1)