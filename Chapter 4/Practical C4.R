library(ISLR)
#10
names(Weekly)
dim(Weekly)
summary(Weekly)
attach(Weekly)
cor(Weekly[,-9])
par(mfrow=c(1,1))
plot(Year, Volume)
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

#Logistic Regression
glm.probs=predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down", 1089)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction)
table(glm.pred, Direction)

#LDA
train=(Year<2009)
Weekly.test=Weekly[!train,]
dim(Weekly.test)
Direction.test=Direction[!train]
glm.fits=glm(Direction~Lag2, family=binomial, subset=train)
glm.probs=predict(glm.fits, Weekly.test, type="response")
glm.pred=rep("Down", 104)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction.test)
table(glm.pred, Direction.test)

#LDA
lda.fit=lda(Direction~Lag2, subset=train)
lda.pred=predict(lda.fit, Weekly.test)
lda.class=lda.pred$class
table(lda.class, Direction.test)
mean(lda.class==Direction.test)

#QDA
qda.fit=qda(Direction~Lag2, subset=train)
qda.pred=predict(qda.fit, Weekly.test)
qda.class=qda.pred$class
table(qda.class, Direction.test)
mean(qda.class==Direction.test)

#KNN K=1
#If only one variable is to be used for KNN classification, ensure that data.frame() is used to encapsulate train.X and test.X to have the function read these arguments with the proper number of dimensions.
attach(Weekly)
train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(data.frame(train.X), data.frame(test.X), train.Direction, k=1)
table(knn.pred, Direction.test)
mean(knn.pred==Direction.test)

#11
library(MASS)
attach(Auto)
Auto=read.csv("Auto.csv", header=TRUE, na.strings="?")
Auto=na.omit(Auto)
dim(Auto)
mpg01=rep(0, 392)
mpg01[mpg>median(mpg)]=1
Frame=c(Auto, data.frame(mpg01))

names(Frame)
plot(mpg, mpg01)
plot(cylinders, mpg01)
plot(displacement, mpg01)
plot(horsepower, mpg01)
plot(weight, mpg01)
plot(acceleration, mpg01)
plot(year, mpg01)
plot(origin, mpg01)
plot(name, mpg01)

attach(Auto)
train=(year<81)
Auto.test=Auto[!train,]
mpg01.test=mpg01[!train]
lda.fit=lda(mpg01~displacement+horsepower+acceleration+weight, subset=train)
lda.pred=predict(lda.fit, Auto.test)
lda.class=lda.pred$class
mean(lda.class==mpg01.test)
table(lda.class, mpg01.test)

qda.fit=qda(mpg01~displacement+horsepower+acceleration+weight, subset=train)
qda.pred=predict(qda.fit, Auto.test)
qda.class=qda.pred$class
mean(qda.class==mpg01.test)
table(qda.class, mpg01.test)

glm.fits=glm(mpg01~displacement+horsepower+acceleration+weight, family=binomial, subset=train)
glm.probs=predict(glm.fits, Auto.test, type="response")
glm.pred=rep(0, 54)
glm.pred[glm.probs>.5]=1
mean(glm.pred==mpg01.test)
table(glm.pred, mpg01.test)

train.X=cbind(displacement, horsepower, acceleration, weight)[train,]
test.X=cbind(displacement, horsepower, acceleration, weight)[!train,]
train.mpg=mpg01[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.mpg, k=50)
mean(knn.pred==mpg01.test)
table(knn.pred, mpg01.test)

#12
Power=function(){
  print(2^3)
}
Power()

Power2=function(b, a){
  print(b^a)
}
Power2(3, 8)
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

Power3=function(b, a){
  ans=b^a
  return(ans)
}

x=c(1:10)
par(mfrow=c(2,2))
plot(x, Power3(x, 2), xlab="x", ylab="x^2", main="f(x)=x^2")
plot(x, Power3(x, 2), xlab="x", ylab="x^2", main="f(x)=x^2", log="x")
plot(x, Power3(x, 2), xlab="x", ylab="x^2", main="f(x)=x^2", log="y")
plot(x, Power3(x, 2), xlab="x", ylab="x^2", main="f(x)=x^2", log="xy")
par(mfrow=c(1,1))

PlotPower=function(r, a){
  x=r
  s=toString(a)
  plot(x, Power3(x, a), xlab="x", ylab=paste("x^", s, sep=""), main=paste("f(x)=x^", s, sep=""))
}
PlotPower(1:10, 3)
PlotPower(1:100, 5)

#13
#To be done in the future?