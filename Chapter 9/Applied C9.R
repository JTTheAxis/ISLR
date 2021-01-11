#4 
#Simulated data set with clear, non-linear separations
library(e1071)
set.seed(1)
x=matrix(rnorm(100*2), ncol=2)
y=c(rep(-1, 25), rep(1, 50), rep(-1, 25))
x[1:25,]=x[1:25,]-4
x[76:100,]=x[76:100,]+4
plot(x, col=(3-y))
train=sample(1:100, 50)
dat=data.frame(x=x, y=as.factor(y))
#linear
svmlinear=svm(y~., data=dat[train,], kernel="linear", cost=10, scale=FALSE)
plot(svmlinear, dat)
trainpred=predict(svmlinear, dat[train,])
table(y[train], trainpred)
#linear svm has a training error of 20%
ypred=predict(svmlinear, dat[-train,])
table(ypred, y[-train])
#linear svm has a test error of 30%

#non-linear (radial in this case)
svmradial=svm(y~., data=dat[train,], kernel="radial", gamma=1, scale=FALSE)
plot(svmradial, dat)
trainpred=predict(svmradial, dat[train,])
table(y[train], trainpred)
#radial svm has a training error of 0%
ypred=predict(svmradial, dat[-train,])
table(ypred, y[-train])
#radial svm has a test error of 0%

#5
#Non-linear logistic regression

#a) Generate dataset
set.seed(1)
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0)
x=data.frame(x1, x2)
data=data.frame(x=x, y=as.factor(y))
#b) Plot observations
plot(x1, x2, col=(3-y))

#c) Logistic Regression
glm.fit=glm(y~., data=data, family=binomial, subset=train)

#d) Training plot of predicted labels
glm.probs=predict(glm.fit, data, type="response")
glm.pred=rep(0, 500)
glm.pred[glm.probs>0.5]=1
plot(x1, x2, col=(3-glm.pred))

#e) Non-linear logistic regression
glm.poly=glm(y~I(x1^2)+I(x2^2)+(x1*x2), data=data, family=binomial, subset=train)

#f) Training plot of predicted labels (non-linear regression)
glm.probs=predict(glm.poly, data, type="response")
glm.pred=rep(0, 500)
glm.pred[glm.probs>0.5]=1
plot(x1, x2, col=(3-glm.pred))

#g) Support vector classifier and training plot
svmfit=svm(y~., data=data, kernel="linear", cost=10, scale=FALSE)
ypred=predict(svmfit, data)
table(ypred, y)
plot(x1, x2, col=(3-as.numeric(as.vector(ypred))))

#h) Support vector machine and training plot
svmradial=svm(y~., data=data, kernel="radial", gamma=1, scale=FALSE)
ypred=predict(svmradial, data)
table(ypred, y)
plot(x1, x2, col=(3-as.numeric(as.vector(ypred))))

#6
#Barely Linearly Separable

#a) Dataset with two barely linearly separable classes
set.seed(5)
xtrain=matrix(rnorm(50*2, sd=0.5), ncol=2)
ytrain=c(rep(-1, 25), rep(1, 25))
xtrain[ytrain==1,]=xtrain[ytrain==1,]+1
data=data.frame(x=xtrain, y=as.factor(ytrain))
plot(xtrain, col=(3-ytrain))

#b) Cross-validation
costs=c(0.001, 0.01, 0.1, 1, 5, 10, 100)
errors=rep(0, 7)
for (i in 1:length(costs)){
  svmfit=svm(y~., data=data, kernel="linear", cost=costs[i], scale=FALSE)
  pred=predict(svmfit, data)
  errors[i]=sum(ifelse(pred==ytrain, 0, 1))
}
errors
tune.out=tune(svm, y~., data=data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
#cost=1 has the best training performance
bestmod=tune.out$best.model
summary(bestmod)

#c) Test set
xtest=matrix(rnorm(50*2, sd=0.5), ncol=2)
ytest=c(rep(-1, 25), rep(1, 25))
xtest[ytest==1,]=xtest[ytest==1,]+1
testdata=data.frame(x=xtest, y=as.factor(ytest))
plot(xtest, col=(3-ytest))

errors=rep(0, 7)
for (i in 1:length(costs)){
  svmfit=svm(y~., data=testdata, kernel="linear", cost=costs[i], scale=FALSE)
  pred=predict(svmfit, data)
  errors[i]=sum(ifelse(pred==ytest, 0, 1))
}
errors

#7
#Auto and high/low gas mileage

#a) Creating the response
library(ISLR)
attach(Auto)
High=as.factor(ifelse(mpg>median(mpg), 1, 0))
auto=data.frame(Auto, High)

attach(auto)
#b) SVC and CV
linerrors=rep(0, 7)
for (i in 1:length(costs)){
  svmfit=svm(High~.-mpg, data=auto, kernel="linear", cost=costs[i])
  pred=predict(svmfit, auto)
  linerrors[i]=sum(ifelse(pred==High, 0, 1))
}
linerrors
tune.out=tune(svm, High~.-mpg, data=auto, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestlinear=tune.out$best.model

#c) SVMs and CVs
#polynomial kernel
polyerrors=rep(0, 6)
ds=c(1,2,3,4,5,6)
for (i in 1:length(ds)){
  svmpoly=svm(High~.-mpg, data=auto, kernel="polynomial", d=ds[i])
  pred=predict(svmpoly, auto)
  polyerrors[i]=sum(ifelse(pred==High, 0, 1))
}
polyerrors
tune.out=tune(svm, High~.-mpg, data=auto, kernel="polynomial", ranges=list(d=c(1,2,3,4,5,6)))
summary(tune.out)
bestpoly=tune.out$best.model

#radial kernel
raderrors=rep(0, 6)
gammas=c(0.01, 0.1, 1, 5, 10, 100)
for (i in 1:length(gammas)){
  svmradial=svm(High~.-mpg, data=auto, kernel="radial", gamma=gammas[i], cost=1)
  pred=predict(svmradial, auto)
  raderrors[i]=sum(ifelse(pred==High, 0, 1))
}
raderrors
tune.out=tune(svm, High~.-mpg, data=auto, kernel="radial", ranges=list(gamma=c(0.01, 0.1, 1, 5, 10, 100),costs=costs))
summary(tune.out)
bestrad=tune.out$best.model

#d) Plotting a few pairs of variables with the SVMs
svmfit=svm(High~., data=auto, kernel="radial", gamma=1, cost=1)
plot(svm, auto, horsepower~weight)
plot(svm, auto, horsepower~displacement)
plot(svm, auto, weight~displacement)
plot(svm, auto, acceleration~weight)
plot(svm, auto, horsepower~year)
plot(svm, auto, weight~year)
plot(svm, auto, displacement~year)
#Keep getting error when trying to plot SVM, but only with the auto dataset and not other datasets
#Fix at some point

#8
#OJ
attach(OJ)

#a) Training/test split
set.seed(13)
train=sample(1:nrow(OJ), 800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]

#b) SVC
svc=svm(Purchase~., data=OJ.train, kernel="linear", cost=0.01)
summary(svc)

#c) Training and test error
trainpred=predict(svc, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(78+57)/800
ypred=predict(svmpoly, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(25+16)/270

#d) CV
tune.out=tune(svm, Purchase~., data=OJ, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestlinear=tune.out$best.model
#best model is with cost=0.1

#e) Optimal model errors
trainpred=predict(bestlinear, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(75+61)/800
ypred=predict(bestlinear, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(25+16)/270

#f) b-e with polynomial kernel
svmpoly=svm(Purchase~., data=OJ.train, kernel="polynomial", d=3)
summary(svmpoly)

trainpred=predict(svmpoly, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(100+32)/800
ypred=predict(svmpoly, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(37+12)/270

tune.out=tune(svm, Purchase~., data=OJ, kernel="polynomial", ranges=list(d=c(1,2,3,4,5,6), cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestpoly=tune.out$best.model
#best model is with d=1, cost=10

trainpred=predict(bestpoly, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(73+57)/800
ypred=predict(bestpoly, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(25+17)/270

#g) b)-e) with radial kernel
svmrad=svm(Purchase~., data=OJ.train, kernel="radial", gamma=1)
summary(svmrad)

trainpred=predict(svmrad, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(51+38)/800
ypred=predict(svmrad, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(29+24)/270

tune.out=tune(svm, Purchase~., data=OJ, kernel="radial", ranges=list(gamma=c(0.01,0.1,1,5,10), cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestrad=tune.out$best.model
#best model is with gamma=0.01, cost=1

trainpred=predict(bestrad, OJ.train)
table(trainpred, OJ.train[, "Purchase"])
(74+58)/800
ypred=predict(bestrad, OJ.test)
table(ypred, OJ.test[, "Purchase"])
(25+17)/270
