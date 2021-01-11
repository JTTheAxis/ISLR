#Support Vector Classifier
#e1071 library is a good general-use library
library(e1071)
#svm() and argument "kernel" set to "linear" creates support vector classifiers
#argument "cost" specifies what the value of C from the expression  for SVCs will be
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1, 10), rep(1, 10))
x[y==1,]=x[y==1,]+1
plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
#scale argument is set to TRUE if each feature is to be standardized to mean 0 and
#standard deviation 1

plot(svmfit, dat)
#svmfit$index returns the indices of the observations which are support vectors in this model
svmfit$index

summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index

#built-in cross-validation in e1071, tune() function performs 10-fold CV
set.seed(1)
tune.out=tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
#use $best.model to access the best stored model as assessed by CV
bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest, y=as.factor(ytest))

#predictions can be done as in other classification methods
ypred=predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)

#a linearly separable case
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

#less overfitting
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)

#Support Vector Machine
#svm() is still used, but with "kernel" set to different values
#"polynomial" or "radial" for their respective kernels
#degree and gamma can be set as well for their respective kernel types
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150), rep(2, 50))
dat=data.frame(x=x, y=as.factor(y))
plot(x, col=y)
train=sample(200, 100)
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)

#increasing the cost value can reduce training errors, at the risk of introducing more test error
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0,5, 1, 2, 3, 4)))
summary(tune.out)
#viewing the test predictions
table(true=dat[-train, "y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))

#ROC Curves
#ROCR package can be used to plot ROC curves
library(ROCR)
#pred refers to the predicted values on the test set
#truth is the "actual" output values of the test set
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf,...)
}

#While we normally refer to the class labels when we get outputs from SVMs,
#it is entirely possible to get the actual fitted values from the hyperplane equation as well
#we set the argument "decision.values" to true when fitting our SVM
#predict() will then output the according fitted values
svmfit.opt=svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main="Training Data")
#increased tuning value for better training accuracy
svmfit.flex=svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted,dat[train, "y"], add=T, col="red")

#test values
fitted=attributes(predict(svmfit.opt, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted=attributes(predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], add=T, col="red")

#SVM with Multiple Classes
#One-against-One approach is used
set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0, 2]=x[y==0, 2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+1))

svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit,dat)
#e1071 is also capable of handling support vector regression, if the response vector is indeed numerical

#Application to Gene Expression
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

#linear kernel should be used for this dataset
#large number of features relative to the number of observations
#Flexibility from non-linear methods will be unnecessary
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)
#Note the lack of any training errors
#Large number of variables relative to number of observations means many hyperplanes fully separate classes

dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
#cost=10 results in 2 test set errors