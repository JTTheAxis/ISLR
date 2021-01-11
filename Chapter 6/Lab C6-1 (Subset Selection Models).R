library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#Best Subset Selection
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

#plotting point singularities
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

#Regfit plot function, top row contains black squares to fill in variables included in the optimal model according to the chosen value for scaling (ex. r2, bic, etc)
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, 6)

#Forwards and Backwards Stepwise Selection
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#Validation set model selection
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,])
#model.matrix is used to build an "X" matrix from a dataset. Now, run a loop for every size i<p, and multiply them into the test model matrix to compute MSE. 
val.errors=rep(NA, 19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
min(val.errors)
#Lowest error comes from 7-predictor model
coef(regfit.best, 7)

#Defining a predict function 
predict.regsubsets=function(object, newdata, id, ...){
  #object$call returns the values used in the function call which definied the object
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

#Cross-validation implementation
k=10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

#Performing cross-validation here requires a matrix of k by p
#k rows to represent the k different folds through which CV is performed
#p columns to represent the optimal model with i<=p predictors

for(j in 1:k){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type="b")
#best model selected by cross-validation has 10 variables
reg.best=regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 10)
     