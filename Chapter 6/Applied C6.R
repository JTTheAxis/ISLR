#8
#Simulated Best Subset Selection
library(leaps)
set.seed(1)
x=rnorm(100)
e=rnorm(100)
y=2+x+2*x^2+4*x^3+e
data=data.frame(x, y)
y.best=regsubsets(y~poly(x, 10), data, nvmax=10)

#which can be used to return true/false vector indicating which variables are included in optimal n-variable subsets
optimal=function(model, type, func=which.min){
  model.summary=summary(model)
  model.coef=func(model.summary[[type]])
  model.which=model.summary$which[model.coef,]
  print(coef(model, model.coef))
}

optimal(y.best, "cp")
optimal(y.best, "bic")
optimal(y.best, "adjr2", func=which.max)

par(mfrow=c(2,2))
for(para in c("cp", "bic", "adjr2")){
  plot(x=summary(y.best)[[para]], xlab="Varcount", ylab=para, type="l")
}

#It can be concluded that the best model varies based on what score is being used.
#Cp: 4th degree (5 vars)
#BIC: 3rd degree (4 vars)
#Adjusted R2: 5th degree (6 vars)

#Forward Stepwise Selection
y.forward=regsubsets(y~poly(x, 10), data, nvmax=10, method="forward")
optimal(y.forward, "cp")
optimal(y.forward, "bic")
optimal(y.forward, "adjr2", func=which.max)

par(mfrow=c(2,2))
#Note: when plotting, subset the x-value with [[arg]] and not $arg. 
for(para in c("cp", "bic", "adjr2")){
  plot(x=summary(y.forward)[[para]], xlab="Varcount", ylab=para, type="l")
}

#Backward Stepwise Selection
y.backward=regsubsets(y~poly(x, 10), data, nvmax=10, method="backward")
optimal(y.backward, "cp")
optimal(y.backward, "bic")
optimal(y.backward, "adjr2", func=which.max)

par(mfrow=c(2,2))
for(para in c("cp", "bic", "adjr2")){
  plot(x=summary(y.backward)[[para]], xlab="Varcount", ylab=para, type="l")
}

#Interestingly enough, the results of forward/backward selection turn out to be the exact same as best subset.

#Lasso with CV
library(glmnet)
model=model.matrix(y~poly(x, 10), data)[, -1]
set.seed(20)
train=sample(1:100, 50)
grid=10^seq(10, -2, length=100)
y.lasso=glmnet(model[train, ], y[train], alpha=1, lambda=grid)
par(mfrow=c(1,1))
plot(y.lasso)

lassocvcoef=function(data, y, train, grid){
  #Function to streamline process of cross-validating a lasso model and returning best coefficients
  cv=cv.glmnet(data[train, ], y[train], alpha=1)
  whole=glmnet(data, y, alpha=1, lambda=grid)
  plot(cv)
  minlam=cv$lambda.min
  print(predict(whole, type="coefficients", s=minlam)[1:10, ])
}
lassocvcoef(model, y, train, grid)
#Lasso returns an optimal model of a 5th degree polynomial, with the 4th and 5th degree terms having very low values.
#This makes sense, as the true relationship involves only a third degree polynomial.
#Lasso estimates the 4th and 5th degree terms to be insignificant compared to the other terms.

#Modified Response Vector
y=100+2*x^7+e
data=data.frame(x, y)
y.best=regsubsets(y~poly(x, 10), data, nvmax=10)

optimal(y.best, "cp")
optimal(y.best, "bic")
optimal(y.best, "adjr2", func=which.max)

model=model.matrix(y~poly(x, 10), data)[, -1]
set.seed(20)
train=sample(1:100, 50)
grid=10^seq(10, -2, length=100)
y.lasso=glmnet(model[train, ], y[train], alpha=1, lambda=grid)
par(mfrow=c(1,1))
plot(y.lasso)

lassocvcoef(model, y, train, grid)

#9
set.seed(1)
dim(College)
fix(College)
names(College)
attach(College)
train=sample(1:nrow(College), nrow(College)/2)
test=(-train)
#Linear model
lm.fit=lm(Apps~., data=College, subset=train)
mean((Apps-predict(lm.fit, College))[-train]^2)

library(glmnet)
#Make sure to remove the 1st column, which is the intercept
x=model.matrix(Apps~., College)[, -1]
y=College$Apps
y.test=y[test]
#Ridge Regression
set.seed(1)
apps.ridge.cv=cv.glmnet(x[train, ], y[train], alpha=0)
l=apps.ridge.cv$lambda.min
apps.ridge.pred=predict(apps.ridge.cv, s=l, newx=x[test, ])
mean((y.test-apps.ridge.pred)^2)

#Lasso
set.seed(1)
apps.lasso.cv=cv.glmnet(x[train, ], y[train], alpha=1)
l=apps.lasso.cv$lambda.min
apps.lasso.pred=predict(apps.lasso.cv, s=l, newx=x[test, ])
mean((y.test-apps.lasso.pred)^2)
whole=glmnet(x, y, alpha=1)
apps.lasso.coef=predict(whole, type="coefficients", s=l)[1:18, ] 
apps.lasso.coef[apps.lasso.coef!=0]

library(pls)
#PCR
set.seed(1)
apps.pcr.cv=pcr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(apps.pcr.cv, val.type="MSEP")
#Lowest MSE is from M=17
apps.pcr.pred=predict(apps.pcr.cv, x[test, ], ncomp=17)
mean((y.test-apps.pcr.pred)^2)

#PCR
set.seed(10)
apps.pls.cv=plsr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
validationplot(apps.pls.cv, val.type="MSEP")
#Lowest MSE is from M=17
apps.pls.pred=predict(apps.pls.cv, x[test, ], ncomp=17)
mean((y.test-apps.pls.pred)^2)

#10
#Making the simulated data
#rbind and cbind combine the arguments as rows/columns of a matrix, respectively
#If multiple predictors are used, like in this question, then follow this procedure:
#1. Bind many generated vectors together as columns into a single matrix using cbind()
#2. Follow up and multiply both the predictor matrix and the coefficient vector together
#3. Response vector generated through step 2, add it with the error vector
#4. Use data.frame() to bind the frame of predictors and the response vector together
set.seed(100)
p=20
n=1000
vectors=c()
for (i in 1:p){
  vectors=cbind(vectors, rnorm(n, mean=sample(-100:100, 1), sd=sample(0:50, 1)))
}

e=rnorm(n)
beta=sample(0:100,p , replace=T)
beta[!sample(0:1, p, replace=T)]=0

y=vectors %*% beta + e
model=data.frame(vectors, y)

train=sample(1:1000, 100)
test=(-train)

#Best subset selection
regfit.best=regsubsets(y~., data=model[train,], nvmax=20)
#Training RSS plot
plot(c(1:20), summary(regfit.best)$rss)

test.mat=model.matrix(y~., data=model[test,])
val.errors=rep(NA, 20)
for(i in 1:20){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((model$y[test]-pred)^2)
}
#Test RSS plot
plot(c(1:20), val.errors)
which.min(val.errors)
val.errors[10]
#Minimum test error comes from the best 10 variable model.

coef(regfit.best, 10)
beta[beta!=0]

#Plotting the sqrt of the sum of the squared differences between the
#coefficients of the best subset selected of each size and the true 
#coefficient vector
means=rep(0, 20)
for (i in 1:p){
  current=rep(0, 20)
  bestsub=coef(regfit.best, i)[-1]
  included=summary(regfit.best)$which[i, -1]
  incvars=c()
  for (m in 1:20){
    if (included[m]){
      incvars=c(incvars, m)
    }
  }
  for (j in 1:i){
    current[incvars[j]]=bestsub[j]
  }
  means[i]=sqrt(sum((beta-current)^2))
}
plot(1:20, means)
