#Ridge Regression with glmnet()
x=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10, -2, length=100)
#Note: glmnet() automatically standardizes the variables.Pass argument standardize=FALSE to negate it.
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
#Coef() is used to access the coefficient estimates for every tuning value used, stored in a matrix
#Matrix has p+1 rows (per predictor, including the intercept) and 100 columns (per tuning value)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#l2 norm calculation
sqrt(sum(coef(ridge.mod)[-1,50]^2))


ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
predict(ridge.mod, s=50, type="coefficients")[1:20,]

#Validation
set.seed(14)
train=sample(1:nrow(x), nrow(x)/2)
test=-train
y.test=y[test]

ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test, ])
mean((ridge.pred-y.test)^2)

#In theory, if we fit a model with just an intercept, the test observations would have simply been computed using the mean of the training observations
mean((mean(y[train])-y.test)^2)

#Very large tuning parameter would give the same result
ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

#Least squares regression can be used to check if ridge regression is an improvement
#Least squares regression is simply ridge regression with tuning parameter=0
#Note: Exact=T is passed as an argument in order to mitigate approximation by glmnet() as it does not know whether or not the user wishes to actually use least squares or not
ridge.pred=predict(ridge.mod, s=0, newx=x[test,], x=x, y=y, exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, x=x, y=y, type="coefficients")[1:20]

#Cross-validating Ridge Regression
set.seed(14)
#built-in cv.glmnet() to cross-validate shrinkage methods; default k=10, alterable using argument nfolds=n
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#Resulting CV tuning value gives a lower MSE than using a tuning value of 4
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

#Refit regression model on full data set, using CV tuning value, and examining coefficients
out=glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20, ]

#Lasso
#Same as with ridge regression, except now we pass alpha=1 into glmnet()
lasso.mod=glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(14)
cv.out=cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out, type="coefficients", s=bestlam)[1:20, ]
lasso.coef
#Some coefficients get shrunk to 0 entirely by lasso, as expected; ridge regression never shrinks to 0
lasso.coef[lasso.coef!=0]
