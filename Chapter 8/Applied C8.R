#7 
#Test error for various values of "mtry" and "ntree" on Boston
library(randomForest)
library(MASS)
set.seed(234)
train=sample(1:nrow(Boston), nrow(Boston)/2)
par(mfrow=c(1, 1))
plot(1:500, ylim=c(10,20), xlab="Number of Trees", ylab="Test Error", type="n")
for (i in 4:ncol(Boston)){
  errors=c()
  for (j in 1:500){
    rf.boston=randomForest(medv~., data=Boston, subset=train, mtry=i, ntree=j)
    yhat=predict(rf.boston, newdata=Boston[-train,])
    e=mean((yhat-Boston[-train, "medv"])^2)
    errors=cbind(errors, e)
    print(j)
  }
  lines(1:500, errors, lwd=i/2)
}

#8
#Sales from Carseats as a qualitative variable

#a) Split
set.seed(2)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train=Carseats[train,]
Carseats.test=Carseats[-train,]

#b) Regression tree
library(tree)
tree.carseats=tree(Sales~., Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
yhat.carseats=predict(tree.carseats, newdata=Carseats.test)
mean((yhat.carseats-Carseats.test[, "Sales"])^2)

#c) Cross-validation
cv.carseats=cv.tree(tree.carseats)
cv.carseats

#d) Bagging
bag.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=ncol(Carseats)-1, importance=TRUE)
yhat.bag=predict(bag.carseats, newdata=Carseats.test)
mean((yhat.bag-Carseats.test[, "Sales"])^2)
importance(bag.carseats)

#e) Random Forest
rf.carseats=randomForest(Sales~., data=Carseats, subset=train, importance=T)
yhat.rf=predict(rf.carseats, newdata=Carseats.test)
mean((yhat.rf-Carseats.test[, "Sales"])^2)
importance(rf.carseats)
plot(1:10, xlab="m", ylab="Test Error", type="n")
errors=c()
for (i in 1:(ncol(Carseats)-1)){
  rf.carseats=randomForest(Sales~., data=Carseats, subset=train, mtry=i, importance=T)
  yhat.rf=predict(rf.carseats, newdata=Carseats.test)
  error=mean((yhat.rf-Carseats.test[, "Sales"])^2)
  errors=cbind(errors, error)
}
lines(1:10, errors)

#9 
#OJ from ISLR

#a) Training set and test set
set.seed(1)
train=sample(1:nrow(OJ), 800)
OJ.train=OJ[train, ]
OJ.test=OJ[-train, ]

#b) Tree
tree.OJ=tree(Purchase~., data=OJ, subset=train)
summary(tree.OJ)

#c) Detail
tree.OJ

#d) Plot
plot(tree.OJ)
text(tree.OJ)

#e) Test
tree.pred=predict(tree.OJ, OJ.test, type="class")
table(tree.pred, OJ.test[, "Purchase"])

#f) Cross-Validate
cv.OJ=cv.tree(tree.OJ, FUN=prune.misclass)
cv.OJ

#g) Plot Cross-Validation
plot(cv.OJ$size, cv.OJ$dev, xlab="Tree size", ylab="Classification Error", type="l")

#i) Pruned tree
prune.OJ=prune.misclass(tree.OJ, best=2)

#j) Compare training error between pruned and unpruned
summary(tree.OJ)
summary(prune.OJ)

#k) Compare test error between pruned and unpruned
table(tree.pred, OJ.test[, "Purchase"])
error.unpruned=(16+30)/270
prune.pred=predict(prune.OJ, OJ.test, type="class")
table(prune.pred, OJ.test[, "Purchase"])
error.pruned=(29+16)/270
error.unpruned
error.pruned

#10 
#Salary from Hitters

#a) Clean and log-transform the Salary column
hit=Hitters
hit=na.omit(hit)
nrow(hit)
hit[, "Salary"]=log(hit[, "Salary"])

#b) Training and Test Split
train=1:200
hit.train=hit[train,]
hit.test=hit[-train,]

#c) Boosting and training MSE vs shrinkage value
library(gbm)
par(mfrow=c(1,2))
mse.train=c()
mse.test=c()
for (i in seq(0, 1, by=0.001)){
  boost.hit=gbm(Salary~., data=hit.train, distribution="gaussian", n.trees=1000, shrinkage=i)
  mse.train=cbind(mse.train, mean((boost.hit$train.error)^2))
  yhat.boost=predict(boost.hit, newdata=hit.test, n.trees=1000)
  mse.test=cbind(mse.test, mean((yhat.boost-hit.test[, "Salary"])^2))
}
plot(seq(0, 1, by=0.001), mse.train, xlab="Shrinkage Value", ylab="Training Error", type="l")

#d) Plotting test MSE vs shrinkage value
plot(seq(0, 1, by=0.001), mse.test, xlab="Shrinkage Value", ylab="Test Error", type="l")

#e) Comparisons to other methods (linear and lasso)
boost.hit=gbm(Salary~., data=hit.train, distribution="gaussian", n.trees=1000, shrinkage=0.1)
yhat.boost=predict(boost.hit, newdata=hit.test, n.trees=1000)
mean((yhat.boost-hit.test[, "Salary"])^2)
lm.hit=lm(Salary~., data=hit, subset=train)
yhat.lm=predict(lm.hit, newdata=hit.test)
mean((yhat.lm-hit.test[, "Salary"])^2)
library(glmnet)
grid=10^seq(10, -2, length=100)
x=model.matrix(Salary~., hit)[, -1]
y=hit.train[, "Salary"]
lasso.hit=glmnet(x[train,], y, alpha=1, lambda=grid)
cv.hit=cv.glmnet(x[train,], y, alpha=1)
bestlam=cv.hit$lambda.min
yhat.lasso=predict(lasso.hit, s=bestlam, newx=x[-train,])
mean((yhat.lasso-hit.test[, "Salary"])^2)

#f) Variable importance
summary(boost.hit)

#g) Bagging
bag.hit=randomForest(Salary~., data=hit, subset=train, mtry=ncol(hit)-1)
yhat.rf=predict(bag.hit, newdata=hit.test)
mean((yhat.rf-hit.test[, "Salary"])^2)

#11
#Caravan

#a) Splitting set
set.seed(1)
attach(Caravan)
buy=ifelse(Purchase=="Yes", 1, 0)
Caravan$Purchase=buy
train=sample(1:nrow(Caravan), 1000)
Caravan.train=Caravan[train,]
Caravan.test=Caravan[-train,]

#b) Boosting
boost.Caravan=gbm(Purchase~., data=Caravan.train, distribution="bernoulli", n.trees=1000, shrinkage=0.1)
summary(boost.Caravan)

#c) Test set and comparison with logistic regression
boost.pred=predict(boost.Caravan, Caravan.test, type="response", n.trees=1000)
buy=ifelse(boost.pred>=0.2, "Yes", "No")
#removing caravan to reset from the change to binary
remove(Caravan)
attach(Caravan)
Caravan.train=Caravan[train,]
Caravan.test=Caravan[-train,]
table(buy, Caravan.test[, "Purchase"])
48/(48+350)

#logistic regression
glm.caravan=glm(Purchase~., data=Caravan.train, family=binomial)
glm.probs=predict(glm.caravan, Caravan.test, type="response")
glm.pred=rep("No", 4822)
glm.pred[glm.probs>=0.2]="Yes"
table(glm.pred, Caravan.test[, "Purchase"])
52/(319+52)      
