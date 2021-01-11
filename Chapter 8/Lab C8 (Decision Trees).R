#Classification Trees
library(tree)
library(ISLR)
remove(Carseats)
attach(Carseats)
set.seed(1)
High=ifelse(Sales<=8, "No", "Yes")
Carseats=data.frame(Carseats, High)
#Make sure to convert response to type factor instead of another type such as character
#Tree will be forced to coerce "NA"s if this is not done
Carseats$High=as.factor(Carseats$High)
tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)
#Plot(tree) to plot tree
#Text(tree) to add labels of each node
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales, Carseats, subset=train)
#setting predict()'s argument "type" to "class" returns class predictions
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(104+50)/200

#cv.tree() used for tree pruning
#"FUN" argument used to indicate how to guide pruning
#FUN=prune.misclass tells function to use error rate rather than deviance
set.seed(3)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
#Note that dev, despite normally referring to deviance, actually shows
#the values according to the type of value specified under "FUN"
par(mfrow=c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
#apply actual pruning with prune.misclass
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)
#testing pruned tree
tree.pred=predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(97+58)/200

#Best argument in prune.misclass() can be modified to produce differently sized pruned trees
prune.carseats=prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred=predict(prune.carseats,Carseats.test, type="class")
table(tree.pred, High.test)
(102+53)/200

#Regression Trees
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
prune.boston=prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)
 
#Predictions after CV shows that best tree is unpruned
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

#Bagging and Random Forests
library(randomForest)
set.seed(1)
#"mtry" argument indicates the value of "m" for random forests, or the number of predictors to be chosen per tree
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston
yhat.bag=predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

#"ntree" argument indicates number of trees to be grown in the forest
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag=predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#Note that bagging and random forests are identically implemented save the value of the "mtry" argument
set.seed(1)
rf.boston=randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf=predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
#Importance of variables can be shown using importance()
#Two types of importance
#One is the mean percentage decrease in accuracy in predictions when a given variable is excluded
#The other is the increase in node purity when a given variable is included
importance(rf.boston)
varImpPlot(rf.boston)

#Boosting
library(gbm)
#gbm() function used for boosting
#"distribution" argument determines what distribution to use for the tree
#Gaussian for regression, and Bernoulli for classification
set.seed(1)
boost.boston=gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)
summary(boost.boston)
#Summary of a boosted forest returns each variable's influence on the tree's decisions
#Goes along with a bar graph

#Plotting partial dependence plots can be done with plot() and setting "i" equal to the desired variable
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost=predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

#Shrinkage parameter for boosting can be set using "shrinkage" argument, with default 0.001
boost.boston=gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost=predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
