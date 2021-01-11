library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])
attach(Smarket)
par(mfrow=c(1,1))
plot(Year, Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs=predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred, Direction)
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fits, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fits, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

#Specific values of lag1/lag2
predict(glm.fits, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.2005)
lda.pred
names(lda.pred)

lda.class=lda.pred$class
length(lda.class)
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
sum(lda.pred$posterior[,1]>.9)

qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class=predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

library(class)
attach(Smarket)
train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.x=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred, test.Y)
knn.pred=knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
knn.pred=knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

glm.fits=glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs=predict(glm.fits, Caravan[test,], type="response")
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred, test.Y)
glm.pred=rep("No", 1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred, test.Y)
