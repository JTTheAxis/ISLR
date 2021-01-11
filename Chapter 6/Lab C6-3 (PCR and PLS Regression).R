#PCR
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
#Setting scale=TRUE standardizes each predictor as well
summary(pcr.fit)
#Setting validation="CV" results in k=10 CV

#validation plot with val.type="MSEP" plots the CV MSE
validationplot(pcr.fit, val.type="MSEP")

#Training PCR
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

#MSE calculation
pcr.pred=predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)

#Full data set
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

#PLS
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

pls.pred=predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

#7-component PCR fit explained the same amount of variance in Salary as 2-component PLS fit
#This is becuse PCR only maximizes explanation of predictors, while PLS focuses on both predictors and response
