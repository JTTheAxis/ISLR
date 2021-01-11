#6c
#Simulation of Experiment
set.seed(1)
c=matrix(rnorm(50 * 1000), ncol = 50)
t=matrix(rnorm(50 * 1000), ncol = 50)
X=cbind(c, t)
#initiating linear trend across the rows
X[1, ]=seq(19, -19+.38, -.38)
pr.out=prcomp(scale(X))
summary(pr.out)$importance[, 1]
#10.7% of variation explained

X=rbind(X, c(rep(10, 50), rep(0, 50)))
pr.out=prcomp(scale(X))
summary(pr.out)$importance[, 1]
#Increased to 12.3% of variation explained when adding in which machine was used for each tissue sample
#This is because the importance of time as a predictor explains a good amount of variation in the data

