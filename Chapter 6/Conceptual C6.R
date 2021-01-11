#6 

#Ridge Regression
y=10
lambda=3
x=seq(-10, 10, 0.1)
ridge=100-20*x+4*x^2
plot(x, ridge)
min=y/(1+lambda)
points(min, (y-min)^2+lambda*min^2, col="red", pch=4, lwd=5)
#Confirmed that the minimized coefficient formula works

#Lasso
#Case 1: Large Observation (+)
y=10
lambda=5
x=c(-10:10)
lasso=100-20*x+x^2+5*abs(x)
plot(x, lasso)
points(7.5, 43.75, col="red")

#Case 2: Large Observation (-)
y=-10
lambda=5
x=c(-10:10)
lasso=100+20*x+x^2+5*abs(x)
plot(x, lasso)
points(-7.5, 43.75, col="red")

#Case 3: Small Observation (+)
y=2
lambda=5
x=c(-10:10)
lasso=4-4*x+x^2+5*abs(x)
plot(x, lasso)
points(0, 4, col="red")

#Case 4: Small Observation (-)
y=-2
lmabda=5
x=c(-10:10)
lasso=4+4*x+x^2+5*abs(x)
plot(x, lasso)
points(0, 4, col="red")
