library(ISLR)
attach(Wage)

#Poly Regression and Step Functions
fit=lm(wage~poly(age, 4), data=Wage)
coef(summary(fit))
#raw=TRUE in poly to use actual, non-orthogonal values
fit2=lm(wage~poly(age, 4, raw=T), data=Wage)
coef(summary(fit2))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)

fit2b=lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)

#Grid creation for age
agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds=predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
#se.bands represents the 95% confidence interval, with one of the bounds in each of its columns
#shows the 95% confint for every age

#mar and oma arguments in par() control plot margins
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
#lines draws a line through the x-values of the first arg (age.grid here), with the y-values of the second arg
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2=predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit-preds2$fit))

#anova() is used to perform an analysis of variance (with an F-test)
#Tests the null hypothesis that a simple model M1 is sufficient
#Alternate hypothesis is that a more complex model M2 is required
#M1 and M2 must be "nested" to use anova()
#In other words, M1's predictors must be a subset of M2's.

fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age, 2), data=Wage)
fit.3=lm(wage~poly(age, 3), data=Wage)
fit.4=lm(wage~poly(age, 4), data=Wage)
fit.5=lm(wage~poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

#Upon observing the resulting p-values of the comparisons between models,
#we look for comparisons with p-values that are still reasonably low
#In this case, a linear fit is insufficient, as is a quadratic. 
#Cubic and Quartic models seem to do well, but the p-value of the Quintic is too high.

coef(summary(fit.5))
#If orthogonal polynomials are used, then one can simply return the coefficient summary
#p-values will be identical, and the t-values squared are equal to the F-statistics in ANOVA

#Anova is functional regardless of the polynomials being orthogonal
#Also works when multiple predictors are in the model
fit.1=lm(wage~education+age, data=Wage)
fit.2=lm(wage~education+poly(age, 2), data=Wage)
fit.3=lm(wage~education+poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)
#CV is a good alternative to ANOVA for finding optimal degree

#Predicting wage
fit=glm(I(wage>250)~poly(age, 4), data=Wage, family=binomial)
preds=predict(fit, newdata=list(age=age.grid), se=T)
#Calculating confidence intervals is slightly more complicated, as the glm() returns logit values
#We must transform the predictions and standard errors before computing the confint
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))
#Alternatively, just use the type="response" option to return direct glm probabilities
preds=predict(fit, newdata=list(age=age.grid), type="response")
#In this case, however, the returned probs would have been negative, which makes no sense

plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0, .2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="l", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#Step function
#Use cut() 
table(cut(age, 4))
fit=lm(wage~cut(age, 4), data=Wage)
coef(summary(fit))
#Specified breakpoints can be set with breaks argument
#cut() creates an ordered variable, lm() then creates a set of dummy variables for the regression
#Plot in the same manner as the polynomial fits

#Splines
library(splines)
#Regression spline
#bs() function makes entire matrix of basis functions for splines with specified knots
#Default is cubic splines
fit=lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
pred=predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty="dashed")
lines(age.grid, pred$fit-2*pred$se, lty="dashed")

#df argument can be used to produce knots at uniform quantiles instead of predetermined ones
dim(bs(age, knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

#bs() also has degree argument for varying spline degree

#Natural splines fit with ns(), same format as bs()
fit2=lm(wage~ns(age,df=4))
pred2=predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

#smoothing spline is with smooth.spline(), same format as above two
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age, wage, df=16)
fit2=smooth.spline(age, wage, cv=TRUE)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
#Specifying df=n leads to a determined value of lambda, which results in a df of n
#Using CV leads to it selecting the lambda value which leads to minimum RSS itself

#Local regression is performed with loess()
#Note that it is necessary to use data.frame for the prediction argument with loess()
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Regression")
fit=loess(wage~age, span=.2, data=Wage)
fit2=loess(wage~age, span=.5, data=Wage)
lines(age.grid, predict(fit,data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)
#span=0.2, 0.5 leads to each neighborhood containing 20% and 50% of the data respectively
#Locfit library can be used for more local regression options

#GAMs
#Using natural splines as a predictor in GAMs can be done simply below
gam1=lm(wage~ns(year,4)+ns(age, 5)+education, data=Wage)
#However, using smoothing splines or other components will reuqire the gam library
library(gam)
#The s() function indicates a smoothing spline to be used
gam.m3=gam(wage~s(year,4)+s(age,5)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
#book is outdated, use plot.Gam, not plot.gam
#plot.Gam can be used for GAMs as well as other models
plot.Gam(gam1, se=TRUE, col="red")

#ANOVA tests an be used to determine the optimal model 
gam.m1=gam(wage~s(age, 5)+education, data=Wage)
gam.m2=gam(wage~year+s(age, 5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3)
#As we can see from ANOVA's results, a GAM with a linear year is better than no year at all
#A non-linear year is not required however.
summary(gam.m3)

#p-values for year and age represent a null hypothesis of linear model
#versus an alternative hypothesis of non-linear relationships
#predicting on gam objects
pres=predict(gam.m2, newdata=Wage)
#lo() for local regression in GAMs, same as s() for splines
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage)
plot(gam.lo, se=TRUE, col="green")
#nest multiple predictors into one lo() call to add interaction terms
gam.lo.i=gam(wage~lo(year, age, span=0.5)+education, data=Wage)

#akima library for plotting 2-d surfaces
library(akima)
plot(gam.lo.i)

#Logistic regression GAM can be fit using I() for the qualitative variable, and family=binomial
gam.lr=gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")

#<HS education category has no high-earners, so we remove it
table(education, I(wage>250))
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green")
