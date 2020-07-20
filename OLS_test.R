
#############################################################
### R-code for master thesis, 06.04.2017
### Written by Albert Kwame Osei-Owusu
#############################################################
## Linear regression 

## Basic histogram from the vector "flow". Each bin is .5 wide.
## These both result in the same output:

data=subset(dat, year %in% 1970:2006)
options(scipen = 3)
x <- data$flow
h<-hist(x, breaks=50, col="red", xlab="Trade flow", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


hist(data$flow, prob=TRUE)
lines(density(data$flow, bw=1))


# Testing for normality
qqPlot(m1)
qqPlot(m1, id.n=3)
# Look for the tails, points should be close to the line or within the confidence intervals.

shapiro.test(m1)

# Testing for heteroskedasticity, Null is constant variance
ncvTest(ols1)
ncvTest(ols2)
ncvTest(ols3)
ncvTest(ols4)
ncvTest(ols5)

ncvTest(m1)
ncvTest(m2)


# Breush/Pagan and Cook/Weisberg score test for non-constant error variance.
bptest(ols1)
bptest(ols2)
bptest(ols3)
bptest(ols4)
bptest(ols5)
bptest(m1)
bptest(m2)



# See also residualPlots(reg1).
# Quantileplots compare the Studentizedresiduals vsa t-distribution
# Other tests:shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)


# Testing for serial correlation The null hypothesis is that there is no correlation among residuals
durbinWatsonTest(ols4)

bgtest(ols1)
bgtest(ols2)
bgtest(ols3)
bgtest(ols4)
bgtest(ols5)
bgtest(m1)
bgtest(m2)


#Linear regression (heteroskedasticity-robust standard errors)
ols1$robse <- vcovHC(ols1, type="HC1") 
coeftest(ols1,ols1$robse)

ols1$robse <- vcovHC(ols1, type="HC1", method="arellano") 
coeftest(ols1,ols1$robse)

ols2$robse <- vcovHC(ols2, type="HC1") 
coeftest(ols2,ols2$robse)

ols3$robse <- vcovHC(ols3, type="HC1") 
coeftest(ols3,ols3$robse)

ols4$robse <- vcovHC(ols4, type="HC1") 
coeftest(ols4,ols4$robse)

ols5$robse <- vcovHC(ols5, type="HC1") 
coeftest(ols5,ols5$robse)

m1$robse <- vcovHC(m1, type="HC1", method="arellano") 
coeftest(m1,m1$robse)

m2$robse <- vcovHC(m2, type="HC1", method="arellano") 
coeftest(m2,m2$robse)


# Waldtest
#Robust test
waldtest(m2,m1, vcov=NeweyWest(m2, lag=3, prewhite=F))

#Standard test
waldtest(m2,m1)


# Predicted values and residuals
# Predicted values
dat$flow_hat1 <- fitted(ols1)
dat$flow_hat2 <- fitted(ols2)
dat$flow_hat2 <- fitted(m1)


# Residuals values
dat$flow_resid1 <- residuals(ols1)
dat$flow_resid2 <- residuals(ols2)


library("miscTools")
compPlot(dat$flow,dat$flow_hat1,xlab="trade flow",ylab="predicted values")

plot(data$flow,dat$flow_hat1,xlab="price",ylab="predicted values")
abline(lm(dat$log(flow)~dat$flow_hat1),col="red")

plot(data.set$price,ols$residuals,xlab="price",ylab="residuals")
abline(1,0)

### Predicted vrs Observed trade flows ###
data = subset(dat, flow>0 & year %in% 1970:2006)


library(miscTools)
qflow <- exp(fitted(m1))
compPlot( qflow, data$flow )
compPlot( qflow, dat$flow, log = "xy" )




# Diagnostics for linear regression 
residualPlots(m1)
residualPlots(m2)


# Using???income??? as is.
# Variable???income??? shows some patterns.
# Other options:

#Residuals vs fitted only
residualPlots(m1, ~ 1, fitted=TRUE)
residualPlots(m1)

# Residuals vs bothinE only
residualPlots(m1, ~ bothinE, fitted=FALSE)
residualPlots(m1, ~ bothin, fitted=FALSE) 


# What to look for: No patterns, no problems.
# All p???s should be non-significant.
# Model ok if residuals have mean=0 and variance=1 (Fox,316)
# Tukey test null hypothesis: model is additive.


# Influential variables-Added-variable plots (see next page for thegraph)
avPlots(ols1, id.n=2, id.cex=0.7)
# id.n???id most influential observation
# id.cex ???font size for id.

# Outliers ???QQ-Plots 
qqPlot(m1, id.n=3)
# id.n???id observations with high residuals


# Outliers???Bonferonnitest
outlierTest(m1)

# No Studentizedresiduals with Bonferonnip < 0.05
# Null for the Bonferonni adjusted outlier test is the observation is an outlier. 
# Here observation related to ???medical.technicians??? is an outlier.


# Test for multicollinearity
library(car)
vif(ols1)
alias(ols1)

# GVIF DfGVIF^(1/(2*Df))
# A gvif> 4 suggests collinearity.
# ???When there are strong linear relationships among the predictors in a regression analysis, 
# the precision of the estimated regression coefficients in linear models declines


