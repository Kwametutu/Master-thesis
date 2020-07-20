
#############################################################
### R-code for master thesis, 06.04.2017
### Written by Albert Kwame Osei-Owusu
#############################################################
### Initial stuff ###

rm(list=ls())

## setting path to library
.libPaths("/Users/albertosei-owusu/Desktop/R Packages")

# load packages
library(foreign)
library(dplyr)
library(ggplot2)
library(plm)
library(stargazer)
library(ggrepel)
library(Matrix)
library(lfe)
library(data.table)

pack<-c("car","sandwich","ggplot2","lmtest","ggrepel","RColorBrewer",
        "plm","dplyr","plyr","mgcv","foreign","xtable","AER","stargazer",
        "ggrepel","lfe","gridExtra","cowplot","gplots","tseries")

lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
setwd(results)

#############################################################
### 1. Preliminaries ###
#############################################################

## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))

# number of obs.
nrow(dat)
dim(dat)

# years covered
levels(factor(dat$year))

# number of exporters
summary(levels(factor(dat$iso_o)))

#alt
length(unique(dat$iso_o))

# number of importers
levels(factor(dat$iso_d))
summary(levels(factor(dat$iso_d)))

#alt
length(unique(dat$iso_d))

# Descriptive statistics
names(dat)

# Number of observations
nrow(dat)

# Fixed effects: Heterogeneity across countries (or entities)
plotmeans(log(flow) ~ iso_o, main="Heterogeineity across countries", 
          data=subset(dat, flow > 0))

# Counting zeros
colSums(dat != 0)

dat1 <- data.frame(a = sample(1:25, 25),
                  b = rep(0, 25),
                  c = sample(1:25, 25))
nonzero <- function(x) sum(x != 0)
numcolwise(nonzero)(dat1)


# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(m6, type=c("bp"))


# Cross-sectional dependence/contemporaneous correlation
# null : Cross sectional dependence exist
pcdtest(m6, test = c("lm"))
pcdtest(m10, test = c("cd"))

# Waldtest/F-test
waldtest(m1,m2)
waldtest(mfe, mfe_int, vcov=vcovHC(mfe_int ,method="arellano"))
lrtest(m1,m2)

# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(m4, m44)


# Display the fixed effects (constants for each country)
fixef(m6) 
fixef(m10) 
 

#Scatter plot
yhat <- m1$fitted
scatterplot(yhat~log(dat$flow), 
            boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)


abline(lm(log(flow)~factor(iso_o)+factor(iso_d)+factor(year)+
            log(distw)+col_cur+contig+comlang_off+col_hist+
            bothinE + oneinE),lwd=3, col="red")



# Testing for fixed effects, null: OLS better than fixed
pFtest(m4, m1)


# Hausman Test (random vs fixed effects)  null : RE is consistent
phtest(m6, m7)
phtest(m10, m11)


# Testing for serial correlation
pbgtest(m6) 
pbgtest(m66) 
pbgtest(m8) 
pbgtest(m10) 
pbgtest(m12) 
 
# Testing for unit roots/stationarity
dat1 = subset(dat, flow>0 & year %in% 1970:2006)
Panel.set <- plm.data(dat1, index = c("pairid", "year"))
adf.test(Panel.set$flow, k=2)


# Testing for heteroskedasticity
# The null hypothesis for the Breusch-Pagan test is homoskedasticity
bptest(m6)
bptest(m66)
bptest(m8)
bptest(m10)
bptest(m12)

# Controlling for heteroskedasticity: Fixed effects
# Original coefficients
coeftest(m6) 
coeftest(m10) 

# Heteroskedasticity consistent coefficients
coeftest(m6, vcovHC)
coeftest(m10, vcovHC)

# Heteroskedasticity consistent coefficients (Arellano)
coeftest(m6, vcovHC(m6, method = "arellano"))
coeftest(m8, vcovHC(m8, method = "arellano")) 
coeftest(m10, vcovHC(m10, method = "arellano")) 
coeftest(m12, vcovHC(m12, method = "arellano"))

# Heteroskedasticity consistent coefficients, type 3
coeftest(m6, vcovHC(m6, type = "HC3")) 
coeftest(m8, vcovHC(m8, type = "HC3")) 
coeftest(m10, vcovHC(m10, type = "HC3"))
coeftest(m12, vcovHC(m12, type = "HC3")) 

