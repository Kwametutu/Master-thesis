

#############################################################
### R-code for master thesis, 22.03.2017
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
library(Matrix)
library(lfe)


pack<-c("car","sandwich","ggplot2","lmtest","ggrepel","RColorBrewer","plm",
        "dplyr","mgcv","foreign","xtable","AER","stargazer", 
        "ggrepel","lfe","gridExtra","cowplot","data.table","ggthemes")

lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
setwd(results)

##specify the path of the folder containing the results of regressions
codes="/Users/albertosei-owusu/Desktop/Data/Gravity/Master Codes"
setwd(codes)


#############################################################
### 1. Preliminaries ###
#############################################################

## 1.a Load dataset and create dummies
setwd(path)
dat = read.dta(paste0(path,"col_regfile09.dta"))




## 2.a Run regression

# RTAs info : http://rtais.wto.org/UI/PublicMaintainRTAHome.aspx

## Creating RTAs
ecowasset = c("BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
              "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER")

## countries that were part of COMESA from 1993-2006 ###
comesa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
           "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE")

sadc = c("AGO","BWA","LSO","MOZ","NAM","ZAF")

eac = c("BDI","KEN","RWA","TZA","UGA")

cemac = c("CMR","CAF","TCD","COD","COG","GNQ","GAB","STP")



##specify the path of the folder containing the results of regressions
codes="/Users/albertosei-owusu/Desktop/Data/Gravity/Master Codes"
setwd(codes)


##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)



#############################################################
### 2. Average trade creation and diversion  ###
#############################################################

# Note. It might be a good idea to consider a subset of the 
# data. E.g. choose only the data from the 1970-2000 period

## 2.a Run regression

## Intra-bloc trade dummy
dat$bothinE = ifelse(dat$iso_o %in% ecowasset & 
                       dat$iso_d %in% ecowasset &
                       dat$year >= 1975, 1, 0)

dat$bothinC = ifelse(dat$iso_o %in% comesa & 
                       dat$iso_d %in% comesa &
                       dat$year >= 1994, 1, 0)

## Extra-bloc export dummy
dat$oneinE = ifelse(dat$iso_o %in% ecowasset & 
                      !(dat$iso_d %in% ecowasset) &
                      dat$year >= 1975, 1, 0)


dat$oneinC = ifelse(dat$iso_o %in% comesa & 
                      !(dat$iso_d %in% comesa) &
                      dat$year >= 1994, 1, 0)

## Extra-bloc import dummy
dat$oneinE1 = ifelse(!(dat$iso_o %in% ecowasset) & 
                       dat$iso_d %in% ecowasset &
                       dat$year >= 1975, 1, 0)



dat$oneinC1 = ifelse(!(dat$iso_o %in% comesa) & 
                       dat$iso_d %in% comesa &
                       dat$year >= 1994, 1, 0)




#### Multilateral trade agreements from 1970-2006 ####

## Robust  regressions ###

#### Multilateral trade agreements from 1970-2006 ####

epa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
        "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE",
        "BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
        "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER","MRT",
        "BWA","LSO","MOZ","NAM","ZAF","TZA",
        "CMR","CAF","TCD","COG","GNQ","GAB","STP")


eulist= c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
          "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
          "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
          "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")



## http://trade.gov/agoa/eligibility/
## https://agoa.info/about-agoa.html

agoa = c("AGO","BEN","BWA","BFA","BDI","CIV","CMR","CPV","TCD","DJI",
         "ETH","GAB","GMB","GHA","GNB","KEN","LSO","LBR","MWI","MRT",
         "MUS","MOZ","NAM","NGA","RWA","STP","SEN","SYC","SLE","SWZ",
         "TZA","UGA","ZAF","ZMB")



# EPA extra-bloc export dummy
dat$oneinEPAX = ifelse(dat$iso_o %in% epa  & 
                         !(dat$iso_d %in% eulist) &
                         dat$year >= 2000, 1, 0)


# EPA extra-bloc import dummy
dat$oneinEPAM= ifelse(!(dat$iso_o %in% eulist)  & 
                        dat$iso_d %in% epa &
                        dat$year >= 2000, 1, 0)


# AGOA Extra-bloc export dummy 
dat$oneinAGX = ifelse(dat$iso_o %in% agoa & 
                        !(dat$iso_d %in% "USA") &
                        dat$year >= 2000, 1, 0)


# AGOA Extra-bloc import from USA
dat$oneinAGM = ifelse(!(dat$iso_o %in% "USA")  & 
                        dat$iso_d %in% agoa &
                        dat$year >= 2000, 1, 0)


# Creating new dataframe

dat1 = subset(dat,select=c(year,iso_o,iso_d,flow,gdp_o,gdp_d,distw,contig,comlang_off,col_hist,
                              bothinE,oneinE,oneinE1,bothinC,oneinC,oneinC1), flow > 0 & year %in% 1970:2006)
                           

newdata = na.omit(dat1)
summary(newdata)
names(newdata)



## OLS and FE estimation

# OLS, time and time-invariant country effects
m1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+factor(year)+
          factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1, 
        data = newdata)
summary(m1)

cov1 <- vcovHC(m1, type = "HC0", method="arellano")
robust.se1 <- sqrt(diag(cov1))


newdata$prlm1 <- exp(predict(m1, type="response"))
reslm1 <- residuals(m1, type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


compPlot(newdata$prlm1,newdata$flow,xlab="predicted",ylab="actual",
         lim=c(0,500000))

plot(newdata$prlm1,newdata$flow,
     xlab="predicted",ylab="actual", ylim=c(0,350000))
abline(a=0,b=1)


## ECOWAS
dat1 = select(subset(newdata, iso_d %in% ecowasset & 
                       newdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)


dat3 = select(subset(newdata, iso_d %in% ecowasset & 
                       newdata$iso_o %in% ecowasset),
              year, prlm1)


#### COMESA ############

dat1 = select(subset(newdata, iso_d %in% comesa & 
                       newdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(newdata, iso_d %in% comesa & 
                       newdata$iso_o %in% comesa),
              year, prlm1)


dat2$predicted = aggregate(prlm1 ~ ., data = dat3, sum)



# create country-year dummies
newdata$iso_o_yr = paste0(newdata$iso_o, newdata$year)
newdata$iso_d_yr = paste0(newdata$iso_d,newdata$year)

m11 = lm(log(flow)~log(gdp_o)+log(gdp_d)+factor(year)+
          factor(iso_o_yr)+factor(iso_d_yr)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1, 
        data = newdata)
summary(m11)



## 5.b double demeaning approach
m2 = felm(log(flow)~factor(year) +
            log(gdp_o)+log(gdp_d) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1|iso_o+iso_d,
          data = newdata)
summary(m2, robust=TRUE)



# OLS, time and time invariant country effects

# create country-year dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1|iso_o_yr+iso_d_yr,
          data = newdata)
summary(m3,robust=TRUE)


m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+bothinC|iso_o_yr+iso_d_yr,
          data = newdata)
summary(m3,robust=TRUE)



# FE, time and time invariant country effects
newdata$pairid = paste0(newdata$iso_o, newdata$iso_d)
m4 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
         model = "within",
         index=c("pairid", "year"),
         data = newdata)
summary(m4)


cov4 <- vcovHC(m4, type = "HC0", method="arellano")
robust.se4 <- sqrt(diag(cov4))

newdata$prlm4 <- exp(predict(m4, type="response"))
reslm4 <- residuals(m4, type="pearson")
hist(reslm4, prob=TRUE)
lines(density(reslm4, bw=1))
plot(ecdf(reslm4), do.points=FALSE, verticals=TRUE)


library(miscTools)
compPlot(newdata$prlm4,newdata$flow,
         xlab="predicted",ylab="actual")

plot(newdata$prlm4,newdata$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



#### ECOWAS #####

dat1 = select(subset(newdata, iso_d %in% ecowasset & 
                       newdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)


dat3 = select(subset(newdata, iso_d %in% ecowasset & 
                       newdata$iso_o %in% ecowasset),
              year, prlm4)

dat2$predicted = aggregate(prlm4 ~ ., data = dat3, sum)



#### COMESA ############

dat1 = select(subset(newdata, iso_d %in% comesa & 
                       newdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(newdata, iso_d %in% comesa & 
                       newdata$iso_o %in% comesa),
              year, prlm4)


dat2$predicted = aggregate(prlm4 ~ ., data = dat3, sum)


# FE, time and time varying country effects

m44 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
            factor(iso_o_yr)+factor(iso_d_yr)+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          model = "within",
          index=c("pairid", "year"),
          data = newdata)
summary(m4)

cov44 <- vcovHC(m44, type = "HC0", method="arellano")
robust.se44 <- sqrt(diag(cov44))



## 5.c Poisson (PPML) estimator
# time invariant country effects included (takes a long time to run)
 
# creating a new dataframe
mdata = subset(dat,select=c(year,iso_o,iso_d,flow,gdp_o,gdp_d,distw,contig,comlang_off,col_hist,
                           bothinE,oneinE,oneinE1,bothinC,oneinC,oneinC1),year %in% 1970:2006)
mdata=na.omit(mdata)



m5 = glm(flow ~ factor(year)+ factor(iso_o)+factor(iso_d)+
           log(gdp_o)+log(gdp_d) +
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1,
         family="poisson", 
        data = mdata)
summary(m5)

cov5 <- vcovHC(m5, type = "HC0", method="arellano")
robust.se5 <- sqrt(diag(cov5))



mdata$prlm5 <- predict(m5, type="response")
reslm5 <- residuals(m5, type="pearson")
hist(reslm5, prob=TRUE)
lines(density(reslm5, bw=1))
plot(ecdf(reslm5), do.points=FALSE, verticals=TRUE)


library(miscTools)
compPlot(mdata$prlm5,mdata$flow,
         xlab="predicted",ylab="actual")

plot(mdata$prlm5,mdata$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



dat1 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, prlm5)

dat2$predicted = aggregate(prlm5 ~ ., data = dat3, sum)



#### COMESA ############

dat1 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)


dat3 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, prlm5)

dat2$predicted = aggregate(prlm5 ~ ., data = dat3, sum)


# time varying country effects included (takes a very long time to run)
m6 = glm(flow ~ factor(year) + factor(iso_o_yr)+factor(iso_d_yr)+
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1,
         family="poisson",
         data = subset(newdata,year %in% 2002:2006))
summary(m6)

cov6 <- vcovHC(m6, type = "HC0", method="arellano")
robust.se6 <- sqrt(diag(cov6))



# producing a regression table
stargazer(m1,m3,m4,m5,
          type = "html", no.space = TRUE,
          se=list(robust.se1,robust.se4,robust.se5),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          omit = c("year", "iso","gdp_o","gdp_d","distw","col_hist","contig","comlang_off"),
          omit.stat = c("f", "aic", "ll"),
          out="model99.html",
          add.lines = list(c("Fixed effects", "TI", "TV", "TI", "TV", "TI")))




### ROBUST CHECKS ####

## Robust  regressions ###

#### Multilateral trade agreements from 1970-2006 ####

epa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
        "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE",
        "BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
        "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER","MRT",
        "BWA","LSO","MOZ","NAM","ZAF","TZA",
        "CMR","CAF","TCD","COG","GNQ","GAB","STP")


eulist= c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
          "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
          "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
          "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")



## http://trade.gov/agoa/eligibility/
## https://agoa.info/about-agoa.html

agoa = c("AGO","BEN","BWA","BFA","BDI","CIV","CMR","CPV","TCD","DJI",
         "ETH","GAB","GMB","GHA","GNB","KEN","LSO","LBR","MWI","MRT",
         "MUS","MOZ","NAM","NGA","RWA","STP","SEN","SYC","SLE","SWZ",
         "TZA","UGA","ZAF","ZMB")



# EPA extra-bloc export dummy
dat$oneinEPAX = ifelse(dat$iso_o %in% epa  & 
                         !(dat$iso_d %in% eulist) &
                         dat$year >= 2000, 1, 0)


# EPA extra-bloc import dummy
dat$oneinEPAM= ifelse(!(dat$iso_o %in% eulist)  & 
                        dat$iso_d %in% epa &
                        dat$year >= 2000, 1, 0)


# AGOA Extra-bloc export dummy 
dat$oneinAGX = ifelse(dat$iso_o %in% agoa & 
                        !(dat$iso_d %in% "USA") &
                        dat$year >= 2000, 1, 0)


# AGOA Extra-bloc import from USA
dat$oneinAGM = ifelse(!(dat$iso_o %in% "USA")  & 
                        dat$iso_d %in% agoa &
                        dat$year >= 2000, 1, 0)




## OLS and FE estimation

# OLS, time and time-invariant country effects
m1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+factor(year)+
          factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1+
          oneinAGX+oneinAGM +
          oneinEPAX+oneinEPAM,
        data = mdata)
summary(m1)

cov1 <- vcovHC(m1, type = "HC0", method="arellano")
robust.se1 <- sqrt(diag(cov1))


mdata$prlm1 <- exp(predict(m1, type="response"))
reslm1 <- residuals(m1, type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


library(miscTools)
compPlot(mdata$prlm1,mdata$flow,
         xlab="predicted",ylab="actual")

plot(mdata$prlm1,mdata$flow,
     xlab="predicted",ylab="actual",ylim=c(0,400000))
abline(a=0,b=1)



dat1 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, prlm1)

dat2$predicted = aggregate(prlm1 ~ ., data = dat3, sum)



#### COMESA ############

dat1 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)


dat3 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, prlm1)

dat2$predicted = aggregate(prlm1 ~ ., data = dat3, sum)




## 5.b double demeaning approach
m2 = felm(log(flow)~factor(year) +
            log(gdp_o)+log(gdp_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1+
            oneinAGX+oneinAGM +
            oneinEPAX+oneinEPAM |iso_o+iso_d,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m2, robust=TRUE)



# OLS, time and time invariant country effects

# create country-year dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1+
            oneinAGX+oneinAGM +
            oneinEPAX+oneinEPAM |iso_o_yr+iso_d_yr,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m3,robust=TRUE)


m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+bothinC+
            bothinAG+bothinEPA|iso_o_yr+iso_d_yr,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m3,robust=TRUE)


# FE, time and time invariant country effects
mdata$pairid = paste0(mdata$iso_o,mdata$iso_d)

m4 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           oneinAGX+oneinAGM +
           oneinEPAX+oneinEPAM,
         model = "within",
         index=c("pairid", "year"),
         data = mdata)
summary(m4)

cov4 <- vcovHC(m4, type = "HC0", method="arellano")
robust.se4 <- sqrt(diag(cov4))


mdata$prlm4 <- exp(predict(m4, type="response"))
reslm4 <- residuals(m4, type="pearson")
hist(reslm4, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


library(miscTools)
compPlot(mdata$prlm4,mdata$flow,
         xlab="predicted",ylab="actual")

plot(mdata$prlm4,mdata$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



dat1 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, prlm4)

dat2$predicted = aggregate(prlm4 ~ ., data = dat4, sum)



#### COMESA ############

dat1 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)


dat3 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, prlm4)

dat2$predicted = aggregate(prlm4 ~ ., data = dat3, sum)



# FE, time and time varying country effects

m44 = plm(log(flow) ~ factor(year)+
            factor(iso_o_yr)+factor(iso_d_yr)+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1+
            oneinAGX+oneinAGM +
            oneinEPAX+oneinEPAM,
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m4)

cov44 <- vcovHC(m44, type = "HC0", method="arellano")
robust.se44 <- sqrt(diag(cov44))



## 5.c Poisson (PPML) estimator
# time invariant country effects included (takes a long time to run)
m5 = glm(flow ~ factor(year)+ factor(iso_o)+factor(iso_d)+
           log(gdp_o)+log(gdp_d) +
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           oneinAGX+oneinAGM +
           oneinEPAX+oneinEPAM,
         family="poisson", 
         data = subset(dat, year %in% 1970:2006))
summary(m5)

cov5 <- vcovHC(m5, type = "HC0", method="arellano")
robust.se5 <- sqrt(diag(cov5))


# Creating new dataframe
mdata = subset(dat,select=c(year,iso_o,iso_d,flow,gdp_o,gdp_d,distw,contig,comlang_off,col_hist,
          bothinE,oneinE,oneinE1,bothinC,oneinC,oneinC1,oneinEPAX,oneinEPAM,oneinAGX,oneinAGM),
          flow > 0 & year %in% 1970:2006)                  
               

mdata=na.omit(mdata)


# time invariant country effects included (takes a very long time to run)
m6 = glm(flow ~ factor(year)+ factor(iso_o)+factor(iso_d)+
           log(gdp_o)+log(gdp_d) +
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           oneinAGX+oneinAGM +
           oneinEPAX+oneinEPAM,
         family="poisson", 
         data = mdata)
summary(m6)


cov6 <- vcovHC(m6, type = "HC0", method="arellano")
robust.se6 <- sqrt(diag(cov6))


mdata$prlm6 <- predict(m6, type="response")
reslm6 <- residuals(m6, type="pearson")
hist(reslm6, prob=TRUE)
lines(density(reslm6, bw=1))
plot(ecdf(reslm6), do.points=FALSE, verticals=TRUE)


library(miscTools)
compPlot(mdata$prlm6,mdata$flow,
         xlab="predicted",ylab="actual")

plot(mdata$prlm6,mdata$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)



dat1 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)

dat3 = select(subset(mdata, iso_d %in% ecowasset & 
                       mdata$iso_o %in% ecowasset),
              year, prlm6)

dat2$predicted = aggregate(prlm6 ~ ., data = dat3, sum)



#### COMESA ############

dat1 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

options(scipen=3)


dat3 = select(subset(mdata, iso_d %in% comesa & 
                       mdata$iso_o %in% comesa),
              year, prlm6)

dat2$predicted = aggregate(prlm6 ~ ., data = dat3, sum)



# time varying country effects included (takes a very long time to run)
m6 = glm(flow ~ factor(year) + factor(iso_o_yr)+factor(iso_d_yr)+
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1,
         family="poisson",
         data = subset(dat,year %in% 2002:2006))
summary(m6)


# producing a regression table
stargazer(m1,m3,m4,m5,
          type = "html", no.space = TRUE,
          title = "Table : Summary of robust gravity regressions",
          se=list(robust.se1,robust.se4,robust.se5),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX","COMESAM",
                             "AGOA","AGOAX","AGOAM","EPA","EPAX","EPAM"),
          omit = c("year", "iso","gdp_o","gdp_d","distw","col_hist","contig","comlang_off"),
          omit.stat = c("f", "aic", "ll","adj.rsq"),
          out="model100.html",
          add.lines = list(c("Fixed effects", "TI", "TV", "TI", "TV")))



###  Test for functional form misspecification  ###
## Ramsey???s (1969) Regression Equation Specification Error Test (RESET) ###

# Null : model is correctly specified
resettest(m1)
resettest(m3)
resettest(m4)
resettest(m5)


### Predicted vrs Observed trade flows ###
data = subset(dat, flow>0 & year %in% 1970:2006)


library(miscTools)
qflow <- exp(fitted(m1))
compPlot( qflow, data$flow )
compPlot( qflow, dat$flow, log = "xy" )


data = subset(newdata, year == 1995 & flow > 0)

ols1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
            contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          data = data)
summary(ols1)



data$prlm1 <- exp(predict(ols1, type="response"))
plot(data$prlm1,data$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


### Missing trade graphs ###

## ECOWAS

## Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/missing_trade_ecowas.csv", 
                                 col_types = cols(value = col_number()))
View(dat)

dat$value1= dat$value/1e3


tiff(filename="Missing trade ECOWAS", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,value1,colour=flow)) +
  geom_line(aes(colour = flow, group = flow)) +
  scale_y_continuous(name = "Trade flow ($US billion)") +
  xlab("Year")+
  ggtitle("Predicted Vs Actual annual intra-ECOWAS imports") +
  scale_colour_manual("Trade flow", values = c("red","green", "blue")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()



## COMESA

## Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/missing_trade_comesa.csv", 
                col_types = cols(value = col_number()))
View(dat)

dat$value1= dat$value/1e3


tiff(filename="Missing trade ECOWAS", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,value1,colour=flow)) +
  geom_line(aes(colour = flow, group = flow)) +
  scale_y_continuous(name = "Trade flow ($US billion)") +
  xlab("Year")+
  ggtitle("Predicted Vs Actual annual intra-COMESA imports") +
  scale_colour_manual("Trade flow", values = c("red","green", "blue")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()


