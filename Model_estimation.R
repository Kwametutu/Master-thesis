

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
dat <- read_dta("col_regfile09.dta")
View(dat)


## 2.a Run regression

# RTAs info : http://rtais.wto.org/UI/PublicMaintainRTAHome.aspx

## Creating RTAs
ecowasset = c("BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
              "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER")

## countries that were part of COMESA from 1993-2006 ###
comesa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
           "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE")

comesa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
           "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
           "UGA","ZMB","ZWE")

sadc = c("AGO","BWA","LSO","MOZ","NAM","ZAF")

eac = c("BDI","KEN","RWA","TZA","UGA")

cemac = c("CMR","CAF","TCD","COD","COG","GNQ","GAB","STP")
            

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




##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)



#############################################################
### OLS Regressions ###
#############################################################

# Cross-section gravity equation estimation
ols1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
            contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          data = subset(dat, year == 1995 & flow > 0))
summary(ols1)


cov1 <- vcovHC(ols1, type = "HC", method="arellano")
robust.se1 <- sqrt(diag(cov1))

coef1=coeftest(ols1)[,1]

plot(lm(log(flow) ~ 1, data = subset(dat, year == 1995 & flow > 0)))
prlm1 <- exp(predict(ols1, type="response"))
reslm1 <- residuals(ols1, type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)





ols2 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
            contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          data = subset(dat, year == 2000 & flow > 0))
summary(ols2)

cov2 <- vcovHC(ols2, type = "HC", method="arellano")
robust.se2 <- sqrt(diag(cov2))

coef2=coeftest(ols2)[,1]


ols3 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
            contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          data = subset(dat, year == 2002 & flow > 0))
summary(ols3)

cov3 <- vcovHC(ols3, type = "HC", method="arellano")
robust.se3 <- sqrt(diag(cov3))

coef3=coeftest(ols3)[,1]


ols4 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
            contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          data = subset(dat, year == 2006 & flow > 0))
summary(ols4)

cov4 <- vcovHC(ols4, type = "HC", method="arellano")
robust.se4 <- sqrt(diag(cov4))

coef4=coeftest(ols4)[,1]


## Presenting results for selected years
stargazer(ols1,ols2,ols3,ols4, header=FALSE, type='html', 
          title = "Table 1 : Simple Cross-sectional OLS estimation of log-linear gravity model",
          se=list(robust.se1,robust.se2,robust.se3,robust.se4),
          no.space = TRUE,
          align = TRUE,
          column.labels=c("1995","2000","2002","2004","2006"),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          out="ols_model1.html",
          omit.stat= c("adj.rsq", "f", "ser"))



###############################################################
### OLS Regressions with time invariant country fixed effects ###
###############################################################

ols1 = lm(log(flow)~factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, year == 1995 & flow > 0))
summary(ols1)

cov1 <- vcovHC(ols1, type = "HC", method="arellano")
robust.se1 <- sqrt(diag(cov1))

coef1=coeftest(ols1)[,1]


ols2 = lm(log(flow)~factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, year == 2000 & flow > 0))
summary(ols2)

cov2 <- vcovHC(ols2, type = "HC", method="arellano")
robust.se2 <- sqrt(diag(cov2))

coef2=coeftest(ols2)[,1]


ols3 = lm(log(flow)~factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, year == 2002 & flow > 0))
summary(ols3)

cov3 <- vcovHC(ols3, type = "HC", method="arellano")
robust.se3 <- sqrt(diag(cov3))

coef3=coeftest(ols3)[,1]


ols4 = lm(log(flow)~factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, year == 2006 & flow > 0))
summary(ols4)

cov4 <- vcovHC(ols4, type = "HC", method="arellano")
robust.se4 <- sqrt(diag(cov4))

coef4=coeftest(ols4)[,1]



## Presenting results for selected years
stargazer(ols1,ols2,ols3,ols4, header=FALSE, type='html', 
          title = "Table 2: Simple Cross-sectional OLS estimation of log-linear gravity model with time invariant country fixed effects",
          se=list(robust.se1,robust.se2,robust.se3,robust.se4),
          no.space = TRUE,
          align = TRUE,
          column.labels=c("1995","2000","2002","2006"),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Distance","Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("distw","contig","comlang_off","col_hist",
                   "bothinE","oneinE","oneinE1","bothinC","oneinC","oneinC1"),
          out="ols_model2.html",
          omit.stat= c("adj.rsq", "f", "ser"))



#############################################################
### OLS Regressions with fixed effects ###
#############################################################

# A) OLS without time/year fixed effects
m1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
           contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m1)

cov1 <- vcovHC(m1, type = "HC0", method="arellano")
robust.se1 <- sqrt(diag(cov1))

plot(lm(flow ~ 1, data = subset(dat, flow>0 & year %in% 1970:2006)))
prlm1 <- exp(predict(m1 , type="response"))
reslm1 <- residuals(m1 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


View(prlm1)


# B) OLS with time/year fixed effects
m2 = lm(log(flow)~log(gdp_o)+log(gdp_d)+factor(year)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m2)

cov2 <- vcovHC(m2, type = "HC0", method="arellano")
robust.se2 <- sqrt(diag(cov2))


plot(lm(flow ~ 1, data = subset(dat, flow>0 & year %in% 1970:2006)))
prlm1 <- exp(predict(m2 , type="response"))
reslm1 <- residuals(m2 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


# C) OLS without year fixed effects but time invariant importer, exporter effects
m3 = lm(log(flow)~log(gdp_o)+log(gdp_d)+
          factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1, 
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m3)


cov3 <- vcovHC(m3, type = "HC0", method="arellano")
robust.se3 <- sqrt(diag(cov3))


plot(lm(flow ~ 1, data = subset(dat, flow>0 & year %in% 1970:2006)))
prlm1 <- exp(predict(m3 , type="response"))
reslm1 <- residuals(m3 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



# D) OLS with year fixed effects and time invariant importer, exporter effects
m4 = lm(log(flow)~log(gdp_o)+log(gdp_d)+
          factor(year)+factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m4)

cov4 <- vcovHC(m4, type = "HC0", method="arellano")
robust.se4 <- sqrt(diag(cov4))


plot(lm(flow ~ 1, data = subset(dat, flow>0 & year %in% 1970:2006)))
prlm1 <- exp(predict(m4 , type="response"))
reslm1 <- residuals(m4 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


# E) OLS with year and time varying importer, exporter fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m5 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
          log(distw) + contig + comlang_off + col_hist +
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m5)

m5 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
          log(distw) + contig + comlang_off + col_hist +
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1,
        data = subset(dat, flow>0 & year %in% 2000:2006))
summary(m5)

cov5 <- vcovHC(m5, type = "HC0", method="arellano")
robust.se5 <- sqrt(diag(cov5))


plot(lm(flow ~ 1, data = subset(dat, flow>0 & year %in% 1970:2006)))
prlm1 <- exp(predict(m5 , type="response"))
reslm1 <- residuals(m5 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



# Omitting NAs
dat1=na.omit(dat)

## double demeaning
m55 = felm(log(flow)~factor(year)+log(distw)+
              +contig+comlang_off+col_hist+
              bothinE+oneinE+oneinE1+
              bothinC+oneinC+oneinC1 | iso_o_yr + iso_d_yr,
            data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m55,robust=TRUE)


m55 = felm(log(flow)~log(distw)+
             +contig+comlang_off+col_hist+
             bothinE+bothinC | iso_o_yr + iso_d_yr,
           data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m55,robust=TRUE)


stargazer(m55, header=FALSE, type='text', 
          title = "Table 3: OLS gravity models of trade",
          no.space = TRUE,
          dep.var.labels=c("Trade flow"),
          keep = c("distw",
                   "contig","comlang_off","col_hist",
                   "bothinE",
                   "bothinC"),
          covariate.labels=c("Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","COMESA"),
          out="model77.txt",
          omit.stat= c("adj.rsq", "f", "ser"),
          add.lines = list(c("Fixed effects", "None", "Year", "TI", "TV","TV")))                         



## Present results
stargazer(m1,m2,m4, header=FALSE, type='html', 
          title = "Table 3: OLS gravity models of trade",
          se=list(robust.se1,robust.se2,robust.se4),
          no.space = TRUE,
          dep.var.labels=c("Trade flow"),
          keep = c("gdp_o","gdp_d","distw",
                   "contig","comlang_off","col_hist",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          out="model3.html",
          omit.stat= c("adj.rsq", "f", "ser"),
add.lines = list(c("Fixed effects", "None", "Year", "TI", "TV")))



#############################################################
### Panel Regressions  ###
#############################################################



# D) Within with year/time fixed effects

# country "x" time effects
# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m6 = plm(log(flow) ~ factor(year) +
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
           model = "within",
           index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m6)

cov6 = vcovHC(m6,type = "HC0", method = "arellano")
robust.se6 <- sqrt(diag(cov6))


prlm1 <- exp(predict(m6 , type="response"))
reslm1 <- residuals(m6 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)

plot(data$prlm1,data$flow,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


# Within without year/time fixed effects
m66 = plm(log(flow)~ bothinE+oneinE+oneinE1+
                     bothinC+oneinC+oneinC1, 
                     model = "within",
                     index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m66)


cov66 = vcovHC(m66,type = "HC0", method = "arellano")
robust.se66 <- sqrt(diag(cov66))



prlm1 <- predict(m66 , type="response")
reslm1 <- residuals(m66 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(m6, m66)

# Hausman Test (random vs fixed effects)  null : RE is consistent
phtest(m6, m7)



# E) Within with year fixed effects and time varying country fixed effects
# Note: this takes forever to run

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m7 = plm(log(flow)~ factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
           log(distw) + contig + comlang_off + col_hist +
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
           model = "within",
           index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m7)

m7 = plm(log(flow)~ factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
                    bothinE+oneinE+oneinE1+
                    bothinC+oneinC+oneinC1, 
                    model = "within",
                    index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 2000:2006))
summary(m7)

cov7 = vcovHC(m7,type = "HC0",type = "HC0", method = "arellano")
robust.se7 <- sqrt(diag(cov7))


# F) Within with time/year fixed effects and GDP controls
m8 = plm(log(flow) ~ factor(year)+ log(gdp_o)+log(gdp_d)+
                     bothinE+oneinE+oneinE1+
                     bothinC+oneinC+oneinC1, 
                     model = "within",
                     index=c("pairid", "year"),
                  data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m8)


cov8 = vcovHC(m8,type = "HC0", method = "arellano")
robust.se8 <- sqrt(diag(cov8))


prlm1 <- predict(m8 , type="response")
reslm1 <- residuals(m8 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



##Change working directory
path=setwd(results)


## Present results

stargazer(m66,m6,m8,header=FALSE, type='html', 
          title = "Table 4: Static Panel gravity regressions",
          se=list(robust.se66,robust.se6,robust.se8),
          no.space = TRUE,
          align = TRUE,
          dep.var.labels=c("Trade flow"),
         covariate.labels=c("Exporter income","Importer income",
                             "ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("gdp_o","gdp_d",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model4.html",
          omit.stat= c("adj.rsq", "f", "ser"),
add.lines = list(c("Fixed effects", "TI", "Year,TI", "TI,GDPs")))



stargazer(m66,m6,m8,header=FALSE, type='html', 
          title = "Table 4: Static Panel gravity regressions",
          se=list(robust.se6,robust.se66,robust.se8),
          no.space = TRUE,
          align = TRUE,
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Exporter income","Importer income",
                             "ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("gdp_o","gdp_d",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model40.html",
          omit.stat= c("adj.rsq", "f", "ser"),
add.lines = list(c("Fixed effects", "TI","Year,TI", "TI,GDPs")))




##### Panel Regression #####
## Robust  regression ###

#### Multilateral trade agreements from 1970-2006 ####

epa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
        "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE",
        "BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
        "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER","MRT",
        "BWA","LSO","MOZ","NAM","ZAF","TZA",
        "CMR","CAF","TCD","COG","GNQ","GAB","STP",
        "AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
        "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
        "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
        "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

Afepa = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
        "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE",
        "BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
        "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER","MRT",
        "BWA","LSO","MOZ","NAM","ZAF","TZA",
        "CMR","CAF","TCD","COG","GNQ","GAB","STP")


eu28 = c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
         "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
         "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
         "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

## http://trade.gov/agoa/eligibility/
## https://agoa.info/about-agoa.html

agoa = c("USA","AGO","BEN","BWA","BFA","BDI","CIV","CMR","CPV","TCD","DJI",
         "ETH","GAB","GMB","GHA","GNB","KEN","LSO","LBR","MWI","MRT",
         "MUS","MOZ","NAM","NGA","RWA","STP","SEN","SYC","SLE","SWZ",
         "TZA","UGA","ZAF","ZMB")


# EPA intra-bloc dummy
dat$bothinEPA = ifelse(dat$iso_o %in% epa & 
                        dat$iso_d %in% epa &
                        dat$year >= 2000, 1, 0)


# EPA extra-bloc export dummy
dat$oneinEPAX = ifelse(dat$iso_o %in% epa  & 
                        !(dat$iso_d %in% epa) &
                        dat$year >= 2000, 1, 0)


# EPA extra-bloc import dummy
dat$oneinEPAM= ifelse(!(dat$iso_o %in% epa)  & 
                        dat$iso_d %in% epa &
                        dat$year >= 2000, 1, 0)

# AGOA Intra-bloc dummy
dat$bothinAG = ifelse(dat$iso_o %in% agoa & 
                         dat$iso_d %in% agoa &
                         dat$year >= 2000, 1, 0)


# AGOA Extra-bloc export dummy 
dat$oneinAGX = ifelse(dat$iso_o %in% agoa & 
                         !(dat$iso_d %in% agoa) &
                         dat$year >= 2000, 1, 0)



# AGOA Extra-bloc import from USA
dat$oneinAGM = ifelse(!(dat$iso_o %in% agoa)  & 
                         dat$iso_d %in% agoa &
                         dat$year >= 2000, 1, 0)




eu_to_acp
acp_to_eu


# D) Within with year/time fixed effects

# country "x" time effects
# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m6 = plm(log(flow) ~ factor(year) +
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           bothinAG+oneinAGX+oneinAGM +
           bothinEPA+oneinEPAX+oneinEPAM,
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m6)

cov6 = vcovHC(m6,type = "HC0", method = "arellano")
robust.se6 <- sqrt(diag(cov6))


prlm1 <- predict(m6 , type="response")
reslm1 <- residuals(m6 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



# Within without year/time fixed effects
m66 = plm(log(flow)~ bothinE+oneinE+oneinE1+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1+
            bothinAG+oneinAGX+oneinAGM +
            bothinEPA+oneinEPAX+oneinEPAM,
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m66)

cov66 = vcovHC(m66,type = "HC0", method = "arellano")
robust.se66 <- sqrt(diag(cov66))



# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(m6, m66)

# Hausman Test (random vs fixed effects)  null : RE is consistent
phtest(m6, m7)



prlm1 <- predict(m66 , type="response")
reslm1 <- residuals(m66 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)




# E) Within with year fixed effects and time varying country fixed effects
# Note: this takes forever to run

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)


m7 = plm(log(flow)~ factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
           log(distw) + contig + comlang_off + col_hist +
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           bothinAG+oneinAGX+oneinAGM +
           bothinEPA+oneinEPAX+oneinEPAM,
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat,type = "HCO", flow>0 & year %in% 1970:2006))
summary(m7)



## double demeaning
m77 = felm(log(flow)~factor(year)+log(distw)+
             +contig+comlang_off+col_hist+
             bothinE+oneinE+oneinE1+
             bothinC+oneinC+oneinC1+
             bothinAG+oneinAGX+oneinAGM +
             bothinEPA+oneinEPAX+oneinEPAM | iso_o_yr + iso_d_yr,
           data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m77,robust=TRUE)



# F) Within with time/year fixed effects and GDP controls
m8 = plm(log(flow) ~ factor(year)+ log(gdp_o)+log(gdp_d)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           bothinAG+oneinAGX+oneinAGM +
           bothinEPA+oneinEPAX+oneinEPAM,
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m8)


cov8 = vcovHC(m8,type = "HC0", method = "arellano")
robust.se8 <- sqrt(diag(cov8))



prlm1 <- predict(m8 , type="response")
reslm1 <- residuals(m8 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)




##Change working directory
path=setwd(results)


## Present results

stargazer(m66,m6,m8,
          type = "html", no.space = TRUE,
          title = "Table 4: Static Panel gravity regressions",
          se=list(robust.se66,robust.se6,robust.se8),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX","COMESAM",
                             "AGOA","AGOAX","AGOAM","EPA","EPAX","EPAM"),
          omit = c("year", "iso","gdp_o","gdp_d","distw","contig","comlang_off","col_hist"),
          omit.stat = c("f", "aic", "ll"),
          out="model42.html",
          add.lines = list(c("Fixed effects", "None", "TI,year", "TI,year,gdps")))





#############################################################
### 3. Evolution of trade creation and diversion ###
#############################################################

## 3.a Introduce interaction terms
m9 = plm(log(flow) ~ log(distw) + contig + comlang_off + col_hist +
           factor(year) +
           factor(year):bothinE +
           factor(year):oneinE +
           factor(year):oneinE1 +
           factor(year):bothinC +
           factor(year):oneinC +
           factor(year):oneinC1,           
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m9)


stargazer(m9, title="Results", 
          dep.var.labels="Trade flow", 
          style="aer",
          align=TRUE,
          type = "text")

# Setting path for figures
graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures"
setwd(graphs)

## 3.b Plot results
# Evolution of intra-bloc trade
plot(1975:2006, (coef(m9)[37:68]+coef(m9)[5:36]), type = "b",
     xlab = "year",
     ylab = "bothinE coefficient",
     main = "Evolution of the ECOWAS intra-bloc trade coefficient")


plot(1994:2006, (coef(m9)[133:145]+coef(m9)[24:36]), type = "b",
     xlab = "year",
     ylab = "bothinC coefficient",
     main = "Evolution of the COMESA intra-bloc trade coefficient")


# Evolution of extra-bloc export
plot(1975:2006, (coef(m9)[69:100]+coef(m9)[5:36]), type = "b",
     xlab = "year",
     ylab = "oneinE coefficient",
     main = "Evolution of the ECOWAS extra-bloc export coefficient")

plot(1994:2006, (coef(m9)[146:158]+coef(m9)[24:36]), type = "b",
     xlab = "year",
     ylab = "oneinC coefficient",
     main = "Evolution of the COMESA extra-bloc export coefficient")

# Evolution of extra-bloc import
plot(1975:2006, (coef(m9)[101:132]+coef(m9)[5:36]), type = "b",
     xlab = "year",
     ylab = "oneinE1 coefficient",
     main = "Evolution of the ECOWAS extra-bloc import coefficient")

plot(1994:2006, (coef(m9)[159:171]+coef(m9)[24:36]), type = "b",
     xlab = "year",
     ylab = "oneinC1 coefficient",
     main = "Evolution of the COMESA extra-bloc import coefficient")



## Plots ##
########## ECOWAS ##################

### alt
dat4 <- data.frame( year=c(1975:2006),
                    bothinE = c(coef(m9)[37:68]+coef(m9)[5:36]),
                    oneinE = c(coef(m9)[69:100]+coef(m9)[5:36]),
                    oneinE1 =c(coef(m9)[101:132]+coef(m9)[5:36]))

## transposing dataframe
dat5 <- t(dat4)
View(dat5)

## Multiple line of trade creation and diversion
tiff(filename="ECOWAS_TCTD.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")
14.1705

lines(plot(dat4$year,dat4$bothinE, type = "l",
           xlab = "year",
           ylab = "coefficients",ylim=c(0.861,2.98),
           main = "Evolution of trade creation and trade diversion of ECOWAS",
           col= "red"))
lines(dat4$year,dat4$oneinE,type="l",col="blue")
lines(dat4$year,dat4$oneinE1,type="l",col="black")

legend("topleft", 
       border="black",col=c("red","blue","black") ,
       lty=c(1,1),
       legend=c("Intra-bloc trade","Extra-bloc export", "Extra-bloc import"),
       bg ="white")

dev.off()



########## COMESA ##################

### alt
dat6 <- data.frame( year=c(1994:2006),
                    bothinC = c(coef(m9)[133:145]+coef(m9)[24:36]),
                    oneinC =  c(coef(m9)[146:158]+coef(m9)[24:36]),
                    oneinC1 = c(coef(m9)[159:171]+coef(m9)[24:36]))

## transposing dataframe
dat7 <- t(dat6)
View(dat7)

## Multiple line of trade creation and diversion

tiff(filename="COMESA_TCTD.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

lines(plot(dat6$year,dat6$bothinC, type = "l",
           xlab = "year", ylim = c(1.785,3.035),
           ylab = "coefficients",
           main = "Evolution of trade creation and trade diversion of COMESA",
           col= "red"))
lines(dat6$year,dat6$oneinC,type="l",col="blue")
lines(dat6$year,dat6$oneinC1,type="l",col="black")

legend("topleft", 
       border="black",col=c("red","blue","black") ,
       lty=c(1,1),
       legend=c("Intra-bloc trade","Extra-bloc export", "Extra-bloc import"),
       bg ="white")

dev.off()


setwd(results)



#############################################################
### 4. PPML estimator of average trade creation and diversion  ###
#############################################################

# read more: http://personal.lse.ac.uk/tenreyro/lgw.html

m20 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            factor(iso_o)+factor(iso_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year==1995),family="poisson")
summary(m20)

cov20 <- vcovHC(m20, type = "HC0", method="arellano")
robust.se20 <- sqrt(diag(cov20))

m21 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            factor(iso_o)+factor(iso_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year==2000), family="poisson")
summary(m21)


cov21 <- vcovHC(m21, type = "HC0", method="arellano")
robust.se21 <- sqrt(diag(cov21))



m22 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            factor(iso_o)+factor(iso_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year==2002), family="poisson")
summary(m22)


cov22 <- vcovHC(m22, type = "HC0", method="arellano")
robust.se22 <- sqrt(diag(cov22))


m23 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            factor(iso_o)+factor(iso_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year==2004), family="poisson")
summary(m23)

cov23<- vcovHC(m20, type = "HC0", method="arellano")
robust.se23 <- sqrt(diag(cov23))


m24 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            factor(iso_o)+factor(iso_d) + 
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year==2006), family="poisson")
summary(m24)

cov24 <- vcovHC(m24, type = "HC0", method="arellano")
robust.se24 <- sqrt(diag(cov24))


## Presenting results for selected years
stargazer(m20,m21,m22,m23,m24, header=FALSE, type='html', 
          title = "Table 5 : Poisson Pseudo-maximum likelihood estimator (PPML) of log-linear gravity model",
          no.space = TRUE,
          column.labels=c("1995","2000","2002","2004","2006"),
          se=list(robust.se20,robust.se21,robust.se22,robust.se23,robust.se24),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("gdp_o","gdp_d","distw",
                   "contig","comlang_off","col_hist",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model50.html",
          omit.stat= c("adj.rsq", "f", "ser"))



stargazer(m20,m21,m22, header=FALSE, type='html', 
          title = "Table 5 : Poisson Pseudo-maximum likelihood estimator (PPML) of log-linear gravity model",
          no.space = TRUE,
          column.labels=c("1995","2000","2002"),
          se=list(robust.se20,robust.se21,robust.se22),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("gdp_o","gdp_d","distw",
                   "contig","comlang_off","col_hist",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model5.html",
          omit.stat= c("adj.rsq", "f", "ser"))



# A) PPML without importer, exporter and year fixed effects
m30 = glm(flow ~ log(gdp_o) + log(gdp_d) +
            log(distw)+contig+comlang_off+col_hist+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year %in% 1970:2006), family="poisson")
summary(m30)

cov30 <- vcovHC(m30, type = "HC0", method="arellano")
robust.se30 <- sqrt(diag(cov30))



prlm1 <- predict(m30 , type="response")
reslm1 <- residuals(m30 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)



# B) PPML with and time/year and time invariant importer, exporter fixed effects
m31 = glm(flow~ log(gdp_o)+log(gdp_d) +
            log(distw)+contig+comlang_off+col_hist+
            factor(year)+factor(iso_o)+factor(iso_d)+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year %in% 1970:2006), family = "poisson")
summary(m31)

cov31 <- vcovHC(m31, type = "HC0", method="arellano")
robust.se31 <- sqrt(diag(cov31))


prlm1 <- predict(m31 , type="response")
reslm1 <- residuals(m31 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)




# C) PPML with time varying importer, exporter fixed effects
# and year fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m32 = glm(flow~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
            log(distw) + contig + comlang_off + col_hist +
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year %in% 2002:2006), family = poisson())
summary(m32)

cov32 <- vcovHC(m32, type = "HC0", method="arellano")
robust.se32 <- sqrt(diag(cov32))


prlm1 <- predict(m32 , type="response")
reslm1 <- residuals(m32 , type="pearson")
hist(reslm1, prob=TRUE)
lines(density(reslm1, bw=1))
plot(ecdf(reslm1), do.points=FALSE, verticals=TRUE)


# D) With year fixed effects
m33 = glm(flow~ log(gdp_o) + log(gdp_d) + factor(year) +
            log(distw) + contig + comlang_off + col_hist +
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1,
          data = subset(dat,year %in% 1970:2006), family = "poisson")
summary(m33)


cov33 <- vcovHC(m33, type = "HC0", method="arellano")
robust.se33 <- sqrt(diag(cov33))


# Present results
stargazer(m30,m33,m31,m32, header=FALSE, type='html', 
          title = "Table 6:Poisson Pseudo-maximum likelihood estimator (PPML) of log-linear gravity model",
          no.space = TRUE,
          dep.var.labels=c("Trade flow"),
          se=list(robust.se30,robust.se33,robust.se31,robust.se32),
          keep = c("gdp_o","gdp_d","distw",
                   "contig","comlang_off","col_hist",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          out="model6.html",
          omit.stat = c("f", "aic", "ll"),
          add.lines = list(c("Fixed effects", "None", "Year", "TI", "TV")))



#### GMM Estimations ####

# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

# A) Within with time/year fixed effects and GDP controls
m1 = plm(log(flow) ~ lag(log(flow))+ factor(year)+
           log(gdp_o)+log(gdp_d)+ 
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m1)


cov1 = vcovHC(m1,type = "HC0", method = "arellano")
robust.se1 <- sqrt(diag(cov1))




# B) Within with year fixed effects and time varying country fixed effects
# Note: this takes forever to run

m2 = plm(log(flow) ~ lag(log(flow))+factor(iso_o_yr)+
           +factor(iso_d_yr)+factor(year)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1990:2006))
summary(m2)

cov2 = vcovHC(m2,type = "HC0", method = "arellano")
robust.se2 <- sqrt(diag(cov2))



## Present results
stargazer(m1,m2,header=FALSE, type='html', 
          title = "Table 4: Static Panel gravity regressions",
          se=list(robust.se1,robust.se2),
          no.space = TRUE,
          align = TRUE,
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("flow_t-1","Exporter income","Importer income","",
                             "ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX", "COMESAM"),
          keep = c("lag(log(flow),1)","gdp_o","gdp_d",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model41.html",
          omit.stat= c("adj.rsq", "f", "ser"))



######## GMM ###########

### Fixed effects ###
m3 = pgmm(log(flow) ~ lag(log(flow)) + factor(year) +
                  log(gdp_o) + log(gdp_d) + 
                  bothinE+oneinE+oneinE1+
                  bothinC+oneinC+oneinC1 | lag(log(flow),2:5),
          transformation = "d",
          index=c("year","pairid"),
     data = subset(dat,flow>0 & year %in% 2000:2006))
summary(m3)

cov3 = vcovHC(m3,type = "HC0", method = "arellano")
robust.se3 <- sqrt(diag(cov3))



m5 = pgmm(log(v)~lag(log(v))+log(china_x)+log(gdp_i)+log(pop_i)+log(gdp_j)+log(pop_j) |
            lag(log(v),2:5),
          data = dat[iso3_j != "CHN" & v !=0 & china_x != 0 & eac == 1,],
          transformation = "d",
          index = c("year", "cpair"))


### Difference GMM ###
m4 = pgmm(log(flow) ~ lag(log(flow)) + factor(year) +
            log(gdp_o)+log(gdp_d)+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1 | lag(log(flow),2:5),
          transformation = "d",
          effect = "twoways", model = "twosteps",
          index=c("pairid", "year"),
          data = subset(dat,flow>0 & year %in% 1970:2006))
summary(m4)

cov4 = vcovHC(m4,type = "HC0", method = "arellano")
robust.se4 <- sqrt(diag(cov4))



### System GMM ####

#### Endogenous trade flows  ####
m5 = pgmm(log(flow) ~ lag(log(flow)) + factor(year) +
            log(gdp_o)+log(gdp_d)+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1 | lag(log(flow),2:5),
          transformation = "ld",
          effect = "twoways", model = "twosteps",
          index=c("pairid", "year"),
          data = subset(dat,flow>0 & year %in% 1970:2006))
summary(m5)


cov5 = vcovHC(m5,type = "HC0", method = "arellano")
robust.se5 <- sqrt(diag(cov5))




#### Endogenous regional trade  agreements ####

m66 = plm(log(flow) ~ lag((log(flow)),1) + factor(year) +
            lag((bothinE),1) + lag((oneinE),1) + lag((oneinE1),1) +
            lag((bothinC),1) + lag((oneinC),1) + lag((oneinC1),1) +
            log(gdp_o)+log(gdp_d)+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m66)


cov66 = vcovHC(m66,type = "HC0", method = "arellano")
robust.se66 <- sqrt(diag(cov66))



m6 = pgmm(log(flow) ~ lag((log(flow)),1) + factor(year) +
            lag((bothinE),1) + lag((bothinC),1) +
            log(gdp_o)+log(gdp_d)+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1 | lag(log(flow),2:5) + lag((bothinE),2:5) + 
            lag((bothinC),2:5) ,
          transformation = "ld",
          effect = "twoways", model = "twosteps",
          index=c("pairid", "year"),
          data = subset(dat,flow>0 & year %in% 1970:2006))
summary(m6)

cov6 = vcovHC(m6,type = "HC0", method = "arellano")
robust.se6 <- sqrt(diag(cov6))



## Present results
stargazer(m3,m4,m5,m6,header=FALSE, type='html', 
          title = "Table 7: Dynamic panel gravity models",
          se=list(robust.se3,robust.se4,robust.se5,robust.se6),
          no.space = TRUE,
          align = TRUE,
          dep.var.labels=c("Trade flow"),
          keep = c("lag(log(flow),1)","gdp_o","gdp_d",
                   "bothinE","oneinE","oneinE1",
                   "bothinC","oneinC","oneinC1"),
          out="model.html",
          omit.stat= c("adj.rsq", "f", "ser"))

## End of script