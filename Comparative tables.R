
# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)


## 1.a Load dataset and create dummies
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



## OLS and FE estimation

# OLS, time and time-invariant country effects
m1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+factor(year)+
          factor(iso_o)+factor(iso_d)+
          log(distw)+contig+comlang_off+col_hist+
          bothinE+oneinE+oneinE1+
          bothinC+oneinC+oneinC1, 
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m1)

cov1 <- vcovHC(m1, type = "HC0", method="arellano")
robust.se1 <- sqrt(diag(cov1))


## 5.b double demeaning approach
m2 = felm(log(flow)~factor(year) +
            log(gdp_o)+log(gdp_d) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1|iso_o+iso_d,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m2, robust=TRUE)


# OLS, time and time invariant country effects

# create country-year dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1|iso_o_yr+iso_d_yr,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m3,robust=TRUE)


m3 = felm(log(flow)~factor(year) +
            log(distw)+contig+comlang_off+col_hist+ 
            bothinE+bothinC|iso_o_yr+iso_d_yr,
          data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m3,robust=TRUE)



# FE, time and time invariant country effects
dat$pairid = paste0(dat$iso_o, dat$iso_d)
m4 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1, 
           model = "within",
           index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m4)

cov4 <- vcovHC(m4, type = "HC0", method="arellano")
robust.se4 <- sqrt(diag(cov4))


# FE, time and time varying country effects

m44 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
            factor(iso_o_yr)+factor(iso_d_yr)+
            bothinE+oneinE+oneinE1+
            bothinC+oneinC+oneinC1, 
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
           bothinC+oneinC+oneinC1,
         family="poisson", 
         data = subset(dat, year %in% 1970:2006))
summary(m5)

cov5 <- vcovHC(m5, type = "HC0", method="arellano")
robust.se5 <- sqrt(diag(cov5))


# time varying country effects included (takes a very long time to run)
m6 = glm(flow ~ factor(year) + factor(iso_o_yr)+factor(iso_d_yr)+
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1,
         family="poisson",
         data = subset(dat,year %in% 2002:2006))
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


## alt
stargazer(m1,m4,m5,
          type = "html", no.space = TRUE,
          title = "Table : Summary of robust gravity regressions",
          se=list(robust.se1,robust.se4,robust.se5),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX","COMESAM"),
          omit = c("year", "iso","gdp_o","gdp_d","distw","col_hist","contig","comlang_off"),
          omit.stat = c("f", "aic", "ll","adj.rsq"),
          out="model99.html",
          add.lines = list(c("Fixed effects", "TI", "TI","TI")))



### ROBUST CHECKS ####

## Robust  regressions ###

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
        data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m1)

cov1 <- vcovHC(m1, type = "HC0", method="arellano")
robust.se1 <- sqrt(diag(cov1))


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
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m4 = plm(log(flow) ~ log(gdp_o)+log(gdp_d)+factor(year)+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           oneinAGX+oneinAGM +
           oneinEPAX+oneinEPAM,
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2006))
summary(m4)

cov4 <- vcovHC(m4, type = "HC0", method="arellano")
robust.se4 <- sqrt(diag(cov4))



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



## 5. Poisson (PPML) estimator
# time invariant country effects included (takes a long time to run)
m5 = glm(flow ~ factor(year)+ factor(iso_o)+factor(iso_d)+
           log(gdp_o)+log(gdp_d) +
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1+
           oneinAGX+oneinAGM+
           oneinEPAX+oneinEPAM,
         family="poisson", 
         data = subset(dat, year %in% 1970:2006))
summary(m5)

cov5 <- vcovHC(m5, type = "HC0", method="arellano")
robust.se5 <- sqrt(diag(cov5))



# time varying country effects included (takes a very long time to run)
m6 = glm(flow ~ factor(year) + factor(iso_o_yr)+factor(iso_d_yr)+
           log(distw)+contig+comlang_off+col_hist+
           bothinE+oneinE+oneinE1+
           bothinC+oneinC+oneinC1,
         family="poisson",
         data = subset(dat,year %in% 2002:2006))
summary(m6)

cov6 <- vcovHC(m6, type = "HC0", method="arellano")
robust.se6 <- sqrt(diag(cov6))



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


## alt
stargazer(m1,m4,m5,
          type = "html", no.space = TRUE,
          title = "Table : Summary of robust gravity regressions",
          se=list(robust.se1,robust.se4,robust.se5),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("ECOWAS","ECOWASX",
                             "ECOWASM","COMESA","COMESAX","COMESAM",
                             "AGOAX","AGOAM","EPAX","EPAM"),
          omit = c("year", "iso","gdp_o","gdp_d","distw","col_hist","contig","comlang_off"),
          omit.stat = c("f", "aic", "ll","adj.rsq"),
          out="model100.html",
          add.lines = list(c("Fixed effects", "TI", "TI", "TI")))



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


