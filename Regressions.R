
#############################################################
### R-code for master thesis 1, 22.03.2017
### Written by Albert Kwame Osei-Owusu
#############################################################
### Initial stuff ###

rm(list=ls())

## setting path to library
.libPaths("/Users/albertosei-owusu/Desktop/R Packages")

# load packages

#library(grid)
#library(gridExtra)
#library(cowplot)


pack<-c("car","sandwich","ggplot2","lmtest","ggrepel","RColorBrewer","plm",
        "dplyr","mgcv","foreign","xtable","AER","stargazer", 
        "ggrepel","lfe","gridExtra","cowplot")


lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"



#############################################################
### 1. Preliminaries ###
#############################################################

## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))



## 2.a Run regression

ecowasset = c("BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
              "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER")



##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)


#############################################################
### 2. Average trade creation and diversion  ###
#############################################################

# Note. It might be a good idea to consider a subset of the 
# data. E.g. choose only the data from the 1985-2000 period

## 2.a Run regression

dat$bothinE = ifelse(dat$iso_o %in% ecowasset & 
                       dat$iso_d %in% ecowasset &
                       dat$year >= 1975, 1, 0)

dat$oneinE = ifelse(!(dat$iso_o %in% ecowasset) & 
                      dat$iso_d %in% ecowasset &
                      dat$year >= 1975, 1, 0)

dat$bothinC = ifelse(dat$iso_o %in% comesa & 
                      dat$iso_d %in% comesa &
                      dat$year >= 1982, 1, 0)

dat$oneinC = ifelse(!(dat$iso_o %in% comesa) & 
                      dat$iso_d %in% comesa &
                      dat$year >= 1982, 1, 0)


##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)


#
ols1 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
                    contig+comlang_off+col_hist+bothinE+oneinE, 
          data = subset(dat, year == 1985 & flow > 0))
summary(ols1)

ols2 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
                    contig+comlang_off+col_hist+bothinE+oneinE, 
          data = subset(dat, year == 1990 & flow > 0))
summary(ols2)

ols3 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
                    contig+comlang_off+col_hist+bothinE+oneinE, 
          data = subset(dat, year == 1995 & flow > 0))
summary(ols3)

ols4 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
                    contig+comlang_off+col_hist+bothinE+oneinE, 
          data = subset(dat, year == 2000 & flow > 0))
summary(ols4)

ols5 = lm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
                    contig+comlang_off+col_hist+bothinE+oneinE, 
          data = subset(dat, year == 2005 & flow > 0))
summary(ols5)


## Presenting results for selected years
stargazer(ols1, ols2,ols3,ols4,ols5, header=FALSE, type='text', 
          title = "Table 1 : Simple Cross-sectional OLS estimation",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("1985","1990","1995","2000","2005"),
          dep.var.labels=c("Trade flow"),
          covariate.labels=c("Exporter income","Importer income","Distance",
                             "Border","Common language",
                             "Colonial history","ECOWAS","OneinE"),
          out="ols_model1.txt",
          omit.stat= c("adj.rsq", "f", "ser"))


stargazer(ols1, ols2,ols3,ols4,ols5, header=FALSE, type='text', 
          title = "Table 2 : Simple Cross-sectional OLS estimation",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("1985","1990","1995","2000","2005"),
          dep.var.labels=c("Coefficients"),
          out="ols_model2.txt",
          omit.stat= c("adj.rsq", "f", "ser"))


# A) OLS with importer, exporter and year fixed effects
m1 = lm(log(flow)~factor(iso_o)+factor(iso_d)+factor(year)+
          log(distw)+col_cur+contig+comlang_off+col_hist+
          bothinE + oneinE,
        data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m1)

## double demeaning
m11 = felm(log(flow)~log(gdp_o)+log(gdp_d)+log(distw)+
             col_cur+contig+comlang_off+col_hist+
             bothinE + oneinE | year + iso_o + iso_d, 
           data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m11)



# B) OLS with time varying importer, exporter fixed effects
# and year fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m2 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
          log(distw) + contig + comlang_off + col_hist +
          + bothinE + oneinE,
        data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m2)


m2 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
          log(distw) + contig + comlang_off + col_hist +
          + bothinE + oneinE,
        data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m2)


## double demeaning
m22 = felm(log(flow)~log(distw)+
             + contig + comlang_off + col_hist +
             + bothinE + oneinE | year + iso_o_yr + iso_d_yr, 
           data = subset(dat, flow>0 & year %in% 1970:2000))
summary(m22)

m22 = felm(log(flow)~log(distw)+
             + contig + comlang_off + col_hist +
             + bothinE + oneinE | year + iso_o_yr + iso_d_yr, 
           data = subset(dat, flow>0 & year %in% 1970:1990))
summary(m22)

# C) Within with year fixed effects

# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m3 = plm(log(flow)~ factor(year) +
           log(distw) + contig + comlang_off + col_hist +
           bothinE + oneinE, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m3)

m3 = plm(log(flow)~ factor(year) + factor(iso_o) + factor(iso_d) +
           log(distw) + contig + comlang_off + col_hist +
           bothinE + oneinE, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m3)

# D) Within with year fixed effects and time varying country fixed effects
# Note: this takes forever to run
m4 = plm(log(flow)~ factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
           log(distw) + contig + comlang_off + col_hist +
           bothinE + oneinE, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m4)


## double demeaning
m44 = felm(log(flow)~ log(distw) + contig + comlang_off + col_hist + 
             bothinE + oneinE | year + iso_o_yr + iso_d_yr, 
           data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m44)

m44 = felm(log(flow)~ log(distw) +
             bothinE + oneinE |  year + iso_o_yr + iso_d_yr, 
           data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m44)


# E) Within with year fixed effects and GDP controls
m5 = plm(log(flow)~ factor(year)+ log(gdp_o)+log(gdp_d)+
           log(distw) + contig + comlang_off + col_hist +
           bothinE + oneinE, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m5)


##Change working directory
path=setwd(results)


## 2.b Present results
stargazer(m1, m3, m44, header=FALSE, type='text', 
          title = "Table 1: Gravity models of trade",
          no.space = TRUE,
          keep = c("bothinE","oneinE"),
          out="model2.txt",
          omit.stat= c("adj.rsq", "f", "ser"))

stargazer(m1,m3,m5, header=FALSE, type='text', 
          title = "Table 2 : Panel regressions",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("OLS with imp,exp and year FE",
                          "Within with year FE",
                          "Within with year FE and GDP controls"),
          dep.var.labels=c("Coefficients"),
          keep = c("bothinE","oneinE"),
          out="model3.txt",
          omit.stat= c("adj.rsq", "f", "ser"))

stargazer(m1,m11,m22,m3,m5, header=FALSE, type='text', 
          title = "Table 3 : Regressions",
          no.space = TRUE,
          align = TRUE,
          dep.var.labels=c("Coefficients"),
          keep=c( "gdp_o","gdp_d",
                  "distw","col_cur","contig", "comlang_off", "col_hist",
                  "bothinE","oneinE"),
          out="model4.txt",
          omit.stat= c("adj.rsq", "f", "ser"))

##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)

## 2.b Present results
stargazer(m1, m3, m44, header=FALSE, type='text', 
          title = "Table 1: Gravity models of trade",
          align = TRUE,
          column.labels=c("imp,exp and year FE",
                          "year FE",
                          "year FE and time varying country FE"),
          dep.var.labels=c("Coefficients"),
          keep = c("bothinE","oneinE"),
          out="models.txt",
          omit.stat= c("adj.rsq", "f", "ser"))

stargazer(m1,m3,m5, header=FALSE, type='text', 
          title = "Table 2 : Panel regressions",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("imp,exp and year FE",
                          "year FE",
                          "year FE and GDP controls"),
          dep.var.labels=c("Coefficients"),
          keep = c("bothinE","oneinE"),
          out="model1.txt",
          omit.stat= c("adj.rsq", "f", "ser"))


#############################################################
### 3. Evolution of trade creation and diversion ###
#############################################################

## 3.a Introduce interaction terms
m6 = plm(log(flow)~ log(distw) + contig + comlang_off + col_hist +
           factor(year) +
           factor(year):bothinE +
           factor(year):oneinE,
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m6)

stargazer(m6, title="Results", 
          dep.var.labels="Property sales prices", 
          style="aer",
          align=TRUE,
          type = "text")

## 3.b Plot results
plot(1975:2005, (coef(m6)[36:66]+coef(m6)[5:35]), type = "b",
     xlab = "year",
     ylab = "bothinE coefficient",
     main = "Evolution of the bothinE coefficient")

plot(1975:2005, (coef(m6)[67:97]+coef(m6)[5:35]), type = "b",
     xlab = "year",
     ylab = "oneinE coefficient",
     main = "Evolution of the oneinE coefficient")


#############################################################
### 4. Export diversion ###
#############################################################

## 4.a Generate ECOWAS extra export dummy
dat$oneinE1 = ifelse(dat$iso_o %in% ecowasset & 
                       !(dat$iso_d %in% ecowasset) &
                       dat$year >= 1975, 1, 0)

# Run regression
# E) Within with year fixed effects and GDP controls
m7 = plm(log(flow)~ factor(year)+ log(gdp_o)+log(gdp_d)+
           log(distw) + contig + comlang_off + col_hist +
           bothinE + oneinE + oneinE1, 
         model = "within",
         index=c("pairid", "year"),
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m7)


## 4.b
# ECOWAS export to ROW
eco1 = select(subset(dat, iso_o %in% ecowasset & 
                           !(dat$iso_d %in% ecowasset)),
                  year, flow)

eco2 = aggregate(flow ~ ., data = eco1, sum)

# Building the plot (using the ggplot2 package)
ggplot(eco2, aes(year, log(flow))) + 
  geom_line() + 
  geom_point() +
  ylab("trade flow") + 
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS exports to ROW")

######################################################

# F) OLS with importer, exporter and year fixed effects
m8 = lm(log(flow)~factor(iso_o)+factor(iso_d)+factor(year)+
          log(distw) + contig + comlang_off + col_hist +
          bothinE + oneinE + oneinE1 ,
        data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m8)


# G) OLS with time varying importer, exporter fixed effects
# and year fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m9 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
          log(distw) + bothinE + oneinE + oneinE1 ,
        data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m9)

## double demeaning
m99 = felm(log(flow)~log(distw) + contig + comlang_off + col_hist +
            bothinE + oneinE + oneinE1 | year + iso_o_yr + iso_d_yr, 
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m99)

m99 = felm(log(flow)~log(distw)+
             bothinE + oneinE + oneinE1 | year + iso_o_yr + iso_d_yr, 
           data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m99)


# H) Within with year fixed effects

# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m10 = plm(log(flow)~ factor(year) +
            log(distw) + contig + comlang_off + col_hist +
            bothinE + oneinE + oneinE1 , 
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m10)



##Change working directory
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
path=setwd(results)

## Present results
stargazer(m8,m99,m10,m7, header=FALSE, type='text', 
          title = "Table 3 : Panel regressions",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("imp,exp and year","vimp,vexp and year",
                          "year FE",
                          "year FE and GDP controls"),
          dep.var.labels=c("Coefficients"),
          keep = c("bothinE","oneinE", "oneinE1"),
          out="model3.txt",
          omit.stat= c("adj.rsq", "f", "ser"))

## Introduce interaction terms
m11 = plm(log(flow)~ log(distw) + col_cur + contig + comlang_off + col_hist +
            factor(year) +
            factor(year):bothinE+
            factor(year):oneinE+
            factor(year):oneinE1, 
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m11)

## 3.b Plot results
par(mfrow=c(1,3))
plot(1975:2005, (coef(m11)[37:67]+coef(m11)[6:36]), type = "b",
     xlab = "year",
     ylab = "bothinE coefficient",
     main = "Evolution of the bothinE coefficient")

plot(1975:2005, (coef(m11)[68:98]+ coef(m11)[6:36]), type = "b",
     xlab = "year",
     ylab = "oneinE coefficient",
     main = "Evolution of the oneinE coefficient")


plot(1975:2005, (coef(m11)[99:129]+ coef(m11)[6:36]), type = "b",
     xlab = "year",
     ylab = "oneinE1 coefficient",
     main = "Evolution of the onein1E coefficient")

### alt
dat4 <- data.frame( year=c(1975:2005),
                    bothinE = c(coef(m11)[37:67]+coef(m11)[6:36]),
                    oneinE = c(coef(m11)[68:98]+coef(m11)[6:36]),
                    oneinE1 =c(coef(m11)[99:129]+coef(m11)[6:36]))

## transposing dataframe
dat5 <- t(dat4)
View(dat5)

## Multiple line of trade creation and diversion
lines(plot(dat4$year,dat4$bothinE, type = "l",
           xlab = "year", ylim=c(0.864,2.792),
           ylab = "coefficients",
           main = "Evolution of trade creation and trade diversion",
           col= "red"))
lines(dat4$year,dat4$oneinE,type="l",col="blue")
lines(dat4$year,dat4$oneinE1,type="bl",col="black")

legend("topleft", 
       border="black",col=c("red","blue","black") ,
       lty=c(1,1),
       legend=c("Import diversion","Trade creation", "Export Diversion"),
       bg ="white")

# alt
legend("bottomright", 
       border="black",col=c("red","blue","black") ,
       lty=c(1,1),
       legend=c("Import diversion","Trade creation", "Export diversion"),
       bg ="white")



#############################################################
### 5. The Average RTA effect ###
#############################################################


## 5.a 

# RTA dummy

## Regional trade agreements
m14 = lm(log(flow)~log(gdp_o)+log(gdp_d)+
           log(distw)+contig+comlang_off+
           col_hist+col_cur+rta,
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m14)

# J) OLS with importer, exporter and year fixed effects
m14 = lm(log(flow)~factor(iso_o)+factor(iso_d)+factor(year)+
           log(distw)+contig+comlang_off+
           col_hist+col_cur+rta,
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m14)


# K) OLS with time varying importer, exporter fixed effects
# and year fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m15 = lm(log(flow)~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
           log(distw)+contig+comlang_off+
           col_hist+col_cur+rta,
         data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m15)

# H) Within with year fixed effects

# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m16 = plm(log(flow)~ factor(year) +
            log(distw)+contig+comlang_off+
            col_hist+col_cur+rta, 
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m16)

# I) Within with year fixed effects and time varying country fixed effects
# Note: this takes forever to run
m17 = plm(log(flow)~ factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
            log(distw)+contig+comlang_off+
            col_hist+col_cur+rta,
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m17)


## Present results
stargazer(m8,m10,m7, header=FALSE, type='text', 
          title = "Table 2 : Panel regressions",
          no.space = TRUE,
          align = TRUE,
          column.labels=c("imp,exp and year FE",
                          "year FE",
                          "year FE and GDP controls"),
          dep.var.labels=c("Coefficients"),
          keep = c("bothin","onein"),
          out="model2.txt",
          omit.stat= c("adj.rsq", "f", "ser"))


# 5.c
##### PPML estimator ####

# A) PPML without importer, exporter and year fixed effects
m20 = glm(flow~ log(gdp_o) + log(gdp_d) +
            log(distw)+col_cur+contig+comlang_off+col_hist+
            bothinE + oneinE,
          data = subset(dat,year %in% 1970:2005), family = poisson())
summary(m20)



# B) PPML with importer, exporter and year fixed effects
m21 = glm(flow~factor(iso_o)+factor(iso_d)+factor(year)+
            log(distw)+col_cur+contig+comlang_off+col_hist+
            bothinE + oneinE,
          data = subset(dat,year %in% 1970:2005), family = poisson)
summary(m21)

m21 = glm(flow~factor(iso_o)+factor(iso_d)+factor(year)+
            log(distw)+col_cur+contig+comlang_off+col_hist+
            bothinE + oneinE,
          data = subset(dat,year %in% 1970:2005), family = poisson())
summary(m21)


# C) PPML with time varying importer, exporter fixed effects
# and year fixed effects

# Generate time varying importer/exporter dummies
dat$iso_o_yr = paste0(dat$iso_o, dat$year)
dat$iso_d_yr = paste0(dat$iso_d, dat$year)

m22 = glm(flow~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
            log(distw) + col_cur + contig + comlang_off + col_hist +
            + bothinE + oneinE,
          data = subset(dat,year %in% 1970:2005), family = poisson())
summary(m22)

m22 = glm(flow~factor(iso_o_yr)+factor(iso_d_yr)+factor(year)+
            log(distw) + col_cur + contig + comlang_off + col_hist +
            + bothinE + oneinE,
          data = subset(dat,year %in% 1970:2005), family = poisson)
summary(m22)


# D) Within with year fixed effects

# generate country pair
dat$pairid = paste0(dat$iso_o, dat$iso_d)

m23 = glm(log(flow)~ factor(year) +
            log(distw) + col_cur + contig + comlang_off + col_hist +
            bothinE + oneinE, 
          model = "within",
          index=c("pairid", "year"),
          data = subset(dat, flow>0 & year %in% 1970:2005))
summary(m23)


## End of script