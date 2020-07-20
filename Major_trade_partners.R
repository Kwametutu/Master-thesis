


#############################################################
### R-code for master thesis, 22.03.2017
### Written by Albert Kwame Osei-Owusu
#############################################################
### Initial stuff ###

rm(list=ls())

## setting path to library
.libPaths("/Users/albertosei-owusu/Desktop/R Packages")

pack<-c("car","sandwich","ggplot2","lmtest","ggrepel","RColorBrewer","plm",
        "dplyr","mgcv","foreign","xtable","AER","stargazer", 
        "ggrepel","lfe","gridExtra","cowplot","data.table","ggthemes")

lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables/"



#############################################################
### 1. Preliminaries ###
#############################################################

## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))

# number of obs.
nrow(dat)
dim(dat)

# Further check on number of zero trade flows

dat1=subset(dat,flow>0 & year %in% 1970:2006)
dim(dat1)

dat2=subset(dat,flow==0 & year %in% 1970:2006)
dim(dat2)

dat3=subset(dat,flow==0)
dim(dat3)

# years covered
levels(factor(dat$year))
summary(dat$year)

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

# selecting relevant variables
data=subset(dat,flow>0 & year %in% 1970:2006, select=c("year","flow","gdp_o","gdpcap_o","pop_o","distw"))


# number of variables in the dataframe 
n<-dim(data)[2]
n

options(scipen = 2)

# creating an empty matrix 
stats <-matrix(NA, nrow=n, ncol=6)
colnames(stats)=c("Min", "1st Quantile","Median", "Mean", "3rd Quantile","Max")
rownames(stats)= c("Year","Trade flow","Gross Domestic Product (GDP)","Income per capita",
                   "Population","Distance")

## adding summary statistics to the empty matrix
for(i in seq(1,n,1)){
  stats[i,1]<-min(data[,i],na.rm = TRUE)
  stats[i,2]<-quantile(data[,i], probs = 0.25,na.rm = TRUE)
  stats[i,3]<-quantile(data[,i], probs = 0.5,na.rm = TRUE)
  stats[i,4]<-mean(data[,i],na.rm = TRUE)
  stats[i,5]<-quantile(data[,i], probs = 0.75,na.rm = TRUE)
  stats[i,6]<-max(data[,i],na.rm = TRUE)
}

View(stats)

setwd(results)

# Summary statistics
stargazer(dat)


# getting the stargazer function  to create a nice descriptive table 
stargazer(stats, type = "text",
          title="Descriptive statistics",
          digits=3)

## the xtable function
print(xtable(stats), type="HTML", file="Descripive Statistics.html")

## alt
zz <- file("stats.txt", open="wt")
sink(zz)
sink(zz, type="message")

# alt
summary(data)

options(scipen=4)
install.packages("pastecs")
library(pastecs)
stat.desc(dat)
stat.desc(dat[,c("year","gdp_o","gdp_d","pop_o","pop_d","distw","flow")])


# Regional trade agreements
ecowasset = c("BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
              "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER")

eulist = c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
           "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
           "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
           "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

comesa = c("AGO","BDI","COM","COG","DJI","ETH","EGY","KEN","MDG",
           "MWI","MUS","NAM","RWA","SYC","SOM","SDN","SWZ","TZA",
           "UGA","ZMB","ZWE")

sadc = c("AGO","BWA","COD","LSO","MWI","MUS","MOZ",
         "NAM","SYC","ZAF","SWZ","TZA","ZMB","ZWE")


# Create COMESA-origin dummy
dat$eco_o = ifelse(dat$iso_o %in% c("BEN", "BFA", "CPV","CIV", "GMB", "GHA",
                                    "GIN","GNB","LBR","MLI","NGA", 
                                    "SEN", "SLE", "TGO","NER"),1,0)

# Create COMESA-destination dummy
dat$eco_d = ifelse(dat$iso_d %in% c("BEN", "BFA", "CPV","CIV", "GMB", "GHA",
                                    "GIN","GNB","LBR","MLI","NGA", "SEN", 
                                    "SLE", "TGO","NER"),1,0)

# Create EU-origin dummy
dat$eu_o = ifelse(dat$iso_o %in% c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
                                   "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
                                   "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
                                   "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN"), 1,0 )


# Create EU-destination dummy
dat$eu_d = ifelse(dat$iso_d %in% c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
                                   "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
                                   "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
                                   "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN"), 1,0 )


## Setting directory for graphs
graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/COMESA"
setwd(graphs)


## Aggregate COMESA export to EU 28
eu = subset(dat, iso_d %in% eulist & 
              iso_o %in% ecowasset & year %in% 1970:2006)

eu1 = aggregate(flow ~ ., data = select(eu, year, flow), sum)
View(eu1)


### Aggregate COMESA import from EU 28
eu = subset(dat, iso_o %in% eulist & 
               iso_d %in% ecowasset & year %in% 1970:2006)

eu1 = aggregate(flow ~ ., data = select(eu, year, flow), sum)
View(eu1)


## Aggregate COMESA export to China
ch = subset(dat, iso_d == "CHN" & 
              iso_o %in% ecowasset & year %in% 1970:2006)

ch1 = aggregate(flow ~ ., data = select(ch, year, flow), sum)
View(ch1)


### Aggregate COMESA import from China
ch = subset(dat, iso_o =="CHN" & 
              iso_d %in% ecowasset & year %in% 1970:2006)

ch1 = aggregate(flow ~ ., data = select(ch, year, flow), sum)
View(ch1)


## Aggregate COMESA export to India
ind = subset(dat, iso_d == "IND" & 
              iso_o %in% ecowasset & year %in% 1970:2006)

ind1 = aggregate(flow ~ ., data = select(ind, year, flow), sum)
View(ind1)


### Aggregate COMESA import from India
ind = subset(dat, iso_o =="IND" & 
              iso_d %in% ecowasset & year %in% 1970:2006)

ind1 = aggregate(flow ~ ., data = select(ind, year, flow), sum)
View(ind1)



## Aggregate COMESA export to UK
uk = subset(dat, iso_d == "GBR" & 
               iso_o %in% ecowasset & year %in% 1970:2006)

uk1 = aggregate(flow ~ ., data = select(uk, year, flow), sum)
View(uk1)


### Aggregate COMESA import from UK
uk = subset(dat, iso_o =="GBR" & 
               iso_d %in% ecowasset & year %in% 1970:2006)

uk1 = aggregate(flow ~ ., data = select(uk, year, flow), sum)
View(uk1)



## Aggregate COMESA export to US
us = subset(dat, iso_d == "USA" & 
              iso_o %in% ecowasset & year %in% 1970:2006)

us1 = aggregate(flow ~ ., data = select(us, year, flow), sum)
View(us1)


### Aggregate COMESA import from US
us = subset(dat, iso_o =="USA" & 
              iso_d %in% ecowasset & year %in% 1970:2006)

us1 = aggregate(flow ~ ., data = select(us, year, flow), sum)
View(us1)



## Aggregate COMESA export to Brazil
bra= subset(dat, iso_d == "BRA" & 
              iso_o %in% ecowasset & year %in% 1970:2006)

bra1 = aggregate(flow ~ ., data = select(bra, year, flow), sum)
View(bra1)


### Aggregate COMESA import from Brazil
bra= subset(dat, iso_o =="BRA" & 
              iso_d %in% ecowasset & year %in% 1970:2006)

bra1 = aggregate(flow ~ ., data = select(bra, year, flow), sum)
View(bra1)


##### COMESA ######

# Regional trade agreements
ecowasset = c("BEN", "BFA", "CPV","CIV", "GMB", "GHA", "GIN", "GNB",
              "LBR","MLI","NGA", "SEN", "SLE", "TGO","NER")

eulist = c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
           "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
           "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
           "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

comesa = c("AGO","BDI","COM","COG","DJI","ETH","EGY","KEN","MDG",
           "MWI","MUS","NAM","RWA","SYC","SOM","SDN","SWZ","TZA",
           "UGA","ZMB","ZWE")

sadc = c("AGO","BWA","COD","LSO","MWI","MUS","MOZ",
         "NAM","SYC","ZAF","SWZ","TZA","ZMB","ZWE")


# Create COMESA-origin dummy
dat$com_o = ifelse(dat$iso_o %in% c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
                                    "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
                                    "UGA","ZMB","ZWE"),1,0)



# Create COMESA-destination dummy

dat$com_d = ifelse(dat$iso_d %in% c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
                                    "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
                                    "UGA","ZMB","ZWE"),1,0)


## Setting directory for graphs
graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/COMESA"
setwd(graphs)


## Aggregate COMESA export to EU 28
eu = subset(dat, iso_d %in% eulist & 
              iso_o %in% comesa & year %in% 1970:2006)

eu1 = aggregate(flow ~ ., data = select(eu, year, flow), sum)
View(eu1)


### Aggregate COMESA import from EU 28
eu = subset(dat, iso_o %in% eulist & 
              iso_d %in% comesa & year %in% 1970:2006)

eu1 = aggregate(flow ~ ., data = select(eu, year, flow), sum)
View(eu1)


## Aggregate COMESA export to China
ch = subset(dat, iso_d == "CHN" & 
              iso_o %in% comesa & year %in% 1970:2006)

ch1 = aggregate(flow ~ ., data = select(ch, year, flow), sum)
View(ch1)


### Aggregate COMESA import from China
ch = subset(dat, iso_o =="CHN" & 
              iso_d %in% comesa & year %in% 1970:2006)

ch1 = aggregate(flow ~ ., data = select(ch, year, flow), sum)
View(ch1)


## Aggregate COMESA export to India
ind = subset(dat, iso_d == "IND" & 
               iso_o %in% comesa & year %in% 1970:2006)

ind1 = aggregate(flow ~ ., data = select(ind, year, flow), sum)
View(ind1)


### Aggregate COMESA import from India
ind = subset(dat, iso_o =="IND" & 
               iso_d %in% comesa & year %in% 1970:2006)

ind1 = aggregate(flow ~ ., data = select(ind, year, flow), sum)
View(ind1)



## Aggregate COMESA export to Switzerland
swz = subset(dat, iso_d == "CHE" & 
              iso_o %in% comesa & year %in% 1970:2006)

swz1 = aggregate(flow ~ ., data = select(swz, year, flow), sum)
View(swz1)


### Aggregate COMESA import from Switzerland
swz = subset(dat, iso_o =="CHE" & 
              iso_d %in% comesa & year %in% 1970:2006)

swz1 = aggregate(flow ~ ., data = select(swz, year, flow), sum)
View(swz1)



## Aggregate COMESA export to South Africa
zaf= subset(dat, iso_d == "ZAF" & 
              iso_o %in% comesa & year %in% 1970:2006)

zaf1 = aggregate(flow ~ ., data = select(zaf, year, flow), sum)
View(zaf1)


### Aggregate COMESA import from South Africa
zaf = subset(dat, iso_o =="ZAF" & 
              iso_d %in% comesa & year %in% 1970:2006)

zaf1 = aggregate(flow ~ ., data = select(zaf, year, flow), sum)
View(zaf1)
