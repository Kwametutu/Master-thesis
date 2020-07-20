
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
        "ggrepel","lfe","gridExtra","cowplot","data.table")

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
data=subset(dat, select=c("year","flow","gdp_o","gdpcap_o","pop_o","distw"))

# number of variables in the dataframe 
n<-dim(data)[2]

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

eu = c("AUT", "BEL", "FIN", "FRA", "DEU", "GRC", "IRL", "ITA", 
       "NLD", "PRT", "ESP", "SWE","GBR", "CYP", "CZE", "EST",
       "HUN", "LVA", "LTU", "MLT", "POL", "SVK", "SVN")

comesa = c("AGO","BDI","COM","COG","DJI","ETH","EGY","KEN","MDG",
           "MWI","MUS","NAM","RWA","SYC","SOM","SDN","SWZ","TZA",
           "UGA","ZMB","ZWE")

sadc = c("AGO","BWA","COD","LSO","MWI","MUS","MOZ",
         "NAM","SYC","ZAF","SWZ","TZA","ZMB","ZWE")


# Create ECOWAS-origin dummy
dat$eco_o = ifelse(dat$iso_o %in% c("BEN", "BFA", "CPV","CIV", "GMB", "GHA",
                                       "GIN","GNB","LBR","MLI","NGA", 
                                    "SEN", "SLE", "TGO","NER"),1,0)

# Create ECOWAS-destination dummy
dat$eco_d = ifelse(dat$iso_d %in% c("BEN", "BFA", "CPV","CIV", "GMB", "GHA",
                                    "GIN","GNB","LBR","MLI","NGA", "SEN", 
                                    "SLE", "TGO","NER"),1,0)

# Create EU-origin dummy
dat$eu_o = ifelse(dat$iso_o %in% c("AUT", "BEL", "FIN", "FRA", "DEU", "GRC", 
                                       "IRL", "ITA", "NLD", "PRT", "ESP", "SWE", 
                                       "GBR", "CYP", "CZE", "EST", "HUN", "LVA", 
                                       "LTU", "MLT", "POL", "SVK", "SVN"), 1,0 )
# Create EU-destination dummy
dat$eu_d = ifelse(dat$iso_d %in% c("AUT", "BEL", "FIN", "FRA", "DEU", "GRC", 
                                       "IRL", "ITA", "NLD", "PRT", "ESP", "SWE", 
                                       "GBR", "CYP", "CZE", "EST", "HUN", "LVA", 
                                       "LTU", "MLT", "POL", "SVK", "SVN"), 1,0 )

# ecowas exports to eu
dat1 = subset(dat, eco_o == 1 & eu_d == 1)
dat2 = aggregate(flow ~ ., data = select(dat1, year,flow), sum)
dat2$lflow=log(dat2$flow)

plot(dat2$year, dat2$lflow, type = "o", main = "Ecowas export to EU", col= "blue",
     xlab = "year", ylab = "export")
abline(v=1975, col="red")

# Building the plot (using the ggplot2 package)
ggplot(dat2, aes(year, log(flow))) + 
  geom_line() + 
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 1975) +
  ggtitle("Ecowas exports to selected high income countries")


### Trend of export and imports in ECOWAS
### Aggregate global exports
glo = aggregate(flow ~ ., data = select(dat, year, flow), sum)


## Aggregate ECOWAS annual exports  
eco1= subset(dat, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)

## Aggregate ECOWAS annual imports 
eco3= subset(dat, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)


# ECOWAS export shares
eco2$ecoshare = eco2$flow/glo$flow

# ECOWAS import shares
eco4$ecoshare = eco4$flow/glo$flow

## Plotting two graphs
par(mfrow=c(1,2))
plot(eco2$year, eco2$ecoshare, type = "o", 
     xlab = "",
     ylab = "export share",
     main = "Annual ECOWAS export shares")

plot(eco4$year, eco4$ecoshare, type = "o", 
     xlab = "",
     ylab = "import share",
     main = "Annual ECOWAS import shares")

p1 <- ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Annual ECOWAS export shares")

p2 <- ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Annual ECOWAS import shares")

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)


## ECOWAS export shares to EU15

# EU 15 countries:
# Note that the EU15 dummy includes non-EU countries!
eulist = c("AUT", "BEL", "DNK", "FIN", "FRA", "DEU", "GRC",
           "LUX", "NLD", "POR", "ESP", "SWE", "GBR")

### Aggregate EU import
eu1 = subset(dat, iso_d %in% eulist)
eu2 = aggregate(flow ~ ., data = select(eu1, year, flow), sum)

## ECOWAS share of export to EU
eco1= subset(eu1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)

### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## EU export to ECOWAS
eu3= subset(eco3, iso_o %in% eulist)
eu4 = aggregate(flow ~ ., data = select(eu3, year, flow), sum)


# ECOWAS export share to EU
eco2$ecoshare = eco2$flow/eu2$flow

# ECOWAS import share from EU
eco4$ecoshare = eu4$flow/eco4$flow

par(mfrow=c(1,2))
plot(eco2$year, eco2$ecoshare, type = "l", ylim = c(0,0.025), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to EU")

plot(eco4$year, eco4$ecoshare, type = "l", ylim = c(0,0.734), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from EU")

p1=ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS export shares to EU")

p2=ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS import shares from EU")

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)

## ECOWAS export shares to US

### Aggregate US import
us1 = subset(dat, iso_d == "USA")

us2 = aggregate(flow ~ ., data = select(us1, year, flow), sum)

## ECOWAS share of export to US
eco= subset(us, iso_o %in% ecowasset)
eco1 = aggregate(flow ~ ., data = select(eco, year, flow), sum)


## ECOWAS import shares from US
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## US export to ECOWAS
us3= subset(eco3, iso_o == "USA")
us4 = aggregate(flow ~ ., data = select(us3, year, flow), sum)

# ECOWAS export share to US
eco1$ecoshare = eco1$flow/us1$flow

# ECOWAS import share from EU
eco4$ecoshare = us4$flow/eco4$flow
 

plot(eco1$year, eco1$ecoshare, type = "l", ylim = c(0,0.046), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to US")

plot(eco4$year, eco4$ecoshare, type = "l", ylim = c(0,0.135), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from US")


p1=ggplot(eco1, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS export shares to US")

p2=ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS import shares from US")

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)


## ECOWAS export shares to China

### Aggregate China import
ch1 = subset(dat, iso_d == "CHN" & year %in% 1960:2000)

ch2 = aggregate(flow ~ ., data = select(ch1, year, flow), sum)

## ECOWAS share of export to China
eco1= subset(ch1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)


## ECOWAS import shares from China
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## China export to ECOWAS
ch3= subset(eco3, iso_o == "CHN")
ch4 = aggregate(flow ~ ., data = select(ch3, year, flow), sum)

# ECOWAS export share to China
eco2$ecoshare = eco2$flow/ch2$flow

# ECOWAS import share from China
eco4$ecoshare = ch4$flow/eco4$flow


plot(eco2$year, eco2$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to China")

plot(eco4$year, eco4$ecoshare, type = "l", ylim = c(0,0.1291), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from China")


p1=ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS export shares to China")

p2=ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS import shares from China")

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)

# Top 5 exporting countries in ECOWAS in each year
# calculating aggregate (total) export for each country
dat1=subset(dat, iso_o %in% ecowasset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)
View(dat2)

for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries that import from ECOWAS in each year

# calculating aggregate (total) import for each country
dat1=subset(dat, iso_o %in% ecowasset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_d, year, flow), sum)
View(dat2)

for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}

# USA,EU-FRA,DEU,ESP,ITA,NLD, CHN, BRA


# Top 5 importers in Ecowas
# calculating aggregate (total) import for each country
dat1=subset(dat, iso_d %in% ecowasset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)
View(dat2)

# Top 5 exporting countries to ECOWAS in each year
for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}

## FRA,CHN,USA,GBR,DEU,KOR,JPN


# Top 5 importers in Ecowas
# calculating aggregate (total) import for each country
dat1=subset(dat, iso_d %in% ecowasset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_d, year, flow), sum)
View(dat2)

for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}



# trade pattern of top 3 exporters in ECOWAS

# To the eu
ecoset=c("GHA","CIV","NGA")

# ecowas exports to eu
dat1 = subset(dat, iso_o %in% ecoset & eu_d == 1)
dat2 = aggregate(flow ~ ., data = select(dat1, year,flow), sum)
dat2$lflow=log(dat2$flow)

# Aggregate
plot(dat2$year, dat2$lflow, type = "o", main = "Ecowas export to EU", col= "blue",
     xlab = "year", ylab = "export")
abline(v=1975, col="red")


# Individual countries
dat2 = aggregate(flow ~ ., data = select(dat1, year,iso_o,flow), sum)

library(ggplot2)
ggplot(dat2, aes(year, log(flow), colour = iso_o )) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Ecowas exports to EU")


## ECOWAS export shares to EU

# EU 15 countries:
# Note that the EU15 dummy includes non-EU countries!
eulist = c("AUT", "BEL", "DNK", "FIN", "FRA", "DEU", "GRC",
           "LUX", "NLD", "POR", "ESP", "SWE", "GBR")

### Aggregate EU import
eu = subset(dat, iso_d %in% eulist)
eu1 = aggregate(flow ~ ., data = select(eu, year, flow), sum)

## export to EU
eco= subset(eu, iso_o %in% ecoset)
eco1 = aggregate(flow ~ ., data = select(eco, year, flow), sum)

eco1$lflow=log(eco1$flow)

## Aggregate
plot(eco1$year, eco1$lflow, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to EU")
abline(v=1975, col="blue")


## Individual countries
eco1 = aggregate(flow ~ ., data = select(eco, year,iso_o, flow), sum)
ggplot(eco1, aes(year, log(flow), col= iso_o)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Export to EU")



## 1.b Plot evolution of imports
# Intra ECOWAS trade
dat1 = select(subset(dat, iso_d %in% ecowasset & 
                           dat$iso_o %in% ecowasset),
                  year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)


# Building the plot (using the ggplot2 package)
p1=ggplot(dat2, aes(year, log(flow))) +
  ylab("trade flow") + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Intra ECOWAS imports")
  

# ECOWAS import from ROW
dat1 = select(subset(dat, iso_d %in% ecowasset & 
                                      !(dat$iso_o %in% ecowasset)),
                             year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)


# Building the plot (using the ggplot2 package)
p2=ggplot(dat2, aes(year, log(flow))) +
  ylab("trade flow") + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS imports from ROW")


# ECOWAS export to ROW
dat1 = select(subset(dat, iso_o %in% ecowasset & 
                       !(dat$iso_d %in% ecowasset)),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)


# Building the plot (using the ggplot2 package)
p3=ggplot(dat2, aes(year, log(flow))) + 
  ylab("trade flow") + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("ECOWAS exports to ROW")


library(gridExtra)
grid.arrange(p1,p2,p3, ncol=3, nrow =1)


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

