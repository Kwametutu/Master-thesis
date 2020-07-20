
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
graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/ECOWAS"
setwd(graphs)



## 1.b Plot evolution of imports


## ECOWAS imports 

## Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/ecowas_imports_main.csv", 
                col_types = cols(flow = col_number()))
View(dat)

dat$flow1 = dat$flow/1e3



tiff(filename="ECOWAS_trade.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow1,colour=partner)) +
  geom_line(aes(colour = partner, group = partner)) +
  scale_y_continuous(name = "Trade flow (US$ billions)") +
  xlab("Year")+
  ggtitle("ECOWAS trade patterns") +
  scale_colour_manual("Flow", values = c("red","blue","black")) +
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



## ECOWAS annual average growth in trade 1971-2016

## Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/ECOWAS_Annual_average_trade_growth _rates.csv", 
            col_types = cols(value = col_number()))
View(dat)

dat1=subset(dat, year %in% 1981:2016)


tiff(filename="ECOWAS_annual_trade_growth.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat1, aes(year,value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "average annual growth rate (%)") +
  xlab("Year")+
  ggtitle("ECOWAS trade patterns") +
  scale_colour_manual("Flow", values = c("red","blue")) +
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


# Building the plot (using the ggplot2 package)

# Themes
theme_wsj()
theme_stata()
theme_hc()
theme_gdocs()
theme_economist()



## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))


# Intra ECOWAS trade
dat1 = select(subset(dat, iso_d %in% ecowasset & 
                       dat$iso_o %in% ecowasset & year %in% 1970:2006),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 = dat2$flow/1e3

tiff(filename="Intra-ECOWAS.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Intra-ECOWAS imports") +
  geom_vline(xintercept = 1975, color="red") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p1

dev.off()



# Import data
dat <- read_csv("~/Documents/TRADE DATA/Intra_ECOWAS_imports.csv", 
                col_types = cols(flow = col_number()))
View(dat)

dat$flow1 = dat$flow/1e3

tiff(filename="ECOWAS_imports_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Intra ECOWAS imports") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p1

dev.off()


## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))


# ECOWAS imports from ROW
dat1 = select(subset(dat, year %in% 1970:2006 & iso_d %in% ecowasset & 
                       !(dat$iso_o %in% ecowasset)),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 <- dat2$flow/1e3


# Building the plot (using the ggplot2 package)

tiff(filename="ECO-imp-ROW.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("ECOWAS imports from ROW") +
  geom_vline(xintercept = 1975, color="red") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p1


dev.off()



# Import data
library(readr)
dat3<- read_csv("~/Documents/TRADE DATA/row_ecowas_imports_current.csv", 
                col_types = cols(flow = col_number()))
View(dat3)

dat3$flow1 = dat3$flow/1e2

tiff(filename="ECO-imp-ROW_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat3, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("ECOWAS imports from ROW") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p1

dev.off()




# ECOWAS export to ROW
dat1 = select(subset(dat, year %in% 1970:2006 & iso_o %in% ecowasset & 
                       !(dat$iso_d %in% ecowasset)),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 <- dat2$flow/1e3

# Building the plot (using the ggplot2 package)

tiff(filename="ECO-exp-ROW.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p2 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("ECOWAS exports to ROW") +
  geom_vline(xintercept = 1975, color="red") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p2


dev.off()

grid.arrange(p1, p2, ncol=2)

# Import data
library(readr)
dat<- read_csv("~/Documents/TRADE DATA/row_ecowas_exports_current.csv", 
               col_types = cols(flow = col_number()))
View(dat)

dat$flow1 = dat$flow/1e3


tiff(filename="ECO-exp-ROW_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("ECOWAS exports to ROW") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_gdocs()
p1

dev.off()




## Bar graphs
# link : http://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2/

library(readr)
dat <- read_csv("~/Documents/TRADE DATA/row_ECOWAS.csv", 
                col_types = cols(value = col_number()))
View(dat)


tiff(filename="ECO_exp_rorw.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p1 = ggplot(dat,aes(year,y = value,fill=partner)) +
  geom_bar(position="dodge", stat="identity") +
  xlab("Year") + ylab("Trade flow (% by destination)") +
  ggtitle("ECOWAS exports") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Trade flow",
                       breaks=c("Rest of the region", "Rest of world")) +
  theme_gdocs()+
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


## Major trade partners #####
# Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/ecowas_trade_partners.csv", 
                col_types = cols(exp = col_number(), 
                                 imp = col_number(), trade = col_number()))
View(dat)

dat$trade1 = dat$trade/1e6

nat=c("China","Brazil","India")

dat1 = subset(dat, year %in% 1970:2006 & !(country %in% nat)) 

tiff(filename="Major_trade_partners.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat1, aes(year,trade1,colour=country)) +
  geom_line(aes(colour = country, group = country)) +
  scale_y_continuous(name = "Trade value in $US billions") +
  xlab("Year") +
  ggtitle("Merchandise trade flows of ECOWAS with major Trade partners") +
  scale_colour_manual("Partners", values = c("red", "green","brown","black",
                                             "purple","orange")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()

# Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/ecowas_major_trade_partners_main.csv", 
                col_types = cols(exp = col_number(), 
                                 imp = col_number(), trade = col_number()))
View(dat)

dat$trade1 = dat$trade/1e3

tiff(filename="Major_trade_partners.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat, aes(year,trade1,colour=country)) +
  geom_line(aes(colour = country, group = country)) +
  scale_y_continuous(name = "Trade value in $US billions") +
  xlab("Year") +
  ggtitle("Merchandise trade flows of ECOWAS with major Trade partners") +
  scale_colour_manual("Partners", values = c("red","green","brown","black",
                                             "purple","orange")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()





# Composition of intra-ECOWAS trade
#-----------------------------------------------------------------------

# Importing data set
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Comp_intra-ECOWAS_exports.csv", 
                col_types = cols(percent = col_number()))
View(dat)


tiff(filename="Comp_intra_ECO_exp.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p1 = ggplot(dat,aes(year,y = percent,fill=comp)) +
  geom_bar(position="fill", stat="identity") +
  xlab("Year") + ylab("Trade flow (% by destination)") +
  ggtitle("Composition of intra-ECOWAS exports") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Component",
                       breaks=c("Primary products", "manufactured products","other products")) +
  theme_gdocs()+
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


## line graph
p1 <- ggplot(dat,aes(year,y = percent,colour=comp)) +
  geom_line(aes(colour = comp, group = comp)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year") +
  ggtitle("Composition of intra-ECOWAS exports") +
  scale_colour_manual("Partners", values = c("brown","orange","green")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()





## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))


# Top 5 exporting countries in ECOWAS in each year

# calculating aggregate (total) export for each country
dat1=subset(dat, iso_o %in% ecowasset  & year %in% 1970:2006)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)

for (y in 2000:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries that import from ECOWAS in each year

# calculating aggregate (total) import for each country
dat1=subset(dat, iso_o %in% ecowasset & year %in% 1970:2006)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_d, year, flow), sum)

for (y in 2000:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}

# USA,EU-FRA,DEU,ESP,ITA,NLD,CHN,BRA


# Top 5 importers in Ecowas
# calculating aggregate (total) import for each country
dat1=subset(dat, iso_d %in% ecowasset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)
View(dat2)

# Top 5 exporting countries to ECOWAS in each year
for (y in 2000:2006) {
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




### BLOC/COUNTRY-LEVEL ####


### Trend of export and imports in ECOWAS (Global)

data = subset(dat,year %in% 1970:2006)


### Aggregate global exports
glo = aggregate(flow ~ ., data = select(data, year, flow), sum)


## Aggregate ECOWAS annual exports  
eco1= subset(data, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)
View(eco2)


## Aggregate ECOWAS annual imports 
eco3= subset(data, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)
View(eco4)

# ECOWAS export shares
eco2$ecoshare = eco2$flow/glo$flow

# ECOWAS import shares
eco4$ecoshare = eco4$flow/glo$flow

## Plotting two graphs

tiff(filename="global_trade.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

par(mfrow=c(1,2))
plot(eco2$year, eco2$ecoshare, type = "l", 
     xlab = "Year",
     ylab = "export share",
     main = "Annual ECOWAS export shares")

plot(eco4$year, eco4$ecoshare, type = "l", 
     xlab = "Year",
     ylab = "import share",
     main = "Annual ECOWAS import shares")

dev.off()

## Alternate plot ###

tiff(filename="gexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(eco2, aes(year,ecoshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Annual ECOWAS export shares") +
  geom_vline(xintercept = 1975, color="red") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_hc()
p1

dev.off()


tiff(filename="gimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")



p1 <- ggplot(eco4, aes(year,ecoshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Annual ECOWAS export shares") +
  geom_vline(xintercept = 1975, color="red") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))+
  theme_hc()
p1



dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)





## ECOWAS export shares to EU28

# EU 28 countries:
# Note that the EU15 dummy includes non-EU countries!
eulist = c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
           "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
           "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
           "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

### Aggregate EU import
eu1 = subset(dat, iso_d %in% eulist & year %in% 1970:2006)
eu2 = aggregate(flow ~ ., data = select(eu1, year, flow), sum)

## ECOWAS share of export to EU
eco1= subset(eu1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)

### Aggregate ECOWAS import 
eco3 = subset(data, iso_d %in% ecowasset)
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



tiff(filename="2EUexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export to EU28 shares")

dev.off()


p9 <- ggplot(eco2, aes(year,ecoshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export to EU28 shares") +
  theme_economist() 
p9






tiff(filename="2EUimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref.EU 28)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)



p9 <- ggplot(eco4, aes(year,ecoshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import to EU28 shares") +
  theme_economist() 
p9





## ECOWAS export shares to GBR

### Aggregate gbr import
gbr1 = subset(dat, iso_d == "GBR" & year %in% 1970:2006)

gbr2 = aggregate(flow ~ ., data = select(gbr1, year, flow), sum)

## ECOWAS share of export to gbr
eco= subset(gbr1, iso_o %in% ecowasset)
eco1 = aggregate(flow ~ ., data = select(eco, year, flow), sum)


## ECOWAS import shares from gbr
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset & year %in% 1970:2006)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## gbr export to ECOWAS
gbr3= subset(eco3, iso_o == "GBR")
gbr4 = aggregate(flow ~ ., data = select(gbr3, year, flow), sum)

# ECOWAS export share to gbr
eco1$ecoshare = eco1$flow/gbr2$flow

# ECOWAS import share from EU
eco4$ecoshare = gbr4$flow/eco4$flow



##Plotting the two graphs
plot(eco1$year, eco1$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to Great Britian")


plot(eco4$year, eco4$ecoshare, type = "l",
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from Great Britian")

dev.off()



tiff(filename="2GBRexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco1, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export shares (ref.Great Britian)")

dev.off()



tiff(filename="2GBRimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref.Great Britian)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)





## ECOWAS export shares to USA

### Aggregate US import
us1 = subset(data, iso_d == "USA")

us2 = aggregate(flow ~ ., data = select(us1, year, flow), sum)

## ECOWAS share of export to US
eco= subset(us1, iso_o %in% ecowasset)
eco1 = aggregate(flow ~ ., data = select(eco, year, flow), sum)


## ECOWAS import shares from US
### Aggregate ECOWAS import 
eco3 = subset(data, iso_d %in% ecowasset)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## US export to ECOWAS
us3= subset(eco3, iso_o == "USA")
us4 = aggregate(flow ~ ., data = select(us3, year, flow), sum)

# ECOWAS export share to US
eco1$ecoshare = eco1$flow/us2$flow

# ECOWAS import share from EU
eco4$ecoshare = us4$flow/eco4$flow
 


##Plotting the two graphs
plot(eco1$year, eco1$ecoshare, type = "l", ylim = c(0,0.046), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to US")


plot(eco4$year, eco4$ecoshare, type = "l", ylim = c(0,0.135), 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from US")

dev.off()



tiff(filename="2USexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco1, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export shares (ref. USA)")

dev.off()



tiff(filename="2USimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref. USA)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)




## ECOWAS export shares to China

### Aggregate China import
ch1 = subset(dat, iso_d == "CHN" & year %in% 1970:2006)

ch2 = aggregate(flow ~ ., data = select(ch1, year, flow), sum)

## ECOWAS share of export to China
eco1= subset(ch1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)


## ECOWAS import shares from China
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset  & year %in% 1970:2006)
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


tiff(filename="2CHNexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export shares (ref. China)")

dev.off()


tiff(filename="2CHNimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref. China)")

dev.off()


library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)



## ECOWAS export shares to India

### Aggregate India import
ind1 = subset(dat, iso_d == "IND" & year %in% 1970:2006)

ind2 = aggregate(flow ~ ., data = select(ind1, year, flow), sum)

## ECOWAS share of export to India
eco1= subset(ind1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)


## ECOWAS import shares from India
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset  & year %in% 1970:2006)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## India export to ECOWAS
ind3= subset(eco3, iso_o == "IND")
ind4 = aggregate(flow ~ ., data = select(ind3, year, flow), sum)

# ECOWAS export share to India
eco2$ecoshare = eco2$flow/ind2$flow

# ECOWAS import share from India
eco4$ecoshare = ind4$flow/eco4$flow


plot(eco2$year, eco2$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to India")

plot(eco4$year, eco4$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from India")


tiff(filename="2INDexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export shares (ref. India)")

dev.off()


tiff(filename="2INDimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref. India)")

dev.off()


library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)



## ECOWAS export shares to France

### Aggregate France import
fra1 = subset(dat, iso_d == "FRA" & year %in% 1970:2006)

fra2 = aggregate(flow ~ ., data = select(fra1, year, flow), sum)

## ECOWAS share of export to France
eco1= subset(fra1, iso_o %in% ecowasset)
eco2 = aggregate(flow ~ ., data = select(eco1, year, flow), sum)


## ECOWAS import shares from France
### Aggregate ECOWAS import 
eco3 = subset(dat, iso_d %in% ecowasset  & year %in% 1970:2006)
eco4 = aggregate(flow ~ ., data = select(eco3, year, flow), sum)

## France export to ECOWAS
fra3= subset(eco3, iso_o == "FRA")
fra4 = aggregate(flow ~ ., data = select(fra3, year, flow), sum)

# ECOWAS export share to France
eco2$ecoshare = eco2$flow/fra2$flow

# ECOWAS import share from France
eco4$ecoshare = fra4$flow/eco4$flow


plot(eco2$year, eco2$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS export shares to France")

plot(eco4$year, eco4$ecoshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "ECOWAS import shares from France")


tiff(filename="2IFRAexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco2, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS export shares (ref. France)")

dev.off()


tiff(filename="2FRAimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(eco4, aes(year, ecoshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS import shares (ref. France)")

dev.off()









