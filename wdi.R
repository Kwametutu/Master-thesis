
## Extracting the World Bank Development Indicators (WDI)

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
library(lfe)
library(RJSONIO)
library(WDI)
library(data.table)
library(dtplyr)

#library(grid)
#library(gridExtra)
#library(cowplot)


pack<-c("car","sandwich","ggplot2","lmtest","RColorBrewer","plm","dplyr",
        "mgcv","foreign","xtable","AER","stargazer","lfe","data.table",
         "ggthemes","lfe","gridExtra","cowplot","RJSONIO","dtplyr","WDI")

lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"

## Load dataset and create dummies
##dat = read.dta(paste0(path,"gravdata.dta"))


### indicators ####
indicator = c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
              "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
              "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MRCH.CD.WT",
              "TM.VAL.MANF.ZS.UN")


### ECOWAS ####
dat <-  WDI(country = c("BJ", "BF","CV","CI","GM","GH","GN","GW", "LR", "ML",
                        "NE","NG", "SN", "SL", "TG"),
            indicator = c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
                          "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
                          "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"),
            start = 1970, end = 2014, 
            extra = T)

dat <-  WDI(country = c("BJ", "BF","CV","CI","GM","GH","GN","GW", "LR", "ML",
                        "NE","NG", "SN", "SL", "TG"),
            indicator = c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
                          "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
                          "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"),
            start = 1995, end = 2014, 
            extra = T)

## Removing some columns
dat$iso2c <- NULL
dat$region <- NULL
dat$capital <- NULL
dat$longitude <- NULL
dat$latitude <- NULL  
dat$income <- NULL
dat$lending <- NULL


## Assigning country code to Cabo Verde
dat$iso3c[is.na(dat$iso3c)] <- "CPV"


## Renaming columns
setnames(dat, c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
                "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
                "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"),
              c("population", "gdp_c", "gdp_b","gdp_pc_c","gdp_pc_b", "gdp_growth",
                "merch_exp","merch_imp","trade","agric_exp","merch_trade","manu_imp"))


wdi=subset(dat, select=c("country","iso3c","year","population", "gdp_c", "gdp_b","gdp_pc_c","gdp_pc_b",
                         "gdp_growth","merch_exp","merch_imp","trade","agric_exp","merch_trade","manu_imp"))

#View(wdi)
#save(wdi,file="Ecowas.Rda")
#load("Ecowas.Rda")



## 1.b

# number of countries
summary(levels(factor(dat$iso3c)))

# Calculating Population, total
dat1 = aggregate(population ~ .,data = select(wdi,iso3c, year, population),sum)
dat2 = aggregate(population ~ .,data = select(wdi,year, population),sum)

dat2=subset(dat1,year==2006)
colSums(dat2["population"])
dat2=subset(dat1,year==2015)



# Top 5 countries 
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$population, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries 
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$population, decreasing=F)[1:5],]
  print(d1)
}


# calculating GDP growth
dat1 = aggregate(gdp_growth ~ ., data = select(dat, iso3c, year, gdp_growth), mean)

dat2 = aggregate(gdp_growth  ~ ., data = select(dat,year, gdp_growth), mean)


# Setting path for graph

graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/ECOWAS_profile"
setwd(graphs)



# Building the plot (using the ggplot2 package)

tiff(filename="ECO-growth_rates.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(dat2, aes(year,gdp_growth)) +
  geom_line() +
  scale_y_continuous(name = "Growth rates(%)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle(" Annual average growth rates in ECOWAS")+
  theme_hc() 

dev.off()

dat1 = aggregate(gdp_growth ~ ., data = select(dat, iso3c, year, gdp_growth), mean)

# Top 5 countries with highest growth rates
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_growth, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest growth rates
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_growth, decreasing=F)[1:5],]
  print(d1)
}


# calculating aggregate (total) GDP
dat1 = aggregate(gdp_b ~ ., data = select(dat, iso3c, year, gdp_b), mean)
dat2 = aggregate(gdp_b ~ ., data = select(dat,year, gdp_b), sum)


dat1 = aggregate(gdp_c ~ ., data = select(dat, iso3c, year, gdp_c), mean)
dat2 = aggregate(gdp_c ~ ., data = select(dat,year, gdp_c), sum)

colSums(dat2["gdp_b"])


# Top 5 countries with highest GDP, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest GDP, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=F)[1:5],]
  print(d1)
}



# calculating GDP per capita, PPP (constant 2011 international $)
dat1 = aggregate(gdp_pc_b ~ ., data = select(dat, iso3c, year, gdp_pc_b), mean)

dat2 = aggregate(gdp_pc_b ~ ., data = select(dat1,year, gdp_pc_b), mean)

dat2=subset(dat1,year==2006)
dat2=subset(dat1,year==2015)
mean(dat2$gdp_pc_b)


# Top 5 countries with GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with lowest GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=F)[1:5],]
  print(d1)
}




# calculating aggregate (total) GDP
dat1 = aggregate(gdp_b ~ ., data = select(dat, iso3c, year, gdp_b), mean)
dat2 = aggregate(gdp_b ~ ., data = select(dat,year, gdp_b), sum)

colSums(dat2["gdp_b"])


# Top 5 countries with highest GDP, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest GDP, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=F)[1:5],]
  print(d1)
}



# calculating GDP per capita, PPP (constant 2011 international $)
dat1 = aggregate(gdp_pc_b ~ ., data = select(dat, iso3c, year, gdp_pc_b), mean)

dat2 = aggregate(gdp_pc_b ~ ., data = select(dat1,year, gdp_pc_b), mean)

dat2=subset(dat1,year==2006)
dat2=subset(dat1,year==2015)
mean(dat2$gdp_pc_b)



# Top 5 countries with GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with lowest GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=F)[1:5],]
  print(d1)
}





# Top 5 countries with exports

# Calculating exports
dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)

dat2=subset(dat1,year==2006)
dat2=subset(dat1,year==2015)
colSums(dat2["merch_exp"])


# Share of merchandise exports
dat2=subset(dat1,year==2006)
dat2$expshare= dat2$merch_exp/78063890000
colSums(dat2["expshare"])


# Top 5 countries with highest export shares
for (y in 2006) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$expshare, decreasing=T)[1:5],]
  print(d1)
}


dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)
dat2=subset(dat1,year==2015)
dat2$expshare= dat2$merch_exp/84097553180
colSums(dat2["expshare"])

# Top 5 countries with highest export shares
for (y in 2015) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$expshare, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with exports
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_exp, decreasing=T)[1:5],]
  print(d1)
}


##specify the path of the folder containing the graphs
graphs="/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/ECOWAS_profile/"
setwd(graphs)

library(ggthemes)

## Trend of  ECOWAS total merchandise exports

dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)
dat2$merch_exp1 = dat2$merch_exp/1e9


tiff(filename="ECO-merchexp_line.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(dat2, aes(year,merch_exp1)) +
  geom_line() +
  scale_y_continuous(name = "Trade value (billion $US)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS annual merchandise exports") +
  theme_hc() 

dev.off()



# Top 5 countries with imports

# Calculating imports
dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), sum)
dat2 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)

dat2=subset(dat1,year==2006)
dat2=subset(dat1,year==2015)
colSums(dat2["merch_imp"])


# Share of merchandise imports
dat2=subset(dat1,year==2006)
dat2$impshare= dat2$merch_imp/51908225731
colSums(dat2["impshare"])


# Top 5 countries with highest export shares
for (y in 2006) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$impshare, decreasing=T)[1:5],]
  print(d1)
}


dat2 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)
dat2=subset(dat1,year==2015)
dat2$impshare= dat2$merch_imp/96726366410
colSums(dat2["impshare"])

# Top 5 countries with highest export shares
for (y in 2015) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$impshare, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with imports
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_imp, decreasing=T)[1:5],]
  print(d1)
}



## Trend of  ECOWAS total merchandise imports
dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), sum)
dat2 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)
dat2$merch_imp1 = dat2$merch_imp/1e9


tiff(filename="ECO-merchimp_line.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


ggplot(dat2, aes(year,merch_imp1)) +
  geom_line() +
  scale_y_continuous(name = "Trade value (billion $US)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("ECOWAS annual merchandise imports") +
  theme_hc() 

dev.off()


## Plotting both curves on the same graph
dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)
dat2$exports = dat2$merch_exp/1e9


dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), sum)
dat3 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)
dat3$imports = dat3$merch_imp/1e9

dat6 <- data.frame( year=c(1970:2014),exports = dat2$exports,imports = dat3$imports)

dat7 <- t(dat6)




## Multiple line of Merchandise trade

tiff(filename="ECO_Merchandise trade.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

lines(plot(dat6$year,dat6$exports,type="l", 
           xlab = "year",
           ylab = "trade value (billion $US)",
           main = "Merchandise trade in ECOWAS region",
           col= "red"))
lines(dat6$year,dat6$imports,type="l",col="blue")

legend("topleft", 
       border="black",col=c("red","blue"),
       lty=c(1,1),
       legend=c("Exports","Imports"),
       bg ="white")

dev.off()



# Top 5 countries with agric_exp
# Calculating agric_exp
dat1 = aggregate(agric_exp ~ ., data = select(dat, iso3c, year, agric_exp), mean)


# Top 5 countries with agric_exp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$agric_exp, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with agric_exp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$agric_exp, decreasing=F)[1:5],]
  print(d1)
}


# Top 5 countries with merch_trade

# Calculating merch_trade
dat1 = aggregate(merch_trade ~ ., data = select(dat, iso3c, year, merch_trade), mean)


# Top 5 countries with merch_trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_trade, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with merch_trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_trade, decreasing=F)[1:5],]
  print(d1)
}



# Top 5 countries with trade

# Calculating trade
dat1 = aggregate(trade ~ ., data = select(dat, iso3c, year, trade), mean)


# Top 5 countries with trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$trade, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$trade, decreasing=F)[1:5],]
  print(d1)
}



# Top 5 countries with merch_imp

# Calculating trade
dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), mean)


# Top 5 countries with merch_imp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_imp, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with manu_imp

# Calculating trade
dat1 = aggregate(manu_imp ~ ., data = select(dat, iso3c, year, manu_imp), mean)


# Top 5 countries with manu_imp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$manu_imp, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with exports

# Calculating exports
dat1 = aggregate(merch_exp ~ ., data = select(dat,year, merch_exp), sum)


dat2 = aggregate(merch_imp ~ ., data = select(dat,year, merch_imp), sum)


dat3 = data.frame(year=dat1$year,exports=dat1$merch_exp,imports=dat2$merch_imp)


## Importing data set
library(readr)
dat3 <- read_csv("~/Desktop/Data/Gravity/export_import.csv", 
                col_types = cols(Value = col_number()))
View(dat3)

dat3$value= dat3$Value/1e9
dat4=subset(dat3, Year %in% 1970:1990)
dat5=subset(dat3, Year %in% 1991:2006)
dat6=subset(dat3, Year %in% 2007:2016)

library(ggthemes)

## Graphing a multiple Bar chart
tiff(filename="ECO_Merchtrade_70-90.jpeg", width=30,height=30, bg=FALSE, res=500, units="cm")

p1 = ggplot(dat4,aes(factor(Year),y = value,fill=Tn)) +
  geom_bar(stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Trade flow in billions of $US") +
  ggtitle("ECOWAS annual merchandise trade flows (1970-1900)") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Trade flow",
                       breaks=c("Exports", "Imports")) +
  theme_hc() 

p1



dev.off()



tiff(filename="ECO_Merchtrade_91-06.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p2 = ggplot(dat5,aes(factor(Year),y = value,fill=Tn)) +
  geom_bar(stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Trade flow in billions of $US") +
  ggtitle("ECOWAS annual merchandise trade flows (1991-2006)") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Trade flow",
                       breaks=c("Exports", "Imports")) +
  theme_hc() 

p2

dev.off()


tiff(filename="ECO_Merchtrade_07-16.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p3 = ggplot(dat6,aes(factor(Year),y = value,fill=Tn)) +
  geom_bar(stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Trade flow in billions of $US") +
  ggtitle("ECOWAS annual merchandise trade flows (2007-2016)") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Trade flow",
                       breaks=c("Exports", "Imports")) +
  theme_hc() 

p3

dev.off()



tiff(filename="ECO_1.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat4, aes(factor(Year),value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  theme_hc() +
  xlab("Year")+
  ggtitle("ECOWAS annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
  theme_hc() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p1


p1 <- ggplot(dat3, aes(Year,value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  theme_gdocs() +
  xlab("Year")+
  ggtitle("ECOWAS annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
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


tiff(filename="ECO_2.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p2 <- ggplot(dat5, aes(factor(Year),value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  xlab("Year")+
  ggtitle("ECOWAS annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
  theme_hc() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p2

dev.off()


tiff(filename="ECO_3.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p3 <- ggplot(dat6, aes(factor(Year),value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  xlab("Year")+
  ggtitle("ECOWAS annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
  theme_hc() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p3

dev.off()

grid.arrange(p1,p2,p3,nrow=3,ncol=1)


dev.off()


#grid.arrange(p1, p2, ncol=2)


# Setting path for graph

graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures/COMESA_profile"
setwd(graphs)




###### COMESA ##############

comesa = c("BI","CD","DJ","EG","ET","ER","KE","KM","LY","MG","MW",
           "MU","NA","RW","SC","SD","SZ","UG","ZM", "ZW")


### COMESA ####
dat <-  WDI(country =   c("BI","CD","DJ","EG","ET","ER","KE","KM","LY","MG","MW",
                          "MU","NA","RW","SC","SD","SZ","UG","ZM", "ZW"),
            indicator = c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
                          "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
                          "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"),
             start = 1990, end = 2015, 
             extra = T)


dat <-  WDI(country =  c("AO","BI","CG","DJ","EG","ET","ER","KE","KM","LY","MG","MW",
                         "MU","NA","RW","SC","SD","SZ","UG","ZM", "ZW"),
            indicator = c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
                          "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
                          "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"),
            start = 1995, end = 2015, 
            extra = T)

## Removing some columns
dat$iso2c <- NULL
dat$region <- NULL
dat$capital <- NULL
dat$longitude <- NULL
dat$latitude <- NULL  
dat$income <- NULL
dat$lending <- NULL


## Renaming columns
setnames(dat,c("SP.POP.TOTL","NY.GDP.MKTP.PP.CD","NY.GDP.MKTP.PP.KD","NY.GDP.PCAP.PP.CD",
               "NY.GDP.PCAP.PP.KD","NY.GDP.MKTP.KD.ZG","TX.VAL.MRCH.CD.WT","TM.VAL.MRCH.CD.WT",
               "NE.TRD.GNFS.ZS","TX.VAL.AGRI.ZS.UN","TG.VAL.TOTL.GD.ZS","TM.VAL.MANF.ZS.UN"), 
         c("population", "gdp_c", "gdp_b","gdp_pc_c","gdp_pc_b", "gdp_growth",
           "merch_exp","merch_imp","trade","agric_exp","merch_trade","manu_imp"))


wdi=subset(dat, select=c("country","iso3c","year","population", "gdp_c", "gdp_b","gdp_pc_c","gdp_pc_b",
                         "gdp_growth","merch_exp","merch_imp","trade","agric_exp","merch_trade","manu_imp"))

#View(wdi)
#save(wdi,file="Comesa.R")




## 1.b

# number of countries
summary(levels(factor(dat$iso3c)))
unique(dat$country)

# Calculating Population, total
dat1 = aggregate(population ~ .,data = select(wdi,iso3c,year, population),sum)

dat2 = aggregate(population ~ .,data = select(dat1,year, population),sum)


dat2=subset(dat1,year==2006)
dat2$popshare= dat2$population/366274151
colSums(dat2["popshare"])

dat2=subset(dat1,year==2015)
dat2$popshare= dat2$population/502593539
colSums(dat2["popshare"])



# Top 5 countries 
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$population, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries 
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$population, decreasing=F)[1:5],]
  print(d1)
}



# calculating GDP growth (Annual growth rates)
dat1 = aggregate(gdp_growth ~ ., data = select(dat, iso3c, year, gdp_growth), mean)

dat2 = aggregate(gdp_growth  ~ ., data = select(dat,year, gdp_growth), mean)



# Building the plot (using the ggplot2 package)

tiff(filename="Growth_rates.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(dat2, aes(year,gdp_growth)) +
  geom_line() +
  scale_y_continuous(name = "Growth rates(%)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Annual average growth rates in COMESA")+
  theme_hc() 

dev.off()


# Top 5 countries with highest growth rates
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_growth, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest growth rates
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_growth, decreasing=F)[1:5],]
  print(d1)
}


# Richest countries : http://ireport.cnn.com/docs/DOC-1264947

# calculating aggregate (total) GDP
dat1 = aggregate(gdp_b ~ ., data = select(dat, iso3c, year, gdp_b), mean)
dat2 = aggregate(gdp_b ~ ., data = select(dat,year, gdp_b), sum)


dat1 = aggregate(gdp_c ~ ., data = select(dat, iso3c, year, gdp_c), mean)
dat2 = aggregate(gdp_c ~ ., data = select(dat,year, gdp_c), sum)



# Top 5 countries with highest GDP, PPP (constant 2011 international $)

for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest GDP, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_b, decreasing=F)[1:5],]
  print(d1)
}



# calculating GDP per capita, PPP (constant 2011 international $)
dat1 = aggregate(gdp_pc_b ~ ., data = select(dat, iso3c, year, gdp_pc_b), mean)

dat2 = aggregate(gdp_pc_b ~ ., data = select(dat1,year, gdp_pc_b), mean)

dat2=subset(dat1,year==2006)
mean(dat2$gdp_pc_b)

dat2=subset(dat1,year==2015)
mean(dat2$gdp_pc_b)


# Top 5 countries with GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=T)[1:5],]
  print(d1)
}


# Bottom 5 countries with lowest GDP per capita, PPP (constant 2011 international $)
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$gdp_pc_b, decreasing=F)[1:5],]
  print(d1)
}



## Trend of  COMESA total merchandise exports
# Calculating exports
dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)

dat2=subset(dat1,year==2006)
dat2=subset(dat1,year==2015)
colSums(dat2["merch_exp"])


# Share of merchandise exports
dat2=subset(dat1,year==2006)
dat2$expshare= dat2$merch_exp/120814545302
colSums(dat2["expshare"])

for (y in 2006) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$expshare, decreasing=T)[1:5],]
  print(d1)
}


dat2=subset(dat1,year==2015)
dat2$expshare= dat2$merch_exp/106388369250
colSums(dat2["expshare"])

for (y in 2015) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$expshare, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with exports
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_exp, decreasing=T)[1:5],]
  print(d1)
}


dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)
dat2$merch_exp1 = dat2$merch_exp/1e9

tiff(filename="COM_merchexp.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(dat2, aes(year,merch_exp1)) +
  geom_line() +
  scale_y_continuous(name = "Trade value (billion $US)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1994, color="red") +
  ggtitle("COMESA annual merchandise exports") +
  theme_hc() 

dev.off()



# Top 5 countries with imports

# Calculating imports
dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), sum)

dat2 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)

dat2=subset(dat1,year==2006)
colSums(dat2["merch_imp"])


dat2=subset(dat1,year==2015)
colSums(dat2["merch_imp"])



# Top 5 countries with imports
for (y in 2006:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_imp, decreasing=T)[1:5],]
  print(d1)
}


# Share of merchandise imports
dat2=subset(dat1,year==2006)
dat2$impshare= dat2$merch_imp/86733561000
colSums(dat2["impshare"])

for (y in 2006) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$impshare, decreasing=T)[1:5],]
  print(d1)
}


dat2=subset(dat1,year==2015)
dat2$impshare= dat2$merch_imp/195710619003
colSums(dat2["impshare"])

for (y in 2015) {
  d = subset(dat1, year == y)
  d1 = dat2[order(dat2$impshare, decreasing=T)[1:5],]
  print(d1)
}



## Plotting the graph for merchandise imports

tiff(filename="COM_merchimp.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

dat2 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)
dat2$merch_imp1 = dat2$merch_imp/1e9

ggplot(dat2, aes(year,merch_imp1)) +
  geom_line() +
  scale_y_continuous(name = "Trade value (billion $US)") +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1994, color="red") +
  ggtitle("COMESA annual merchandise imports") +
  theme_hc() 

dev.off()



## plotting both curves on the same graph
dat1 = aggregate(merch_exp ~ ., data = select(dat, iso3c, year, merch_exp), sum)
dat2 = aggregate(merch_exp ~ ., data = select(dat1,year, merch_exp), sum)
dat2$exports = dat2$merch_exp/1e9


dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), sum)
dat3 = aggregate(merch_imp ~ ., data = select(dat1,year, merch_imp), sum)
dat3$imports = dat3$merch_imp/1e9

dat6 <- data.frame( year=c(1990:2015),exports = dat2$exports,imports = dat3$imports)



## transposing dataframe
dat7 <- t(dat6)
View(dat7)

## Multiple line of trade creation and diversion

tiff(filename="COM_Merchandise trade.jpeg", width=20,height=25, bg=FALSE, res=400, units="cm")

lines(plot(dat6$year,dat6$exports,type="l", 
           xlab = "year",
           ylab = "trade value (billion $US)",ylim=c(0,222),
           main = "Merchandise trade in COMESA region",
           col= "red"))
lines(dat6$year,dat6$imports,type="l",col="blue")

legend("topleft", 
       border="black",col=c("red","blue"),
       lty=c(1,1),
       legend=c("Exports","Imports"),
       bg ="white")

dev.off()




# Top 5 countries with agric_exp
# Calculating agric_exp
dat1 = aggregate(agric_exp ~ ., data = select(dat, iso3c, year, agric_exp), mean)


# Top 5 countries with agric_exp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$agric_exp, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with agric_exp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$agric_exp, decreasing=F)[1:5],]
  print(d1)
}



# Top 5 countries with merch_trade

# Calculating merch_trade
dat1 = aggregate(merch_trade ~ ., data = select(dat, iso3c, year, merch_trade), mean)


# Top 5 countries with merch_trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_trade, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with merch_trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_trade, decreasing=F)[1:5],]
  print(d1)
}



# Top 5 countries with trade

# Calculating trade
dat1 = aggregate(trade ~ ., data = select(dat, iso3c, year, trade), mean)


# Top 5 countries with trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$trade, decreasing=T)[1:5],]
  print(d1)
}

# Bottom 5 countries with trade
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$trade, decreasing=F)[1:5],]
  print(d1)
}



# Top 5 countries with merch_imp

# Calculating trade
dat1 = aggregate(merch_imp ~ ., data = select(dat, iso3c, year, merch_imp), mean)


# Top 5 countries with merch_imp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$merch_imp, decreasing=T)[1:5],]
  print(d1)
}



# Top 5 countries with manu_imp

# Calculating trade
dat1 = aggregate(manu_imp ~ ., data = select(dat, iso3c, year, manu_imp), mean)


# Top 5 countries with manu_imp
for (y in 2007:2015) {
  d = subset(dat1, year == y)
  d1 = d[order(d$manu_imp, decreasing=T)[1:5],]
  print(d1)
}


# Top 5 countries with exports

# Calculating exports
dat1 = aggregate(merch_exp ~ ., data = select(dat,year, merch_exp), sum)


dat2 = aggregate(merch_imp ~ ., data = select(dat,year, merch_imp), sum)


dat3 = data.frame(year=dat1$year,exports=dat1$merch_exp,imports=dat2$merch_imp)


## Importing data set
library(readr)
dat3 <- read_csv("~/Desktop/Data/Gravity/Comesa_trade_flow.csv", 
                 col_types = cols(Value = col_number()))
View(dat3)


dat3$value= dat3$Value/1e3
dat4=subset(dat3, Year %in% 1995:2005)
dat5=subset(dat3, Year %in% 2006:2016)



## Graphing a multiple Bar chart

tiff(filename="CO_Merchtrade_1990-2005.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p1 = ggplot(dat4,aes(factor(Year),y = value,fill=Tn)) +
  geom_bar(stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Trade flow in billions of $US") +
  ggtitle("COMESA annual merchandise trade flows (1990-2005)") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
        axis.title = element_text(size = 18),legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12)) +
  guides(fill=guide_legend(title="Trade flow")) +
  scale_shape_discrete(name  ="Trade flow",
                       breaks=c("Exports", "Imports")) +
  theme_hc()

p1

dev.off()


tiff(filename="CO_Merchtrade_2006-2016.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p2 = ggplot(dat5,aes(factor(Year),y = value,fill=Tn)) +
  geom_bar(stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Trade flow in billions of $US") +
  ggtitle("COMESA annual merchandise trade flows (2006-2016)") +
  theme(plot.title=element_text(size=20), axis.text = element_text(size= 14),
      axis.title = element_text(size = 18),legend.text = element_text(size = 14),
      legend.title=element_text(face = "bold", size = 12)) +
      guides(fill=guide_legend(title="Trade flow")) +
      scale_shape_discrete(name  ="Trade flow",
      breaks=c("Exports", "Imports")) +
      theme_hc()
p2

dev.off()


## line graphs

tiff(filename="CO_1.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat4, aes(factor(Year),value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  theme_gdocs() +
  xlab("Year")+
  ggtitle("COMESA annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
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


tiff(filename="CO_2.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p2 <- ggplot(dat5, aes(factor(Year),value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  xlab("Year")+
  ggtitle("COMESA annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p2

dev.off()


tiff(filename="CO_3.jpeg", width=30,height=30, bg=FALSE, res=400, units="cm")

p3 <- ggplot(dat3, aes(Year,value,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade value in billions of $US") +
  xlab("Year") +
  ggtitle("COMESA annual merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p3


dev.off()

#grid.arrange(p1,p2,nrow=3,ncol=1)


