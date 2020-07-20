

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


## 1.a Load dataset and create dummies
dat = read.dta(paste0(path,"col_regfile09.dta"))


### Regional trade agreements ####

comesaset = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
           "MDG","MWI","MUS","RWA","SYC","SDN","SWZ","UGA","ZMB","ZWE")

comesaset = c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
              "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
              "UGA","ZMB","ZWE")



# Create COMESA-origin dummy
dat$com_o = ifelse(dat$iso_o %in% c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
                                    "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
                                    "UGA","ZMB","ZWE"),1,0)



# Create COMESA-destination dummy

dat$com_d = ifelse(dat$iso_d %in% c("AGO","BDI","COM","COD","DJI","EGY","ERI","ETH","KEN","LBY",
                                    "LSO","MDG","MWI","MUS","NAM","RWA","SYC","SDN","SWZ","TZA",
                                    "UGA","ZMB","ZWE"),1,0)


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




## 1.b Plot evolution of imports


## COMESA imports 

## Import data

library(readr)
dat <- read_csv("~/Documents/TRADE DATA/comesa_imports_main.csv", 
                col_types = cols(flow = col_number()))
View(dat)

dat$flow1 = dat$flow/1e3


tiff(filename="COMESA_trade.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat, aes(year,flow1,colour=partner)) +
  geom_line(aes(colour = partner, group = partner)) +
  scale_y_continuous(name = "Trade flow (US$ billions)") +
  xlab("Year")+
  ggtitle("COMESA trade patterns") +
  scale_colour_manual("Flow", values = c("red","blue","black")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title=element_text(face = "bold", size = 12))
p1

dev.off()



# Intra COMESA trade
dat1 = select(subset(dat, iso_d %in% comesaset & 
                       dat$iso_o %in% comesaset & year %in% 1970:2006),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 = dat2$flow/1e3

# Building the plot (using the ggplot2 package)

tiff(filename="Intra-COMESA.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p9 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  theme_gdocs() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Intra-COMESA imports") 
p9

dev.off()




# Import data
library(readr)
dat3 <- read_csv("~/Documents/TRADE DATA/intra_comesa_imports_current.csv", 
                 col_types = cols(flow = col_number()))
View(dat3)

dat3$flow1 = dat3$flow/1e3

tiff(filename="Intra-COMESA_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat3, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("Intra-COMESA imports") +
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



# COMESA import from ROW
dat1 = select(subset(dat, year %in% 1970:2006 & iso_d %in% comesaset & 
                       !(dat$iso_o %in% comesaset)),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 <- dat2$flow/1e3

# Building the plot (using the ggplot2 package)

tiff(filename="COM-imp-ROW.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("COMESA imports from ROW") +
  geom_vline(xintercept = 1994, color="red") +
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



# COMESA export to ROW
dat1 = select(subset(dat, year %in% 1970:2006 & iso_o %in% comesaset & 
                       !(dat$iso_d %in% comesaset)),
              year, flow)

dat2 = aggregate(flow ~ ., data = dat1, sum)

dat2$flow1 = dat2$flow/1e3

# Building the plot (using the ggplot2 package)

tiff(filename="COM-exp-ROW.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p2 <- ggplot(dat2, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("COMESA exports to ROW") +
  geom_vline(xintercept = 1994, color="red") +
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
dat3<- read_csv("~/Documents/TRADE DATA/row_comesa_imports_current.csv", 
                col_types = cols(flow = col_number()))
View(dat3)

dat3$flow1 = dat3$flow/1e3

tiff(filename="COM-imp-ROW_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat3, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("COMESA imports from ROW") +
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
dat3 <- read_csv("~/Documents/TRADE DATA/row_comesa_exports_current.csv", 
                 col_types = cols(flow = col_number()))
View(dat3)

dat3$flow1 = dat3$flow/1e3


tiff(filename="COM-exp-ROW_70-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat3, aes(year,flow1)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  ggtitle("COMESA exports to ROW") +
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
dat3 <- read_csv("~/Documents/TRADE DATA/row_COMESA.csv", 
                col_types = cols(value = col_number()))
View(dat3)


tiff(filename="COM_exp_rorw.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p1 = ggplot(dat3,aes(year,y = value1,fill=partner)) +
  geom_bar(stat="identity") +
  xlab("Year") + ylab("Trade flow (% by destination)") +
  ggtitle("COMESA exports") +
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
dat <- read_csv("~/Documents/TRADE DATA/comesa_major_trade_partners_main.csv", 
                col_types = cols(exp = col_number(), 
                                 imp = col_number(), trade = col_number()))
View(dat)

dat$trade1 = dat$trade/1e3

dat1 = subset(dat, year %in% 1970:2006)

tiff(filename="Major_trade_partners.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

p1 <- ggplot(dat1, aes(year,trade1,colour=country)) +
  geom_line(aes(colour = country, group = country)) +
  scale_y_continuous(name = "Trade value in $US billions") +
  xlab("Year") +
  ggtitle("Merchandise trade flows of COMESA with major Trade partners") +
  scale_colour_manual("Partners", values = c("red", "green","brown","black","purple")) +
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



# Composition of COMESA
# Importing data set
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Comp_intra-COMESA_exports.csv", 
                col_types = cols(percent = col_number()))
View(dat)


tiff(filename="Comp_intra_COM_exp.jpeg", width=20,height=20, bg=FALSE, res=400, units="cm")

p1 = ggplot(dat,aes(year,y = percent,fill=comp)) +
  geom_bar(position="fill", stat="identity") +
  xlab("Year") + ylab("Trade flow (% by destination)") +
  ggtitle("Composition of intra-COMESA exports") +
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











# Top 5 exporting countries in COMESA in each year
# calculating aggregate (total) export for each country
dat1=subset(dat, iso_o %in% comesaset  & year %in% 1970:2006)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)

for (y in 2000:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}



# Top 5 countries that import from COMESA in each year

# calculating aggregate (total) import for each country
dat1=subset(dat, iso_o %in% comesaset & year %in% 1970:2006)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_d, year, flow), sum)

for (y in 2000:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}

# USA,EU-FRA,DEU,ESP,ITA,NLD,CHN,BRA


# Top 5 importers in COMESA
# calculating aggregate (total) import for each country
dat1=subset(dat, iso_d %in% comesaset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_o, year, flow), sum)
View(dat2)

# Top 5 exporting countries to COMESA in each year
for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}




# Top 5 importers in COMESA
# calculating aggregate (total) import for each country
dat1=subset(dat, iso_d %in% comesaset)
dat2 = aggregate(flow ~ ., data = select(dat1, iso_d, year, flow), sum)


for (y in 1992:2006) {
  d = subset(dat2, year == y)
  d1 = d[order(d$flow, decreasing=T)[1:5],]
  print(d1)
}




### BLOC/COUNTRY-LEVEL ####


### Trend of export and imports in COMESA

data = subset(dat,year %in% 1970:2006)


### Aggregate global exports
com = aggregate(flow ~ ., data = select(data, year, flow), sum)


## Aggregate COMESA annual exports  
com1= subset(data, iso_o %in% comesaset)
com2 = aggregate(flow ~ ., data = select(com1, year, flow), sum)

## Aggregate COMESA annual imports 
com3= subset(data, iso_d %in% comesaset)
com4 = aggregate(flow ~ ., data = select(com3, year, flow), sum)


# COMESA export shares
com2$comshare = com2$flow/com$flow

# COMESA import shares
com4$comshare = com4$flow/com$flow



## Plotting two graphs
par(mfrow=c(1,2))
plot(com2$year, com2$comshare, type = "o", 
     xlab = "",
     ylab = "export share",
     main = "Annual COMESA export shares")



plot(com4$year, com4$comshare, type = "o", 
     xlab = "",
     ylab = "import share",
     main = "Annual COMESA import shares")



p1 <- ggplot(com2, aes(year, comshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Annual COMESA export shares")



p2 <- ggplot(com4, aes(year, comshare)) + 
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = 1975) +
  ggtitle("Annual COMESA import shares")

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)




## Alternate plot ###

tiff(filename="gexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(com2, aes(year, comshare)) + 
  geom_line() + 
  ylab('Export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA export shares")

dev.off()


p9 <- ggplot(com2, aes(year,comshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA export shares") +
  theme_economist() 
p9




tiff(filename="gimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


ggplot(com4, aes(year, comshare)) + 
  geom_line() + 
  ylab('Imports shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA import shares")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)


p9 <- ggplot(com4, aes(year,comshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA import shares") +
  theme_economist() 
p9





## COMESA export shares to EU28

# EU 28 countries:
# Note that the EU15 dummy includes non-EU countries!
eulist = c("AUT", "BEL","BGR","FIN", "FRA", "DEU", "GRC", 
           "IRL", "ITA", "NLD", "PRT", "ESP", "SWE","GBR",
           "CYP", "CZE", "EST", "HUN","HRV", "LVA", "ESP",
           "LTU","LUX", "MLT", "POL","ROU", "SVK", "SVN")

### Aggregate EU import
eu1 = subset(dat, iso_d %in% eulist & year %in% 1970:2006)
eu2 = aggregate(flow ~ ., data = select(eu1, year, flow), sum)

## COMESA share of export to EU
com1= subset(eu1, iso_o %in% comesaset)
COM2 = aggregate(flow ~ ., data = select(com1, year, flow), sum)

### Aggregate COMESA import 
COM3 = subset(data, iso_d %in% comesaset)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## EU export to COMESA
eu3= subset(COM3, iso_o %in% eulist)
eu4 = aggregate(flow ~ ., data = select(eu3, year, flow), sum)


# COMESA export share to EU
COM2$COMshare = COM2$flow/eu2$flow

# COMESA import share from EU
COM4$COMshare = eu4$flow/COM4$flow

par(mfrow=c(1,2))
plot(COM2$year, COM2$COMshare, type = "l", ylim = c(0,0.025), 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to EU")

plot(COM4$year, COM4$COMshare, type = "l", ylim = c(0,0.734), 
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from EU")



tiff(filename="2EUexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM2, aes(year, COMshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref.EU 28)")

dev.off()




p9 <- ggplot(COM2, aes(year,COMshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA import shares") +
  theme_economist() 
p9





tiff(filename="2EUimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref.EU 28)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)


p9 <- ggplot(COM4, aes(year,COMshare)) +
  geom_line() +
  scale_y_continuous(name = "Trade flow (%)") +
  theme_economist() +
  xlab("Year") +
  ylab("trade flow") +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("Annual COMESA import shares") +
  theme_economist() 
p9





## COMESA export shares to GBR

### Aggregate gbr import
gbr1 = subset(dat, iso_d == "GBR" & year %in% 1970:2006)

gbr2 = aggregate(flow ~ ., data = select(gbr1, year, flow), sum)

## COMESA share of export to gbr
COM= subset(gbr1, iso_o %in% comesaset)
com1 = aggregate(flow ~ ., data = select(COM, year, flow), sum)


## COMESA import shares from gbr
### Aggregate COMESA import 
COM3 = subset(dat, iso_d %in% comesaset & year %in% 1970:2006)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## gbr export to COMESA
gbr3= subset(COM3, iso_o == "GBR")
gbr4 = aggregate(flow ~ ., data = select(gbr3, year, flow), sum)

# COMESA export share to gbr
com1$COMshare = com1$flow/gbr2$flow

# COMESA import share from EU
COM4$COMshare = gbr4$flow/COM4$flow



##Plotting the two graphs
plot(com1$year, com1$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to Great Britian")


plot(COM4$year, COM4$COMshare, type = "l",
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from Great Britian")

dev.off()



tiff(filename="2GBRexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(com1, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref.Great Britian)")

dev.off()



tiff(filename="2GBRimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref.Great Britian)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)





## COMESA export shares to USA

### Aggregate US import
us1 = subset(data, iso_d == "USA")

us2 = aggregate(flow ~ ., data = select(us1, year, flow), sum)

## COMESA share of export to US
COM= subset(us1, iso_o %in% comesaset)
com1 = aggregate(flow ~ ., data = select(COM, year, flow), sum)


## COMESA import shares from US
### Aggregate COMESA import 
COM3 = subset(data, iso_d %in% comesaset)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## US export to COMESA
us3= subset(COM3, iso_o == "USA")
us4 = aggregate(flow ~ ., data = select(us3, year, flow), sum)

# COMESA export share to US
com1$COMshare = com1$flow/us2$flow

# COMESA import share from EU
COM4$COMshare = us4$flow/COM4$flow



##Plotting the two graphs
plot(com1$year, com1$COMshare, type = "l", ylim = c(0,0.046), 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to US")


plot(COM4$year, COM4$COMshare, type = "l", ylim = c(0,0.135), 
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from US")

dev.off()



tiff(filename="2USexpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(com1, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref. USA)")

dev.off()



tiff(filename="2USimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref. USA)")

dev.off()

library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)




## COMESA export shares to China

### Aggregate China import
ch1 = subset(dat, iso_d == "CHN" & year %in% 1970:2006)

ch2 = aggregate(flow ~ ., data = select(ch1, year, flow), sum)

## COMESA share of export to China
com1= subset(ch1, iso_o %in% comesaset)
COM2 = aggregate(flow ~ ., data = select(com1, year, flow), sum)


## COMESA import shares from China
### Aggregate COMESA import 
COM3 = subset(dat, iso_d %in% comesaset  & year %in% 1970:2006)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## China export to COMESA
ch3= subset(COM3, iso_o == "CHN")
ch4 = aggregate(flow ~ ., data = select(ch3, year, flow), sum)

# COMESA export share to China
COM2$COMshare = COM2$flow/ch2$flow

# COMESA import share from China
COM4$COMshare = ch4$flow/COM4$flow


plot(COM2$year, COM2$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to China")

plot(COM4$year, COM4$COMshare, type = "l", ylim = c(0,0.1291), 
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from China")


tiff(filename="2CHNexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM2, aes(year, COMshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref. China)")

dev.off()


tiff(filename="2CHNimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref. China)")

dev.off()


library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)



## COMESA export shares to India

### Aggregate India import
ind1 = subset(dat, iso_d == "IND" & year %in% 1970:2006)

ind2 = aggregate(flow ~ ., data = select(ind1, year, flow), sum)

## COMESA share of export to India
com1= subset(ind1, iso_o %in% comesaset)
COM2 = aggregate(flow ~ ., data = select(com1, year, flow), sum)


## COMESA import shares from India
### Aggregate COMESA import 
COM3 = subset(dat, iso_d %in% comesaset  & year %in% 1970:2006)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## India export to COMESA
ind3= subset(COM3, iso_o == "IND")
ind4 = aggregate(flow ~ ., data = select(ind3, year, flow), sum)

# COMESA export share to India
COM2$COMshare = COM2$flow/ind2$flow

# COMESA import share from India
COM4$COMshare = ind4$flow/COM4$flow


plot(COM2$year, COM2$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to India")

plot(COM4$year, COM4$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from India")


tiff(filename="2INDexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM2, aes(year, COMshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref. India)")

dev.off()


tiff(filename="2INDimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref. India)")

dev.off()


library(gridExtra)
grid.arrange(p1,p2, ncol=2, nrow =1)



## COMESA export shares to France

### Aggregate France import
fra1 = subset(dat, iso_d == "FRA" & year %in% 1970:2006)

fra2 = aggregate(flow ~ ., data = select(fra1, year, flow), sum)

## COMESA share of export to France
com1= subset(fra1, iso_o %in% comesaset)
COM2 = aggregate(flow ~ ., data = select(com1, year, flow), sum)


## COMESA import shares from France
### Aggregate COMESA import 
COM3 = subset(dat, iso_d %in% comesaset  & year %in% 1970:2006)
COM4 = aggregate(flow ~ ., data = select(COM3, year, flow), sum)

## France export to COMESA
fra3= subset(COM3, iso_o == "FRA")
fra4 = aggregate(flow ~ ., data = select(fra3, year, flow), sum)

# COMESA export share to France
COM2$COMshare = COM2$flow/fra2$flow

# COMESA import share from France
COM4$COMshare = fra4$flow/COM4$flow


plot(COM2$year, COM2$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA export shares to France")

plot(COM4$year, COM4$COMshare, type = "l", 
     xlab = "",
     ylab = "export share",
     main = "COMESA import shares from France")


tiff(filename="2IFRAexppshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM2, aes(year, COMshare)) + 
  geom_line() + 
  ylab('export shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA export shares (ref. France)")

dev.off()


tiff(filename="2FRAimpshares.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")

ggplot(COM4, aes(year, COMshare)) + 
  geom_line() + 
  ylab('import shares') +
  geom_point(color="darkblue") +
  geom_vline(xintercept = 1975, color="red") +
  ggtitle("COMESA import shares (ref. France)")

dev.off()











