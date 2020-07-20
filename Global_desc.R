

#############################################################
### R-code for master thesis, 18.05.2017
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
        "ggthemes","ggrepel","lfe","gridExtra","cowplot","gplots","tseries")

lapply(pack, require, character.only=T)

# specify the path of the folder containing the dataset
path <- "/Users/albertosei-owusu/Desktop/Data/Gravity/"
setwd(path)

##specify the path of the folder containing the results of regressions
results="/Users/albertosei-owusu/Desktop/Data/Gravity/Tables"
setwd(results)


graphs <- "/Users/albertosei-owusu/Desktop/Data/Gravity/Figures"
setwd(graphs)



## Merchnadise trade in Africa

## Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Africa_merch_trade.csv", 
                col_types = cols(flow = col_number()))
View(dat)

dat$value1= dat$value/1e6


p1 <- ggplot(dat, aes(year,value1,colour=Tn)) +
  geom_line(aes(colour = Tn, group = Tn)) +
  scale_y_continuous(name = "Trade flow ($US million)") +
  xlab("Year")+
  ggtitle("Denmark's Merchandise trade flows") +
  scale_colour_manual("Trade flow", values = c("red", "blue")) +
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




## Intra bloc imports in Africa

## Importing data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/intra_Afr_reg_trade_shares.csv", 
                col_types = cols(imp = col_number()))
View(dat)

tiff(filename="Intra_bloc_imp-95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,imp,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year")+
  ggtitle("Intra bloc imports") +
  scale_colour_manual("blocs", values = c("red", "blue","green","black","purple")) +
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




## Intra COMESA and ECOWAS imports 

## Importing data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Intra_bloc_imports_final.csv", 
                col_types = cols(flow = col_number()))
View(dat)

dat$flow1 = dat$flow/1e3

tiff(filename="Intra_bloc_imp-1970-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow1,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (in billions of $US)") +
  xlab("Year")+
  ggtitle("Intra bloc imports") +
  scale_colour_manual("blocs", values = c("red", "blue")) +
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



## COMESA and ECOWAS imports shares 

## Importing data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Import_shares_blocs.csv", 
                col_types = cols(share = col_number()))
View(dat)



tiff(filename="Bloc_imp_shares-1970-16.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,share,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% of total world)") +
  xlab("Year")+
  ggtitle("Annual import shares") +
  scale_colour_manual("blocs", values = c("red", "blue")) +
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




## COMESA and ECOWAS export shares 


## Importing data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Export_shares_blocs.csv", 
                col_types = cols(share = col_number()))
View(dat)

tiff(filename="Bloc_imp_shares-1970-16.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p2 <- ggplot(dat, aes(year,share,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% of total world)") +
  xlab("Year")+
  ggtitle("Annual export shares") +
  scale_colour_manual("blocs", values = c("red", "blue")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 12))
p2

dev.off()


grid.arrange(p1, p2, ncol=2)





## Africa ###


## Intra bloc trade (exports + imports)

# Import data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Intra_Africa_bloc_trade.csv", 
                col_types = cols(exp = col_number(), 
                                 imp = col_number(), trade = col_number()))
View(dat)

dat$trade1= dat$trade/1e3


tiff(filename="Intra_Africa_bloc_trade-95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,trade1,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow ($US billion)") +
  xlab("Year")+
  ggtitle("Intra African trade by regional blocs") +
  scale_colour_manual("blocs", values = c("red", "blue","green","black","purple")) +
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




### BY SHARES IN WORLD TRADE #####

################# BLOCS #############################

## Intra-bloc exports

## Import data

library(readr)
dat <- read_csv("~/Documents/TRADE DATA/Intra_trade_export/intra_bloc_export_%.csv", 
                col_types = cols(flow = col_number()))

tiff(filename="Intra_bloc_exp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow,colour=bloc)) +
    geom_line(aes(colour = bloc, group = bloc)) +
    scale_y_continuous(name = "Trade flow (% by destination)") +
    xlab("Year")+
  ggtitle("Intra bloc exports") +
  scale_colour_manual("blocs", values = c("red", "blue","green","yellow",
                                          "orange","purple","darkgrey","brown","black")) +
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



## Intra bloc imports

## Importing data
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/intra_trade_import/intra_bloc_import_%.csv", 
                col_types = cols(flow = col_number()))

tiff(filename="Intra_bloc_imp-95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year")+
  ggtitle("Intra bloc imports") +
  scale_colour_manual("blocs", values = c("red", "blue","green","yellow",
                                          "orange","purple","darkgrey","brown","black")) +
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



# Extra bloc exports

##import data set
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/row_trade_export/row_bloc_export_%.csv", 
                col_types = cols(flow = col_number()))


tiff(filename="Extra_bloc_exp-95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year")+
  ggtitle("Extra bloc exports") +
  scale_colour_manual("blocs", values = c("red", "blue","green","yellow",
                                          "orange","purple","darkgrey","brown","black")) +
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




## Intra Region exports


## Importing data
dat <- read_csv("~/Documents/TRADE DATA/Intra_trade_export/intra_reg_export_%.csv", 
                col_types = cols(flow = col_number()))


tiff(filename="Intra_reg_exp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <-  ggplot(dat, aes(year,flow,colour=reg)) +
  geom_line(aes(colour = reg, group = reg)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  theme_gdocs() +
  xlab("Year")+
  ggtitle("Intra region exports") +
  scale_colour_manual("Regions", values = c("red", "yellow","brown","black",
                                            "purple")) +
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




## Intra Region imports

library(readr)
dat <- read_csv("~/Documents/TRADE DATA/intra_trade_import/intra_reg_imports_%.csv", 
                col_types = cols(flow = col_number()))


tiff(filename="Intra_reg_imp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <-  ggplot(dat, aes(year,flow,colour=reg)) +
  geom_line(aes(colour = reg, group = reg)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  theme_gdocs() +
  xlab("Year")+
  ggtitle("Intra region imports") +
  scale_colour_manual("Regions", values = c("red", "yellow","brown","black",
                                            "purple")) +
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



## From rest of the world ####


## Extra bloc imports

##import data sets
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/row_trade_import/row_bloc_imports_%.csv", 
                col_types = cols(flow = col_number()))


tiff(filename="Extra_bloc_imp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <- ggplot(dat, aes(year,flow,colour=bloc)) +
  geom_line(aes(colour = bloc, group = bloc)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year")+
  ggtitle("Extra bloc imports") +
  scale_colour_manual("blocs", values = c("red", "blue","green","yellow",
                                          "orange","purple","darkgrey","brown","black")) +
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


####### REGION #########

## Extra-region imports

library(readr)
dat <- read_csv("~/Documents/TRADE DATA/row_trade_import/row_reg_imports_%.csv", 
                col_types = cols(flow = col_number()))



tiff(filename="Extra_reg_imp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p1 <-  ggplot(dat, aes(year,flow,colour=reg)) +
  geom_line(aes(colour = reg, group = reg)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  theme_gdocs() +
  xlab("Year")+
  ggtitle("Extra region imports") +
  scale_colour_manual("Regions", values = c("red", "yellow","brown","black",
                                            "purple")) +
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


# Extra region exports 

# Importing data set
library(readr)
dat <- read_csv("~/Documents/TRADE DATA/row_trade_export/row_export_reg_%.csv", 
                col_types = cols(flow = col_number()))



tiff(filename="Extra_reg_exp_95-15.jpeg", width=14.1705,height=14.1705, bg=FALSE, res=400, units="cm")


p9 <- ggplot(dat, aes(year,flow,colour=reg)) +
  geom_line(aes(colour = reg, group = reg)) +
  scale_y_continuous(name = "Trade flow (% by destination)") +
  xlab("Year")+
  ggtitle("Extra region exports") +
  scale_colour_manual("Regions", values = c("red", "yellow","brown","black",
                                            "purple")) +
  theme_gdocs() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.title=element_text(face = "bold", size = 14))
p9


dev.off()
