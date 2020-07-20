#########################################################################
# Creating the gravity dataset
#########################################################################

### Preliminaries
#################################################
memory.limit(8192) # Check how much RAM yoy have

# Load packages
library(foreign)
library(data.table) # you might have to install this package first

# Set your own path (to where the data is stored)
path = "/Users/albertosei-owusu/Desktop/Data/Gravity"

### Reading the trade data
#########################################################################

dat = read.csv(paste0(path, "pork_trade.csv"), sep = ";")
setnames(dat, c("ReporterISO3", "PartnerISO3", "ReporterName", "PartnerName"), 
         c("iso_d", "iso_o", "destination", "origin"))


# country specific cepii data
geo_cepii = read.dta(paste0(path, "geo_cepii.dta"))

# Dyadic cepii data
dist_cepii = read.dta(paste0(path, "dist_cepii.dta"))

# Merge trade data with dyadic gravity data
dist = data.table(dist_cepii)
dat = data.table(dat)
setkeyv(dist, c('iso_o','iso_d'))
setkeyv(dat, c('iso_o','iso_d'))
dat = merge(dat, dist)

