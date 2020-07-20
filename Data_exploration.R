
## Subsetting data set
data=subset(dat, year %in% 2006)


# descriptive stats
summary(data)
names(data)



# selecting the variables to use in the correlation matrix
cor_data<-data[c("flow"                     
                     ,"year"                 
                     ,"gdp_o"    
                     ,"gdp_d"                   
                     ,"col_hist"                 
                     ,"contig"                
                     ,"distw"                   
                     ,"comlang_off"               
                     ,"pop_o"                
                     ,"pop_d")]  



cor_data<-data[c("flow"                     
                 ,"bothinE"                 
                 ,"oneinE"    
                 ,"oneinE1"                   
                 ,"bothinC"                 
                 ,"oneinC"                
                 ,"oneinC1"                   
                 ,"bothinEPA"               
                 ,"oneinEPAX"               
                 ,"oneinEPAM"                
                 ,"bothinAG",
                 "oneinAGX",
                 "oneinAGM")]  


# correlations 
names(cor_data)
dim(cor_data)

# changing the type of variable 
cor_data$cnature_size<-as.numeric(as.character(cor_data$cnature_size))

data.set$cnature_size<-as.numeric(as.character(data.set$cnature_size))

# looking at the correlation matrix
cor(cor_data, use="complete.obs", method="kendall") 
cov(cor_data, use="complete.obs")

# another way of doing a correlation matrix  
library(corrgram)
corrgram(cor_data, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Trade flow")

corrplot(cor_data, method="number")










### Boxplot ####

## http : http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html


## box plot of trade flows by year #####


library(ggplot2)
# Basic box plot

fill <- "#4271AE"
line <- "#1F3552"

## Method 1 ###
p <- ggplot(data, aes(x = year, y = flow)) +
  theme_bw() + 
  geom_boxplot(fill = fill, colour = line, alpha = 0.7,
               outlier.colour = "#1F3552", outlier.shape = 20,aes(group = year)) +
  scale_x_continuous(name = "year") +
  scale_y_continuous(name = "trade flows in\n $US billion") +
  ggtitle("Boxplot of trade flow by year")
  
p


### The Economist theme ####
pack<-c("ggthemes","grid")

for (i in pack){
  install.packages(pkgs=i, dependencies=TRUE)
}

lapply(pack, require, character.only=T)

library(ggthemes)
library(grid)

fill <- "#4271AE"
line <- "#1F3552"

p10 <- ggplot(data, aes(x = year, y = flow)) +
  geom_boxplot(fill = fill, colour = line,aes(group = year)) +
  scale_y_continuous(name = "trade flows in\n $US billion") +
  scale_x_continuous(name = "Year") +
  ggtitle("Boxplot of trade flow by year") +
  theme_economist() +
  geom_jitter() + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))

p10



p10 <- ggplot(data, aes(x = year, y = flow)) +
  geom_boxplot(fill = fill, colour = line,aes(group = year)) +
  scale_y_continuous(name = "trade flows in\n $US billion") +
  scale_x_continuous(name = "Year") +
  ggtitle("Boxplot of trade flow by year") +
  theme_economist() +
  geom_jitter() + 
  theme_bw() + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))

p10



p10 <- ggplot(data, aes(x = year, y = flow)) +
  geom_boxplot(fill = fill, colour = line,aes(group = year)) +
  scale_y_continuous(name = "trade flows in\n $US billion") +
  scale_x_continuous(name = "Year") +
  ggtitle("Boxplot of trade flow by year") +
  theme_economist() +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))

p10



### Histogram ######

# http : http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/

### Method 1 ###

## Basic histogram from the vector "rating". Each bin is .5 wide.
## These both result in the same output:
ggplot(data, aes(x=flow)) + geom_histogram(binwidth=.5)

ggplot(data, aes(x=flow)) + geom_histogram(binwidth=.5)

# qplot(dat$flow, binwidth=.5)

# Density curve
ggplot(data, aes(x=flow)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(data, aes(x=flow)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot



# Histogram and density plots with multiple groups

# Overlaid histograms
ggplot(data, aes(x=flow, fill=year)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Interleaved histograms
ggplot(data, aes(x=flow, fill=year)) +
  geom_histogram(binwidth=.5, position="dodge")

# Density plots
ggplot(data, aes(x=flow, colour=year)) + geom_density()

# Density plots with semi-transparent fill
ggplot(data, aes(x=flow, fill=year)) + geom_density(alpha=.3)



# Using facets:
  
ggplot(data, aes(x=year)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
facet_grid(year ~ .)

# With mean lines, using cdat from above
ggplot(dat, aes(x=flow)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(year ~ .) +
  geom_vline(data=cdat, aes(xintercept=flow.mean),
             linetype="dashed", size=1, colour="red")

# Draw with black outline, white fill
ggplot(data, aes(x=flow)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")


# Find the mean of each group
library(plyr)
cdat <- ddply(data, "year", summarise, flow.mean=mean(flow))
cdat
#>   cond rating.mean
#> 1    A -0.05775928
#> 2    B  0.87324927

# Overlaid histograms with means
ggplot(data, aes(x=flow, fill=year)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=flow.mean,  colour=year),
             linetype="dashed", size=1)

# Density plots with means
ggplot(data, aes(x=flow, colour=year)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=flow.mean,  colour=year),
             linetype="dashed", size=1)


#### Method 2 #########

options(scipen = 3)
x <- data$flow
h<-hist(x, breaks=50, col="red", xlab="Trade flow", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


