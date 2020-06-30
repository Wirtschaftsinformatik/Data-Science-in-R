library(choroplethr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)
library(gpclib)
library(readr)
library(R6)


########## Pre Processing #######################
# We load the raw data, manage variable types
# We change PLZ to a 3 digit format
#
#################################################

# set the working directory to the path where your files are!
setwd("~/Documents/uni/courses/eCom")

pingData <- read.csv("ping_zeiten.csv",colClasses = c("PLZ"="character"))

pingData$PLZ = as.factor(pingData$PLZ)
str(pingData)
pingData$PLZ3 = substr(pingData$PLZ, 1, 3)
pingData$PLZ3 = as.factor(pingData$PLZ3)

########## Plotting Distribtutions #######################

ggplot(data=pingData, aes(x=PingZeit)) + geom_density(fill="red")
ggplot(data=pingData, aes(x=AnzahlHops)) + geom_density(fill="red")

ggplot(data=pingData, aes(x=AnzahlHops,fill=InternetProvider)) + geom_density(alpha=0.5)
ggplot(data=pingData, aes(x=PingZeit,fill=InternetProvider)) + geom_density(alpha=0.5)
ggplot(data=pingData, aes(x=PingZeit,fill=PLZ3)) + geom_density(alpha=0.5)


ggplot(data=pingData, aes(y=PingZeit,x=AnzahlHops,color=InternetProvider)) + geom_point(size=5)


########## Plotting a Map #######################
# We use the 3 digit pklz shapefile from:
# https://www.suche-postleitzahl.org/downloads
# Load the shapefile, create an id variable
# create a dataframe with the plz shapes by region
# create a second dataframe with ping times and regions
ger_plz <- readOGR(dsn = ".", layer = "plz-3stellig")

gpclibPermit()
#convert the raw data to a data.frame as ggplot works on data.frames

ger_plz@data$id <- rownames(ger_plz@data)
ger_plz.point <- fortify(ger_plz, region="id")
ger_plz.df <- inner_join(ger_plz.point,ger_plz@data, by="id")

head(ger_plz.df)
#ggplot(ger_plz.df, aes(long, lat, group=group )) + geom_polygon()

#data file
# variable name 'region' is needed for choroplethr
ger_plz.df$region <- ger_plz.df$plz
head(ger_plz.df)
# the new data frame
df <- pingData[,c("PLZ3","PingZeit")]
df2 <- pingData[,c("PLZ3","AnzahlHops")]
names(df) <- c("region","value")
names(df2) <- c("region","value")
# create ping average by region
df <- aggregate(. ~ region, data = df, mean)
df2 <- aggregate(. ~ region, data = df2, mean)
#subclass choroplethr to make a class for your-my need
GERPLZChoropleth <- R6Class("GERPLZChoropleth",
                            inherit = choroplethr:::Choropleth,
                            public = list(
                              initialize = function(user.df) {
                                super$initialize(ger_plz.df, user.df)
                              }
                            )
)
#choropleth needs these two columnames - 'region' and 'value'
colnames(df) = c("region", "value")

##########################################################
# Ping Times ...
# instantiate new class with data
c <- GERPLZChoropleth$new(df)

#plot the data
c$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
c$title = "Comparison of Ping Time per Zipcode"
c$legend= "Average Ping time per Zipcode"
c$set_num_colors(9)
c$render()


##########################################################
# Hops ...
# instantiate new class with data
c2<- GERPLZChoropleth$new(df2)

#plot the data
c2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
c2$title = "Comparison of Internet Hops per Zipcode in e-commerce"
c2$legend= "Average Hops per Zipcode"
c2$set_num_colors(9)
c2$render()
