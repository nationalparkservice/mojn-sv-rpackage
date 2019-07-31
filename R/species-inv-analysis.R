
library(vegan)
library(ggplot2)
library(springvegetation)

spring<- "Grapevine"


#Make a database connection, create an object, get the raw data for the vegetation inventory

rawData<- GetRawData(OpenDatabaseConnection())
inventoryDataTable <- rawData$VegetationInventory
invDataBySpring <- rawData$VegetationInventory[which(rawData$VegetationInventory$SpringName==spring), ]
accumDataTable <- table(invDataBySpring[, c(8,9)])


#Generate accumulation curve data using vegan

accumulationCurve <-  specaccum (accumDataTable, "rarefaction", permutations=500)


#Data Wrangling: pull data from vegan and create table wit sites, richness, and standard deviantion

data<- data.frame(accumulationCurve$sites, accumulationCurve$richness, accumulationCurve$sd)

names(data)[1] <- "sites"
names(data)[2] <- "richness"
names(data)[3] <- "sd"


#Generate figures in Ggplot2


curveFigurev1 <- ggplot (data, aes(x= sites, y= richness)) + geom_point () +
   theme_bw () +
   geom_point(color="black", shape=21, size=3, fill='black') +
   geom_errorbar (aes (x=sites, ymin=richness-sd, ymax=richness+sd, width=.5), color='black') +
   labs (x="Sampling Effort (# transects)", y= "Species Richness") +
   ggtitle ("Species Accumulation Curve", spring) +
   theme (axis.title=element_text (size=15, face="bold")
   ) 

curveFigurev1

#Is this one better looking?
curveFigurev2 <- ggplot (data, aes(x= sites, y= richness)) + 
   geom_point () +
   geom_errorbar (aes (x=sites, ymin=richness-sd, ymax=richness+sd)) +
   labs (x="Sampling Effort (# transects)", y= "Species Richness") +
   ggtitle ("Species Accumulation Curve", spring) 

curveFigurev2

