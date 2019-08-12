

spring<- "Rogers"

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

curveFigure <- ggplot (data, aes(x= sites, y= richness)) + 
   geom_point (size=2) +
   ylim(2,40) +
   geom_errorbar (aes (x=sites, ymin=richness-sd, ymax=richness+sd, width=.2)) +
   labs (x="Sampling Effort (# transects)", y= "Species Richness") +
   ggtitle ( spring) 

curveFigure

