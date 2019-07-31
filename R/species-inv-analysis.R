
library(vegan)

Data <- rawData$VegetationInventory

#For all data across all springs
x <- table(Data[,c(8,9)])


accumulationCurve <- specaccum(x,"rarefaction",permutations=500)



plot (accumulationCurve, ylim=c(0,100),xlim=c(0,20),
     lwd=1.5, cex.lab=1.5, cex.main=1.5,
     ci.col="lightgray",xlab="Sample Size", ylab="Number of Species", 
     main= "Species Accumulation Curves of Blue Point Spring \n species inventory belt transect")
     
     
#For one spring


#Make a database connection, and get raw data

#GetRawData

spring<- "Blue Point"


rawData<- GetRawData(OpenDatabaseConnection())
Spring <- rawData$VegetationInventory[which(rawData$VegetationInventory$SpringName==spring),]

x <- table(Spring[,c(8,9)])

accumulationCurve <- specaccum(x,"rarefaction",permutations=500)


plot (accumulationCurve, ylim=c(0,50),xlim=c(0,17),
      lwd=1.5, cex.lab=1.5, cex.main=1.5,
      ci.col="lightgray",xlab="Sample Size", ylab="Number of Species", 
      main= c("SAC for Species Inventory Belt Transects at", spring))

