##################################################################################
##Process gday output data
##Created on 09/Dec/2016
##Author: Mingkai Jiang
##Comparing dry, wet and obs for 1850-2100, cnp version
##
##################################################################################
##################################################################################

##Set working directory
setwd("~/Documents/PostDoc/GDAY/Amazon/AMAZ")

##Prepare title, unit, and short abbreviation list
title.list <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_AMB_OBS_OBS.csv", sep=""), 
                         sep=",", skip = 1, nrow = 3, stringsAsFactors=F)
colnames(title.list) <- as.character(title.list[3,])
title.list <- as.data.frame(title.list, stringsAsFactors=F)
colnames(title.list) <- as.character(title.list[3,])
title.list <- title.list[-3,]

##################################################
##Read in cnp output data and prepare data format
##################################################
obsDF <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_AMB_OBS_OBS.csv", sep=""), 
                    sep=",", skip = 3, header=T)

##Set date and time
obsDF$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")

dryDF <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_AMB_DRY_OBS.csv", sep=""), 
                    sep=",", skip = 3, header=T)

##Set date and time
dryDF$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")


wetDF <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_AMB_WET_OBS.csv", sep=""), 
                    sep=",", skip = 3, header=T)

##Set date and time
wetDF$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")

# Compute annual mean
obsADF <- subset(obsDF, DOY==1)
wetADF <- obsADF
dryADF <- obsADF

for (i in 1999:2100) {
    for (j in 3:ncol(obsDF)) {
        obsADF[obsADF$YEAR == i, j] <- mean(obsDF[obsDF$YEAR == i, j])
        dryADF[dryADF$YEAR == i, j] <- mean(dryDF[dryDF$YEAR == i, j])
        wetADF[wetADF$YEAR == i, j] <- mean(wetDF[wetDF$YEAR == i, j])
    }
}



##########################
##Plotting time series data
##########################

pdf(paste(getwd(), "/drought_analysis/all_plots/annual_pattern_cnp_dry_wet_obs.pdf",sep=""))

for (i in 12:ncol(obsADF))
{
    y.lab <- title.list[1,i]
    y.min <- min(obsADF[,i], dryADF[,i], wetADF[,i])
    y.max <- max(obsADF[,i], dryADF[,i], wetADF[,i])
    
    plot(obsADF$YEAR, obsADF[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "black")
    points(dryADF$YEAR, dryADF[,i], 
           type="l", col="red")
    points(wetADF$YEAR, wetADF[,i], 
           type="l", col="blue")
    #abline(v=1999, col="grey")
    legend("topleft", c("obs", "dry", "wet"),
           col=c("black", "red", "blue"),
           lty=1)
}


dev.off()

##################################################
##Read in elev CO2 data
##################################################
obsDFele <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_ELE_OBS_OBS.csv", sep=""), 
                       sep=",", skip = 3, header=T)

##Set date and time
obsDFele$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

dryDFele <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_ELE_DRY_OBS.csv", sep=""), 
                       sep=",", skip = 3, header=T)

##Set date and time
dryDFele$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")


wetDFele <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDA_ELE_WET_OBS.csv", sep=""), 
                       sep=",", skip = 3, header=T)

##Set date and time
wetDFele$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

# Compute annual mean
obsADFele <- subset(obsDFele, DOY==1)
wetADFele <- obsADFele
dryADFele <- obsADFele

for (i in 1999:2100) {
    for (j in 3:ncol(obsDFele)) {
        obsADFele[obsADFele$YEAR == i, j] <- mean(obsDFele[obsDFele$YEAR == i, j])
        dryADFele[dryADFele$YEAR == i, j] <- mean(dryDFele[dryDFele$YEAR == i, j])
        wetADFele[wetADFele$YEAR == i, j] <- mean(wetDFele[wetDFele$YEAR == i, j])
    }
}



##########################
##Plotting time series data
##########################

pdf(paste(getwd(), "/drought_analysis/all_plots/annual_pattern_cnp_dry_wet_obs_ele.pdf",sep=""))

for (i in 12:ncol(obsADFele))
{
    y.lab <- title.list[1,i]
    y.min <- min(obsADFele[,i], dryADFele[,i], wetADFele[,i])
    y.max <- max(obsADFele[,i], dryADFele[,i], wetADFele[,i])
    
    plot(obsADFele$YEAR, obsADFele[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "black")
    points(dryADFele$YEAR, dryADFele[,i], 
           type="l", col="red")
    points(wetADFele$YEAR, wetADFele[,i], 
           type="l", col="blue")
    #abline(v=1999, col="grey")
    legend("topleft", c("obs", "dry", "wet"),
           col=c("black", "red", "blue"),
           lty=1)
}


dev.off()


##########################
##Plotting time series data
##########################

pdf(paste(getwd(), "/drought_analysis/all_plots/annual_pattern_cnp_ele_vs_amb_obs.pdf",sep=""))

for (i in 12:ncol(obsADFele))
{
    y.lab <- title.list[1,i]
    y.min <- min(obsADFele[,i], obsADF[,i])
    y.max <- max(obsADFele[,i], obsADF[,i])
    
    plot(obsADFele$YEAR, obsADFele[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "red")
    points(obsADF$YEAR, obsADF[,i], 
           type="l", col="blue")
    
    legend("topleft", c("ele", "amb"),
           col=c("red", "blue"),
           lty=1)
}


dev.off()

##########################
##Plotting time series data  dry
##########################

pdf(paste(getwd(), "/drought_analysis/all_plots/annual_pattern_cnp_ele_vs_amb_dry.pdf",sep=""))

for (i in 12:ncol(dryADFele))
{
    y.lab <- title.list[1,i]
    y.min <- min(dryADFele[,i], dryADF[,i])
    y.max <- max(dryADFele[,i], dryADF[,i])
    
    plot(dryADFele$YEAR, dryADFele[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "red")
    points(dryADF$YEAR, dryADF[,i], 
           type="l", col="blue")
    
    legend("topleft", c("ele", "amb"),
           col=c("red", "blue"),
           lty=1)
}


dev.off()

##########################
##Plotting time series data  wet
##########################

pdf(paste(getwd(), "/drought_analysis/all_plots/annual_pattern_cnp_ele_vs_amb_wet.pdf",sep=""))

for (i in 12:ncol(wetADFele))
{
    y.lab <- title.list[1,i]
    y.min <- min(wetADFele[,i], wetADF[,i])
    y.max <- max(wetADFele[,i], wetADF[,i])
    
    plot(wetADFele$YEAR, wetADFele[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "red")
    points(wetADF$YEAR, wetADF[,i], 
           type="l", col="blue")
    
    legend("topleft", c("ele", "amb"),
           col=c("red", "blue"),
           lty=1)
}


dev.off()

##########################
# Plot climate
with(obsADF, plot(YEAR, CO2, ylim=c(300, 800)))
with(obsADFele, points(YEAR, CO2, col = "red"))

with(obsADF, plot(YEAR, APARd, type="l", ylim=c(2, 4)))
with(dryADF, points(YEAR, APARd, type="l", col="red"))
with(wetADF, points(YEAR, APARd, type="l", col="blue"))

with(obsADF, plot(YEAR, SW, type="l", ylim=c(200, 300)))
with(dryADF, points(YEAR, SW, type="l", col="red"))
with(wetADF, points(YEAR, SW, type="l", col="blue"))


test1 <- obsADF[1:20, "Betad"]
test2 <- obsADF[1:20, "YEAR"]
cbind(test2, test1)