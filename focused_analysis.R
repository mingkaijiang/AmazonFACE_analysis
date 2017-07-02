##################################################################################
##Process gday output data
##Created on 18/Jan/2017
##Author: Mingkai Jiang
##Comparing dry, wet and obs for 1999-2100, cn vs cnp version
##
##################################################################################
##################################################################################

##Set working directory
setwd("~/Documents/Research/Projects/Amazon/AMAZ")

##Prepare title, unit, and short abbreviation list
title.list <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_AMB_OBS.csv", sep=""), 
                         sep=",", skip = 1, nrow = 3, stringsAsFactors=F)
colnames(title.list) <- as.character(title.list[3,])
title.list <- as.data.frame(title.list, stringsAsFactors=F)
colnames(title.list) <- as.character(title.list[3,])
title.list <- title.list[-3,]

##################################################
##Read in output data and prepare data format
##################################################
## read cn amb obs
DF1 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_AMB_OBS.csv", sep=""), 
                  sep=",", skip = 3, header=T)
DF1$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                      by = "day")

## read cn amb dry
DF2 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_AMB_DRY.csv", sep=""), 
                  sep=",", skip = 3, header=T)
DF2$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                     by = "day")

## read cn amb wet
DF3 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_AMB_WET.csv", sep=""), 
                sep=",", skip = 3, header=T)
DF3$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                     by = "day")

## read cn elev obs
DF4 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_ELE_OBS.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF4$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

## read cn elev dry
DF5 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_ELE_DRY.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF5$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

## read cn elev wet
DF6 <- read.table(paste(getwd(), "/drought/outputs/AmaFACE1_D_GDA_ELE_WET.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF6$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

## read cnp amb obs
DF7 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_AMB_OBS.csv", sep=""), 
                    sep=",", skip = 3, header=T)
DF7$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")

## read cnp amb dry
DF8 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_AMB_DRY.csv", sep=""), 
                    sep=",", skip = 3, header=T)
DF8$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")

## read cnp amb wet
DF9 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_AMB_WET.csv", sep=""), 
                    sep=",", skip = 3, header=T)
DF9$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                       by = "day")

## read cnp elev obs
DF10 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_ELE_OBS.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF10$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

## read cnp elev dry
DF11 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_ELE_DRY.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF11$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")

## read cnp elev wet
DF12 <- read.table(paste(getwd(), "/drought_p/outputs/AmaFACE1_D_GDP_ELE_WET.csv", sep=""), 
                       sep=",", skip = 3, header=T)
DF12$Date <- seq.Date(from=as.Date("01/01/1999", "%d/%m/%Y"), to=as.Date("31/12/2100", "%d/%m/%Y"),
                          by = "day")


##################################################
## Compute annual means for all variables
##################################################
## creating annual dataframe
aDF1 <- aDF2 <- aDF3 <- aDF4 <- aDF5 <- aDF6 <- aDF7 <- aDF8 <- aDF9 <- aDF10 <- aDF11 <- aDF12 <- subset(DF1, DOY==1)

## ncol of dataframe
nc <- dim(aDF1)[2]

## assigning annual averages onto annual dataframe
for (i in 1999:2100) {
    for (j in 3:(nc-1)) {
        aDF1[aDF1$YEAR == i, j] <- mean(DF1[DF1$YEAR == i, j])
        aDF2[aDF2$YEAR == i, j] <- mean(DF2[DF2$YEAR == i, j])
        aDF3[aDF3$YEAR == i, j] <- mean(DF3[DF3$YEAR == i, j])
        aDF4[aDF4$YEAR == i, j] <- mean(DF4[DF4$YEAR == i, j])
        aDF5[aDF5$YEAR == i, j] <- mean(DF5[DF5$YEAR == i, j])
        aDF6[aDF6$YEAR == i, j] <- mean(DF6[DF6$YEAR == i, j])
        aDF7[aDF7$YEAR == i, j] <- mean(DF7[DF7$YEAR == i, j])
        aDF8[aDF8$YEAR == i, j] <- mean(DF8[DF8$YEAR == i, j])
        aDF9[aDF9$YEAR == i, j] <- mean(DF9[DF9$YEAR == i, j])
        aDF10[aDF10$YEAR == i, j] <- mean(DF10[DF10$YEAR == i, j])
        aDF11[aDF11$YEAR == i, j] <- mean(DF11[DF11$YEAR == i, j])
        aDF12[aDF12$YEAR == i, j] <- mean(DF12[DF12$YEAR == i, j])
    }
}


##########################
##Plotting time series data 
##########################


#########
##CN
#########
## generating plot variable list
pr_var <- c(3, 4, 5, 6, 7, 8, 9, 10,   # climate
            13, 17,                    # GPP, RECO,
            25,                        # ET,
            33, 34, 36,                # CL, CW, CFR,
            42,                        # CSOIL,
            52,                        # LAI,
            55, 56, 58,                # NL, NW, NFR,
            64, 65, 66,                # NSOIL, NPMIN, NPORG,
            76, 78, 80)                # NUP, NMIN, NLEACH

## Ploting cn version annual averages
pdf(paste(getwd(), "/drought_analysis/focused_variables/annual_averages_cn.pdf",sep=""))

## selected variables
for (i in pr_var)
{
    y.lab <- title.list[1,i]
    y.min <- min(aDF1[,i], aDF2[,i], aDF3[,i], aDF4[,i], aDF5[,i], aDF6[,i])
    y.max <- max(aDF1[,i], aDF2[,i], aDF3[,i], aDF4[,i], aDF5[,i], aDF6[,i])
    
    plot(aDF1$YEAR, aDF1[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "black")
    points(aDF2$YEAR, aDF2[,i], 
           type="l", col="red")
    points(aDF3$YEAR, aDF3[,i], 
           type="l", col="blue")
    points(aDF4$YEAR, aDF4[,i], 
           type="l", col="grey")
    points(aDF5$YEAR, aDF5[,i], 
           type="l", col="orange")
    points(aDF6$YEAR, aDF6[,i], 
           type="l", col="cyan")

    legend("topright", c("amb-obs", "amb-dry", "amb-wet",
                        "ele-obs", "ele-dry", "ele-wet"),
           col=c("black", "red", "blue",
                 "grey", "orange", "cyan"),
           lty=1)
}

dev.off()

#########
##CNP
#########

## generating plot variable list
pr_var <- c(3, 4, 5, 6, 7, 8, 9, 10,   # climate
            13, 17,                    # GPP, RECO,
            25,                        # ET,
            33, 34, 36,                # CL, CW, CFR,
            42,                        # CSOIL,
            52,                        # LAI,
            55, 56, 58,                # NL, NW, NFR,
            64, 65, 66,                # NSOIL, NPMIN, NPORG,
            76, 78, 80,                # NUP, NMIN, NLEACH,
            90, 91, 93,                # PL, PW, NFR,
            99, 100, 101, 102, 103,    # PSOIL, PLAB, PSEC, POCC, PPAR,
            104, 105,                  # PPMIN, PPORG,
            110, 112, 113)             # PUP, PMIN, PLEACH

## Ploting cn version annual averages
pdf(paste(getwd(), "/drought_analysis/focused_variables/annual_averages_cnp.pdf",sep=""))

## slected variables
for (i in pr_var)
{
    y.lab <- title.list[1,i]
    y.min <- min(aDF7[,i], aDF8[,i], aDF9[,i], aDF10[,i], aDF11[,i], aDF12[,i])
    y.max <- max(aDF7[,i], aDF8[,i], aDF9[,i], aDF10[,i], aDF11[,i], aDF12[,i])
    
    plot(aDF7$YEAR, aDF7[,i], xlab = "Year", ylab = y.lab,
         ylim = c(y.min, y.max),
         type="l", col = "black")
    points(aDF8$YEAR, aDF8[,i], 
           type="l", col="red")
    points(aDF9$YEAR, aDF9[,i], 
           type="l", col="blue")
    points(aDF10$YEAR, aDF10[,i], 
           type="l", col="grey")
    points(aDF11$YEAR, aDF11[,i], 
           type="l", col="orange")
    points(aDF12$YEAR, aDF12[,i], 
           type="l", col="cyan")
    
    legend("topright", c("amb-obs", "amb-dry", "amb-wet",
                        "ele-obs", "ele-dry", "ele-wet"),
           col=c("black", "red", "blue",
                 "grey", "orange", "cyan"),
           lty=1)
}

dev.off()




### Plot Drought response for CNP versionl only
## Ambient condition time series plot

pdf(paste(getwd(), "/drought_analysis/focused_variables/drought_response_amb_time_series.pdf",sep=""))
par(mfrow=c(2,2), c(2.1, 6.1, 2.1, 6.1), mgp=c(3,1,0))

with(aDF7, plot(GPP*365~YEAR, type="l", lwd = 1.5, col = "black", 
                ylim=c(2500, 3500), ylab = expression(paste("GPP [g ", m^-2, " ", yr^-1, "]"))))
with(aDF8, points(GPP*365~YEAR, type="l", lwd = 1.5, col = "red"))
with(aDF9, points(GPP*365~YEAR, type="l", lwd = 1.5, col = "blue"))
#legend("topright", c("OBS", "DRY", "WET"), col=c("black", "red", "blue"),
#       lwd = 1.0)

with(aDF7, plot(LAI~YEAR, type="l", lwd = 1.5, col = "black", 
                ylim=c(4.5, 6.5), ylab = "LAI"))
with(aDF8, points(LAI~YEAR, type="l", lwd = 1.5, col = "red"))
with(aDF9, points(LAI~YEAR, type="l", lwd = 1.5, col = "blue"))
legend("bottomright", c("OBS", "DRY", "WET"), col=c("black", "red", "blue"),
       lwd = 1.0)

with(aDF7, plot(PAR~YEAR, type="l", lwd = 1.5, col = "black", 
                ylim=c(30,40), ylab = expression(paste("PAR [mol ", m^-2, " ", d^-1, "]"))))
with(aDF8, points(PAR~YEAR, type="l", lwd = 1.5, col = "red"))
with(aDF9, points(PAR~YEAR, type="l", lwd = 1.5, col = "blue"))
#legend("topright", c("OBS", "DRY", "WET"), col=c("black", "red", "blue"),
#       lwd = 1.0)

#with(aDF7, plot( SWPA~YEAR, type="l", lwd = 1.5, col = "black", 
#                 ylim = c(340, 440), ylab = "Plant available soil water [mm]"))
#with(aDF8, points(SWPA~YEAR, type="l", lwd = 1.5, col = "red"))
#with(aDF9, points(SWPA~YEAR, type="l", lwd = 1.5, col = "blue"))
#legend("topright", c("OBS", "DRY", "WET"), col=c("black", "red", "blue"),
#       lwd = 1.0)

with(aDF7, plot(Betad*100~YEAR, type="l", lwd = 1.5, col = "black", 
                 ylim = c(80, 100), ylab = "Soil moisture stress [%]"))
with(aDF8, points(Betad*100~YEAR, type="l", lwd = 1.5, col = "red"))
with(aDF9, points(Betad*100~YEAR, type="l", lwd = 1.5, col = "blue"))
#legend("topright", c("OBS", "DRY", "WET"), col=c("black", "red", "blue"),
#       lwd = 1.0)

dev.off()


### Plot CO2 response, comparing dry, wet and obs
## Process data first
gppDF <- data.frame(seq(1999, 2100), NA, NA, NA)
colnames(gppDF) <- c("YEAR", "OBS", "DRY", "WET")

gppDF$OBS <- (aDF10$GPP - aDF7$GPP) / (aDF7$GPP)
gppDF$DRY <- (aDF11$GPP - aDF8$GPP) / (aDF8$GPP)
gppDF$WET <- (aDF12$GPP - aDF9$GPP) / (aDF9$GPP)


laiDF <- gppDF
laiDF$OBS <- (aDF10$LAI - aDF7$LAI) / (aDF7$LAI)
laiDF$DRY <- (aDF11$LAI - aDF8$LAI) / (aDF8$LAI)
laiDF$WET <- (aDF12$LAI - aDF9$LAI) / (aDF9$LAI)

nDF <- gppDF
nDF$OBS <- (aDF10$NPMIN - aDF7$NPMIN) / (aDF7$NPMIN)
nDF$DRY <- (aDF11$NPMIN - aDF8$NPMIN) / (aDF8$NPMIN)
nDF$WET <- (aDF12$NPMIN - aDF9$NPMIN) / (aDF9$NPMIN)

pDF <- gppDF
pDF$OBS <- (aDF10$PLAB - aDF7$PLAB) / (aDF7$PLAB)
pDF$DRY <- (aDF11$PLAB - aDF8$PLAB) / (aDF8$PLAB)
pDF$WET <- (aDF12$PLAB - aDF9$PLAB) / (aDF9$PLAB)

nupDF <- gppDF
nupDF$OBS <- (aDF10$NUP - aDF7$NUP) / (aDF7$NUP)
nupDF$DRY <- (aDF11$NUP - aDF8$NUP) / (aDF8$NUP)
nupDF$WET <- (aDF12$NUP - aDF9$NUP) / (aDF9$NUP)

pupDF <- gppDF
pupDF$OBS <- (aDF10$PUP - aDF7$PUP) / (aDF7$PUP)
pupDF$DRY <- (aDF11$PUP - aDF8$PUP) / (aDF8$PUP)
pupDF$WET <- (aDF12$PUP - aDF9$PUP) / (aDF9$PUP)


## Plotting
pdf(paste(getwd(), "/drought_analysis/focused_variables/drought_response_co2_response.pdf",sep=""))
par(mfrow=c(2,2), c(2.1, 6.1, 2.1, 6.1), mgp=c(3,1,0))

with(gppDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
                ylim=c(-20, 20), ylab = "GPP response [%]"))
with(gppDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
with(gppDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))

with(laiDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
                 ylim=c(-10, 10), ylab = "LAI response [%]"))
with(laiDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
with(laiDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))

#with(nDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
#                 ylim=c(-20, 20), ylab = "Soil mineral N response [%]"))
#with(nDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
#with(nDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))


#with(pDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
#               ylim=c(-20, 20), ylab = "Soil labile P response [%]"))
#with(pDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
#with(pDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))


with(nupDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
               ylim=c(-20, 20), ylab = "N uptake response [%]"))
with(nupDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
with(nupDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))

with(pupDF, plot(OBS*100~YEAR, type="l", lwd = 1.5, col = "black", 
                 ylim=c(-20, 20), ylab = "P uptake response [%]"))
with(pupDF, points(DRY*100~YEAR, type="l", lwd = 1.5, col = "red"))
with(pupDF, points(WET*100~YEAR, type="l", lwd = 1.5, col = "blue"))

dev.off()

