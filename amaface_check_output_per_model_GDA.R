# script to check your simulations for AmaFACE simulations
# sanity and quality check for each model 

# please let me (Katrin) know if there are any inconsistencies in the script or we need to add something


# Katrin Fleischer, Anthony Walker, 6 February 2017
#-----------------------------------------------------------------------------------------------

rm(list=ls())

# upload file with output names, units and type of output variable
setwd("~/Documents/Research/Projects/Amazon/AMAZ")
units<-read.table("output_names_units.csv",sep=";",header=T)

# set working directory to path where your output sits (this is mine)
setwd("~/Documents/Research/Projects/Amazon/AMAZ/drought/outputs")

#load required libraries
library(ggplot2)
library(reshape2)
library(lattice)

date<-Sys.Date()

# READING and FORMATTING ----------------------------------------------------------------------------
# fill in your model abbreviation here, example: ORCHIDEE=ORC
modcod<-"GDA"

# read in the output files for ambient and elevated simulation
# if you havent deleted the header rows (as specified) you need to add "skip=x", with x being the number of rows to skip

mets<-c("OBS")  #can be set to c("OBS","DRY","WET")

print(paste("checking simulations AMB and ELE for:",mets,sep=" "))
  
out<-read.table(paste("AmaFACE1_D_",modcod,"_AMB_",mets,".csv",sep=""),sep=",", skip=4)
outco2<-read.table(paste("AmaFACE1_D_",modcod,"_ELE_",mets,".csv",sep=""),sep=",", skip=4)



# check length of frames, need to be TRUE, otherwise you have to many or little days
# 102 years with 365 days plus 25 years with a leap day
if (nrow(out)!=(102*365)+25|nrow(outco2)!=(102*365)+25) {print(paste("number of rows of model output does not match!!"))}

# paste them together by rows, remove original
out<-rbind(out,outco2)
rm(outco2)

# add the 128 column names (or comment out if you have columns names in the csv file:
# in that case use above: out<-read.table(paste("AmaFACE1_D_",modcod,"_AMB_OBS.csv",sep=""),sep=",",header=T))
# but then double-check that the column names match as specified in the output protocol
colnames(out)<-colnames(units)

# add the names for any extra variables that you may have added
# e.g. for ORC they were 2: P deposition and P weathering
# colnames(out)[129:130]<-c("PDEP","PWEA")
# or remove them: 
out<-out[,c(1:128)]

# add the simulation column, and move it forward
out$SIM<-c(rep("amb",nrow(out)/2),rep("ele",nrow(out)/2))
out<-out[,c(1:2,ncol(out),3:(ncol(out)-1))]

# have a look at the created data frames
print(str(out))

#factorize simulation column, and set missing to NA
out$SIM<-as.factor(out$SIM)
out[out==-9999]<-NA


# ANNUAL AGGREGATION - only by calender year here to check that everything is sensible and fluxes add up
#---------------------------------------------------------------------------------------------------

#data.frame(fop,names(out))
years<-unique(data.frame(out$YEAR,out$SIM))

#create empty annual data frame, take out DoY
ann<-out[years$out.YEAR,names(out)]
ann[,]<-NA
ann$YEAR<-years$out.YEAR
ann$SIM<-years$out.SIM
ann$DOY<-NULL
rownames(ann)<-NULL
yvars<-names(ann)[-c(1,2)]

for (y in unique(years$out.YEAR)) {
    print(paste("processing ",y))
    for (v in yvars) {
        for (s in unique(out$SIM)) {
            
            
            indv<-which(colnames(units)==v)
            #take first entry for "name", mean for "clim", sum for "flux", and first entry (DoY1) for pool
            #excpetion for PREC, since its a clim variable but we want the total oer year  
            # if (units[2,indv]=="time") ann[ann$YEAR==y&ann$SIM==s,v]<-as.character(out[out$YEAR==y&out$SIM==s,v][1])
            if (units[2,indv]=="clim"&names(units)[indv]!="PREC") ann[ann$YEAR==y&ann$SIM==s,v]<-mean(out[out$YEAR==y&out$SIM==s,v])
            if (units[2,indv]=="flux"|names(units)[indv]=="PREC") ann[ann$YEAR==y&ann$SIM==s,v]<-sum(out[out$YEAR==y&out$SIM==s,v])
            if (units[2,indv]=="pool") ann[ann$YEAR==y&ann$SIM==s,v]<-out[out$YEAR==y&out$SIM==s,v][1]
            
        }
    }
}
ann$YEAR<-as.numeric(ann$YEAR)

# add row of units after aggreagtion, annual fluxes now in yr-1
units[3,]<-gsub("d-1","yr-1",units[1,])

# check sensibility of fluxes and pools (plot output and check visually if fluxes/pools are in sensible range)
# replace d-1 with yr-1 in main!!

ofile <- paste(date,'_OUT_abs_',modcod,'.pdf',sep='')
pdf(ofile,width=10,height=8)

for (y in yvars) {
    print(y)
    indy<-which(colnames(units)==y)
    
    timeplot<-xyplot(get(y)~YEAR,ann,groups=SIM,pch=19,cex=2,type="a",lwd=2.5,
                     main=paste(y,"in",units[3,which(colnames(units)==y)],sep=" "),col=c("blue","red"),ylab=y,
                     scales=list(tck=c(-0.5,0),alternating=F,relation='free'),
                     key=list(space='inside',border=T,columns=2,text=list(levels(ann$SIM)),
                              lines=list(col=c("blue","red"),lty=1,lwd=3)))
    print(timeplot)
}

dev.off()


# check which variables are missing in output, make sure these are really not outputted by your model
vars_miss<-NULL
for (y in yvars) {
    if(sum(is.na(ann[,y]))==length(ann[,y]))  vars_miss<-c(vars_miss,y)
}
print(paste(c("variables missing from output: ",vars_miss)))


#add change in pools (delta) for mass balance check
delta<-ann[,names(units)[units[2,]=="pool"]]
delta[,]<-NA

for(y in names(units)[units[2,]=="pool"]) {
    for (s in levels(ann$SIM)) {
        for (yr in unique(ann$YEAR)[1:length(unique(ann$YEAR))-1]) {
            
            ind<-ann$SIM==s&ann$YEAR==yr
            ind1<-ann$SIM==s&ann$YEAR==yr+1
            
            delta[ind,y]<-ann[ind1,y]-ann[ind,y]
            
        }}}
names(delta)<-paste("delta",names(delta),sep="")
ann<-cbind(ann,delta) 


#add maximum value for some variables for mass balance check
peak<-ann[,c("LAI","NCON","CL","CFR","CCR","CW","CSTOR")]
peak[,]<-NA

for(y in c("LAI","NCON","CL","CFR","CCR","CW","CSTOR")) {
    for (s in levels(ann$SIM)) {
        for (yr in unique(ann$YEAR)) {
            
            ind<-ann$SIM==s&ann$YEAR==yr
            peak[ind,y]<-max(out[out$YEAR==yr&out$SIM==s,y])
        }}}

names(peak)<-paste("peak",names(peak),sep="")
ann<-cbind(ann,peak) 


#set missing to 0 for plotting
ann[is.na(ann)]<-0


#mass balance closure checks
pdf(paste(date,'_QC_',modcod,'.pdf',sep=''),width=10,height=8)

#productivity
xyplot(I(NPP+RAU)~GPP,ann,groups=SIM,
       main='NPP+RAU~GPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NEP+RECO)~GPP,ann,groups=SIM,
       main='NEP+RECO~GPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(RHET+RAU)~RECO,ann,groups=SIM,
       main='RHET+RAUTO~RECO',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(RL+RW+RCR+RFR+RGR)~RAU,ann,groups=SIM,
       main='RL+RW+RCR+RFR+RGR~RAU',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(LAI~CL,ann,groups=SIM,
       main='LAI~CL',auto.key=T,
       type=c('p','r'),
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)})

xyplot(peakLAI~peakCL,ann,groups=SIM,
       main='peak LAI~peak CL',auto.key=T,
       type=c('p','r'),
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)})

#water balance
xyplot(I(ES+EC+T)~ET,ann,groups=SIM,
       main='ES+EC+T~ET',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PREC-(RO+DRAIN+ET))~deltaSW,ann,groups=SIM,
       main='PREC-RO-DRAIN-ET~deltaSW',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

# carbon growth rates
xyplot(I(CGL-CLITIN)~deltaCL,ann,groups=SIM,
       main='CGL-CLITIN~deltaCL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGW-CWLIN)~deltaCW,ann,groups=SIM,
       main='CGW-CWLIN~deltaCW',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGFR-CFRLIN)~deltaCFR,ann,groups=SIM,
       main='CGFR-CFRLIN~deltaCFR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGCR-CCRLIN)~deltaCCR,ann,groups=SIM,
       main='CGCR-CCRLIN~deltaCCR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP,ann,groups=SIM,
       main='I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CFLITA+CFLITB)~CFLIT,ann,groups=SIM,
       main='CFLITA+CFLITB~CFLIT',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET)~I(deltaCSOIL+deltaCCLITB+deltaCFLIT),ann,groups=SIM,
       main='CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET~deltaCSOIL+deltaCCLITB+deltaCFLIT',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+deltaCSTOR)~NPP,ann,groups=SIM,
       main='CGW+CGL+CGFR+CGCR+CREPR+deltaCSTOR~NPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+RAU)~GPP,ann,groups=SIM,
       main='CGW+CGL+CGFR+CGCR+CREPR+RAU~GPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(CGW+CGL+CGFR+CGCR+CREPR)~NPP,ann,groups=SIM,
       main='CGW+CGL+CGFR+CGCR+CREPR~NPP',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

# N cycle
xyplot(I(NGL-NLITIN-NLRETR)~deltaNL,ann,groups=SIM,
       main='NGL-NLITIN-NLRETR~deltaNL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NGW-NWLIN-NWRETR)~deltaNW,ann,groups=SIM,
       main='NGW-NWLIN-NWRETR~deltaNW',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NGFR-NFRLIN-NFRRETR)~deltaNFR,ann,groups=SIM,
       main='NGFR-NFRLIN-NFRRETR~deltaNFR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NGCR-NCRLIN-NCRRETR)~deltaNCR,ann,groups=SIM,
       main='NGCR-NCRLIN-NCRRETR~deltaNCR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW),ann,groups=SIM,
       main='I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW) (this or the below plot should fall on the 1:1)',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR,ann,groups=SIM,
       main='NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGR-NGCR-NGW~deltaNSTOR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB),ann,groups=SIM,
       main='NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL~deltaNSOIL+deltaNFLIT+deltaNCLITB',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(NPMIN+NPORG)~NSOIL,ann,groups=SIM,
       main='NPMIN+NPORG~NSOIL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

# P cycle

xyplot(I(PGL-PLITIN-PLRETR)~deltaPL,ann,groups=SIM,
       main='PGL-PLITIN-PLRETR~deltaPL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PGW-PWLIN-PWRETR)~deltaPW,ann,groups=SIM,
       main='PGW-PWLIN-PWRETR~deltaPW',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PGFR-PFRLIN-PFRRETR)~deltaPFR,ann,groups=SIM,
       main='PGFR-PFRLIN-PFRRETR~deltaPFR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PGCR-PCRLIN-PCRRETR)~deltaPCR,ann,groups=SIM,
       main='PGCR-PCRLIN-PCRRETR~deltaPCR',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW)~deltaPSTOR,ann,groups=SIM,
       main='PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW~deltaPSTOR (this or the below plot should fall on the 1:1)',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

# we are missing Pdepo missing from output-- should be +PDEP
xyplot(I(PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH)~I(deltaPSOIL+deltaPFLIT+deltaPCLITB),ann,groups=SIM,
       main='PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH~I(deltaPSOIL+deltaPFLIT+deltaPCLITB)',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PPMIN+PPORG)~PSOIL,ann,groups=SIM,
       main='PPMIN+PPORG~PSOIL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PLAB+PSEC+POCC+PPAR)~PPMIN,ann,groups=SIM,
       main='PLAB+PSEC+POCC+PPAR~PPMIN',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

xyplot(I(PLAB+PSEC+POCC+PPAR+PPORG)~PSOIL,ann,groups=SIM,
       main='PLAB+PSEC+POCC+PPAR+PPORG~PSOIL',auto.key=T,
       scales=list(relation='free'),
       panel=function(...){
           panel.xyplot(...)
           panel.abline(a=0,b=1)})

dev.off()

