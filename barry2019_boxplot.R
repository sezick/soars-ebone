###  Code by S. Zick
###  Updated: 15 Jun 2020
###  This code: 1) loads a list "barry_EFs" with 5 elements: coordinates, 
###  1000 error fields (EFs), NAM forecast, ST4 observation, & pointwise mean
###  All precip is in log mm and must be converted to mm (by taking exp(x))
###  2) Transforms variables to units of mm.
###  3) Finds the index for various lat,lon coordinates
###  4) Makes box plots for those locations

# clear all variables
rm(list=ls(all=TRUE))
dev.off() # close all plots

# load libraries
# install.packages(c("raster","fields"))
library(raster) 
library(fields)
# install.packages(c("car","MASS"))
library(car)
library(MASS)
# library(xtable)

# May need to change this file path
workpath="/Volumes/GoogleDrive/Shared drives/SOARS-Ebone/code/"
load(paste(workpath,"my1000sims_barry.rdata",sep="")) 

#-----------
# 5 Elements in data frame
coords  <- barry_EFs[[1]]
simvals_log <- barry_EFs[[2]]
NAM_log <- barry_EFs[[3]]
ST4_log <- barry_EFs[[4]]
PWmean_log <- barry_EFs[[5]]

#-----------
# Transform log precip to precip (inverse of natural log)
NAM <- cbind(NAM_log[1:2],exp(NAM_log[,3]))
names(NAM)[3]<-paste("values")
# simvalues = exp(simvals_log)
simvalues <- ifelse(simvals_log < 0, -exp(-simvals_log), exp(simvals_log))
ST4 <- cbind(ST4_log[1:2],exp(ST4_log[,3]))
names(ST4)[3]<-paste("values")

# Find index for lat/lon location (here, for Ragley, Louisiana)
latpixel = 30.5
lonpixel = -93.2
a = abs( coords[,2]-latpixel ) + abs( coords[,1]-lonpixel )
index1 = which(a == min(a))
# index for New Orleans
latpixel = 29.95
lonpixel = -90.1
a = abs( coords[,2]-latpixel ) + abs( coords[,1]-lonpixel )
index2 = which(a == min(a))

# Find ST4 (observed 24 hr precip) values for those locations
st4index1 = ST4$value[index1] # 11 mm = 24 hr total in Ragley, LA
st4index2 = ST4$value[index2] # #24.2 mm = 24 hr total in New Orleans, LA

# Make box plots of precip at locations based on 1000 scenarios
par(mfrow=c(1,1))
boxplot(NAM$value[index1]+simvalues[index1,],NAM$value[index2]+simvalues[index2,],
        main="Precipitation Forecasts based on Bayesian Model of NAM Forecast Errors",
        ylab="Precipitation (mm)",
        names = c("Ragley, LA (OBS = 11 mm)", "New Orleans, LA (OBS = 24.2 mm)"))

# Save figure
dev.copy(png,paste(workpath,'boxplots.png'))
dev.off()
