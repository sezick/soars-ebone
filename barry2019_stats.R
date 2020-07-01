###  Code by S. Zick - edited from orginal script by Stephen Walsh
###  Updated: 8 Jun 2020
###  This code: 1) loads a list "barry_EFs" with 5 elements: coordinates, 
###  1000 error fields (EFs), NAM forecast, ST4 observation, & pointwise mean
###  All precip is in log mm and must be converted to mm (by taking exp(x))
###  2) Transforms variables to units of mm.
###  3) Plots NAM precip + 3 precip scenarios and saves the file
###  4) Calculates 3 types of mean values (see code below)

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

# Plot NAM precip (in mm) and first three "precip scenarios" (NAM + error field)
par(mfrow=c(2,2))
plot(rasterFromXYZ(NAM), main = "NAM, mm"); US(add=T, col="gray80")
#plot(rasterFromXYZ(NAM_mm) + PWmean, main = "NAM, no bias adjusted, mm"); US(add=T, col="gray80")
# plot(NAM_unbias, main = "NAM, mm"); US(add=T, col="gray80")

# Plot first three NAM + error fields ("precip scenarios" = NAM + error field)
for (i in 1:3) {
  plot(rasterFromXYZ(cbind(coords, NAM$value+simvalues[,i])), main = paste("NAM + Error field", i))
  US(add=T, col="gray80")
}

# Save figure
dev.copy(png,paste(workpath,'NAM+3EFs.png'))
dev.off()

#-----------
# Stats

# mean errors for all pixels in all 1000 EFs
meanErrorsAll=mean(simvalues); 

# mean for each individual error field
meanErrorsSims=rep(NA,1000)
for (i in 1:1000) {
  meanErrorsSims[i]=mean(simvalues[,i])
}

#mean of all errors sims for each pixel
meanErrorsPixels=rep(NA,length(coords[,1]))
#j=1
for (j in 1:length(meanErrorsPixels)) {
  meanErrorsPixels[j] = mean(simvalues[j,])
}

# Plot mean errors (over all 1000 sims) and mean precip scenarios
# mean of Precip scenarios should look very similar to the mean NAM forecast
par(mfrow=c(1,2))
plot(rasterFromXYZ(cbind(coords, meanErrorsPixels)), main = "mean Errors"); US(add=T, col="gray80")
# plot(rasterFromXYZ(cbind(coords, meanErrorsPixels)), main = "mean Errors",breaks = c(0, 0.5, 1, 1.5, 2)); US(add=T, col="gray80")
# Plot mean "precip scenarios)
plot(rasterFromXYZ(cbind(coords, NAM$value+meanErrorsPixels)), main = "mean Scenarios"); US(add=T, col="gray80")

# Plot NAM precip (left) and mean precip scenarios (right)
# mean of Precip scenarios should look very similar to the mean NAM forecast
par(mfrow=c(1,2))
plot(rasterFromXYZ(NAM), main = "NAM"); US(add=T, col="gray80")
# Plot mean "precip scenarios)
plot(rasterFromXYZ(cbind(coords, NAM$value+meanErrorsPixels)), main = "mean Scenarios"); US(add=T, col="gray80")
# Save figure
dev.copy(png,paste(workpath,'NAM+meanScenarios.png'))
dev.off()
