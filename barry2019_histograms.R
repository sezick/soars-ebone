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
dev.off() # closes all plots, if any plots are open

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


#-----------
# Make 4-panel plot of NAM model forecast + first 3 error fields
par(mfrow=c(2,2))
plot(rasterFromXYZ(NAM), main = "NAM forecast 24-hour precipitation")

# One way to visualize error fields on mm scale
for (i in 1:3) {plot(rasterFromXYZ(cbind(coords, simvalues[,i])), main= paste("Bayesian Model Error Field #", i))}

#-----------
# Stats

# descriptive stats for entire simvalues matrix
summary(as.vector(simvalues)) #pints to screen

# mean errors for all pixels in all 1000 EFs
meanErrorsAll=mean(simvalues); 
# max error for all pixels in all of the 1000 EF scenarios
maxErrorAll = max(simvalues) 
#428.2613 from all of the pixels in all of the scenarios over a 24 period
minErrorAll = min(simvalues)

# Calculate the rest of the stats
meanErrorsPixels=apply(simvalues,1,mean)
meanErrorsSims=apply(simvalues,2,mean)
minErrorsPixels=apply(simvalues,1,min)
maxErrorsPixels=apply(simvalues,1,max)
minErrorsSims=apply(simvalues,2,min)
maxErrorsSims=apply(simvalues,2,max)

#-----------
# Make 4-panel plot of box plots describing errors in various ways
par(mfrow=c(2,2))
boxplot(meanErrorsPixels,main="Mean Errors for 6896 Pixels")
boxplot(meanErrorsSims,main="Mean Errors for 1000 Simulations")
boxplot(minErrorsSims,main="Min Errors for 1000 Simulations")
boxplot(maxErrorsSims,main="Max Errors for 1000 Simulations")
# Save figure
dev.copy(png,paste(workpath,'boxplot_summarystats.png'))
dev.off()

# Histogram & Box Plot of All Simulated Errors
par(mfrow=c(2,1))
# hist(simvalues , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))
hist(simvalues,freq=T, breaks=20, main="",xlab="") # takes a couple of seconds
title(xlab = "Bayesian Model Errors (mm)", line = 2.2, cex.lab = 1)
# title(ylab = "Number of Occurrences", line = 2.5, cex.lab = 1.2)
# For box plot, there are too many data points, so instead we'll draw a random sample
simRandomValues=sample(as.vector(simvalues),100000)
boxplot(simRandomValues,horizontal=T,main="Distribution of 10,000 Randomly Sampled Errors",xlab="Bayesian Model Errors (mm)")
# Save figure
dev.copy(png,paste(workpath,'histogram+boxplot_ALLerrors.png'))
dev.off()
