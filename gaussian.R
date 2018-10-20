rm(list=ls())
#
#------------------ library
suppressWarnings(suppressMessages( library(magicaxis) ))
suppressWarnings(suppressMessages( library(MASS) ))## loading package MASS
#
#------------------ main
args <- commandArgs(trailingOnly = TRUE)
filename=as.character(args[1])
x.norm = na.omit( read.table(filename, header=TRUE, fill=TRUE) )
end<-nrow(x.norm)

x.norm=c(x.norm[,1])
hist(x.norm,main='Histogram of observed data', axes=FALSE)
magaxis(frame.plot=TRUE)
plot(density(x.norm),t='p',main='Density estimate of data', axes=FALSE)
magaxis(frame.plot=TRUE)
#empirical cumulative distribution function
plot(ecdf(x.norm),main='Empirical cumulative distribution function', axes=FALSE)
magaxis(frame.plot=TRUE)

#test the goodness of fit of a gaussian distribution
z.norm<-(x.norm-mean(x.norm))/sd(x.norm)
qqnorm(z.norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line
#
#------------------ Numerical-graphical fit
fit <- fitdistr(x.norm,"normal") ## fitting gaussian pdf parameters
fit
sigma <- fit[1]$estimate[2]
cat('\n sigma^2/2 = ',sigma^2/2,'\n')

h<-hist(x.norm)
xhist<-c(min(h$breaks),h$breaks)
yhist<-c(0,h$density,0)
xfit<-seq(min(x.norm),max(x.norm),length=40) 
yfit<-dnorm(xfit,mean=mean(x.norm),sd=sd(x.norm)) 
plot(xhist,yhist,type='s',ylim=c(0,max(yhist,yfit)), main='Normal pdf and histogram', axes=FALSE)
magaxis(frame.plot=TRUE)
lines(xfit,yfit, col='red')