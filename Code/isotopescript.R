## relook at data now with all isotope values added ##

## loading of data ##

setwd("C:/Users/Robbie/Documents/4th year modules/Research/Rdata")

startdata<-read.table("allisotopechiff.txt",header=T)
head(startdata)

## Look at how isotopic values vary in relation to subspecies ##
list(startdata)
par(mfrow=c(1,1))
plot(startdata$isotope~startdata$subspecies)


## Look at boxplots by period for each subspecies ##

boxplot(startdata$isotope~startdata$period,cex.lab=1.2,xlim=c(0,7),
        ylim=c(-140,-40),pch=19,col="grey",font.lab=2,
        xlab="Period",ylab="Isotopic Value")
par(mfrow=c(1,4))
boxplot(isotope ~period,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,7),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Period",ylab="Isotopic Value")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(0,5),
        ylim=c(-140,-40),pch=19,col="blue",font.lab=2,
        xlab="Period",ylab="Isotopic Value")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(0,5),
        ylim=c(-140,-40),pch=19,col="forestgreen",font.lab=2,
        xlab="Period",ylab="Isotopic Value")


## Check if these differences are statistically significant or just chance ##

lm(startdata$isotope~startdata$subspecies, data=startdata) ## creating of linear model of the affect of subsp on isotope value ##
lm_isotope_subspecies<-lm(startdata$isotope~startdata$subspecies, data=startdata) ## renaming of this ##
summary(lm_isotope_subspecies) ## summary of model ##
anova(lm_isotope_subspecies) ## checking if slope is significantly differnet from zero= yes this is to be expected ##


plot(isotope ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Isotopic Value")
points(isotope ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~jdate,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~jdate,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topleft",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)


plot(isotope ~fat,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,6),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Fat score",ylab="Isotopic Value")
points(isotope ~fat,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~fat,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~fat,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~fat,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~fat,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)


## plot isotopic values as a function of wing length ##

plot(isotope ~wing,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(50,70),
     ylim=c(-80,-40),pch=19,col="red",font.lab=2,
     xlab="Wing length (mm)",ylab="Isotopic Value")
points(isotope ~wing,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~wing,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)


plot(isotope ~wpp2,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(4,10),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="wpp2",ylab="Isotopic Value")
points(isotope ~wpp2,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~wpp2,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~wpp2,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~wpp2,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~wpp2,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)

plot(isotope ~muscle,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(1,3),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="muscle",ylab="Isotopic Value")
points(isotope ~muscle,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~muscle,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)

plot(tars ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(15,22),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Tarsus length (mm)",ylab="Isotopic Value")
points(tars~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(isotope ~muscle,data=startdata[startdata$subspecies=="A",],pch=19,col="forestgreen")
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(isotope ~muscle,data=startdata[startdata$subspecies=="A",]),col="forestgreen",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","forestgreen"),title="Subspecies",cex=0.8)
