## Importing data ##
setwd("C:/Users/Robbie/Documents/4th year modules/Research/Rdata")

startdata<-read.table("alldatachiff.txt",header=T)
head(startdata)

attach(startdata)

## Check data and import ##

names(startdata)
summary(startdata)
tail(startdata)

## checking how big our dataframe is ##

nrow(startdata)
ncol(startdata)

str(startdata)

## Checking data is all being read as numeric or factor approprately ##

is.numeric(idnumber)
is.factor(ring) 
is.factor(species)
is.factor(subspecies)
is.factor(rn)
is.numeric(two)
is.numeric(wp)
is.numeric(wpp2)
is.numeric(wppc)
is.numeric(wpt3)
is.numeric(tars)
is.numeric(tail)
is.numeric(bw)
is.numeric(bd)
is.numeric(bn)
is.numeric(thl)
is.factor(age)
is.factor(moult)
is.factor(ogc)
is.numeric(wing)
is.numeric(weight)
is.numeric(fat)
is.numeric(muscle)
is.factor(ringer)
## make date correct format ##
class(date)
head(date)
as.character(date)
as.Date(as.character(date),format='%d/%m/%Y')

is.numeric(jdate)
is.factor(month)
is.factor(nanjizal)
is.factor(period)

## corrctions for incorrect variable classification ##

age<-as.factor(age)
startdata$ogc<-as.numeric(startdata$ogc)
period<-as.factor(period)

## not sure what data is being read as, need to look at this ##

## looking at mean weights for subsets of data by period and remove na ##

aggregate(startdata$weight,by=list(startdata$period),mean,na.rm=T)



aggregate(startdata$weight_na_removed,by=list(startdata$period),mean)## this code doesnt re-run need to work out how to make columns same lenght ##


## look at histograms of data to check for normality "PDF= Rplot startdata normality"##
par(mfrow=c(3,2))
hist(startdata$idnumber) ## check all idnumber are persent- this is all good ##
hist(startdata$two)
hist(startdata$wp)
hist(startdata$wpp2)
hist(startdata$wppc)
hist(startdata$wpt3)
hist(startdata$tars)
hist(startdata$tail) ## look like this could be non-normal with right skew ##
## test for normality ##
shapiro.test(startdata$tail) ## =0.05 just nonrmal but dont worry about this ##
hist(startdata$bw)
hist(startdata$bd)
hist(startdata$bn)
hist(startdata$thl)
hist(startdata$ogc)
hist(startdata$wing) ## looks like it is bimodal ##
hist(startdata$weight)
hist(startdata$fat) ## slight right skew ##
hist(startdata$muscle) ## right skew ##


## look at how may of each subspecies, retramp/new, age structure and month samples structure are present ##
summary (startdata$subspecies) ## need to check if one of the collyitta should be abutinus. 9=A 86=C 15=T ##
summary (startdata$rn) ## N=102 R=8 ##
summary (startdata$age) ## JUV=94 ADULT=16 ##
Summary (startdata$moult) ## cant work out how moult is being read ##
summary (startdata$month) ## SEPTEMBER=8 OCTOBER=50 NOVEMBER=52 ##
summary (startdata$period) ## Period 1= 15 2= 14 3= 18 4= 24 5= 19 6= 20 

## boxplots of data ##
par(mfrow=c(2,2))
plot(startdata$weight~startdata$subspecies)
plot(startdata$fat~startdata$subspecies)
plot(startdata$jdate~startdata$subspecies)
plot(startdata$month~startdata$subspecies)
plot(startdata$month~startdata$age)

## scatterplots if two continuous datas ##
par(mfrow=c(2,2))
plot(startdata$weight~startdata$wing)
plot(startdata$weight~startdata$fat)
plot(startdata$weight~startdata$tars)
plot(startdata$weight~startdata$thl)

## checking of affect of data on biometrics ##
par(mfrow=c(1,1))
plot(startdata$weight~startdata$jdate)
plot(startdata$fat~startdata$jdate)
plot(startdata$wing~startdata$jdate)
plot(startdata$age~startdata$jdate)
plot(startdata$two~startdata$jdate)
plot(startdata$wpp2~startdata$jdate)
plot(startdata$wp~startdata$jdate)
plot(startdata$wp~startdata$wing)

## checking of expected correlations ##
par(mfrow=c(1,1))
plot(startdata$wing~startdata$wppc)
plot(startdata$wing~startdata$wpp2)
plot(startdata$wing~startdata$wpt3)
plot(startdata$wppc~startdata$wpp2)
plot(startdata$wppc~startdata$wpt3)
plot(startdata$wpp2~startdata$wpt3)


## clear up data removing previous functions ##

ls()## list of all functions ##
rm(convert_to_cm) ## how to remove function convert to cm ##



## looking at mean weights for subsets of data by period ##

aggregate(startdata$weight, by=list(startdata$period),mean)
mean(startdata$weight)
mean(na.omit(startdata$weight))
aggregate(mean (na.omit(startdata$weight,by=list(startdata$period)))) ## Cant get it to remove na and then claculate means 

## attempt to removed all na but now doesnt give all group means ##

na_rm<-na.omit(startdata)
summary(na_rm)
aggregate(na_rm$weight, by=list(na_rm$period),mean)

## start to look at some linear models of data- test with wppc and wing lenght expect to have a significant correlation ##

lm(startdata$wppc~startdata$wing, data=startdata) ## creating of linear model of the affect of wing length on wppc ##
lm_wppc_wing<-lm(startdata$wppc~startdata$wing, data=startdata) ## renaming of this ##
summary(lm_wppc_wing) ## summary of model ##
anova(lm_wppc_wing) ## checking if slope is significantly differnet from zero= yes this is to be expected ##

## look at affect of date on winglength ## 

lm(wing~jdate, data=startdata)
lm_wing_jdate<-lm(wing~jdate, data=startdata)
summary(lm_wing_jdate)
anova(lm_wing_jdate) ## no significant differnce form slope of zero ##

## look at affect of subspecies on jdate ## 

lm(jdate~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_jdate_subspecies<-lm(jdate~subspecies, data=startdata)
summary(lm_jdate_subspecies)
anova(lm_jdate_subspecies) ## there is a statistically significant difference between jdate and subspecies ##

## look at affect of period on weight ## 

lm(weight~period,data=startdata)
lm_weight_period<-lm(weight~period,data=startdata)
summary(lm_weight_period)
anova(lm_weight_period) ## there is a statistically significant difference between weight and period ##

## look at affect of jdate on wing point (actully 2=x)##

lm(wp~jdate*wing,data=startdata)
lm_wp_jdate_wing<-lm(wp~jdate*wing,data=startdata)
summary(lm_wp_jdate_wing)
anova(lm_wp_jdate_wing) ## seems to be a significant correlation between 2=x and j date getting ??


## look at affect of subspecies on weight when controling for fat and wing ## 

lm(weight~subspecies, data=startdata)
lm_weight_subspecies<-lm(weight~subspecies*fat*wing, data=startdata)
summary(lm_weight_subspecies) ## nothing is significant using lm instead use glm ##
anova(lm_weight_subspecies)

## there is a difference in weight between as a result of subspecies, fat and wing measurements. this difference between... ##
## subspecies being even more significant when fat and jdata are controlled for ##


## plotting separate regressions of wing length jdate for the three subspecies ##

summary(lm(wing ~jdate,data=startdata[startdata$subspecies=="C",])) ## for collybitta ##
summary(lm(wing ~jdate,data=startdata[startdata$subspecies=="T",])) ## for tristis ##
summary(lm(wing ~jdate,data=startdata[startdata$subspecies=="A",])) ## for abutinus ##

interaction_jdate_subspecies<-lm(wing~jdate*subspecies,data=startdata)
summary(interaction_jdate_subspecies)

## test whether the intraction term makes the model significantly better ##

nointeraction_jdate_subspecies<-lm(wing~jdate+subspecies,data=startdata)
summary(nointeraction_jdate_subspecies)

## see if there is a difference between these two models ##

anova(interaction_jdate_subspecies,nointeraction_jdate_subspecies) ## the interaction does expalin significantly more variation in data ##

## plot the three subspices wing in relation to capture date separately ##
par(mfrow=c(2,2))
plot(wing ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(50,70),pch=19,col="red",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Wing length")
points(wing ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(wing ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="yellow")
abline(lm(wing ~jdate,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(wing ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(wing ~jdate,data=startdata[startdata$subspecies=="A",]),col="yellow",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","yellow"),title="Subspecies",cex=0.8)

## plot the three subspices fat in relation to capture date separately ##


plot(fat ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(0,6),pch=19,col="red",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Fat score")
points(fat ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(fat ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="yellow")
abline(lm(fat ~jdate,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(fat ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(fat ~jdate,data=startdata[startdata$subspecies=="A",]),col="yellow",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","yellow"),title="Subspecies",cex=0.8)

## plot the three subspices weight in relation to capture date separately ##


plot(weight ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(6,10),pch=19,col="red",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Weight (grams)")
points(weight ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(weight ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="yellow")
abline(lm(weight ~jdate,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(weight ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(weight ~jdate,data=startdata[startdata$subspecies=="A",]),col="yellow",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","yellow"),title="Subspecies",cex=0.8)

## plot the three subspices wpp2 in relation to capture date separately ##

plot(wpp2 ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(4,10),pch=19,col="red",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="wpp2 (mm)")
points(wpp2 ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(wpp2 ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="yellow")
abline(lm(wpp2 ~jdate,data=startdata[startdata$subspecies=="C",]),col="red",lwd=1.5)
abline(lm(wpp2 ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(wpp2 ~jdate,data=startdata[startdata$subspecies=="A",]),col="yellow",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("red","blue","yellow"),title="Subspecies",cex=0.8)


## T.TEST ##
t.test(na_rm$jdate[na_rm$subspecies=="C"],na_rm$jdate[na_rm$subspecies=="C"]) ## not enough data to peform ##

## glm ##

glm(na_rm$jdate~na_rm$subspecies)
glm(subspecies~jdate+fat+wing+tars+subspecies, family = poisson(link = "log"))
fullmodel<-glm(weight~jdate+fat+wing+tars+subspecies, family = poisson(link = "log"))
summary(fullmodel)

summary(model.reduced.1<- update(fullmodel, . ~. -subspecies))

anova(model.reduced.1, fullmodel,test="Chi")





## seems to be a correlation when plotting wp (2=x) and jdate ##

plot(startdata$two~startdata$jdate)


## look at affect of jdate on wing point two=x, seems to be statistically significant ##

lm(two~jdate*wing,data=startdata)
lm_two_jdate_wing<-lm(two~jdate*wing,data=startdata)
summary(lm_two_jdate_wing)
anova(lm_two_jdate_wing) ## seems to be a significant negative correlation between 2=x and j date getting ##

## making a plot that you can add a trendline to ##

par(mfrow=c(1,3))

plot(two ~jdate,data=startdata,cex.lab=1.2,xlim=c(0,60),
     ylim=c(6,11),pch=19,col="Black",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="2 equals x")
abline(lm(two ~jdate,data=startdata),col="Black",lwd=2)





## look at affect of wing length on two=x, seems to be statistically significant nagative correlation##

lm(two~wing,data=startdata)
lm_two_wing<-lm(two~wing,data=startdata)
summary(lm_two_wing)
anova(lm_two_wing) ## seems to be a significant negative correlation between 2=x and wing length??

## making a plot that you can add a trendline to ##

plot(two~wing,data=startdata,cex.lab=1.2,xlim=c(55,65),
     ylim=c(6,11),pch=19,col="Black",font.lab=2,
     xlab="Wing length (mm)",ylab="2 equals x")
abline(lm(two ~wing,data=startdata),col="Black",lwd=2)



## look at affect of jdate on winglength , seems to be statistically significant nagative correlation##

lm(wing~jdate,data=startdata)
lm_wing_jdate<-lm(wing~jdate,data=startdata)
summary(lm_wing_jdate)
anova(lm_wing_jdate) ## seems to be a significant negative correlation between 2=x and wing length??

## making a plot that you can add a trendline to ##

plot(wing~jdate,data=startdata,cex.lab=1.2,xlim=c(0,60),
     ylim=c(55,65),pch=19,col="Black",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Wing length (mm)")
abline(lm(wing~jdate,data=startdata),col="Black",lwd=2)


par(mfrow=c(1,1))


## Plot of wing in relation to capture date- NOT SIGNIFICANT ##

plot(isotope ~wing,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Wing length (mm)",ylab="Feather dH2")
points(isotope ~wing,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(isotope ~wing,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="topright",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(isotope ~wing,data=startdata))
summary(lm(isotope ~wing*subspecies,data=startdata))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="C",]))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="A",]))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="T",]))


## complex model ##
summary(lm(isotope ~wing*subspecies*condition*jdate,data=startdata))
summary(lm(condition ~jdate*fat*subspecies*tars,data=startdata))
summary(lm(jdate ~subspecies*condition*fat,data=startdata))





## plots of isotopic value in realtion to wing- SORT OF SIGNIFICANT ##
plot(isotope ~wing,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Wing length (mm)",ylab="Feather dH2")
points(isotope ~wing,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(isotope ~wing,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(isotope ~wing,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="topright",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(isotope ~wing,data=startdata))
summary(lm(isotope ~wing*subspecies,data=startdata))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="C",]))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="A",]))
summary(lm(isotope ~wing,data=startdata[startdata$subspecies=="T",]))

## Plots of isotopic value in relation to wing for individuals subspecies ##

par(mfrow=c(1,1))
plot(isotope ~wing,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(-100,-50),pch=19,col="red",font.lab=2,
     xlab="Wing length (mm)",ylab="Feather dH2")
plot(isotope ~wing,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(-90,-40),pch=19,col="green4",font.lab=2,
     xlab="Wing length (mm)",ylab="Feather dH2")
plot(isotope ~wing,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(-140,-80),pch=19,col="blue",font.lab=2,
     xlab="Wing length (mm)",ylab="Feather dH2")

?curve


plot(two ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(7,11),pch=19,col="green4",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="two equals")
points(two ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(two ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(two ~jdate,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(two ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(two ~jdate,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)

plot(two ~wing,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(55,65),
     ylim=c(7,11),pch=19,col="green4",font.lab=2,
     xlab="Wing length (mm))",ylab="two equals")
points(two ~wing,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(two ~wing,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(two ~wing,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(two ~wing,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(two ~wing,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)


## body condition plotted against capture date ##

plot(condition ~jdate,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,60),
     ylim=c(4,11),pch=19,col="green4",font.lab=2,
     xlab="Capture date (Days since 20/09/17)",ylab="Body Condition score")
points(condition ~jdate,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(condition ~jdate,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(condition ~jdate,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(condition ~jdate,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(condition ~jdate,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)

## whole model not incluidng subspecies-SIGNIFICANT##
summary(lm(condition ~jdate,data=startdata))
## whole model when controling for subspecies-NOT SIGNIFICANT##
summary(lm(condition ~jdate*subspecies,data=startdata))
## just looking at relationship for collybita ##
summary(lm(condition ~jdate,data=startdata[startdata$subspecies=="C",]))



## plot of condition in ralation to capture period for all data and separate subspecies ##
par(mfrow=c(1,4))
boxplot(condition ~period,data=startdata,cex.lab=1.2,xlim=c(0,7),
        ylim=c(5,11),pch=19,col="grey",font.lab=2,ylab="Standard Mass Index (SMI)")
boxplot(condition ~period,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,7),
        ylim=c(5,11),pch=19,col="forestgreen")
boxplot(condition ~period,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(5,11),pch=19,col="red")
boxplot(condition ~period,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(5,11),pch=19,col="blue")
mtext(text="Period",font=2,side=1,line=3,adj=-3)

summary(lm(condition ~period,data=startdata))
summary(lm(condition ~period*subspecies,data=startdata))
summary(lm(condition ~period,data=startdata[startdata$subspecies=="C",]))
summary(lm(condition ~period,data=startdata[startdata$subspecies=="A",]))
summary(lm(condition ~period,data=startdata[startdata$subspecies=="T",]))

## body codition in relation to isotopic values by subspecies- NOT SIGNIFICANT ##
plot(condition ~isotope,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(-140,-40),
     ylim=c(6,11),pch=19,col="green4",font.lab=2,
     xlab="Isotopic value)",ylab="Body Condition score")
points(condition ~isotope,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(condition ~isotope,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(condition ~isotope,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(condition ~isotope,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(condition ~isotope,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)


summary(lm(condition ~isotope,data=startdata))
summary(lm(condition ~isotope*subspecies,data=startdata))
summary(lm(condition ~isotope,data=startdata[startdata$subspecies=="C",]))
summary(lm(condition ~isotope,data=startdata[startdata$subspecies=="A",]))
summary(lm(condition ~isotope,data=startdata[startdata$subspecies=="T",]))

## Try looing at this just for Tristis ##

plot(condition ~isotope,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(-140,-40),
     ylim=c(6,11),pch=19,col="blue",font.lab=2,
     xlab="Isotopic value)",ylab="Body Condition score")

abline(lm(condition ~isotope,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)

summary(lm(condition ~isotope,data=startdata[startdata$subspecies=="T",]))


## Boxplot shows that there is no difference in body condition between the three subspecies##
boxplot(condition~subspecies,data=startdata, cex.lab=1.6,xlab="Subspecies",ylab="Standard Mass Index score (Body condition)")
summary(lm(condition ~subspecies,data=startdata))

## scattergraphy of relationship between wing length and tail length ##

plot(wing ~tail,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(43,53),
     ylim=c(55,65),pch=19,col="green4",font.lab=2,
     xlab="Tail length)",ylab="Wing length (mm)")
points(wing ~tail,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(wing ~tail,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(wing ~tail,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(wing ~tail,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(wing ~tail,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topright",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)


















## TIMING AND ORIGIN BY SUBSPECIES ##

########################### subspecies differences in stable isotope results ###############################
plot((startdata$isotope~startdata$subspecies),xlab="Subspecies",ylab="Feather ??H2 (???)")

## statistical test for these differences- SIGNIFICANT ##
lm(isotope~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_isotope_subspecies<-lm(isotope~subspecies, data=startdata)
summary(lm_isotope_subspecies)
anova(lm_isotope_subspecies) ## there is a statistically significant difference between isotopic values and subspecies ##


##what about if you control for capture date##
lm(isotope~subspecies*jdate, data=startdata) 
lm_isotope_jdate_subspecies<-lm(isotope~subspecies*jdate, data=startdata)
summary(lm_isotope_jdate_subspecies)
anova(lm_isotope_jdate_subspecies)

################################capture date by subspecies-highly significant########################################
plot((startdata$jdate~startdata$subspecies),xlab="Subspecies",ylab="Capture date (Days since 20/09/2017")
lm(jdate~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_jdate_subspecies<-lm(jdate~subspecies, data=startdata)
summary(lm_jdate_subspecies)
anova(lm_jdate_subspecies)



## MIGRATORY CONDITION BY SUBSPECIES ##

## weight as a function of subspecies- NOT QUITE SIGNIFICANT ##
plot(startdata$weight~startdata$subspecies)
lm(weight~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_weight_subspecies<-lm(weight~subspecies, data=startdata)
summary(lm_weight_subspecies)
anova(lm_tars_subspecies)

## body condition as a function of subspecies- NOT SIGNIFICANT ##
plot(startdata$condition~startdata$subspecies)
lm(condition~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_condition_subspecies<-lm(condition~subspecies, data=startdata)
summary(lm_condition_subspecies)
anova(lm_condition_subspecies)


################################# fat as a function of subspecies- SIGNIFICANT ####################################
plot((startdata$fat~startdata$subspecies),cex.lab=1.5,xlab="Subspecies",ylab="Fat score")
lm(fat~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_fat_subspecies<-lm(fat~subspecies, data=startdata)
summary(lm_fat_subspecies)
anova(lm_fat_subspecies)



## muscle as a function of subspecies- NOT SIGNIFICANT ##
plot(startdata$muscle~startdata$subspecies)
lm(muscle~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_muscle_subspecies<-lm(muscle~subspecies, data=startdata)
summary(lm_muscle_subspecies)
anova(lm_muscle_subspecies)



## BIOMETRICAL DIFFERNECE BY SUBSPECIES IN RELATION TO MIGRATORY BIOLOGICAL SIGNIFICANCE ##

## wing length by subspecies-NOT SIGNIFICANT ##
plot(startdata$wing~startdata$subspecies)
lm(wing~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_wing_subspecies<-lm(wing~subspecies, data=startdata)
summary(lm_wing_subspecies)
anova(lm_wing_subspecies)

## tail length by subspecies- NOT SIGNIFICANT BUT LOOKS LIKE IT SHOULD BE ##
plot(startdata$tail~startdata$subspecies)
lm(tail~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_tail_subspecies<-lm(tail~subspecies, data=startdata)
summary(lm_tail_subspecies)
anova(lm_tail_subspecies)

## wpp2 by subspecies- NOT SIGNIFICANT BUT C LOOKS DIFF TO A AND T ##
plot(startdata$wpp2~startdata$subspecies)
lm(wpp2~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_wpp2_subspecies<-lm(wpp2~subspecies, data=startdata)
summary(lm_wpp2_subspecies)
anova(lm_wpp2_subspecies)

## wpt3 by subspecies- NOT SIGNIFICANT BUT C LOOKS DIFF TO A AND T ##
plot(startdata$wpt3~startdata$subspecies)
lm(wpt3~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_wpt3_subspecies<-lm(wpt3~subspecies, data=startdata)
summary(lm_wpt3_subspecies)
anova(lm_wpt3_subspecies)

## wpt3*wpp2 by subspecies- didnt make it any more significant ##
plot(startdata$t3p2~startdata$subspecies)
lm(t3p2~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_t3p2_subspecies<-lm(t3p2~subspecies, data=startdata)
summary(lm_t3p2_subspecies)
anova(lm_t3p2_subspecies)

## two equals by subspecies- looks different but not significant ##
plot(startdata$two~startdata$subspecies)
lm(two~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_two_subspecies<-lm(two~subspecies, data=startdata)
summary(lm_two_subspecies)
anova(lm_two_subspecies)


## pointedness by subspecies- looks different but not significant ##
plot(startdata$pointedness~startdata$subspecies)
lm(pointedness~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_pointedness_subspecies<-lm(pointedness~subspecies, data=startdata)
summary(lm_pointedness_subspecies)
anova(lm_pointedness_subspecies)

## BIOMETRICAL DIFFERNENCES BY SUBSPECIES RELATED TO SUBSPECIESIFIC VARIATION ##


##################### THL is significantly different between A and T-SIGNIFICANT DIFFERENCE ######################################
plot((startdata$thl~startdata$subspecies),cex.lab=1.5,xlab="Subspecies",ylab="Total head length (THL) (mm)")
lm(thl~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_thl_subspecies<-lm(thl~subspecies, data=startdata)
summary(lm_thl_subspecies)
anova(lm_thl_subspecies) ## there is a statistically significant difference between isotopic values and subspecies ##


## tarsus as a function of subspecies- Not significant ##
plot(startdata$tars~startdata$subspecies)
lm(tars~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_tars_subspecies<-lm(tars~subspecies, data=startdata)
summary(lm_tars_subspecies)
anova(lm_tars_subspecies)


## ogc moult as a factor of subspecies-NO SIGNIFICANT DIFFERENCE ##
plot(startdata$ogc~startdata$subspecies)
lm(ogc~subspecies, data=startdata) 
lm_ogc_subspecies<-lm(ogc~subspecies, data=startdata)
summary(lm_ogc_subspecies)
anova(lm_ogc_subspecies)










## does isotope predict arrival date ##

plot(jdate ~isotope,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(-140,-40),
     ylim=c(0,60),pch=19,col="red",font.lab=2,
     xlab="Feather ??H2 (???)",ylab="Capture date (Days since 20/09/17)")
points(jdate ~isotope,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(jdate ~isotope,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="bottomleft",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(jdate~isotope,data=startdata))
summary(lm(jdate~isotope*subspecies,data=startdata))

summary(lm(isotope~period,data=startdata))
summary(lm(isotope~period*subspecies,data=startdata))
summary(lm(isotope~period,data=startdata[startdata$subspecies=="C",]))
summary(lm(isotope~period,data=startdata[startdata$subspecies=="A",]))
summary(lm(isotope~period,data=startdata[startdata$subspecies=="T",]))

plot(jdate ~isotope,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(-85,-40),
     ylim=c(0,60),pch=19,col="green4",font.lab=2,
     xlab="Feather ??H2 (???)",ylab="Capture date (Days since 20/09/17)")
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)

summary(lm(jdate ~isotope,data=startdata[startdata$subspecies=="C",]))

boxplot(isotope ~period,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(0,6),
     ylim=c(-140,-40),pch=19,col="red",font.lab=2,
     xlab="Period",ylab="Feather ??H2 (???)")
points(jdate ~isotope,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(jdate ~isotope,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(jdate ~isotope,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="bottomleft",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(isotope~period*subspecies,data=startdata))



## Boxplots of isotopic values by periods for all chiff and by subspecies##

par(mfrow=c(1,4))
boxplot(isotope ~period,data=startdata,cex.lab=1.2,xlim=c(0,7),
        ylim=c(-140,-40),pch=19,col="grey",font.lab=2,ylab="Feather ??H2 (???)")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,7),
        ylim=c(-140,-40),pch=19,col="forestgreen")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(-140,-40),pch=19,col="red")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(-140,-40),pch=19,col="blue")
mtext(text="Period",font=2,side=1,line=3,adj=-3)


par(mfrow=c(1,4))
boxplot(isotope ~period,data=startdata,cex.lab=1.2,xlim=c(0,7),
        ylim=c(-140,-40),pch=19,col="grey",font.lab=2,
        xlab="Period",ylab="Feather ??H2 (???)")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,7),
        ylim=c(-140,-40),pch=19,col="forestgreen",font.lab=2,
        xlab="Period",ylab="Feather ??H2 (???)")
mtext(text="Period",font=2,side=1,line=3,adj=-3)
mtext(text="Period",font=2,side=2,line=1,adj=-2)
boxplot(isotope ~period,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(-140,-40),pch=19,col="red",font.lab=2,
        xlab="Period",ylab="Feather ??H2 (???)")
boxplot(isotope ~period,data=startdata[startdata$subspecies=="T",],cex.lab=1.2,xlim=c(-2,5),
        ylim=c(-140,-40),pch=19,col="blue",font.lab=2,
        xlab="Period",ylab="Feather ??H2 (???)")



## body condition and fat scores ##

par(mfrow=c(1,1))
plot(condition~fat,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,pch=19,col="green4",font.lab=2,
        xlab="Fat score",ylab="Condition")
points(condition ~fat,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
points(condition ~fat,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(condition ~fat,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(condition ~fat,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(condition ~fat,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="topleft",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)


## Cecking to see relationship between wing length and tarsus ##

plot(wing ~tars,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(17,22),
     ylim=c(55,65),pch=19,col="red",font.lab=2,
     xlab="Tars",ylab="Wing")
points(wing ~tars,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(wing ~tars,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(wing ~tars,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(wing ~tars,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(wing ~tars,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="bottomright",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(wing ~tars,data=startdata))
summary(lm(wing ~tars*subspecies,data=startdata))
summary(lm(wing ~tars,data=startdata[startdata$subspecies=="C",]))
summary(lm(wing ~tars,data=startdata[startdata$subspecies=="A",]))
summary(lm(wing ~tars,data=startdata[startdata$subspecies=="T",]))

## How strongly correlated are body condition with wing and body condition with tarsus? ##

plot(condition~conditiontars,data=startdata[startdata$subspecies=="A",],cex.lab=1.2,xlim=c(6,11),
     ylim=c(6,11),pch=19,col="red",font.lab=2,
     xlab="Condition Wing",ylab="Condition Tars")
points(condition~conditiontars,data=startdata[startdata$subspecies=="C",],pch=19,col="green4")
points(condition~conditiontars,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
abline(lm(condition~conditiontars,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
abline(lm(condition~conditiontars,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(condition~conditiontars,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
legend(x="bottomright",legend=c("Abutinus","Collybitta","Tristis"),pch=19,lty =1,col=c("red","green4","blue"),title="Subspecies",cex=0.8)

summary(lm(condition~conditiontars,data=startdata))
summary(lm(condition~conditiontars*subspecies,data=startdata))
summary(lm(condition~conditiontars,data=startdata[startdata$subspecies=="C",]))
summary(lm(condition~conditiontars,data=startdata[startdata$subspecies=="A",]))
summary(lm(condition~conditiontars,data=startdata[startdata$subspecies=="T",]))

