## Importing data ##
startdata <- read.table("Data/alldatachiff.txt",header=T)
head(startdata)

isodata <- read.table("Data/allisotopechiff.txt",header=T)


## Check data and import ##

names(startdata)
summary(startdata)
tail(startdata)








## ANOVA between capture date and subspecies ##

lm(jdate~subspecies, data=startdata)
lm_jdate_subspecies<-lm(jdate~subspecies, data=startdata)
summary(lm_jdate_subspecies)
anova(lm_jdate_subspecies) ## there is a statistically significant difference between jdate and subspecies ##

#postHocTest(aov(jdate ~ subspecies, data = startdata), method = "scheffe")

#a1 <- aov(jdate~subspecies, data=startdata) 
#summary(a1)

pairwise.t.test(startdata$jdate, startdata$subspecies, p.adj = "holm")

pairwise.t.test(startdata$isotope, startdata$subspecies, p.adj = "holm")

?IQR
isotopeIQR_A<-startdata$isotope[startdata$subspecies=="A"]
IQR(isotopeIQR_A)

isotopeIQR_C<-startdata$isotope[startdata$subspecies=="C"]
IQR(isotopeIQR_C)

isotopeIQR_T<-startdata$isotope[startdata$subspecies=="T"]
IQR(isotopeIQR_T)
 


jdateIQR_A<-startdata$jdate[startdata$subspecies=="A"]
IQR(jdateIQR_A)

jdateIQR_C<-startdata$jdate[startdata$subspecies=="C"]
IQR(jdateIQR_C)

jdateIQR_T<-startdata$jdate[startdata$subspecies=="T"]
IQR(jdateIQR_T)

#comparison_jdate<-scheffe.test(lm_jdate_subspecies,"subspecies",group=TRUE,console=TRUE)
#??htest
## ANOVA between isotope and subspecies ##

lm(isotope~subspecies, data=startdata) ## doing a lm for this is probably wrong, just a t-test will do ##
lm_isotope_subspecies<-lm(isotope~subspecies, data=startdata)
summary(lm_isotope_subspecies)
anova(lm_isotope_subspecies) ## there is a statistically significant difference between isotope and subspecies ##









## Model 1- What governs variation in arrival date? ##

summary(lm(jdate~isotope*condition,data=startdata))

## 2 way interaction: Simplest model and highly significant but does not include subspecies (Condition wing better and condition tars) ##

summary(lm(jdate~isotope*condition*subspecies,data=startdata))#######################
## 3 way interaction: Inclusion of subspecies makes model less significant but still overall significant (Condition wing better and condition tars) ##

## Try above model for eac individual subspecies ##
summary(lm(jdate~isotope*conditiontars*pointedness,data=startdata[startdata$subspecies=="C",])) #sIGNIFICANT##
summary(lm(jdate~isotope*condition,data=startdata[startdata$subspecies=="A",])) #INsIGNIFICANT##
summary(lm(jdate~isotope*condition,data=startdata[startdata$subspecies=="T",])) #NEAR sIGNIFICANT##


summary(lm(jdate~isotope*conditiontars*pointedness,data=startdata))
## 4 way interaction: Adding of pointedness makes model just insignifcant now 0.055 Using condition tars expalains more than condition wing ##

summary(lm(jdate~isotope*conditiontars*pointedness*subspecies*wing,data=startdata))
## 5 way intersaction: significant but loosse a lot of DF condition tars expalains more than condition wing ##


## Do models 1 and 2 differ significantly? ##
model1<-(lm(jdate~isotope*condition,data=startdata))
model2<-(lm(jdate~isotope*condition*subspecies,data=startdata))
AIC(model1,model2)
hist(residuals(model2))

## simplest models that are split by subspecies ##
summary(lm(jdate~isotope*condition,data=startdata[startdata$subspecies=="C",])) ## significant ##
summary(lm(jdate~isotope*condition,data=startdata[startdata$subspecies=="A",])) ## insignificant ##
summary(lm(jdate~isotope*condition,data=startdata[startdata$subspecies=="T",])) ## insignificant ##
## All models run and dont run out of DF ## 
 
## more complex models include pointedness ##
summary(lm(jdate~isotope*conditiontars*pointedness,data=startdata[startdata$subspecies=="C",])) ## more significant when pointedness added##
summary(lm(jdate~isotope*conditiontars*pointedness,data=startdata[startdata$subspecies=="A",])) ## insuficcient DF ##
summary(lm(jdate~isotope*conditiontars*pointedness,data=startdata[startdata$subspecies=="T",])) ## insuficcient DF ##
## Cant run for A and T as individuals as run out of degrees of freedom. ##





##################     ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ  MODELS   ##########################


################### rerun of MODEL AA with z data ########################
summary(lm(isotope~period*subspecies,data=startdata))
anova(lm(isotope~period*subspecies,data=startdata))

summary(lm(isotope~period,data=startdata[startdata$subspecies=="C",]))
anova(lm(isotope~period,data=startdata[startdata$subspecies=="C",]))

summary(lm(zisotope~period,data=startdata[startdata$subspecies=="A",]))
anova(lm(zisotope~period,data=startdata[startdata$subspecies=="A",]))

summary(lm(zisotope~period,data=startdata[startdata$subspecies=="T",]))
anova(lm(zisotope~period,data=startdata[startdata$subspecies=="T",]))

################### rerun of MODEL 1A with z data ########################
summary(lm(jdate~zisotope*zcondition,data=startdata))
anova(lm(jdate~zisotope*zcondition,data=startdata))

summary(lm(jdate~zisotope*zcondition*subspecies,data=startdata))
anova(lm(jdate~zisotope*zcondition*subspecies,data=startdata))

summary(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="C",]))
anova(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="C",]))

summary(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="A",]))
anova(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="A",]))

summary(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="T",]))
anova(lm(jdate~zisotope*zcondition,data=startdata[startdata$subspecies=="T",]))

summary(lm(jdate~zisotope*subspecies,data=startdata))
anova(lm(jdate~zisotope*subspecies,data=startdata))
## isotope, condition and subspecies are all significant ##

summary(lm(jdate~zisotope*zcondition*zpointedness*subspecies,data=startdata))
anova(lm(jdate~zisotope*zcondition*zpointedness*subspecies,data=startdata))
## inclusion of pointedness makes model significantly worse but some interaction of pointedness with isotope and condition look at in MODEL 4/5 ##


################### rerun of MODEL 1B with z data ########################

summary(lm(jdate~zisotope*zfat*subspecies,data=startdata))
anova(lm(jdate~zisotope*zfat*subspecies,data=startdata))

summary(lm(jdate~zisotope*zfat,data=startdata))
anova(lm(jdate~zisotope*zfat,data=startdata))


summary(lm(jdate~zfat*subspecies,data=startdata))
anova(lm(jdate~zfat*subspecies,data=startdata))



## run model of just collybitta to see if there really is a relationship of fat ##
summary(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="C",]))
anova(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="C",]))

summary(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="A",]))
anova(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="A",]))

summary(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="T",]))
anova(lm(jdate~zisotope*zfat,data=startdata[startdata$subspecies=="T",]))


## adding pointedness makes model much worse ##
summary(lm(jdate~zisotope*zcondition*zfat*zpointedness*subspecies,data=startdata))
anova(lm(jdate~zisotope*zcondition*zfat*zpointedness*subspecies,data=startdata))


################### rerun of MODEL 1c with z data ########################

summary(lm(jdate~zisotope*zpointedness*subspecies,data=startdata))
anova(lm(jdate~zisotope*zpointedness*subspecies,data=startdata))

summary(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="C",]))
anova(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="C",]))
summary(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="A",]))
anova(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="A",]))
summary(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="T",]))
anova(lm(jdate~zisotope*zpointedness,data=startdata[startdata$subspecies=="T",]))



################### rerun of MODEL 1d with z data ########################

summary(lm(jdate~zisotope*zwing*subspecies,data=startdata))
anova(lm(jdate~zisotope*zwing*subspecies,data=startdata))

summary(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="C",]))
anova(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="C",]))
summary(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="A",]))
anova(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="A",]))
summary(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="T",]))
anova(lm(jdate~zisotope*zwing,data=startdata[startdata$subspecies=="T",]))


################# rerun of MODEL 2A with z data ########################
summary(lm(condition~zjdate*zfat*subspecies,data=startdata))
anova(lm(condition~zjdate*zfat*subspecies,data=startdata))

summary(lm(conditiontars~zjdate*zfat*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zfat*subspecies,data=startdata))

summary(lm(condition~jdate*fat*subspecies,data=startdata))
anova(lm(condition~jdate*fat*subspecies,data=startdata))

summary(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="C",]))
anova(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="C",]))
summary(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="A",]))
anova(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="A",]))
summary(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="T",]))
anova(lm(conditiontars~zjdate*zfat,data=startdata[startdata$subspecies=="T",]))


## with condition standardized it stops any correlation with fat that could occur ##

summary(lm(fat~subspecies,data=startdata))
## fat differes significantly between the three subspecies.

################# rerun of MODEL 2B with z data ########################
summary(lm(conditiontars~zjdate*zwing*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zwing*subspecies,data=startdata))

summary(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="C",]))
anova(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="C",]))
summary(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="A",]))
anova(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="A",]))
summary(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="T",]))
anova(lm(conditiontars~zjdate*zwing,data=startdata[startdata$subspecies=="T",]))


################# rerun of MODEL 2C with z data ########################
summary(lm(conditiontars~zjdate*zwing*zfat*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zwing*zfat*subspecies,data=startdata))

################# rerun of MODEL 2C2 with z data ########################
summary(lm(conditiontars~zjdate*zwing*zfat*zmuscle*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zwing*zfat*zmuscle*subspecies,data=startdata))

################# rerun of MODEL 2C3 with z data ########################
summary(lm(conditiontars~zjdate*zwing*zfat*zmuscle*zisotope*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zwing*zfat*zmuscle*zisotope*subspecies,data=startdata))

## is there a difference betwen 2c2 and 2c3 and 2a
model7<-(lm(conditiontars~zjdate*zfat*subspecies,data=startdata))
model5<-(lm(conditiontars~zjdate*zwing*zfat*zmuscle*subspecies,data=startdata))
model6<-(lm(conditiontars~zjdate*zwing*zfat*zmuscle*zisotope*subspecies,data=startdata))
AIC(model5,model6)
AIC(model7,model5)
AIC(model7,model6)
hist(residuals(model5))
hist(residuals(model6))
hist(residuals(model7))

################# rerun of MODEL 2D with z data ########################
summary(lm(conditiontars~zjdate*musclenotwo*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*musclenotwo*subspecies,data=startdata))

summary(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="C",]))
anova(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="C",]))
summary(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="A",]))
anova(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="A",]))
summary(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="T",]))
anova(lm(conditiontars~zjdate*musclenotwo,data=startdata[startdata$subspecies=="T",]))

################# rerun of MODEL 2E with z data ########################
summary(lm(conditiontars~zjdate*zisotope*subspecies,data=startdata))
anova(lm(conditiontars~zjdate*zisotope*subspecies,data=startdata))

summary(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="C",]))
anova(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="C",]))

summary(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="A",]))
anova(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="A",]))

summary(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="T",]))
anova(lm(conditiontars~zjdate*zisotope,data=startdata[startdata$subspecies=="T",]))

################# rerun of MODEL 2f with z data ########################
summary(lm(condition~zisotope*subspecies,data=startdata))
anova(lm(condition~zisotope*subspecies,data=startdata))

summary(lm(condition~zisotope,data=startdata[startdata$subspecies=="C",]))
anova(lm(condition~zisotope,data=startdata[startdata$subspecies=="C",]))

summary(lm(condition~zisotope,data=startdata[startdata$subspecies=="A",]))
anova(lm(condition~zisotope,data=startdata[startdata$subspecies=="A",]))

summary(lm(condition~zisotope,data=startdata[startdata$subspecies=="T",]))
anova(lm(condition~zisotope,data=startdata[startdata$subspecies=="T",]))



################# rerun of MODEL 3a with z data ########################


summary(lm(isotope~zpointedness*subspecies,data=startdata))
anova(lm(isotope~zpointedness*subspecies,data=startdata))

summary(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="C",]))
anova(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="C",]))

summary(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="A",]))
anova(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="A",]))

summary(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="T",]))
anova(lm(isotope~zpointedness,data=startdata[startdata$subspecies=="T",]))

################# rerun of MODEL 3a with z data ########################


summary(lm(isotope~zpointedness*weight*subspecies,data=startdata))
anova(lm(isotope~zpointedness*weight *subspecies,data=startdata))

summary(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="C",]))
anova(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="C",]))

summary(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="A",]))
anova(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="A",]))

summary(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="T",]))
anova(lm(isotope~zpointedness*weight,data=startdata[startdata$subspecies=="T",]))


################# rerun of MODEL 3B with z data ########################


summary(lm(isotope~zwing*zweight*subspecies,data=startdata))
anova(lm(isotope~zwing*zweight*subspecies,data=startdata))

summary(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="C",]))
anova(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="C",]))

summary(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="A",]))
anova(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="A",]))

summary(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="T",]))
anova(lm(isotope~zwing*zweight,data=startdata[startdata$subspecies=="T",]))




################# rerun of MODEL 3C with z data ########################NOT USED


summary(lm(isotope~zpointedness*zwing*subspecies,data=startdata))
anova(lm(isotope~zpointedness*zwing*subspecies,data=startdata))

summary(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="C",]))
anova(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="C",]))

summary(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="A",]))
anova(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="A",]))

summary(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="T",]))
anova(lm(isotope~zconditiontars*zwing,data=startdata[startdata$subspecies=="T",]))





## argument supports model 2B and 2C##
summary(lm(weight~zfat*subspecies,data=startdata))
anova(lm(weight~zfat*subspecies,data=startdata))

summary(lm(weight~zfat,data=startdata[startdata$subspecies=="C",]))
summary(lm(weight~zfat,data=startdata[startdata$subspecies=="A",]))
summary(lm(weight~zfat,data=startdata[startdata$subspecies=="T",]))
 ## correlation of wing and weight ##

par(mar=c(6,6,3,3))
ggplot(startdata,aes(x=fat,y=weight,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Fat score")))+
  ylab("Mass (grams)")+
  scale_y_continuous(breaks=seq(6,11,1))+
  scale_x_continuous(breaks=seq(0,5,1))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray45", "black"), name = "Subspecies",
                     labels = c("Abietinus", "Collybita", "Tristis"))+
  theme(legend.position = "none")



## argument supports model 2B and 2C##

summary(lm(weight~zwing*subspecies,data=startdata))
## as wing and weight dont correlate strongly can argue that it isnt a body size correlation between condition and wing. ##

## correlation of wing and weight ##


PLOT25<-ggplot(startdata,aes(x=wing,y=weight,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Wing length (mm)")))+
  ylab("Mass (grams)")+
  scale_y_continuous(breaks=seq(6,11,1))+
  scale_x_continuous(breaks=seq(55,65,2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("lightblue3","yellow2","firebrick2"), name = "Subspecies",
                     labels = c("Abietinus", "Collybita", "Tristis"))+
  theme(legend.position = "none")

plot((startdata$fat~startdata$subspecies), whisklty = 7,lwd=1.6, ylim=c(0,5),cex.lab=1.2,names=c("Abietinus","Collybita", "Tristis"),las=1, col=(c("lightblue3","yellow2","firebrick2")),xlab="Subspecies",ylab="Fat score")

plot_grid(PLOT26, PLOT25, labels = "AUTO",ncol = 1, align = 'h')

summary(lm(tars~zwing*subspecies,data=startdata))
anova(lm(tars~zwing*subspecies,data=startdata))
## also tarsus and weight dont correlate particularly strongly 

PLOT26<-ggplot(startdata,aes(x=wing,y=tars,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Wing length (mm)")))+
  ylab("Tarsus lenght (mm)")+
  scale_y_continuous(breaks=seq(17,22,1))+
  scale_x_continuous(breaks=seq(55,65,2))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("lightblue3","yellow2","firebrick2"), name = "Subspecies",
                     labels = c("Abietinus", "Collybita", "Tristis"))+
  theme(legend.position = "none")


summary(lm(tars~zweight*subspecies,data=startdata))
anova(lm(tars~zweight*subspecies,data=startdata))
## also tarsus and weight dont correlate particularly strongly 

plot(tars ~weight,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(6,10),
     ylim=c(17,22),pch=19,col="green4",font.lab=2,
     xlab="Mass (Grams)",ylab="Tarsus length (mm)")
points(tars ~weight,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(tars ~weight,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(tars ~weight,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(tars ~weight,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(tars ~weight,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topleft",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)



summary(lm(condition~zmuscle*subspecies,data=startdata))
## as condition and muscle dont correlate strongly can argue that it isnt a body size correlation between muscle and weight??
## but they do correlate strongly. ##

## correlation of wing and weight ##

boxplot(condition ~muscle,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,4),
     ylim=c(6,11),pch=19,col="green4",font.lab=2,
     xlab="Muscle score",ylab="SMI",add=T)
points(condition ~muscle,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(condition ~muscle,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(condition ~muscle,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(condition ~muscle,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(condition ~muscle,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topleft",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)











################### MODEL 4 ###################

summary(lm(isotope~zpointedness*subspecies,data=startdata))
## There is a signficant interaction between pointedness and subspecies Tristis ##


################### MODEL 5 ###################

summary(lm(pointedness~zconditiontars*subspecies,data=startdata))
anova(lm(pointedness~zconditiontars*subspecies,data=startdata))
## No significance ##






## Model 2- What governs variation in body condition? ##

summary(lm(conditiontars~jdate*wing,data=startdata))
## 2 way interaction: Simplest model and highly significant but does not include subspecies (Condition wing better and condition tars) ##

summary(lm(conditiontars~jdate*wing*subspecies,data=startdata))####################
## 3 way interaction: Inclusion of subspecies makes model less significant but still higly overall significant (Condition wing better and condition tars) ##

summary(lm(condition~jdate*fat*subspecies*pointedness,data=startdata))
## 4 way interaction: Adding of pointedness makes model less significant but still highly significant Using condition tars expalains more than condition wing ##

summary(lm(condition~jdate*fat*subspecies*pointedness*wing,data=startdata))
## 5 way intersaction: highly significant but loosse a lot of DF condition tars expalains more than condition wing ##

## Do models 3 and 4 differ significantly ##

model3<-(lm(condition~jdate*fat,data=startdata))
model4<-(lm(condition~jdate*fat*subspecies,data=startdata))
AIC(model3,model4)
hist(residuals(model3))
hist(residuals(model4))

## models by subspecies are intersting as condition is either governed by fat or wing depending on subspecies ##

summary(lm(conditiontars~jdate*fat,data=startdata[startdata$subspecies=="C",])) ## more significant when controlled by fat ##
summary(lm(conditiontars~jdate*wing,data=startdata[startdata$subspecies=="C",])) ## less significant when controlled by wing ##  Best model not including fat##
summary(lm(conditiontars~jdate*pointedness,data=startdata[startdata$subspecies=="C",])) ## less significant when controlled by pointedness ## 
summary(lm(conditiontars~jdate*wing*pointedness,data=startdata[startdata$subspecies=="C",])) ## less significant when controlled by pointedness ## 

summary(lm(conditiontars~jdate*fat,data=startdata[startdata$subspecies=="A",])) ## insignificant condition not controlled by fat ##
summary(lm(conditiontars~jdate*wing,data=startdata[startdata$subspecies=="A",])) ## insignificant condition not controlled by wing ##
summary(lm(conditiontars~jdate*pointedness,data=startdata[startdata$subspecies=="A",])) ## insignificant not controlled by pointedness ## 


summary(lm(conditiontars~jdate*fat,data=startdata[startdata$subspecies=="T",])) ## sigificant codition is controlled by fat ##
summary(lm(conditiontars~jdate*wing,data=startdata[startdata$subspecies=="T",]))## insignificant condition not controlled by wing ##
summary(lm(conditiontars~jdate*pointedness,data=startdata[startdata$subspecies=="T",])) ## insignificant not controlled by pointedness ## 




## model 3 ##########

anova(lm(zcondition~zisotope*zfat*subspecies,data=startdata))
## not important even though significant as it is just correlation between fat and condition causing relationsip##

anova(lm(zcondition~zisotope*subspecies,data=startdata))
## when fat is removed no longer significant ##



## model 4 ###############

anova(lm(isotope~zpointedness*subspecies,data=startdata))
## There is a signficant interaction between pointedness and subspecies ##


## model 5 ###############

anova(lm(zpointedness~zconditiontars*subspecies,data=startdata))
## No significance ##



## Graphing of anova's ##

## Boxplot for arrival date by subspecies ##
plot(startdata$jdate~startdata$subspecies,cex.lab=1.3,xlab="Subspecies",ylab="Arrival date- Days since 20/09/17")

## Boxplot for arrival date by subspecies ## 
plot(startdata$isotope~startdata$subspecies,cex.lab=1.3,xlab="Subspecies",ylab="Isotopic value (dH2")




## Graphing of model 1A- what governs variation in arrival date ##


# Strips back ggplot graphics, so the plot looks like one created in base R
theme_set(theme_bw())
theme_update(text = element_text(size=12),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank())
## Start of plot ##
##ggplot(data=startdata[startdata$subspecies=="C"],aes(x=jdate,y=isotope,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+##

par(mfrow=c(1,3))


ggplot(startdata,aes(x=jdate,y=isotope,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Capture data (Days since 20/09/2017") +
  ylab(expression(paste (delta^"2"~"H")))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray0", "gray70", "gray35"), name = "Condition",
                     labels = c("High", "Medium", "Low"))


## running of graph with catagoric variable bieng subspecies and independent plots being condition ##

##Low condition ##
startdataLow <- startdata[startdata$conditionscore %in% c("L"), ] 

PLOT1<-ggplot(startdataLow,aes(x=jdate,y=isotope,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Capture data (Days since 20/09/2017") +
  ylab(expression(paste (delta^"2"~"H")))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray0", "gray70", "gray40"), name = "Condition",
                     labels = c("Abutinus", "Colybitta", "Tristis"))

## Medium condition ##
startdataMedium <- startdata[startdata$conditionscore %in% c("M"), ] 

PLOT2<-ggplot(startdataMedium,aes(x=jdate,y=isotope,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Capture data (Days since 20/09/2017") +
  ylab(expression(paste (delta^"2"~"H")))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray0", "gray70", "gray40"), name = "Condition",
                     labels = c("Abutinus", "Colybitta", "Tristis"))

## High condition ##
startdataHigh <- startdata[startdata$conditionscore %in% c("H"), ] 

PLOT3<-ggplot(startdataHigh,aes(x=jdate,y=isotope,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Capture data (Days since 20/09/2017") +
  ylab(expression(paste (delta^"2"~"H")))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray0", "gray70", "gray40"), name = "Condition",
                     labels = c("Abutinus", "Colybitta", "Tristis"))


##Putting 3 plots side by side ##
print(PLOT1, PLOT2,NCOL=2)
install.packages("cowplot")
library(cowplot)
?plot_grid
plot_grid(PLOT1, PLOT2, PLOT3, labels = "AUTO")



xlim=c(0,60) ylim=c(-140-40)




plot(weight ~fat,data=startdata[startdata$subspecies=="C",],cex.lab=1.2,xlim=c(0,5),
     ylim=c(6,10),pch=19,col="green4",font.lab=2,
     xlab="Fat score",ylab="Mass (Grams)")
points(weight ~fat,data=startdata[startdata$subspecies=="T",],pch=19,col="blue")
points(weight ~fat,data=startdata[startdata$subspecies=="A",],pch=19,col="red")
abline(lm(weight ~fat,data=startdata[startdata$subspecies=="C",]),col="green4",lwd=1.5)
abline(lm(weight ~fat,data=startdata[startdata$subspecies=="T",]),col="blue",lwd=1.5)
abline(lm(weight ~fat,data=startdata[startdata$subspecies=="A",]),col="red",lwd=1.5)
legend(x="topleft",legend=c("Collybitta","Tristis","Abutinus"),pch=19,lty =1,col=c("green4","blue","red"),title="Subspecies",cex=0.8)

levels(startdata$subspecies)

PLOT4<-ggplot(startdata,aes(x=fat,y=weight,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Fat score")))+
  ylab("Mass (grams)")+
  scale_y_continuous(breaks=seq(6,11,1))+
  scale_x_continuous(breaks=seq(0,5,1))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray45", "black"), name = "Subspecies",
                     labels = c("Abietinus", "Collybita", "Tristis"))+
  theme(legend.position = "none")








#############  MODEL 1A ##################
levels(startdata$conditionscore)
startdata$conditionscore <- factor(startdata$conditionscore, levels = c("L", "M", "H"))

## All subspecies combines##
PLOT4<-ggplot(startdata,aes(x=isotope,y=jdate,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Condition",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT6<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
## Legend adjustements ##
scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Condition",
                   labels = c("Low", "Medium", "High"))+
     theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT5<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Condition",
                     labels = c("Low", "Medium", "High"))+
  ## Turn off legand ##
    theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT7<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(conditionscore)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o" )))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Condition",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

par(mar=c(6,6,3,3))
plot_grid(PLOT4, PLOT5,PLOT7, labels = "AUTO",ncol = 1, align = 'h')

## Graphing of model 2- wat governs variation in body condition ##










#############  MODEL 1B ##################

levels(startdata$fatclass)
startdata$fatclass <- factor(startdata$fatclass, levels = c("L", "M", "H"))

## All subspecies combines##
PLOT8<-ggplot(startdata,aes(x=isotope,y=jdate,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))#+
  #theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT10<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT9<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  ## Turn off legand ##
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT11<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o" )))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


plot_grid(PLOT8, PLOT9, labels = "AUTO",ncol = 1, align = 'h')

## Graphing of model 2- wat governs variation in body condition ##



#############  MODEL 1C ##################

levels(startdata$pointedclass)
startdata$pointedclass <- factor(startdata$pointedclass, levels = c("L", "M", "H"))

## All subspecies combines##
PLOT8<-ggplot(startdata,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT10<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT9<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  ## Turn off legand ##
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT11<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o" )))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


plot_grid(PLOT8, PLOT9, labels = "AUTO",ncol = 1, align = 'h')

## Graphing of model 2- wat governs variation in body condition ##






#############  MODEL 1C2/d ##################

levels(startdata$size)
startdata$size <- factor(startdata$size, levels = c("S", "M", "L"))

## All subspecies combines##
PLOT8<-ggplot(startdata,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT10<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT9<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  ## Turn off legand ##
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT11<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H%o" )))+
  ylab("Arrival date (days after 28/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


plot_grid(PLOT8, PLOT11,labels = "AUTO",ncol = 1, align = 'h')

## Graphing of model 2- wat governs variation in body condition ##





## model 1 c direction of pointedness trend over time ##

levels(startdata$pointedclass)
startdata$pointedclass <- factor(startdata$pointedclass, levels = c("L", "M", "H"))

## All subspecies combines##
PLOT8<-ggplot(startdataT,aes(x=jdate,y=pointedness,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  scale_y_continuous(breaks=seq(0,0.14,0.02))+
  scale_x_continuous(breaks=seq(0,60,10))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))#+
#theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT10<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT9<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  ## Turn off legand ##
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT11<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(pointedclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H" )))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Pointedness",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


plot_grid(PLOT8, PLOT9, PLOT10, PLOT11, labels = "AUTO")

## Graphing of model 2- wat governs variation in body condition ##








## model 1 d direction of winglength trend over time ##

levels(startdata$size)
startdata$size <- factor(startdata$size, levels = c("S", "M", "L"))

## All subspecies combines##
PLOT8<-ggplot(startdataT,aes(x=jdate,y=pointedness,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  scale_y_continuous(breaks=seq(0,0.14,0.02))+
  scale_x_continuous(breaks=seq(0,60,10))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT10<-ggplot(startdataA,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))#+
  #theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT9<-ggplot(startdataC,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  ## Turn off legand ##
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT11<-ggplot(startdataT,aes(x=isotope,y=jdate,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H" )))+
  ylab("Arrival date (days after 20/09/17)")+
  expand_limits(y=c(0,60))+
  expand_limits(x=c(-130,-40))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_x_continuous(breaks=seq(-120,-40,20))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


plot_grid(PLOT8, PLOT9, PLOT10, PLOT11, labels = "AUTO")

## Graphing of model 2- wat governs variation in body condition ##





#############  MODEL 2a ##################
levels(startdata$fatclass)
startdata$fatclass <- factor(startdata$fatclass, levels = c("L", "M", "H"))

## All subspecies combines##
PLOT12<-ggplot(startdata,aes(x=jdate,y=conditiontars,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 20/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT14<-ggplot(startdataA,aes(x=jdate,y=conditiontars,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 20/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT13<-ggplot(startdataC,aes(x=jdate,y=conditiontars,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 20/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT15<-ggplot(startdataT,aes(x=jdate,y=conditiontars,color=as.factor(fatclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 20/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Fat class",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


plot_grid(PLOT12, PLOT13, PLOT14, PLOT15, labels = "AUTO")

## Graphing of model 2- wat governs variation in body condition ##









#############  MODEL 2b ##################

levels(startdata$size)
startdata$size <- factor(startdata$size, levels = c("S", "M", "L"))


## All subspecies combines##
PLOT16<-ggplot(startdata,aes(x=jdate,y=conditiontars,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT18<-ggplot(startdataA,aes(x=jdate,y=conditiontars,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT17<-ggplot(startdataC,aes(x=jdate,y=conditiontars,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT19<-ggplot(startdataT,aes(x=jdate,y=conditiontars,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))+
  theme(legend.position = "none")


plot_grid(PLOT16, PLOT17, labels = "AUTO",ncol = 1, align = 'h')

## Graphing of model 2- wat governs variation in body condition ##





#############  MODEL 2D ##################

levels(startdata$muscleclassnotwo)
startdata$muscleclass <- factor(startdata$muscleclass, levels = c("L", "M", "H"))


## All subspecies combines##
PLOT20<-ggplot(startdata,aes(x=jdate,y=conditiontars,color=as.factor(musclenotwo)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Muscle score",
                     labels = c("1", "2", "3"))#+
#theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT22<-ggplot(startdataA,aes(x=jdate,y=conditiontars,color=as.factor(musclenotwo)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Muscle score",
                     labels = c("1", "2", "3"))+
theme(legend.position = "none")

startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT21<-ggplot(startdataC,aes(x=jdate,y=conditiontars,color=as.factor(musclenotwo)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Muscle score",
                     labels = c("1", "2", "3"))+
theme(legend.position = "none")


startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT23<-ggplot(startdataT,aes(x=jdate,y=conditiontars,color=as.factor(musclenotwo)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Arrival date (days after 28/09/17)")+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(0,60,10))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(0,60))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Muscle score",
                     labels = c("1", "2", "3"))+
theme(legend.position = "none")


plot_grid(PLOT20, PLOT21, labels = "AUTO",ncol = 1, align = 'h')




#############  MODEL 2F ##################

levels(startdata$subspecies)
startdata$muscleclass <- factor(startdata$muscleclass, levels = c("A", "c", "T"))


## All subspecies combines##
PLOT20<-ggplot(startdata,aes(x=isotope,y=conditiontars,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab(expression(paste ("Feather"~~delta^"2"~"H")))+
  ylab("Body condition (SMI)")+
  scale_y_continuous(breaks=seq(4,12,1))+
  scale_x_continuous(breaks=seq(-140,-40,20))+
  expand_limits(y=c(6,11))+
  expand_limits(x=c(-140,-40))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Subspecies",
                     labels = c("A", "C", "T"))+
theme(legend.position = "none")




#############  MODEL 3A ##################

levels(startdata$subspecies)
startdata$muscleclass <- factor(startdata$muscleclass, levels = c("L", "M", "H"))


## All subspecies combines##
PLOT20<-ggplot(startdata,aes(x=pointedness,y=isotope,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Pointedness")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  scale_y_continuous(breaks=seq(-140,-40,20))+
  scale_x_continuous(breaks=seq(0.02,0.14,0.02))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(0.02,0.14))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("lightblue3","yellow2","firebrick2"), name = "Subspecies",
                     labels = c("Abietinus", "Collybita", "Tristis"))#+
  #theme(legend.position = "none")





#############  MODEL 3B ##################

levels(startdata$weightclass)
startdata$weightclass <- factor(startdata$weightclass, levels = c("L", "M", "H"))


## All subspecies combines##
PLOT20<-ggplot(startdata,aes(x=wing,y=isotope,color=as.factor(weightclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Wing length (mm)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  scale_y_continuous(breaks=seq(-140,-40,20))+
  scale_x_continuous(breaks=seq(55,65,2))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(55,65))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Mass class",
                     labels = c("Low", "Medium", "High"))#+
#theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT22<-ggplot(startdataA,aes(x=wing,y=isotope,color=as.factor(weightclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Wing length (mm)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  scale_y_continuous(breaks=seq(-140,-40,20))+
  scale_x_continuous(breaks=seq(55,65,2))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(55,65))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")


startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT21<-ggplot(startdataC,aes(x=wing,y=isotope,color=as.factor(weightclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Wing length (mm)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  scale_y_continuous(breaks=seq(-140,-40,20))+
  scale_x_continuous(breaks=seq(55,65,2))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(55,65))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")



startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT23<-ggplot(startdataT,aes(x=wing,y=isotope,color=as.factor(weightclass)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Wing length (mm)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H%o")))+
  scale_y_continuous(breaks=seq(-140,-40,20))+
  scale_x_continuous(breaks=seq(55,65,2))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(55,65))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  ## Legend adjustements ##
  scale_color_manual(values = c("slategray2", "royalblue1", "midnightblue"), name = "Wing size",
                     labels = c("Low", "Medium", "High"))+
  theme(legend.position = "none")



plot_grid(PLOT20, PLOT21, PLOT23, labels = "AUTO",ncol = 1, align = 'h')








#############  MODEL 3C ################## DONT USE ##########

levels(startdata$size)
startdata$muscleclass <- factor(startdata$muscleclass, levels = c("L", "M", "H"))


## All subspecies combines##
PLOT20<-ggplot(startdata,aes(x=conditiontars,y=isotope,color=as.factor(subspecies)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Body condition (SMI)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H")))+
  scale_y_continuous(breaks=seq(-140,-40,10))+
  scale_x_continuous(breaks=seq(4,12,1))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(6,11))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Abietinus", "Collybita", "Tristis"))#+
#theme(legend.position = "none")


## running of graph with catagoric being body condition and independent plots subspecies ##

startdataA <- startdata[startdata$subspecies %in% c("A"), ] 

PLOT22<-ggplot(startdataA,aes(x=conditiontars,y=isotope,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Body condition (SMI)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H")))+
  scale_y_continuous(breaks=seq(-140,-40,10))+
  scale_x_continuous(breaks=seq(4,12,1))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(6,11))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))#+
#theme(legend.position = "none")


startdataC <- startdata[startdata$subspecies %in% c("C"), ] 

PLOT21<-ggplot(startdataC,aes(x=conditiontars,y=isotope,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Body condition (SMI)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H")))+
  scale_y_continuous(breaks=seq(-140,-40,10))+
  scale_x_continuous(breaks=seq(4,12,1))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(6,11))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))#+
#theme(legend.position = "none")



startdataT <- startdata[startdata$subspecies %in% c("T"), ] 

PLOT23<-ggplot(startdataT,aes(x=conditiontars,y=isotope,color=as.factor(size)))+geom_point()+geom_smooth(method="lm", se=F)+
  xlab("Body condition (SMI)")+
  ylab(expression(paste ("Feather"~~delta^"2"~"H")))+
  scale_y_continuous(breaks=seq(-140,-40,10))+
  scale_x_continuous(breaks=seq(4,12,1))+
  expand_limits(y=c(-140,-40))+
  expand_limits(x=c(6,11))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))+
  ## Legend adjustements ##
  scale_color_manual(values = c("gray80", "gray40", "black"), name = "Wing size",
                     labels = c("Small", "Medium", "Large"))#+
#theme(legend.position = "none")



plot_grid(PLOT20, PLOT21, PLOT22, PLOT23, labels = "AUTO")




summary(lm(isotope~zconditiontars*zwing*subspecies,data=startdata))
anova(lm(isotope~zconditiontars*zwing*subspecies,data=startdata))






## TIMING AND ORIGIN BY SUBSPECIES ##

########################### subspecies differences in stable isotope results ###############################
par(mar=c(6,6,3,3))
plot((startdata$isotope~startdata$subspecies),whisklty = 7,lwd=1.6, ylim=c(-140,-40),cex.lab=1.2,names=c("Abietinus","Collybita", "Tristis"),las=1, col=(c("lightblue3","yellow2","firebrick2")),xlab="Subspecies",ylab=(expression(paste ("Feather"~~delta^"2"~"H%o" ))))

plot((startdata$jdate~startdata$subspecies),whisklty = 7,lwd=1.6, ylim=c(0,60),cex.lab=1.2,names=c("Abietinus","Collybita", "Tristis"),las=1, col=(c("lightblue3","yellow2","firebrick2")),xlab="Subspecies",font.lab=2, ylab="Capture date (days since 28/09/2017)")


## FAT by period for each subspecies ## 
boxplot(fat~period,data=startdata[startdata$subspecies=="A",])
boxplot(fat~period,data=startdata[startdata$subspecies=="C",])
boxplot(fat~period,data=startdata[startdata$subspecies=="T",])    

## average capture date and fatscore in relation to fat class ##


aggregate(startdata$jdate,list(startdata$fatclass),mean)     
aggregate(startdata$fat,list(startdata$fatclass),mean)  
aggregate(startdata$fat,list(startdata$subspecies),mean) 

## standard diviations ##
aggregate(startdata$jdate,list(startdata$fatclass),sd)    
aggregate(startdata$fat,list(startdata$fatclass),sd) 


## support of model 1B ##

PLOT24<-plot((startdata$fat~startdata$subspecies), whisklty = 7,lwd=1.6, ylim=c(0,5),cex.lab=1.4,names=c("Abietinus","Collybita", "Tristis"),las=1, col=(c("gray")),xlab="Subspecies",ylab="Fat score")

PLOT25<-plot((startdata$condition~startdata$subspecies),whisklty = 7,lwd=1.6, ylim=c(6,11),cex.lab=1.4,names=c("Abietinus","Collybita", "Tristis"),las=1, col=(c("gray")),xlab="Subspecies",font.lab=2, ylab="SMI (Body condition")

plot_grid(PLOT24, PLOT25, labels = "AUTO")


par(mfrow=c(1,1))
par(mar=c(6,6,3,3),cex.axis=1,cex.lab=2)
boxplot((startdataC$isotope~startdataC$period),whisklty = 7,lwd=1.6, ylim=c(-140,-40),cex.lab=1.3,las=1,Xlim=c(1,6),col=(c("yellow2")),xlab="Period",ylab=(expression(paste ("Feather"~~delta^"2"~"H%o"))))
boxplot((startdataAalt$isotope~startdataAalt$period),whisklty = 7,lwd=1.6, ylim=c(-140,-40),cex.lab=1.3,las=1,Xlim=c(1,6),col=(c("lightblue3")),xlab="Period",ylab=(expression(paste ("Feather"~~delta^"2"~"H%o" ))))
boxplot((startdataTalt$isotope~startdataTalt$period),whisklty = 7,lwd=1.6, ylim=c(-140,-40),cex.lab=1.3,las=1,Xlim=c(1,6),col=(c("firebrick2")),xlab="Period",ylab=(expression(paste ("Feather"~~delta^"2"~"H%o" ))))

startdataCalt <- startdata[startdata$subspecies %in% c("C"), ] 
startdataAalt <- startdata[startdata$subspecies %in% c("A"), ]
startdataTalt <- startdata[startdata$subspecies %in% c("T"), ]
  
##library(car)
##install.packages("car")



## variance ratio tests ##

bartlett.test(len ~ interaction(supp,dose), data=ToothGrowth)

bartlett.test(len ~ interaction(supp,dose), data=ToothGrowth)
bartlett.test(isotope ~ subspecies, data = startdata)
bartlett.test(jdate ~ subspecies, data = startdata)
leveneTest(isotope ~ subspecies, data = startdata)
leveneTest(jdate ~ subspecies, data = startdata)
leveneTest(conditiontars ~ subspecies, data = startdata)

bartlett.test(isotope ~ interaction (weight,subspecies), data = startdata)
summary(lm(isotope~zwing*zweight*subspecies,data=startdata))


