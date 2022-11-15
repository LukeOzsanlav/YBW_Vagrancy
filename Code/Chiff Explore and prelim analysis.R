## Luke Ozsanlav-Harris
## Created: 15/11/2022

## Preliminary exploration of the stable isotope, morphometric and phenological data from Chiffchaff
## Start re-creating some of the analysis carried out by Robbie that could go into a manuscript

## packages required
pacman::p_load(tidyverse, lubridate, data.table, nlme, effects, ltm)




##------------------------------------##
#### 1. Read in stable Isotope data ####
##------------------------------------##

## Read in text file with data
Chiff <- read.table("Data/alldatachiff.txt",header=T)
glimpse(Chiff)

## create a column for year day from the date, easuer to work with
Chiff <- Chiff %>% 
          mutate(Cap_yday = yday(dmy(date)))




##--------------------------------------##
#### 2. Create some preliminary plots ####
##--------------------------------------##

## Plot the isotopes by subspecies
ggplot() + 
  geom_boxplot(data = Chiff, aes(y = isotope, x = subspecies, colour= subspecies)) +
  theme_light()


## Plot the isotopes by arrival date, group by subspecies
ggplot() +
  geom_point(data = Chiff, aes(y = isotope, x = dmy(date), group= subspecies, colour= subspecies)) +
  theme_light()

## Plot the isotopes by wing length, group by subspecies
ggplot(data = Chiff) +
  geom_point(aes(y = isotope, x = wing, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by wing point, group by subspecies
ggplot(data = Chiff) +
  geom_point(aes(y = isotope, x = wp, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by weight, group by subspecies
ggplot(data = Chiff) +
  geom_point(aes(y = isotope, x = weight, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by fat score, group by subspecies
ggplot(data = Chiff) +
  geom_point(aes(y = isotope, x = fat, group= subspecies, colour= subspecies)) +
  theme_light() 




##-------------------------------------------------##
#### 3. Make correlation matrix of key variables ####
##-------------------------------------------------##


## lets select some variables that I'm interested in that could be continuous predictors
CorVars <- Chiff %>% 
           select(wp, tars, tail, wing, weight, fat, Cap_yday) %>% 
           drop_na()

cor(CorVars) # get matrix of correlation scores for the variables

## There are some high correlations but not too many
## basically any variables that describe the size of birds are correlated

## Wing & Tarsus
## Wing & Tail
## Tail & Tarsus
## Weight and fat


## Now check for correlation between Sub species and the continuous predictors
## set variables to the correct class first
Chiff$subspecies <- as.factor(Chiff$subspecies)
#wp, tars, tail, wing, weight, fat, Cap_yday
biserial.cor(Chiff$wp, Chiff$subspecies)
biserial.cor(Chiff$tars, Chiff$subspecies)
biserial.cor(Chiff$tail, Chiff$subspecies)
biserial.cor(Chiff$wing, Chiff$subspecies)
biserial.cor(Chiff$weight, Chiff$subspecies)
biserial.cor(Chiff$fat, Chiff$subspecies)
biserial.cor(Chiff$Cap_yday, Chiff$subspecies)



##----------------------------------------##
#### 4. Hydrogen Isotope vs Sub-species ####
##----------------------------------------##


## Model how hydrogen isotope varies across the different sub-species
## Will need to account for heterscedacisity between groups, Robbie siad this was a problem in the thesis


## set variables to the correct class
Chiff$subspecies <- as.factor(Chiff$subspecies)

## Run model in nlme
## Weight function allows to account for heteroscad
mod1 <- gls(isotope~ subspecies,
            data=Chiff, 
            weights = varIdent(form = ~1|subspecies), 
            method="ML")

mod <- gls(isotope~ subspecies,
            data=Chiff, 
            #weights = varIdent(form = ~1|subspecies), 
            method="ML")

## comparing the two models, adding the weights agruement doesnt really make that much of a difference
anova(mod1, mod)

## Get sumamry of the model with the weight
## Seems like all the groups are different but Tristis is a lot lower
summary(mod1)




##----------------------------------------##
#### 5. Hydrogen Isotope vs Sub-species ####
##----------------------------------------##

## Increase the model complexity

## drop the rows which do not have data for some of the variables
ChiffComp <- Chiff %>% drop_na(wing, Cap_yday)

## Add interactions between the sub species and the other explanatory variables
modcomp <- gls(isotope~ subspecies*Cap_yday + subspecies*wing,
            data=ChiffComp, 
            weights = varIdent(form = ~1|subspecies), 
            method="ML")

summary(modcomp)


## Lots of the variables that Robbie used are correlated, i.e. the arrival dates differ between species
## So the can not go into the same model. 
## Feels like Robbie's approach might not be that bad
## To start with I could just initially model H2 ~ sup-species
## I could then model each sub-species separately but just have model for each
## I.e. H2 (Trisits only) ~  wing*arrival date + SMI
## This way i could get away with one big model at the start and then three sperate model to explan variation in H2. 











