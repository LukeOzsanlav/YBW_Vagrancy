## Luke Ozsanlav-Harris
## Created: 15/11/2022

## Preliminary exploration of the stable isotope, morphometric and phenological data from Chiffchaff
## Start re-creating some of the analysis carried out by Robbie that could go into a manuscript

## NOTE** there are lots of correlation between variables so modelling needs to be done carefully

## packages required
pacman::p_load(tidyverse, lubridate, data.table, nlme, effects, ltm, ggsignif)




##------------------------------------##
#### 1. Read in stable isotope data ####
##------------------------------------##

## Read in text file with data
Chiff <- read.table("Data/alldatachiff.txt",header=T)
glimpse(Chiff)

## create a column for year day from the date, easier to work with
Chiff <- Chiff %>% 
          mutate(Cap_yday = yday(dmy(date)))

## Study duration
paste0(min(dmy(Chiff$date)), " to ", max(dmy(Chiff$date)))




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
           dplyr::select(wp, tars, tail, wing, weight, fat, Cap_yday, condition) %>% 
           drop_na()

cor(CorVars) # get matrix of correlation scores for the variables

## There are some high correlations but not too many
## basically any variables that describe the size of birds are correlated

## Wing & Tarsus
## Wing & Tail
## Tail & Tarsus
## Weight and fat
## Condition and far


## Now check for correlation between Sub species and the continuous predictors
## set variables to the correct class first
Chiff$subspecies <- as.factor(Chiff$subspecies)

## run a linear model and then
anova(lm(Chiff$wp~ Chiff$subspecies))
anova(lm(Chiff$tars~ Chiff$subspecies))
anova(lm(Chiff$tail~ Chiff$subspecies))
anova(lm(Chiff$wing~ Chiff$subspecies))
anova(lm(Chiff$weight~ Chiff$subspecies))
anova(lm(Chiff$fat~ Chiff$subspecies))      # **Significant: fat*Subspecies
anova(lm(Chiff$Cap_yday~ Chiff$subspecies)) # **Significant: Capture Day*sub-species




##----------------------------------------##
#### 4. Hydrogen Isotope vs Sub-species ####
##----------------------------------------##

## Model how hydrogen isotope varies across the different sub-species
## Will need to account for heteroscedacisity between groups, Robbie said this was a problem in the thesis
## I can control for this so will model all three sub-species at once


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

## comparing the two models, adding the weights argument does not really make that much of a difference
## It's close though so will keep it in
anova(mod1, mod)

## Get summary of the model with the weight
## Seems like all the groups are different but Tristis is a lot lower
summary(mod1)

## plot the model effects
mod1effects <- predictorEffects(mod1)
plot(mod1effects)



## Make the plot for the paper
## rename the sub-species columns first for plotting
ChiffBoxPlot <- Chiff %>%                              
                mutate(subspecies = ifelse(subspecies == "C", "Collybita",
                                           ifelse(subspecies == "A", "Abietinus", "Trisits")))

## create the plot with significance bars
ggplot(data = ChiffBoxPlot, aes(y = isotope, x = subspecies, fill = subspecies)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("Collybita", "Trisits")), map_signif_level = TRUE, colour = "black") +
  geom_signif(comparisons = list(c("Abietinus", "Trisits")), map_signif_level = TRUE, colour = "black", y_position = -35) +
  theme_light() +
  scale_fill_manual(values=c("#009E73", "#0072B2", "#D55E00")) +
  ylab("δ2H") + xlab("Species Group") +
  theme(legend.position = "blank", axis.text=element_text(size=13), axis.title=element_text(size=15))
  


##-------------------------------------------------##
#### 5. Hydrogen Isotope vs Subspecies variables ####
##-------------------------------------------------##

## Model how hydrogen isotopes vary with arrival date, wing length, wing pointedness and mass
## Will need to run a model for each of these sub-species individually, due to differences in arrival date between groups


## First work out how much data is missing for each of these variables
## There 16 NA values by
## 5 NAs for weight but all in Collybita
## 4 NAs in wing but all in Collybita
## 16 NAs in poitedness across all sub-species
Chiff %>% dplyr::select(Cap_yday, wing, pointedness, weight) %>% group_by() %>% summary()


## re-level the subspecies factor (NOT NEEDED CURRENTLY)
# ChiffComp$subspecies <- relevel(ChiffComp$subspecies, ref = "C")


## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
Coll <- Chiff %>% filter(subspecies == "C") %>% drop_na(wing, Cap_yday, weight)
Abie <- Chiff %>% filter(subspecies == "A") %>% drop_na(wing, Cap_yday, weight)
Tris <- Chiff %>% filter(subspecies == "T") %>% drop_na(wing, Cap_yday, weight)


## **MODEL: Collybita ##
## Add interactions between the sub species and the other explanatory variables
ModColl <- gls(isotope ~ wing + Cap_yday + weight,
               data=Coll, 
               method="ML")

# get model summaries
summary(ModColl) # significant effect of wing length (negative relationship)
anova(ModColl)

# plot the predictor effects
ModColleffects <- predictorEffects(ModColl)
plot(ModColleffects)



## **MODEL: Abietinus ##
## Add interactions between the sub species and the other explanatory variables
ModAbie <- gls(isotope ~ wing + Cap_yday + weight,
               data=Abie, 
               method="ML")

# get model summaries
summary(ModAbie)
anova(ModAbie)

# plot the predictor effects
ModAbieeffects <- predictorEffects(ModAbie)
plot(ModAbieeffects)



## **MODEL: Tristis ##
## Add interactions between the sub species and the other explanatory variables
ModTris <- gls(isotope ~ wing + Cap_yday + weight,
               data=Tris, 
               method="ML")

# get model summaries
summary(ModTris)
anova(ModTris)

# plot the predictor effects
ModTriseffects <- predictorEffects(ModTris)
plot(ModTriseffects)




##---------------------------------------------##
#### 5. Arrival date vs Subspecies variables ####
##---------------------------------------------##

## **** Need to work out how condition is calculated, does not specifically say in the thesis

## Model how capture date it related to various sub-species variables
## Workflow will be the same as the above, modeling the three sub-species separately


## First work out how much data is missing for each of these variables
## There 16 NA values by
## 5 NAs for weight but all in Collybita
## 4 NAs in wing but all in Collybita
## 16 NAs in poitedness across all sub-species
Chiff %>% dplyr::select(Cap_yday, wing, fat, condition) %>% group_by() %>% summary()


## re-level the subspecies factor (NOT NEEDED CURRENTLY)
# ChiffComp$subspecies <- relevel(ChiffComp$subspecies, ref = "C")


## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
Coll2 <- Chiff %>% filter(subspecies == "C") %>% drop_na(Cap_yday, wing, fat, condition)
Abie2 <- Chiff %>% filter(subspecies == "A") %>% drop_na(Cap_yday, wing, fat, condition)
Tris2 <- Chiff %>% filter(subspecies == "T") %>% drop_na(Cap_yday, wing, fat, condition)


## **MODEL: Collybita ##
## Add interactions between the sub species and the other explanatory variables
ModColl <- gls(isotope ~ wing + Cap_yday + condition,
               data=Coll, 
               method="ML")

# get model summaries
summary(ModColl) # significant effect of wing length (negative relationship)
anova(ModColl)

# plot the predictor effects
ModColleffects <- predictorEffects(ModColl)
plot(ModColleffects)



## **MODEL: Abietinus ##
## Add interactions between the sub species and the other explanatory variables
ModAbie <- gls(isotope ~ wing + Cap_yday + weight,
               data=Abie, 
               method="ML")

# get model summaries
summary(ModAbie)
anova(ModAbie)

# plot the predictor effects
ModAbieeffects <- predictorEffects(ModAbie)
plot(ModAbieeffects)



## **MODEL: Tristis ##
## Add interactions between the sub species and the other explanatory variables
ModTris <- gls(isotope ~ wing + Cap_yday + weight,
               data=Tris, 
               method="ML")

# get model summaries
summary(ModTris)
anova(ModTris)

# plot the predictor effects
ModTriseffects <- predictorEffects(ModTris)
plot(ModTriseffects)




