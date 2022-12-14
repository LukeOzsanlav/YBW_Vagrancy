## Luke Ozsanlav-Harris
## Created: 15/11/2022

## Preliminary exploration of the stable isotope, morphometric and phenological data from Chiffchaff
## Start re-creating some of the analysis carried out by Robbie that could go into a manuscript

## NOTES** 
## - there are lots of correlation between variables so modelling needs to be done carefully
## - condition that I am currently using is a the residuals or an OLS regression  between wind length and weight

## Model run:
## 1. Isotope ~ Sub Species Group (all)
## 2. Isotope ~ wing + Cap_yday + weight (each)
## 3. Cap_yday ~ wing + isotope + condition (each)

## Results summary:
## H2 istopes for YBW differ from Coll and Abie but are the same as Tris
## Wing length and Isotope negatively correlated in collybita but not any other groups
## Condition and capture day negatively correlated in collybita but not any other groups
## Suggest that Collybita are arriving from a broad front, i.e. east to west front
## No suggestion of this in other species suggestive that variation in H2 is due to on north-south front


## packages required
pacman::p_load(tidyverse, lubridate, data.table, nlme, effects, ltm, ggsignif)




##------------------------------------##
#### 1. Read in stable isotope data ####
##------------------------------------##

## Read in the Chiffchaff data ##

## Read in text file with data
Chiff <- read.table("Data/alldatachiff.txt",header=T)
glimpse(Chiff)

## create a column for year day from the date, easier to work with
Chiff <- Chiff %>% 
          mutate(Cap_yday = yday(dmy(date)))

## Study duration
paste0(min(dmy(Chiff$date)), " to ", max(dmy(Chiff$date)))


## Read in the YBW data ##
YBW <- read_csv("Data/YeBW 2.csv") %>% drop_na(species)
glimpse(YBW)

## select required columns from YBW data set so that it will join to the Chiff data
YBW <- YBW %>% 
       dplyr::select(c(d2H, ring, species, wp, wp.p2, wp.pc, wp.t3, tarsus, SMI.N, 
                       tail, age, wing, weight, fat, muscle, day, pointy)) %>% 
       rename(isotope = d2H, wpp2 = wp.p2, wppc = wp.pc, wpt3 = wp.t3, tars = tarsus, 
              Cap_yday = day, pointedness = pointy, condition = SMI.N) %>% 
       mutate(subspecies = "YBW")
      


## Join the Yellow brow and the chiff data
Iso <- plyr::rbind.fill(Chiff, YBW)




##--------------------------------------##
#### 2. Create some preliminary plots ####
##--------------------------------------##

## Plot the isotopes by subspecies
ggplot() + 
  geom_boxplot(data = Iso, aes(y = isotope, x = subspecies, colour= subspecies)) +
  theme_light()


## Plot the isotopes by arrival date, group by subspecies
ggplot() +
  geom_point(data = Iso, aes(y = isotope, x = dmy(date), group= subspecies, colour= subspecies)) +
  theme_light()

## Plot the isotopes by wing length, group by subspecies
ggplot(data = Iso) +
  geom_point(aes(y = isotope, x = wing, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by wing point, group by subspecies
ggplot(data = Iso) +
  geom_point(aes(y = isotope, x = wp, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by weight, group by subspecies
ggplot(data = Iso) +
  geom_point(aes(y = isotope, x = weight, group= subspecies, colour= subspecies)) +
  theme_light() 

## Plot the isotopes by fat score, group by subspecies
ggplot(data = Iso) +
  geom_point(aes(y = isotope, x = fat, group= subspecies, colour= subspecies)) +
  theme_light() 




##-------------------------------------------------##
#### 3. Make correlation matrix of key variables ####
##-------------------------------------------------##

## lets select some variables that I'm interested in that could be continuous predictors
CorVars <- Iso %>% 
           dplyr::select(wp, tars, tail, wing, weight, fat, Cap_yday, condition) %>% #condition
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
Iso$subspecies <- as.factor(Iso$subspecies)

## run a linear model and then
## all but one of these test is significantly different
anova(lm(Iso$wp~ Iso$subspecies))
anova(lm(Iso$tars~ Iso$subspecies))
anova(lm(Iso$tail~ Iso$subspecies))
anova(lm(Iso$wing~ Iso$subspecies))
anova(lm(Iso$weight~ Iso$subspecies))
anova(lm(Iso$fat~ Iso$subspecies))       
anova(lm(Iso$Cap_yday~ Iso$subspecies)) 




##----------------------------------------##
#### 4. Hydrogen Isotope vs Sub-species ####
##----------------------------------------##

## Model how hydrogen isotope varies across the different sub-species
## Will need to account for heteroscedacisity between groups, Robbie said this was a problem in the thesis
## I can control for this so will model all three sub-species at once


## set variables to the correct class
Iso$subspecies <- as.factor(Iso$subspecies)

## drop rows with missing data
IsoMod1 <- Iso %>% drop_na(isotope, subspecies)

## Run model in nlme
## Weight function allows to account for heteroscad
mod1 <- gls(isotope~ subspecies,
            data=IsoMod1, 
            weights = varIdent(form = ~1|subspecies), 
            method="ML")

mod <- gls(isotope~ subspecies,
            data=IsoMod1, 
            method="ML")

## comparing the two models, adding the weights argument does not really make that much of a difference
## It's close though so will keep it in
anova(mod1, mod)

## Get summary of the model with the weight
## Seems like all the groups are different but Tristis is a lot lower
summary(mod1)
anova(mod1)

## plot the model effects
mod1effects <- predictorEffects(mod1)
plot(mod1effects)



## Make the plot for the paper
## rename the sub-species columns first for plotting
BoxPlot <- Iso %>%                              
                mutate(subspecies = ifelse(subspecies == "C", "P. c. collybita",
                                           ifelse(subspecies == "A", "P. c. abietinus", 
                                                  ifelse(subspecies == "T", "P. c. trisits", "P. inornatus"))))

IsoModBox <- IsoMod1 %>%                              
              mutate(subspecies = ifelse(subspecies == "C", "P. c. collybita",
                                         ifelse(subspecies == "A", "P. c. abietinus", 
                                                ifelse(subspecies == "T", "P. c. trisits", "P. inornatus"))))

## create the plot with significance bars
ggplot(data = BoxPlot, aes(y = isotope, x = subspecies, fill = subspecies)) + 
  geom_jitter(data = IsoModBox, aes(y = isotope, x = subspecies), stroke = 1, alpha = 0.5, colour = "darkgrey", width = 0.1) +
  geom_boxplot(alpha = 0.5, width = 0.4) +
  geom_signif(comparisons = list(c("P. c. collybita", "P. c. trisits")), map_signif_level = TRUE, colour = "black", y_position = -40) +
  geom_signif(comparisons = list(c("P. c. abietinus", "P. c. trisits")), map_signif_level = TRUE, colour = "black", y_position = -35) +
  geom_signif(comparisons = list(c("P. c. abietinus", "P. inornatus")), map_signif_level = TRUE, colour = "black", y_position = -30) +
  geom_signif(comparisons = list(c("P. c. collybita", "P. inornatus")), map_signif_level = TRUE, colour = "black", y_position = -25) +
  theme_light() +
  scale_y_continuous(breaks = seq(-140, -20, by = 20)) +
  scale_fill_manual(values=c("#DDCC77", "#882255", "#88CCEE", "#117733")) +
  ylab("δD") + xlab("Taxonmic Group") +
  theme(legend.position = "blank", axis.text=element_text(size=13), panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "italic"), axis.title=element_text(size=15), panel.grid.major.x = element_blank())

## save the plot
ggsave("Outputs/BoxPlot- Comparison of H2 between groups.png",
       width = 14, height = 14, units = "cm")
  



##-------------------------------------------------##
#### 5. Hydrogen Isotope vs Subspecies variables ####
##-------------------------------------------------##

## Model how hydrogen isotopes vary with arrival date, wing length, wing pointedness and mass
## Will need to run a model for each of these sub-species individually, due to differences in arrival date between groups

## First work out how much data is missing for each of these variables
## There 16 NA values by
## 5 NAs for weight but all in Collybita
## 4 NAs in wing but all in Collybita
## 16 NAs in pointedness across all chiff sub-species, then 12 NAs for YBW (don't think Jake recorded this in all birds, says it was only done in 2/3rds of birds in his thesis)
## 4 NAs in condition with 3 in Collybita and 1 yellow brow
Iso %>% dplyr::select(Cap_yday, wing, pointedness, weight, condition) %>% group_by() %>% summary()

## re-level the subspecies factor (NOT NEEDED CURRENTLY)
# ChiffComp$subspecies <- relevel(ChiffComp$subspecies, ref = "C")

## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
Coll <- Iso %>% filter(subspecies == "C") %>% drop_na(wing, Cap_yday, weight)
Abie <- Iso %>% filter(subspecies == "A") %>% drop_na(wing, Cap_yday, weight)
Tris <- Iso %>% filter(subspecies == "T") %>% drop_na(wing, Cap_yday, weight)
Yellow <- Iso %>% filter(subspecies == "YBW") %>% drop_na(wing, Cap_yday, weight, isotope)



#### 5.1 MODEL: Collybita ####
## Add interactions between the sub species and the other explanatory variables
ModColl <- gls(isotope ~ scale(wing) + scale(Cap_yday) + scale(condition),
               data=Coll, 
               method="ML")

# get model summaries
summary(ModColl) # significant effect of wing length (negative relationship)
anova(ModColl)

# plot the predictor effects
ModColleffects <- predictorEffects(ModColl)
plot(ModColleffects)


## plot of Collybita relationship
## use the effects package to extract the fit for the first variable
divisions <- 200
CollWing <- predictorEffects(ModColl, focal.levels = divisions)
plot(CollWing[1])
effectsColl <- CollWing[1]
fitColl <- as.data.frame(cbind(effectsColl[["wing"]][["fit"]], effectsColl[["wing"]][["lower"]], 
                                 effectsColl[["wing"]][["upper"]], effectsColl[["wing"]][["x"]]))

## change the names to something meaningful
setnames(fitColl, old = c("effectsColl[[\"wing\"]][[\"fit\"]]", "effectsColl[[\"wing\"]][[\"lower\"]]", "effectsColl[[\"wing\"]][[\"upper\"]]"), 
         new = c("fit", "lower", "upper"))

## Now plot using ggplot
ggplot(mapping= aes(x= wing, y = fit)) + 
  geom_point(data= Coll, mapping= aes(x= wing, y = isotope), colour = "#882255", shape = 1, stroke = 1.5) +
  geom_ribbon(data = fitColl, mapping =aes(x= wing, ymin = lower, ymax = upper), 
              alpha = 0.4, colour = NA, fill = "grey")+
  geom_line(data= fitColl, size = 1.25)  +
  xlab("Wing Length/mm") + ylab("Delta H2") +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("Outputs/Collybita winglength vs H2.png",
       width = 18, height = 16, units = "cm")



#### 5.2 MODEL: Abietinus ####
## Add interactions between the sub species and the other explanatory variables
ModAbie <- gls(isotope ~ scale(wing) + scale(Cap_yday) + scale(condition),
               data=Abie, 
               method="ML")

# get model summaries
summary(ModAbie)
anova(ModAbie)

# plot the predictor effects
ModAbieeffects <- predictorEffects(ModAbie)
plot(ModAbieeffects)



#### 5.3 MODEL: Tristis ####
## Add interactions between the sub species and the other explanatory variables
ModTris <- gls(isotope ~ scale(wing) + scale(Cap_yday) + scale(condition),
               data=Tris, 
               method="ML")

# get model summaries
summary(ModTris)
anova(ModTris)

# plot the predictor effects
ModTriseffects <- predictorEffects(ModTris)
plot(ModTriseffects)



#### 5.4 MODEL: Yellow-Brow ####
## Add interactions between the sub species and the other explanatory variables
ModYBW <- gls(isotope ~ scale(wing) + scale(Cap_yday) + scale(condition),
               data=Yellow, 
               method="ML")

# get model summaries
summary(ModYBW) # non-significant effect of wing length p =0.098 (negative relationship)
anova(ModYBW)

# plot the predictor effects
ModYBWeffects <- predictorEffects(ModYBW)
plot(ModYBWeffects)

## plot of Collybita relationship
## use the effects package to extract the fit for the first variable
divisions <- 200
YBWWing <- predictorEffects(ModYBW, focal.levels = divisions)
plot(YBWWing[1])
effectsYBW <- YBWWing[1]
fitYBW <- as.data.frame(cbind(effectsYBW[["wing"]][["fit"]], effectsYBW[["wing"]][["lower"]], 
                               effectsYBW[["wing"]][["upper"]], effectsYBW[["wing"]][["x"]]))

## change the names to something meaningful
setnames(fitYBW, old = c("effectsYBW[[\"wing\"]][[\"fit\"]]", "effectsYBW[[\"wing\"]][[\"lower\"]]", "effectsYBW[[\"wing\"]][[\"upper\"]]"), 
         new = c("fit", "lower", "upper"))

## Now plot using ggplot
ggplot(mapping= aes(x= wing, y = fit)) + 
  geom_point(data= Yellow, mapping= aes(x= wing, y = isotope), colour = "#117733", shape = 1, stroke = 1.5) +
  geom_ribbon(data = fitYBW, mapping =aes(x= wing, ymin = lower, ymax = upper), 
              alpha = 0.4, colour = NA, fill = "grey")+
  geom_line(data= fitYBW, size = 1.25)  +
  xlab("Wing Length/mm") + ylab("Delta H2") +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("Outputs/YBW winglength vs H2.png",
       width = 18, height = 16, units = "cm")





##---------------------------------------------##
#### 6. Arrival date vs Subspecies variables ####
##---------------------------------------------##

## Model how capture date it related to various sub-species variables
## Workflow will be the same as the above, modeling the three sub-species separately


## First work out how much data is missing for each of these variables
## There 16 NA values by
## 4 NAs in wing but all in Collybita
## 4 NAs in fat across mainly Collybita
Iso %>% dplyr::select(Cap_yday, wing, fat, condition) %>% group_by() %>% summary()


## re-level the subspecies factor (NOT NEEDED CURRENTLY)
# ChiffComp$subspecies <- relevel(ChiffComp$subspecies, ref = "C")


## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
Coll2 <- Iso %>% filter(subspecies == "C") %>% drop_na(Cap_yday, wing, fat, condition)
Abie2 <- Iso %>% filter(subspecies == "A") %>% drop_na(Cap_yday, wing, fat, condition)
Tris2 <- Iso %>% filter(subspecies == "T") %>% drop_na(Cap_yday, wing, fat, condition)
YBW2 <- Iso %>% filter(subspecies == "YBW") %>% drop_na(Cap_yday, wing, fat, condition, isotope)



#### 6.1 MODEL: Collybita ####
## Add interactions between the sub species and the other explanatory variables
ModColl2 <- gls(Cap_yday ~ scale(wing) + scale(condition),
                data=Coll2, 
                method="ML")

# get model summaries
summary(ModColl2) # significant effect of wing length (negative relationship)
anova(ModColl2)

# plot the predictor effects
ModColl2effects <- predictorEffects(ModColl2)
plot(ModColl2effects)

## plot of Collybita relationship
## use the effects package to extract the fit for the first variable
divisions <- 200
CollWing <- predictorEffects(ModColl2, focal.levels = divisions)
plot(CollWing[2])
effectsColl <- CollWing[2]
fitColl <- as.data.frame(cbind(effectsColl[["condition"]][["fit"]], effectsColl[["condition"]][["lower"]], 
                               effectsColl[["condition"]][["upper"]], effectsColl[["condition"]][["x"]]))

## change the names to something meaningful
setnames(fitColl, old = c("effectsColl[[\"condition\"]][[\"fit\"]]", "effectsColl[[\"condition\"]][[\"lower\"]]", "effectsColl[[\"condition\"]][[\"upper\"]]"), 
         new = c("fit", "lower", "upper"))

## Now plot using ggplot
ggplot(mapping= aes(x= condition, y = fit)) + 
  geom_point(data= Coll2, mapping= aes(x= condition, y = Cap_yday), colour = "#882255", shape = 1, stroke = 1.5) +
  geom_ribbon(data = fitColl, mapping =aes(x= condition, ymin = lower, ymax = upper), 
              alpha = 0.4, colour = NA, fill = "grey")+
  geom_line(data= fitColl, size = 1.25)  +
  xlab("Body Condition") + ylab("Arrival Date/day of year") +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("Outputs/Collybita condition vs arrival date.png",
       width = 18, height = 16, units = "cm")




#### 6.2 MODEL: Abietinus ####
## Add interactions between the sub species and the other explanatory variables
ModAbie2 <- gls(Cap_yday ~ scale(wing) + scale(condition),
                data=Abie2, 
                method="ML")

# get model summaries
summary(ModAbie2)
anova(ModAbie2)

# plot the predictor effects
ModAbie2effects <- predictorEffects(ModAbie2)
plot(ModAbie2effects)



#### 6.3 MODEL: Tristis ####
## Add interactions between the sub species and the other explanatory variables
ModTris2 <- gls(Cap_yday ~ scale(wing) + scale(condition),
                data=Tris2, 
                method="ML")

# get model summaries
summary(ModTris2)
anova(ModTris2)

# plot the predictor effects
ModTris2effects <- predictorEffects(ModTris2)
plot(ModTris2effects)



#### 6.4 MODEL: Yellow_brow ####
## Add interactions between the sub species and the other explanatory variables
ModYBW2 <- gls(Cap_yday ~ scale(wing) + scale(condition),
               data=YBW2, 
               method="ML")

# get model summaries
summary(ModYBW2) # significant effect of wing length (negative relationship)
anova(ModYBW2)

# plot the predictor effects
ModYBW2effects <- predictorEffects(ModYBW2)
plot(ModYBW2effects)


