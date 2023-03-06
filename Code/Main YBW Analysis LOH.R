## Luke Ozsanlav-Harris
## Created: 15/11/2022

## Analysis of the stable isotope, morphometric and phenological data from Chiffchaff and YBW


## NOTES** 
## - there are lots of correlation between variables so modelling needs to be done carefully
## - condition that I am currently using is a the residuals or an OLS regression  between wind length and weight


## Models run:
## 1. Isotope ~ Sub Species Group (all)
## 2. Isotope ~ wing + Cap_yday (each)
## 3. Cap_yday ~ wing (each)


## packages required
pacman::p_load(tidyverse, lubridate, data.table, nlme, effects, ltm, ggsignif, car, outliers)




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




##--------------------------##
#### 4. Test for outliers ####
##--------------------------##

## **Removed these test in current draft of MS**

## Identify any potential outliers
Aeb <- Iso %>% filter(subspecies == "A"); grubbs.test(Aeb$isotope, opposite = TRUE); grubbs.test(Aeb$isotope, opposite = FALSE)
Tris <- Iso %>% filter(subspecies == "T"); grubbs.test(Tris$isotope, opposite = TRUE); grubbs.test(Tris$isotope, opposite = FALSE)
Col <- Iso %>% filter(subspecies == "C"); grubbs.test(Col$isotope, opposite = TRUE); grubbs.test(Col$isotope, opposite = FALSE)
YBW <- Iso %>% filter(subspecies == "YBW"); grubbs.test(YBW$isotope, opposite = TRUE); grubbs.test(YBW$isotope, opposite = FALSE)

## three tests for homogenity of variance between groups, none significant but need post hoc tests
# leveneTest(lm(isotope~ subspecies, data= Iso))
# bartlett.test(isotope~ subspecies, data= Iso)
# fligner.test(isotope~ subspecies, data= Iso)




##----------------------------------------##
#### 5. Hydrogen Isotope vs Sub-species ####
##----------------------------------------##

## Model how hydrogen isotope varies across the different sub-species
## Will need to account for heteroscedacisity between groups, Robbie said this was a problem in the thesis
## I can control for this so will model all three sub-species at once

## set variables to the correct class
Iso$subspecies <- as.factor(Iso$subspecies)

## drop rows with missing data
IsoMod1 <- Iso %>% drop_na(isotope, subspecies)

## remove the outlier
# IsoMod1 <- IsoMod1 %>% filter(!(subspecies == "A" & isotope < -90))


## Run model in nlme
## Weight function allows to account for heteroscad
mod1 <- gls(isotope~ subspecies,
            data=IsoMod1, 
            weights = varIdent(form = ~1|subspecies), 
            method="ML")

mod <- gls(isotope~ subspecies,
            data=IsoMod1, 
            method="ML")

## comparing the two models, adding the weights argument makes a difference with the outlier removed so will keep it in for now
anova(mod1, mod)

## Get summary of the model with the weight, seems like all the groups are different but Tristis is a lot lower
summary(mod1)
anova(mod1)

## plot the model effects
mod1effects <- predictorEffects(mod1)
# plot(mod1effects)



## Make the plot for the paper
## rename the sub-species columns first for plotting
BoxPlot <- Iso %>%                              
                mutate(subspecies = ifelse(subspecies == "C", "P. c. collybita",
                                           ifelse(subspecies == "A", "Intergrades", 
                                                  ifelse(subspecies == "T", "P. c. trisits", "P. inornatus"))))

IsoModBox <- IsoMod1 %>%                              
              mutate(subspecies = ifelse(subspecies == "C", "P. c. collybita",
                                         ifelse(subspecies == "A", "Intergrades", 
                                                ifelse(subspecies == "T", "P. c. trisits", "P. inornatus"))))

## create the plot with significance bars
bp1 <- ggplot(data = BoxPlot, aes(y = isotope, x = subspecies, fill = subspecies)) + 
  geom_jitter(data = IsoModBox, aes(y = isotope, x = subspecies), stroke = 1, alpha = 0.5, colour = "darkgrey", width = 0.1) +
  geom_boxplot(alpha = 0.5, width = 0.4) +
  geom_signif(comparisons = list(c("P. c. collybita", "P. c. trisits")), map_signif_level = TRUE, colour = "black", y_position = -40) +
  geom_signif(comparisons = list(c("Intergrades", "P. c. trisits")), map_signif_level = TRUE, colour = "black", y_position = -35) +
  geom_signif(comparisons = list(c("Intergrades", "P. inornatus")), map_signif_level = TRUE, colour = "black", y_position = -30) +
  geom_signif(comparisons = list(c("P. c. collybita", "P. inornatus")), map_signif_level = TRUE, colour = "black", y_position = -25) +
  theme_light() +
  scale_y_continuous(breaks = seq(-140, -20, by = 20)) +
  scale_fill_manual(values=c("#882255", "#6f9969", "#efc86e", "#808fe1")) +
  ylab(expression(delta^2*H*"  "*("‰"))) + xlab("Taxonmic Group") +
  theme(legend.position = "blank", axis.text=element_text(size=11), panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "italic"), axis.title=element_text(size=13), panel.grid.major.x = element_blank())

## save the plot
ggsave(plot = bp1, filename =  "Outputs/BoxPlot- Comparison of H2 between groups.png",
       width = 14, height = 14, units = "cm")



##----------------------------------##
#### 5. Hydrogen Isotope variance ####
##----------------------------------##  

## filter the data if needed to remove the outlier
IsoMod2 <- IsoMod1
# IsoMod2 <- IsoMod1 %>% filter(!(subspecies == "A" & isotope < -90)) # double check outlier is removed


## Try just comparing the aubetinus to the other groups
## Not significant with outlier but significant with outlier removed
IsoMod3 <- IsoMod2 %>% filter(subspecies == "YBW" | subspecies == "T")
leveneTest(lm(isotope~ subspecies, data= IsoMod3))
IsoMod4 <- IsoMod2 %>% filter(subspecies == "YBW" | subspecies == "C")
leveneTest(lm(isotope~ subspecies, data= IsoMod4))
IsoMod5 <- IsoMod2 %>% filter(subspecies == "YBW" | subspecies == "A")
leveneTest(lm(isotope~ subspecies, data= IsoMod5))


## **Alternative way** Compare all groups at once which we aren't interested in
## use Anova and Tukeys test to do post hoc comparison of variance
# IsoMod2 <- IsoMod2 %>% 
#         group_by(subspecies) %>% 
#         mutate(iso_med = median(isotope, na.rm=TRUE)) %>% 
#         ungroup()
# 
# IsoMod2$iso_med_res <- abs(IsoMod2$isotope - IsoMod2$iso_med)
# 
# # Then we run an ANOVA, and post-hoc test
# levene.dat.aov <- aov(iso_med_res ~ subspecies, IsoMod2)
# summary(levene.dat.aov)
# TukeyHSD(levene.dat.aov)


## Make summary of the data
Data_Sum <- IsoMod2 %>% 
            group_by(subspecies) %>% 
            summarise(meaniso = mean(isotope),
                      sd = sd(isotope))




##------------------------------------------##
#### 6. Hydrogen Isotope vs Morphometrics ####
##------------------------------------------##

## Model how hydrogen isotopes vary with arrival date, wing length, wing pointedness and mass
## Will need to run a model for each of these sub-species individually, due to differences in arrival date between groups

## First work out how much data is missing for each of these variables
## There 16 NA values by
## 5 NAs for weight but all in Collybita
## 4 NAs in wing but all in Collybita
## 16 NAs in pointedness across all chiff sub-species, then 12 NAs for YBW (don't think Jake recorded this in all birds, says it was only done in 2/3rds of birds in his thesis)
## 4 NAs in condition with 3 in Collybita and 1 yellow brow
Iso %>% dplyr::select(Cap_yday, wing, pointedness, weight, condition) %>% group_by() %>% summary()


## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
# Iso <- Iso %>% filter(!(subspecies == "A" & isotope < -90)) # drop outlier
Coll <- Iso %>% filter(subspecies == "C") %>% drop_na(wing, Cap_yday, isotope)
Abie <- Iso %>% filter(subspecies == "A") %>% drop_na(wing, Cap_yday, isotope)
Tris <- Iso %>% filter(subspecies == "T") %>% drop_na(wing, Cap_yday, isotope)
Yellow <- Iso %>% filter(subspecies == "YBW") %>% drop_na(wing, Cap_yday, isotope)



#### 6.1 MODEL: Collybita ####
## Add interactions between the sub species and the other explanatory variables
ModColl <- gls(isotope ~ scale(wing) + scale(Cap_yday),
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
C1 <- ggplot(mapping= aes(x= wing, y = fit)) + 
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



#### 6.2 MODEL: Abietinus ####
## Add interactions between the sub species and the other explanatory variables
ModAbie <- gls(isotope ~ scale(wing) + scale(Cap_yday),
               data=Abie, 
               method="ML")

# get model summaries
summary(ModAbie)
anova(ModAbie)

# plot the predictor effects
ModAbieeffects <- predictorEffects(ModAbie)
plot(ModAbieeffects)

## plot of Abietinus relationship
## use the effects package to extract the fit for the first variable
divisions <- 200
AebWing <- predictorEffects(ModAbie, focal.levels = divisions)
plot(AebWing[1])
effectsAeb <- AebWing[1]
fitAeb <- as.data.frame(cbind(effectsAeb[["wing"]][["fit"]], effectsAeb[["wing"]][["lower"]], 
                               effectsAeb[["wing"]][["upper"]], effectsAeb[["wing"]][["x"]]))

## change the names to something meaningful
setnames(fitAeb, old = c("effectsAeb[[\"wing\"]][[\"fit\"]]", "effectsAeb[[\"wing\"]][[\"lower\"]]", "effectsAeb[[\"wing\"]][[\"upper\"]]"), 
         new = c("fit", "lower", "upper"))



#### 6.3 MODEL: Tristis ####
## Add interactions between the sub species and the other explanatory variables
ModTris <- gls(isotope ~ scale(wing) + scale(Cap_yday),
               data=Tris, 
               method="ML")

# get model summaries
summary(ModTris)
anova(ModTris)

# plot the predictor effects
ModTriseffects <- predictorEffects(ModTris)
plot(ModTriseffects)

## plot of Tristis relationship
## use the effects package to extract the fit for the first variable
divisions <- 200
TrisWing <- predictorEffects(ModTris, focal.levels = divisions)
plot(TrisWing[1])
effectsTris <- TrisWing[1]
fitTris <- as.data.frame(cbind(effectsTris[["wing"]][["fit"]], effectsTris[["wing"]][["lower"]], 
                              effectsTris[["wing"]][["upper"]], effectsTris[["wing"]][["x"]]))

## change the names to something meaningful
setnames(fitTris, old = c("effectsTris[[\"wing\"]][[\"fit\"]]", "effectsTris[[\"wing\"]][[\"lower\"]]", "effectsTris[[\"wing\"]][[\"upper\"]]"), 
         new = c("fit", "lower", "upper"))




#### 6.4 MODEL: Yellow-Brow ####
## Add interactions between the sub species and the other explanatory variables
ModYBW <- gls(isotope ~ scale(wing) + scale(Cap_yday),
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
YB1 <- ggplot(mapping= aes(x= wing, y = fit)) + 
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



#### 6.5 PLOT: All Groups ####

## Bind the data used in the models
PlotWing <- rbind(Coll, Yellow, Tris, Abie)
table(PlotWing$subspecies)

## bind the model effects data 
fitYBW$subspecies <- "YBW"; fitTris$subspecies <- "T"
fitColl$subspecies <- "C"; fitAeb$subspecies <- "A"
PlotFits <- rbind(fitYBW, fitTris, fitColl, fitAeb)

## plot all of the data
GR1 <- ggplot(mapping= aes(x= wing, y = fit, group = subspecies, colour = subspecies)) + 
  geom_point(data= PlotWing, mapping= aes(x= wing, y = isotope, group = subspecies, colour = subspecies), shape = 1, stroke = 1.5, alpha = 0.5) +
  geom_ribbon(data = PlotFits, mapping =aes(x= wing, ymin = lower, ymax = upper, group = subspecies, colour = subspecies), 
              alpha = 0.3, colour = NA, fill = "grey") +
  geom_line(data= PlotFits, size = 1.25)  +
  xlab("Wing Length/mm") + ylab(expression(delta^2*H*"  "*("‰"))) + labs(colour = "Taxonomic Group") +
  scale_colour_manual(values=c("#882255", "#6f9969", "#efc86e", "#808fe1"), 
                      labels=c("Intergrades", "P. c. collybita", "P. c. trisits", "P. inornatus")) +
  annotate("text", x = 54, y = -59.5, colour = "#6f9969", size = 14, label = "*") +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        legend.text=element_text(size=14, face="italic"),
        legend.title=element_text(size=14),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

##save the output for the MS
ggsave(plot = GR1, 
       filename = "Outputs/All winglength vs H2.png",
       width = 22, height = 18, units = "cm")




##--------------------------------------##
#### 7. Arrival date vs Morphometrics ####
##--------------------------------------##

## Model how capture date it related to various sub-species variables
## Workflow will be the same as the above, modeling the three sub-species separately

## First work out how much data is missing for each of these variables
## There 16 NA values by
## 4 NAs in wing but all in Collybita
## 4 NAs in fat across mainly Collybita
Iso %>% dplyr::select(Cap_yday, wing, fat, condition) %>% group_by() %>% summary()


## Now create three data sets, one for each subspecies
## dropping the rows with missing data for each along the way
# aIso <- Iso %>% filter(!(subspecies == "A" & isotope < -90)) # drop outlier
Coll2 <- Iso %>% filter(subspecies == "C") %>% drop_na(Cap_yday, wing, fat, isotope)
Abie2 <- Iso %>% filter(subspecies == "A") %>% drop_na(Cap_yday, wing, fat, isotope)
Tris2 <- Iso %>% filter(subspecies == "T") %>% drop_na(Cap_yday, wing, fat, isotope)
YBW2 <- Iso %>% filter(subspecies == "YBW") %>% drop_na(Cap_yday, wing, fat, isotope)



#### 7.1 MODEL: Collybita ####
## Add interactions between the sub species and the other explanatory variables
ModColl2 <- gls(Cap_yday ~ scale(wing),
                data=Coll2, 
                method="ML")

# get model summaries
summary(ModColl2) # significant effect of wing length (negative relationship)
anova(ModColl2)

# plot the predictor effects
ModColl2effects <- predictorEffects(ModColl2)
plot(ModColl2effects)

# ## plot of Collybita relationship
# ## use the effects package to extract the fit for the first variable
# divisions <- 200
# CollCond <- predictorEffects(ModColl2, focal.levels = divisions)
# plot(CollCond[2])
# effectsColl2 <- CollCond[2]
# fitColl2 <- as.data.frame(cbind(effectsColl2[["condition"]][["fit"]], effectsColl2[["condition"]][["lower"]], 
#                                effectsColl2[["condition"]][["upper"]], effectsColl2[["condition"]][["x"]]))
# 
# ## change the names to something meaningful
# setnames(fitColl2, old = c("effectsColl2[[\"condition\"]][[\"fit\"]]", "effectsColl2[[\"condition\"]][[\"lower\"]]", "effectsColl2[[\"condition\"]][[\"upper\"]]"), 
#          new = c("fit", "lower", "upper"))
# 
# ## Now plot using ggplot
# C2 <- ggplot(mapping= aes(x= condition, y = fit)) + 
#   geom_point(data= Coll2, mapping= aes(x= condition, y = Cap_yday), colour = "#882255", shape = 1, stroke = 1.5) +
#   geom_ribbon(data = fitColl2, mapping =aes(x= condition, ymin = lower, ymax = upper), 
#               alpha = 0.4, colour = NA, fill = "grey")+
#   geom_line(data= fitColl2, size = 1.25)  +
#   xlab("Body Condition") + ylab("Arrival Date/day of year") +
#   theme_bw() +
#   theme(panel.grid.minor.y = element_blank(),
#         axis.title=element_text(size=18),
#         axis.text=element_text(size=14),
#         legend.text=element_text(size=14),
#         legend.title=element_text(size=14),
#         panel.grid.minor.x = element_blank(), 
#         panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_blank())




#### 7.2 MODEL: Abietinus ####
## Add interactions between the sub species and the other explanatory variables
ModAbie2 <- gls(Cap_yday ~ scale(wing),
                data=Abie2, 
                method="ML")

# get model summaries
summary(ModAbie2)
anova(ModAbie2)

# plot the predictor effects
ModAbie2effects <- predictorEffects(ModAbie2)
plot(ModAbie2effects)

# ## plot of Abietinus relationship
# ## use the effects package to extract the fit for the first variable
# divisions <- 200
# AebCond <- predictorEffects(ModAbie2, focal.levels = divisions)
# plot(AebCond[2])
# effectsAeb2 <- AebCond[2]
# fitAeb2 <- as.data.frame(cbind(effectsAeb2[["condition"]][["fit"]], effectsAeb2[["condition"]][["lower"]], 
#                                 effectsAeb2[["condition"]][["upper"]], effectsAeb2[["condition"]][["x"]]))
# 
# ## change the names to something meaningful
# setnames(fitAeb2, old = c("effectsAeb2[[\"condition\"]][[\"fit\"]]", "effectsAeb2[[\"condition\"]][[\"lower\"]]", "effectsAeb2[[\"condition\"]][[\"upper\"]]"), 
#          new = c("fit", "lower", "upper"))




#### 7.3 MODEL: Tristis ####
## Add interactions between the sub species and the other explanatory variables
ModTris2 <- gls(Cap_yday ~ scale(wing),
                data=Tris2, 
                method="ML")

# get model summaries
summary(ModTris2)
anova(ModTris2)

# plot the predictor effects
ModTris2effects <- predictorEffects(ModTris2)
plot(ModTris2effects)

# ## plot of Tristis relationship
# ## use the effects package to extract the fit for the first variable
# divisions <- 200
# TrisCond <- predictorEffects(ModTris2, focal.levels = divisions)
# plot(TrisCond[2])
# effectsTris2 <- TrisCond[2]
# fitTris2 <- as.data.frame(cbind(effectsTris2[["condition"]][["fit"]], effectsTris2[["condition"]][["lower"]], 
#                                effectsTris2[["condition"]][["upper"]], effectsTris2[["condition"]][["x"]]))
# 
# ## change the names to something meaningful
# setnames(fitTris2, old = c("effectsTris2[[\"condition\"]][[\"fit\"]]", "effectsTris2[[\"condition\"]][[\"lower\"]]", "effectsTris2[[\"condition\"]][[\"upper\"]]"), 
#          new = c("fit", "lower", "upper"))




#### 7.4 MODEL: Yellow_brow ####
## Add interactions between the sub species and the other explanatory variables
ModYBW2 <- gls(Cap_yday ~ scale(wing),
               data=YBW2, 
               method="ML")

# get model summaries
summary(ModYBW2) # significant effect of wing length (negative relationship)
anova(ModYBW2)

# plot the predictor effects
ModYBW2effects <- predictorEffects(ModYBW2)
plot(ModYBW2effects)

# ## plot of Yellow_brow relationship
# ## use the effects package to extract the fit for the first variable
# divisions <- 200
# YBWCond <- predictorEffects(ModYBW2, focal.levels = divisions)
# plot(YBWCond[2])
# effectsYBW2 <- YBWCond[2]
# fitYBW2 <- as.data.frame(cbind(effectsYBW2[["condition"]][["fit"]], effectsYBW2[["condition"]][["lower"]], 
#                                 effectsYBW2[["condition"]][["upper"]], effectsYBW2[["condition"]][["x"]]))
# 
# ## change the names to something meaningful
# setnames(fitYBW2, old = c("effectsYBW2[[\"condition\"]][[\"fit\"]]", "effectsYBW2[[\"condition\"]][[\"lower\"]]", "effectsYBW2[[\"condition\"]][[\"upper\"]]"), 
#          new = c("fit", "lower", "upper"))



# #### 7.5 PLOT: All Groups ####
# 
# ## Bind the data used in the models
# PlotCond <- rbind(Coll2, YBW2, Tris2, Abie2)
# 
# ## bind the model effects data 
# fitYBW2$subspecies <- "YBW"; fitTris2$subspecies <- "T"
# fitColl2$subspecies <- "C"; fitAeb2$subspecies <- "A"
# PlotFits2 <- rbind(fitYBW2, fitTris2, fitColl2, fitAeb2)
# 
# ## plot all of the data
# Gr2 <- ggplot(mapping= aes(x= condition, y = fit, group = subspecies, colour = subspecies)) + 
#   geom_point(data= PlotCond, mapping= aes(x= condition, y = Cap_yday, group = subspecies, colour = subspecies), shape = 1, stroke = 1.5) +
#   geom_ribbon(data = PlotFits2, mapping =aes(x= condition, ymin = lower, ymax = upper, group = subspecies, colour = subspecies), 
#               alpha = 0.2, colour = NA, fill = "grey") +
#   geom_line(data= PlotFits2, size = 1.25)  +
#   xlab("Body Condition/scaled mass index") + ylab("Capture year day") + labs(colour = "Taxonomic Group") +
#   scale_colour_manual(values=c("#DDCC77", "#882255", "#88CCEE", "#117733"), 
#                       labels=c("P. c. abietinus", "P. c. collybita", "P. c. trisits", "P. inornatus")) +
#   theme_bw() +
#   theme(panel.grid.minor.y = element_blank(),
#         axis.title=element_text(size=18),
#         axis.text=element_text(size=14),
#         legend.text=element_text(size=14, face="italic"),
#         legend.title=element_text(size=14),
#         panel.grid.minor.x = element_blank(), 
#         panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_blank())
# 
# ##save the output for the MS
# ggsave(plot = Gr2, 
#        filename = "Outputs/All CapYday vs cond.png",
#        width = 22, height = 18, units = "cm")



