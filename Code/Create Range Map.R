## Luke Ozsanlav-Harris
## Created: 01/02/2023

## Create range map of YBW and Chiffchaffs for manuscript

## Packages reuqired
pacman::p_load(tidyverse, data.table, sf, raster, rnaturalearth, MetBrewer)


##
#### 1. Read in the Spatial data ####
##

## Read in the birdlife range maps
BL <- st_read(dsn = "Spatial/Birdlife Data/",
                  layer = "SppDataRequest")
st_crs(BL)

## plot some of the maps
BL$sci_name
plot(BL$geometry[6]) # collybita

## Each row refers to the following distributions
## Chiff 1- Coll Non-breeding
## Chiff 2- Coll Native resident
## Chiff 3- Coll Breeding
## Chiff 4- Coll Passage

## Chiff 5- Tris Breeding
## Chiff 6- Tris Non-breeding

## Yellow Brow 7- Non-breeding
## Yellow Brow 8- Breeding
## Yellow Brow 9- Passage

## Remove the rows I do not need, i.e. the passage ones
BL <- BL %>%  filter(!row_number() %in% c(4, 9))

## Create a separate object for each species
Col <-  BL %>%  filter(sci_name == "Phylloscopus collybita")
Tris <-  BL %>%  filter(sci_name == "Phylloscopus tristis")
YB <-  BL %>%  filter(sci_name == "Phylloscopus inornatus")


## Read in the eBird data
eB1 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_nonbreeding_mean_2021.tif")
plot(eB1)
eB2 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_postbreeding-migration_mean_2021.tif")
plot(eB2)
eB3 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_prebreeding-migration_mean_2021.tif")
plot(eB3)



##
#### 2. Download coast outline of the world ##
##

## get coast outlines
#
countries <- ne_coastline(scale = "medium", returnclass = "sf")
Russia <- ne_countries(scale = "medium", returnclass = "sf", country = "russia")



#Derain = ["#aab5d5","#97c684","#808fe1","#454a74","#efc86e","#6f9969","#5c66a8"];
print(met.brewer(name="Derain", n=7, type="discrete"))


ggplot() + 
  #geom_sf(data = Russia, aes(geometry = geometry), fill = NA) +
  geom_sf(data = Col, aes(geometry = geometry), fill = "#808fe1", colour = NA, alpha = 0.8) +
  geom_sf(data = Tris, aes(geometry = geometry), fill = "#efc86e", colour = NA, alpha = 0.8) +
  geom_sf(data = YB, aes(geometry = geometry), fill = "#6f9969", colour = NA, alpha = 0.8) +
  geom_sf(data = countries, aes(geometry = geometry), fill = NA) +
  coord_sf(xlim = c(-20, 180), 
           ylim = c(-5, 80), crs = 4326, expand = F) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  





