## Luke Ozsanlav-Harris
## Created: 01/02/2023

## Create range map of YBW and Chiffchaffs for manuscript

## Packages reuqired
pacman::p_load(tidyverse, data.table, sf, raster, rnaturalearth, MetBrewer, ggspatial, marmap, ggnewscale)


##
#### 1. Read in the Spatial data ####
##

## Read in the birdlife range maps
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
BL <- st_read(dsn = "Spatial/Birdlife Data/",
                  layer = "SppDataRequest")
st_crs(BL)

## plot some of the maps
BL$sci_name
plot(BL$geometry[6]) # collybita

## Remove the rows I do not need, i.e. the passage ones
BL <- BL %>%  filter(!row_number() %in% c(4, 9))

## write out this object
st_write(BL, "Spatial/Birdlife Data/SppDataRequest_cropped.shp")

## Create a separate object for each species
Col <-  BL %>%  filter(sci_name == "Phylloscopus collybita")
Tris <-  BL %>%  filter(sci_name == "Phylloscopus tristis")
YB <-  BL %>%  filter(sci_name == "Phylloscopus inornatus")


## Read in the eBird data
# eB1 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_nonbreeding_mean_2021.tif")
# plot(eB1)
# eB2 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_postbreeding-migration_mean_2021.tif")
# plot(eB2)
# eB3 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_prebreeding-migration_mean_2021.tif")
# plot(eB3)



##
#### 2. Download the basemaps I need ##
##

## get coast outlines
countries <- ne_coastline(scale = "medium", returnclass = "sf")
Russia <- ne_countries(scale = "medium", returnclass = "sf", country = "russia")
countries2 <- ne_countries(scale = "medium", returnclass = "sf")

## Get the elevation layer
base_topography_map <- getNOAA.bathy(lon1 = -20, lon2 = 180,
                                     lat1 = -5, lat2 = 80, resolution = 10)

## fortify for plotting
base_topography_fort = fortify(base_topography_map)






##
#### 3. Create the map using ggplot ##
##


m <- ggplot() + 
  # Render the bathymetry map
  geom_raster(data = base_topography_fort, aes(x=x, y=y, fill=z), alpha = 0.6) +
  scale_fill_viridis_c(option="mako", guide = "none") +
  new_scale_fill() +
  
  # add the filled in countries
  geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7") +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = Col, aes(geometry = geometry, fill = "#808fe1"), colour = NA, alpha = 0.9) +
  geom_sf(data = YB, aes(geometry = geometry, fill = "#6f9969"), colour = NA, alpha = 0.9) +
  geom_sf(data = Tris, aes(geometry = geometry, fill = "#efc86e"), colour = NA, alpha = 0.6) +
  scale_fill_manual(name = "Taxonomic Group",
                    values =c("#808fe1"="#808fe1","#efc86e"="#efc86e", "#6f9969"="#6f9969"),
                    labels = c("P. c. collybita", "P. c. trisits", "P. inornatus")) +
  
  # add the coastoutline
  geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set amp limits
  coord_sf(xlim = c(-20, 180),
           ylim = c(-5, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank())
  

## Save the map
ggsave(plot = m, 
       filename = "Outputs/Range_map.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)


