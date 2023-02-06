## Luke Ozsanlav-Harris
## Created: 01/02/2023

## Create range map of YBW and Chiffchaffs for manuscript

## Packages reuqired
pacman::p_load(tidyverse, data.table, sf, raster, rnaturalearth, 
               ggspatial, marmap, ggnewscale, cowplot)




#-----------------------------------#
#### 1. Read in the Spatial data ####
#-----------------------------------#

## Citation for BirdLife data: BirdLife International and Handbook of the Birds of the World (2022) Bird species distribution maps of the world. Version 2022.2. Available at http://datazone.birdlife.org/species/requestdis.
## NOTE:
## Either an electronic or two paper copies of all products published using data supplied 
## by BirdLife International will be sent, free of charge, to the BirdLife Global Secretariat at:
## BirdLife International, The David Attenborough Building, Pembroke Street, 
## Cambridge, CB2 3QZ, UK or via email to science@birdlife.org 

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

## Citation for ebird data
## Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, S. Ligocki, O. Robinson, W. Hochachka, L. Jaromczyk, A. Rodewald, C. Wood, I. Davies, A. Spencer. 2022. 
## eBird Status and Trends, Data Version: 2021; Released: 2022. Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.2173/ebirdst.2021

## Read in the eBird data
# eB1 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_nonbreeding_mean_2021.tif")
# plot(eB1)
eB2 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_postbreeding-migration_mean_2021.tif")
plot(eB2)
# eB3 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_prebreeding-migration_mean_2021.tif")
# plot(eB3)

## re p[orject raster to the same as the ebird data]
eB2 <- projectRaster(eB2, crs = crs(BL))


## Read in the global prcip dataset
Precip <- raster("Spatial/Global_precip/d2h_GS.tif")
plot(Precip)




#---------------------------------#
#### 2. Download global basemaps ##
#---------------------------------#

## get coast outlines
countries <- ne_coastline(scale = "medium", returnclass = "sf")
Russia <- ne_countries(scale = "medium", returnclass = "sf", country = "russia")
countries2 <- ne_countries(scale = "medium", returnclass = "sf")

## Get the elevation layer
base_topography_map <- getNOAA.bathy(lon1 = -20, lon2 = 180,
                                     lat1 = -5, lat2 = 80, resolution = 10)

## fortify for plotting
base_topography_fort = fortify(base_topography_map)




#--------------------------------------------#
#### 3. Create range map for Chiffs and YBW ##
#--------------------------------------------#

m <- ggplot() + 
  # Render the bathymetry map
  # geom_raster(data = base_topography_fort, aes(x=x, y=y, fill=z), alpha = 0.6) +
  # scale_fill_viridis_c(option="mako", guide = "none") +
  # new_scale_fill() +
  
  # add the filled in countries
  geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = Col, aes(geometry = geometry, fill = "#6f9969"), colour = NA, alpha = 0.9) +
  geom_sf(data = YB, aes(geometry = geometry, fill = "#808fe1"), colour = NA, alpha = 0.9) +
  geom_sf(data = Tris, aes(geometry = geometry, fill = "#efc86e"), colour = NA, alpha = 0.6) +
  scale_fill_manual(name = "Taxonomic Group",
                    values =c("#6f9969"="#6f9969","#efc86e"="#efc86e", "#808fe1"="#808fe1"),
                    labels = c("P. c. collybita", "P. c. trisits", "P. inornatus")) +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
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
  
m


## Save the map
ggsave(plot = m, 
       filename = "Outputs/Range_map_Nobathy.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)




#--------------------------------------#
#### 4. Create vagrancy map for YBW ####
#--------------------------------------#

## Post-breeding migratory season 12 Oct - 7 Dec
## This period produces the best mpa of vagrancy
## The source is eBird data

## Remove zero value from the raster
eB2[eB2 <= 0] <- NA
eB2[eB2 > 1] <- NA
plot(eB2)
# glimpse(base_topography_fort)
# glimpse(eB2_df)

## fortify for plotting
eB2_pts <- rasterToPoints(eB2, spatial = TRUE)
# Then to a 'conventional' dataframe
eB2_df  <- data.frame(eB2_pts)
colnames(eB2_df)[1] <- "z"


## Now pot the map
m2 <- ggplot() + 
  
  # add the filled in countries
  geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # Render the eBird map
  geom_raster(data = eB2_df, aes(x=x, y=y, fill=z)) +
  scale_fill_viridis_c(option="viridis", begin = 0,
                       end = 1, name = "eBird post-breeding density (12 Oct - 7 Dec)") +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = YB, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
  scale_colour_manual(name = "BirdLife Breeding/Non-breeding Range",
                      values =c("#808fe1"="#808fe1"),
                      labels = c("")) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set map limits
  coord_sf(xlim = c(-20, 180),
           ylim = c(-5, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        legend.box="vertical", axis.title = element_blank())

m2


## Save the map
ggsave(plot = m2, 
       filename = "Outputs/YBW_vag_map.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)




## Now pot the map
m3 <- ggplot() + 
  
  # add the filled in countries
  geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # Render the eBird map
  geom_raster(data = eB2_df, aes(x=x, y=y, fill=z)) +
  scale_fill_viridis_c(option="viridis", begin = 0,
                       end = 1, name = "eBird post-breeding density (12 Oct - 7 Dec)") +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = YB, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
  scale_colour_manual(name = "BirdLife Breeding/Non-breeding Range",
                      values =c("#808fe1"="#808fe1"),
                      labels = c("")) +
  
  # add map extras
  annotation_scale(location = "bl", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set map limits
  coord_sf(xlim = c(-24, 53),
           ylim = c(19, 72), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        legend.box="vertical", axis.title = element_blank())

m3

## Save the map
ggsave(plot = m3, 
       filename = "Outputs/YBW_vag_EuropeOnly.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)



## Now pot the map
m4 <- ggplot() + 
  
  # add the filled in countries
  geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # Render the eBird map
  geom_raster(data = eB2_df, aes(x=x, y=y, fill=z)) +
  scale_fill_viridis_c(option="viridis", begin = 0,
                       end = 1, name = "eBird post-breeding density (12 Oct - 7 Dec)") +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = YB, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
  scale_colour_manual(name = "BirdLife Breeding/Non-breeding Range",
                      values =c("#808fe1"="#808fe1"),
                      labels = c("")) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set map limits
  coord_sf(xlim = c(70, 180),
           ylim = c(-5, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        legend.box="vertical", axis.title = element_blank())

m4


## Save the map
ggsave(plot = m4, 
       filename = "Outputs/YBW_vag_AsiaOnly.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)


## Combine the two plots
## Sort out legend: https://wilkelab.org/cowplot/articles/shared_legends.html
Pt <- ggdraw() +
  draw_plot(m3, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(m4, x = .5, y = 0, width = 0.5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

## save the combined plot
ggsave(plot = Pt, 
       filename = "Outputs/YBW_vag_SplitPlot.png",
       units = "mm", width = 200, height = 175, dpi = 300,   
)

