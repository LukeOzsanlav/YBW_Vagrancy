## Luke Ozsanlav-Harris
## Created: 01/02/2023

## Create range map of YBW and Chiffchaffs for manuscript

## Packages reuqired
pacman::p_load(tidyverse, data.table, sf, raster, rnaturalearth, patchwork,
               ggspatial, marmap, ggnewscale, cowplot, ggpubr, png)




#-----------------------------------#
#### 1. Read in the Spatial data ####
#-----------------------------------#


## BirdLife data ##

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
# plot(BL$geometry[6]) 


## Remove the rows I do not need, i.e. the passage ones
BL_sub <- BL %>%  filter(!row_number() %in% c(4, 9))

## write out this object
# st_write(BL, "Spatial/Birdlife Data/SppDataRequest_cropped.shp")

## Create a separate object for each species
Col <-  BL_sub %>%  filter(sci_name == "Phylloscopus collybita")
Tris <-  BL_sub %>%  filter(sci_name == "Phylloscopus tristis")
YB <-  BL_sub %>%  filter(sci_name == "Phylloscopus inornatus")


## eBird data ##

## Citation for ebird data
## Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, S. Ligocki, O. Robinson, W. Hochachka, L. Jaromczyk, A. Rodewald, C. Wood, I. Davies, A. Spencer. 2022. 
## eBird Status and Trends, Data Version: 2021; Released: 2022. Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.2173/ebirdst.2021

## Read in the eBird data
# eB1 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_nonbreeding_mean_2021.tif")
# plot(eB1)
eB2 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_postbreeding-migration_mean_2021.tif")
# plot(eB2)
# eB3 <- raster("Spatial/eBird/yebwar3_abundance_seasonal_prebreeding-migration_mean_2021.tif")
# plot(eB3)

# ## re prorject raster to the same as the ebird data
# eB2 <- projectRaster(eB2, crs = crs(BL))
# eB2_repro <- eB2

## Precip istotope data ##

## Read in the global prcip dataset
Precip <- raster("Spatial/Global_precip/d2h_GS.tif")
# plot(Precip)




#-----------------------------------#
#### 2. Download global basemaps ####
#-----------------------------------#

## get coast outlines
countries <- ne_coastline(scale = "medium", returnclass = "sf")
Russia <- ne_countries(scale = "medium", returnclass = "sf", country = "russia")
countries2 <- ne_countries(scale = "medium", returnclass = "sf")

## Get the elevation layer
base_topography_map <- getNOAA.bathy(lon1 = -20, lon2 = 180,
                                     lat1 = -5, lat2 = 80, resolution = 10)

## fortify for plotting
base_topography_fort = fortify(base_topography_map)




#----------------------------------------------#
#### 3. Create range map for Chiffs and YBW ####
#----------------------------------------------#

## Create the range map for all three taxonomic groups
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
  scale_fill_manual(name = expression("Taxonomic Group"*":"),
                    values =c("#6f9969"="#6f9969","#efc86e"="#efc86e", "#808fe1"="#808fe1"),
                    labels = c("P. c. collybita", "P. c. trisits", "P. inornatus")) +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.25, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering,
                         height = unit(0.8, "cm"), width = unit(0.5, "cm"),) +
  
  # set amp limits
  coord_sf(xlim = c(-20, 180),
           ylim = c(-5, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank(), legend.title= element_text(size=14, face="bold"),
        legend.box.background=element_rect(colour = "#BDC3C7"),legend.box.margin=margin(5,5,5,5))
  
# m


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
# plot(eB2)



# ## Now pot the map
# m2 <- ggplot() + 
#   
#   # add the filled in countries
#   geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
#   new_scale_fill() +
#   
#   # add the coastoutline
#   #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
#   
#   # Render the eBird map
#   geom_raster(data = eB2_df, aes(x=x, y=y, fill=z)) +
#   scale_fill_viridis_c(option="viridis", begin = 0,
#                        end = 1, name = "eBird post-breeding density (12 Oct - 7 Dec)") +
#   new_scale_fill() +
#   
#   # add the range areas
#   geom_sf(data = YB, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
#   scale_colour_manual(name = "BirdLife Breeding/Non-breeding Range",
#                       values =c("#808fe1"="#808fe1"),
#                       labels = c("")) +
#   
#   # add map extras
#   annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
#   annotation_north_arrow(location = "br", which_north = "true",
#                          pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
#                          style = north_arrow_orienteering,
#                          height = unit(1, "cm"), width = unit(0.7, "cm"),) +
#   
#   # set map limits
#   coord_sf(xlim = c(-20, 180),
#            ylim = c(-5, 80), crs = 4326, expand = F) +
#   
#   # Plot styling
#   theme_light() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
#         legend.box="vertical", axis.title = element_blank())
# 
# m2
# 
# 
# ## Save the map
# ggsave(plot = m2, 
#        filename = "Outputs/YBW_vag_map.png",
#        units = "mm", width = 200, height = 175, dpi = 300,   
# )


## Make the Europe Map ##

## re-project the countries layer
countries_repro <- st_transform(countries2, crs = crs(eB2))
## Now crop the countires layer
countries_reprocrop <- st_crop(countries_repro, xmin= -2100000, ymin= 2500000, xmax= 5000000, ymax=7850000)

## aggregate the raster layer
eB2Agg <- aggregate(eB2, fact = 5, fun=mean)

## crop the raster
b <- as(extent(-2100000, 5000000, 2500000, 7850000), 'SpatialPolygons')
crs(b) <- crs(eB2Agg)
eB2_cr <- crop(eB2Agg, b)

## fortify for plotting
eB2_pts <- rasterToPoints(eB2_cr, spatial = TRUE)
# Then to a 'conventional' dataframe
eB2_df  <- data.frame(eB2_pts)
colnames(eB2_df)[1] <- "z"


## Now plot the map
m3 <- ggplot() + 
  
  # add the filled in countries
  geom_sf(data = countries_reprocrop, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # Render the eBird map
  geom_raster(data = eB2_df, aes(x=x, y=y, fill=z)) +
  scale_fill_viridis_c(option="viridis", begin = 0,
                       end = 1, name = "eBird density (12 Oct - 7 Dec)") +
  new_scale_fill() +
  
  # add the range areas
  # geom_sf(data = YB, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
  # scale_colour_manual(name = "BirdLife Breeding/Non-breeding Range",
  #                     values =c("#808fe1"="#808fe1"),
  #                     labels = c("")) +
  
  # add map extras
  annotation_scale(location = "bl", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
  #                        style = north_arrow_orienteering,
  #                        height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set map limits
  coord_sf(expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank(), legend.title= element_text(size=11, face="bold"),
        legend.box.background=element_rect(colour = "#BDC3C7"),legend.box.margin=margin(1,1,1,1),
        legend.box="vertical")

# m3



## Make the Asia map ##

## Now crop the countires layer
countries_reprocrop2 <- st_crop(countries_repro, xmin= 7000000, ymin= 0, xmax= 12500000, ymax=8000000)

## crop the raster
b <- as(extent(7000000, 12500000, 0, 8000000), 'SpatialPolygons')
crs(b) <- crs(eB2Agg)
eB2_cr2 <- crop(eB2Agg, b)


## fortify for plotting
eB2_pts2 <- rasterToPoints(eB2_cr2, spatial = TRUE)
# Then to a 'conventional' dataframe
eB2_df2  <- data.frame(eB2_pts2)
colnames(eB2_df2)[1] <- "z"

# ## crop the YBW range
# ## re-project the countries layer
YB_repro <- st_transform(YB, crs = crs(eB2))
## Now crop the countires layer
YB_reprocrop <- st_crop(YB_repro, xmin= 7000000, ymin= 0, xmax= 12500000, ymax=8000000)


## Now pot the map
m4 <- ggplot() + 
  
  # add the filled in countries
  geom_sf(data = countries_reprocrop2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  new_scale_fill() +
  
  # add the coastoutline
  #geom_sf(data = countries, aes(geometry = geometry), size = 0.2) +
  
  # Render the eBird map
  geom_raster(data = eB2_df2, aes(x=x, y=y, fill=z), show.legend = F) +
  scale_fill_viridis_c(option="viridis", begin = 0,
                       end = 1) +
  new_scale_fill() +
  
  # add the range areas
  geom_sf(data = YB_reprocrop, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 0.7, size =1) +
  scale_colour_manual(name = "BirdLife Range",
                      values =c("#808fe1"="#808fe1"),
                      labels = c("")) +
  
  # add map extras
  annotation_scale(location = "br", width_hint = 0.1, pad_y = unit(0.15, "in")) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_orienteering,
                         height = unit(1, "cm"), width = unit(0.7, "cm"),) +
  
  # set map limits
  coord_sf(expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank(), legend.title= element_text(size=12, face="bold"),
        legend.box.background=element_rect(colour = "#BDC3C7"),legend.box.margin=margin(1,1,1,1),
        legend.box="vertical")

# m4



## Add YBW pic ##

## Now read in the YBW image
YB_im <- readPNG("YBW_pic2.png", native = TRUE)

m3_2 <- ggplot()+                  # Combine plot & image
  inset_element(p = YB_im,
                left = 0,
                bottom = 0,
                right = 1,
                top = 1) + theme_light()



## Combine the two plots
## Sort out legend: https://wilkelab.org/cowplot/articles/shared_legends.html
Pt <- ggdraw() +
  draw_plot(m3, x = 0, y = 0, width = 0.5, height = 0.8) +
  draw_plot(m3_2, x = 0, y = 0.7, width = 0.4, height = 0.3) +
  draw_plot(m4, x = 0.5, y = 0.11, width = 0.5, height = 0.9) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

## save the combined plot
ggsave(plot = Pt, 
       filename = "Outputs/YBW_vag_SplitPlot.png",
       units = "mm", width = 230, height = 175, dpi = 300,   
)





#------------------------------#
#### 5. Create Isoscape map ####
#------------------------------#

## re porject raster to the same as the BirdLife data
## I think it was already the same anyway
Precip <- projectRaster(Precip, crs = crs(BL))

## crop the raster
b <- as(extent(-20, 180, 28, 80), 'SpatialPolygons')
crs(b) <- crs(Precip)
Precip_cr <- crop(Precip, b)

## fortify for plotting
Precip_pts <- rasterToPoints(Precip_cr, spatial = TRUE)
# Then to a 'conventional' dataframe
Precip_df  <- data.frame(Precip_pts)
#glimpse(Precip_df)



## Create shapefile of just breeding areas
BL_sub2 <- BL %>%  filter(row_number() %in% c(2,3,5,8))
Col2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus collybita")
Tris2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus tristis")
YB2 <-  BL_sub2 %>%  filter(sci_name == "Phylloscopus inornatus")
glimpse(Col2)

## merge and simplfy the collybita range
Col3 <- Col2 %>% st_union() %>% st_simplify(dTolerance = 15000)

## create the plot of the istopes and ranges
Im <- ggplot() + 
  
  # Render the eBird map
  geom_raster(data = Precip_df, aes(x=x, y=y, fill=d2h_GS)) +
  scale_fill_gradient2(low= "#000000", high = "#FFFFFF", name = expression(delta^2*H*"  "*("‰")), midpoint = -30,
                       guide = guide_colorbar(frame.colour = "#BDC3C7", ticks = TRUE, title.vjust = 1)) +
  # scale_fill_viridis_c(option="inferno", begin = 0,
  #                      end = 1, name = expression(delta^2*H*"  "*("‰"))) +
  new_scale_fill() +
  
  # # add the filled in countries
  # geom_sf(data = countries2, aes(geometry = geometry), fill = "#BDC3C7", colour = "#BDC3C7", alpha = 0.6) +
  # new_scale_fill() +
  
  # add the range areas
  geom_sf(data = Col3, aes(geometry = geometry, colour = "#6f9969"), fill = NA, alpha = 1, linetype = "solid", size = 0.75) +
  geom_sf(data = YB2, aes(geometry = geometry, colour = "#808fe1"), fill = NA, alpha = 1, linetype = "solid", size = 0.75) +
  geom_sf(data = Tris2, aes(geometry = geometry, colour = "#efc86e"), fill = NA, alpha = 1, linetype = "solid", size = 0.75) +
  scale_colour_manual(name = expression("Taxonomic Group"*":"),
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
  coord_sf(xlim = c(-12, 180),
           ylim = c(28, 80), crs = 4326, expand = F) +
  
  # Plot styling
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text=element_text(size=10, face="italic"), legend.position = "bottom",
        axis.title = element_blank(), legend.title= element_text(size=12, face="bold"),
        legend.box.background=element_rect(colour = "#BDC3C7"),legend.box.margin=margin(1,1,1,1),
        legend.box="vertical")


## save the combined plot
ggsave(plot = Im, 
       filename = "Outputs/Isotope_range_map.png",
       units = "mm", width = 200, height = 130, dpi = 300,   
)

