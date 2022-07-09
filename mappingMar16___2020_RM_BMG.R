
# Load packages -----------------------------------------------------------
library(ggspatial)
library(tidyverse)
library(viridis)

# Set default ggplot2 to black-and-white
theme_set(theme_bw())

# Load data ---------------------------------------------------------------

bahamas <- raster::getData("GADM", country = "BHS", level = 0)
coords  <- read.csv("rocksound_coords.csv")

#remove the 'half' treatment
rm_coords <- coords %>%
  filter(Treatment != "half") %>%
  filter(Treatment != "removal")

#specify treatment order so it appears the way we want it in the legend
rm_coords$Treatment <- factor(rm_coords$Treatment, 
                              levels = c("H.mexicana", "A.agassizii", "control"))

# Plot Rock Sound map -----------------------------------------------------

rocksound <-
  ggplot() +
  layer_spatial(data = bahamas, colour = "black") +
  geom_spatial_point(data = coords, alpha = 0.7, size = 5.5, pch = 21, crs = 4326,
                    aes(long, lat, fill = total_cuke_den_sg)) +
  #scale_color_manual(values = c("H.mexicana"="blue", "A.agassizii"="darkorange2", "control"="darkgreen")) +
  #scale_color_brewer(palette = "Set1") +
  annotation_scale(location = "bl", style="bar", pad_x = unit(0.4,"cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(2, "cm"), width = unit(1.8, "cm"),
                         pad_x = unit(0.1,"cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-76.35, -76.16), ylim = c(24.78,24.9)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  scale_colour_viridis(name = "Sea cucumber \ndensity (individuals/"~m^2~")") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))

#look at part 1 of map
rocksound

# Alternative solution: convert sp (bahamas) and df (coords) objects to sf
# bahamas_sf <- sf::st_as_sf(bahamas)
# coords_sf  <- sf::st_as_sf(coords, coords = c("long", "lat"),
#                           crs = 4326, agr = "constant")
#
# rocksound <-
#   ggplot() +
#   geom_sf(data = bahamas_sf, fill = "grey", colour = "black") +
#   geom_sf(data = coords_sf, aes(colour = proportion_ft_original),
#           alpha = 0.6, size = 4) +
#   annotation_scale(location = "bl", style="ticks") +
#   annotation_north_arrow(location = "tl", which_north = "true",
#                          height = unit(1, "cm"), width = unit(1, "cm"),
#                          style = north_arrow_fancy_orienteering) +
#   coord_sf(xlim = c(-76.35, -76.16), ylim = c(24.78,24.9)) +
#   labs(x = "Longitude", y = "Latitude")
#
# rocksound

# Plot Eleuthera map ------------------------------------------------------

eleuthera <-
  ggplot() +
  layer_spatial(data = bahamas, fill = "grey", colour = "black") +
  annotate("rect", xmin=-76.15, xmax=-76.36, ymin=24.77, ymax=24.9,
           alpha=0, color="red") +
  annotation_scale(location = "bl", style="bar", pad_x = unit(1.2,"cm")) +
  coord_sf(xlim = c(-76.9,-76.1), ylim = c(24.6,25.56)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") 



#look at part 2 of map (inset)
eleuthera

# CH1 FIG 1 Include Eleuthera map as inset of Rock Sound map ------------------------

combined <-
  rocksound +
  annotation_custom(
    grob = ggplotGrob(eleuthera),
    xmin = -76.40,
    xmax = -76.24,
    ymin = 24.8407,
    ymax = 24.907) +
  #annotate(geom="text", x=-76.258, y=24.895, label="Rock Sound",
           #color="black", size = 8) +
  annotate(geom="text", x=-76.322, y=24.8772, label="Eleuthera\nIsland",
           color="black", size = 5) +
  #annotate(geom="point", x=-76.335, y=24.8345, color="red", size = 3.5) +
  #annotate(geom="text", x=-76.325, y=24.836, label="CEI",
           #color="red", size = 6) +
  annotate(geom="text", x=-76.24, y=24.8, label="Rock Sound",
           color="black", size = 6) +
  theme( 
  axis.title.x = element_text(size=17),
  axis.title.y = element_text(size=17),
  axis.text.y = element_text(size=18),
  axis.text.x = element_text(size=18),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16))

combined

# Save map ----------------------------------------------------------------
#setwd("/Users/rachelmunger/Documents/SFU/MSc/MSc_Data/cuke_msc/Figures")
#ggsave("total_cuke_den.pdf", combined, height = 8, width = 12)



install.packages('scatterpie')
library(scatterpie)


##### Pie Chart Contributions ####
#Contribution by each spp to bioturbation
ggplot() +
  layer_spatial(data = bahamas, fill = NA, colour = "black") +
  geom_scatterpie(aes(x=long, y=lat, group=seagrass_area),
                 data=coords, pie_scale = 6, cols=c("kg_m2yr_hmex", "kg_m2yr_aaga"), color=NA, alpha=0.7, size =8) +
  scale_fill_manual(values=c("indianred3", "slateblue3")) +
  annotation_scale(location = "bl", style="bar", pad_x = unit(0.4,"cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(2, "cm"), width = unit(1.8, "cm"),
                         pad_x = unit(0.1,"cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-76.35, -76.16), ylim = c(24.78,24.9)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))

#Contribution by each spp to ammonium excretion
ggplot() +
  layer_spatial(data = bahamas, fill = NA, colour = "black") +
  geom_scatterpie(aes(x=long, y=lat, group=seagrass_area),
                  data=coords, pie_scale = 6, cols=c("umolh_m2_hmex", "umolh_m2_aaga"), color=NA, alpha=0.7, size =8) +
  scale_fill_manual(values=c("indianred3", "slateblue3")) +
  annotation_scale(location = "bl", style="bar", pad_x = unit(0.4,"cm"), width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(2, "cm"), width = unit(1.8, "cm"),
                         pad_x = unit(0.1,"cm"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-76.35, -76.16), ylim = c(24.78,24.9)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))




#### WHITE OUTLINES OF BAHAMS AND ELEUTHERA FOR PRESENTATIONS ####
#bahamas
eleuthera1 <-
  ggplot() +
  layer_spatial(data = bahamas, fill = "white", colour = "black") +
  #annotate("rect", xmin=-76.15, xmax=-76.36, ymin=22.77, ymax=23.9,
           #alpha=0, color="red") +
  coord_sf(xlim = c(-79.9,-72.1), ylim = c(20.6,28.56)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "Longitude", y = "Latitude") 

eleuthera1


#island
eleuthera2 <-
  ggplot() +
  layer_spatial(data = bahamas, fill = "white", colour = "black") +
  #annotate("rect", xmin=-76.15, xmax=-76.36, ymin=24.77, ymax=24.9,
       #    alpha=0, color="red") +
  #annotation_scale(location = "bl", style="bar", pad_x = unit(1.2,"cm")) +
  coord_sf(xlim = c(-76.9,-76.1), ylim = c(24.6,25.56)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "Longitude", y = "Latitude") 
eleuthera2

library(tidyverse)

coords %>% 
  summarise(mean(ft_density_grass), mean(dd_density_grass))
