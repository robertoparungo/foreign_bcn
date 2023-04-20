setwd("C:/Users/Rob/OneDrive/Documents/RFiles/thesis")

library(sf)
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridisLite)
library(viridis)


# helper function to move geometries --------------------------------------
place_geometry = function(geometry, bb, scale_x, scale_y,
                          scale_size = 1){
  output_geometry = (geometry - st_centroid(st_combine(geometry))) * scale_size +
    st_sfc(st_point(c(
      bb$xmin + scale_x * (bb$xmax - bb$xmin),
      bb$ymin + scale_y * (bb$ymax - bb$ymin)
    )))
  return(output_geometry)
}

city <- read_excel("Data/dissimilarity calculations.xlsx", 
                   sheet = "city dissimilarity", col_types = c("numeric", 
                                                               "text", "text", "text", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "text", "numeric", "numeric", "numeric", 
                                                               "numeric"))
city2 = city%>% 
  rename("BARRI" = "Codi_Barri")

districts <- st_read("Data/Map/BCN_UNITATS_ADM/0301040100_Districtes_UNITATS_ADM.shp")
neighborhoods <- st_read("Data/Map/BCN_UNITATS_ADM/0301040100_Barris_UNITATS_ADM.shp")


#intersect maps
neighborhoods<-st_intersection(neighborhoods,districts)
neighborhoods = st_make_valid(neighborhoods)

#change barrio as numeric
neighborhoods$BARRI <- as.numeric(as.double(neighborhoods$BARRI))

#test plot
ggplot()+
  geom_sf(data = neighborhoods, color = "transparent", fill = "blue")+
  geom_sf(data = districts, color = "black", fill = "grey80")

#join data
neighborhoods1 = left_join(neighborhoods, city2, by ="BARRI")

#plot

gg1 = ggplot() +
  geom_sf(data = neighborhoods1, color = "transparent", aes(fill = foreignpop)) +
  geom_sf(data = districts, color = "white", fill="transparent",size = 0.3) +
  scale_fill_viridis(discrete=FALSE,
                     name="% Foreign population over total 
neighborhood population (hundreths)",
                     option="viridis",
                     direction=1) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "ivory1", color = NA),
    panel.background = element_rect(fill = "ivory1", color = NA),
    legend.background = element_rect(fill = "ivory1", color = NA),
  )

gg1



#save
dir.create("maps")
ggsave(gg1, filename = "maps/city.png", width = 8.335, height = 6.946, dpi = 150)

