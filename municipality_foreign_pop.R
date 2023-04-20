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

municipality <- read_excel("Data/dissimilarity calculations.xlsx", 
                   sheet = "city dissimilarity", col_types = c("numeric", 
                                                               "text", "text", "text", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "text", "numeric", "numeric", "numeric", 
                                                               "numeric"))
municipality2 <- municipality %>% 
  rename("CUMUN" = "Name")

spain <- st_read("Data/Map/SECC_CE_20210101.shp")


#change barrio as numeric

spain$CUMUN <- as.numeric(as.double(spain$CUMUN))
municipality2$CUMUN <- as.numeric(as.character(municipality2$CUMUN))

#test plot
ggplot()+
  geom_sf(data = spain, color = "transparent", fill = "blue")

#join data
municipality3 = left_join(spain, municipality2, by ="CUMUN")

#filter
filtered_municipality3 <- filter(municipality3, CUMUN %in% municipality2$CUMUN)

#plot

gg1 = ggplot() +
  geom_sf(data = filtered_municipality3, color = "transparent", size =0.1, aes(fill = foreignerovertotal)) +
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
ggsave(gg1, filename = "maps/municipal.png", width = 8.335, height = 6.946, dpi = 150)

