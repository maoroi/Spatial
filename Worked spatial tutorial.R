# Adapted from: https://bedatablog.netlify.com/post/african-mammals-map/

## Getting started 
rm(list = ls())
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rgeos)
library(beepr)
library(here)

# 1. Read in shape files, slightly process for reducing size and save
mammals <- st_read(paste0(here(),'/TERRESTRIAL_MAMMALS_20200117/TERRESTRIAL_MAMMALS.shp'))
beep(2)      # shout when you're done
# reproducible alternative to: "C:/Users/Roi Maor/Desktop/Ch 4 - Environmental correlates/Workspace/Spatial/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")

# Simplify the polygons to reduce the size of the object/file. This may take a few moments but the size of the 
# object (in the R environment) changes from 906Mb to <10 Mb (with dTolerance = 5), which is much easier to handle.
mammals_simple <- mammals %>% 
    st_simplify(dTolerance = 5)
beep(2)                      

mammals_simple <- mammals_simple %>% 
    select(binomial, family, order_, presence, legend, category) # 'code' in 2017 data changed to 'category' in 2019

# Write the object to a file and clean up. I use .gpkg extension here because it just requires one file instead of 
# several files created for shapefiles. Also, it's an open format and it appears to be just as good as shapefiles 
# (at least for everything I've done so far). Look here for more information:
# https://www.gis-blog.com/geopackage-vs-shapefile/

file_name <- paste0(here::here(), "/iucn_mammal_distributions.gpkg")
st_write(mammals_simple, file_name)

rm(list = c("mammals", "mammals_simple", "file_name"))

# 2. 
file_name <- paste0(here::here(), "/iucn_mammal_distributions.gpkg")
mammals <- st_read(file_name)

mammals %>%
    st_set_geometry(NULL) %>%
    distinct(presence, legend) %>% 
    arrange(presence)

mammals_extant <- mammals %>% 
    filter(presence %in% c(1,2,3))

africa_map <- rnaturalearth::ne_countries(continent = "africa",
                                          returnclass = "sf") %>%
    st_set_precision(1e9) %>%
    summarize
{
    par(mar = c(0, 0, 0, 0))
    plot(st_geometry(africa_map))
}

africa_grid <- st_make_grid(africa_map,
                            what = "polygons",
                            cellsize = 0.75,
                            square = F) %>% 
    st_sf() %>% 
    mutate(grid_id = row_number())
# although coordinates are longitude/latitude, st_intersection assumes that they are planar

africa_grid_clipped <- st_intersection(africa_grid, africa_map)

{
    par(mar = c(0, 0, 0, 0))
    plot(africa_map$geometry, reset = F, axes = F)
    plot(st_geometry(africa_grid_clipped), color = "white",
         add = T, border = rgb(0, 0, 0, 0.2))
}
# although coordinates are longitude/latitude, st_intersection assumes that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 
