# Adapted from: https://bedatablog.netlify.com/post/african-mammals-map/

## Getting started 
rm(list = ls())
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rgeos)
library(gridExtra)
library(beepr)
library(here)

# 1. Read in shape files, slightly process for reducing size and save
mammals <- st_read(paste0(here(),'/TERRESTRIAL_MAMMALS_20200117/TERRESTRIAL_MAMMALS.shp'))
beep(3)      # shout when you're done
# reproducible alternative to: "C:/Users/Roi Maor/Desktop/Ch 4 - Environmental correlates/Workspace/Spatial/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")

# Simplify the polygons to reduce the size of the object/file. This may take a few moments but the size of the 
# object (in the R environment) changes from 906Mb to <10 Mb (with dTolerance = 5), which is much easier to handle.
mammals_simple <- mammals %>% 
    st_simplify(dTolerance = 0.01)
    beep(3)                      

mammals_simple <- mammals_simple %>% 
    select(binomial, family, order_, presence, legend, category) # 'code' in 2017 data changed to 'category' in 2019

# Write the object to a file and clean up. I use .gpkg extension here because it just requires one file instead of 
# several files created for shapefiles. Also, it's an open format and it appears to be just as good as shapefiles 
# (at least for everything I've done so far). Look here for more information:
# https://www.gis-blog.com/geopackage-vs-shapefile/

file_name <- paste0(here::here(), "/iucn_mammal_distributions.gpkg")
st_write(mammals_simple, file_name)

rm(mammals, mammals_simple, file_name)

# 2. 
file_name <- paste0(here::here(), "/iucn_mammal_distributions.gpkg")
mammals <- st_read(file_name)
beep(3)

# read taxonomy and trait data
path <- 'C:/Users/Roi Maor/Desktop/New Mam Phylo'
tax <- read.csv(file=paste0(path,'/New taxonomy/MamTax2018.csv'))
phylos <- readLines(paste0(path,'/MamPhy_fullPosterior_BDvr_DNAonly_4098sp_topoFree_NDexp_all10k_v2_nexus.trees'), n = 4500)
phylo <- write.nexus(phylos, file="first100.nex")
dat <- read.csv(file='C:/Users/Roi Maor/Desktop/Ch 4 - Environmental correlates/MCMCglmm/Fulldata18SEP.csv') # 1421 species

mammals %>%
    st_set_geometry(NULL) %>%
    distinct(presence, legend) %>% 
    arrange(presence)

mammals_extant <- mammals %>% 
    filter(presence %in% c(1,2,3))

# map of Africa from https://www.naturalearthdata.com
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
                            cellsize = 0.5, 
                            square = F) %>% 
    st_sf() %>% 
    mutate(grid_id = row_number())
# although coordinates are longitude/latitude, st_relate_pattern assumes that they are planar

africa_grid_clipped <- st_intersection(africa_grid, africa_map)
# although coordinates are longitude/latitude, st_intersection assumes that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries
{
    # for whatever reason, this plot does not work like in the example
    par(mar = c(0, 0, 0, 0))
    plot(africa_map, reset = F, axes = F)
    plot(st_geometry(africa_grid_clipped), add = T, border = 'white') # rgb(0, 0, 0, 0.2))
}

# Only keep population ranges within Africa
africa_mammals <- st_intersection(mammals_extant, africa_map) %>% 
    group_by(binomial) %>% 
    summarize()
{
    par(mar = c(0, 0, 0, 0))
    plot(st_geometry(africa_mammals)) # spaghetti bowl of all ranges
}

# Combine ranges with the grid 
 #This may take a few minutes
species_per_cell <- africa_grid_clipped %>% 
    st_join(africa_mammals)

species_per_cell_sums <- species_per_cell %>% 
    group_by(grid_id) %>% 
    summarize(species_n = n())
beep(8)

# standard plot
plot(species_per_cell_sums["species_n"])

# quick and easy (NOT) ggplot
png(filename = 'africa.png', width = 10, height = 20, units = "in", bg = "white", pointsize = 36, res = 600)
noct_mammals_map <- ggplot() +
    geom_sf(data = species_per_cell_sums, aes(fill = species_n), size = 0, col=NA) +
    scale_fill_gradient2(name = "Number of\nSpecies", low = "white", mid = "dodgerblue2", high = "black", #low = "#004529", mid = "#f7fcb9", high = "#7f0000",
                         midpoint = max(species_per_cell_sums$species_n)/2) +
    geom_sf(data = africa_map, fill = NA) +
    labs(title = "Mammal Species in Africa") +
    theme_void() +
    theme(legend.position = c(0.1, 0.1), legend.justification = c(0, 0), legend.key.size = unit(0.5, units="in"), 
          plot.title = element_text(hjust = .5))

cath_mammals_map <- ggplot() +
    geom_sf(data = species_per_cell_sums, aes(fill = species_n), size = 0, col=NA) +
    scale_fill_gradient2(name = "Number of\nSpecies", low = "white", mid = "#22dd11", high = "black", #low = "#004529", mid = "#f7fcb9", high = "#7f0000",
                         midpoint = max(species_per_cell_sums$species_n)/2) +
    geom_sf(data = africa_map, fill = NA) +
    labs(title = "Mammal Species in Africa") +
    theme_void() +
    theme(legend.position = c(0.1, 0.1), legend.justification = c(0, 0), legend.key.size = unit(0.5, units="in"), 
          plot.title = element_text(hjust = .5))

diur_mammals_map <- ggplot() +
    geom_sf(data = species_per_cell_sums, aes(fill = species_n), size = 0, col=NA) +
    scale_fill_gradient2(name = "Number of\nSpecies", low = "white", mid = "goldenrod2", high = "black", #low = "#004529", mid = "#f7fcb9", high = "#7f0000",
                         midpoint = max(species_per_cell_sums$species_n)/2) +
    geom_sf(data = africa_map, fill = NA) +
    labs(title = "Mammal Species in Africa") +
    theme_void() +
    theme(legend.position = c(0.1, 0.1), legend.justification = c(0, 0), legend.key.size = unit(0.5, units="in"), 
          plot.title = element_text(hjust = .5))

crep_mammals_map <- ggplot() +
    geom_sf(data = species_per_cell_sums, aes(fill = species_n), size = 0, col=NA) +
    scale_fill_gradient2(name = "Number of\nSpecies", low = "white", mid = "grey50", high = "black", #low = "#004529", mid = "#f7fcb9", high = "#7f0000",
                         midpoint = max(species_per_cell_sums$species_n)/2) +
    geom_sf(data = africa_map, fill = NA) +
    labs(title = "Mammal Species in Africa") +
    theme_void() +
    theme(legend.position = c(0.1, 0.1), legend.justification = c(0, 0), legend.key.size = unit(0.5, units="in"), 
          plot.title = element_text(hjust = .5))

grid.arrange(noct_mammals_map , cath_mammals_map , diur_mammals_map, crep_mammals_map, ncol = 1)
dev.off()
