# Trying out ARDEM 2.0. 

# Get packages.

library(ncdf4) # netcdf manipulation
library(raster) # raster manipulation
library(sf) # vector manipulation
library(nngeo) # specialized vector operations
library(ggplot2) # plotting
library(dplyr) # data manipulation
library(magrittr) # pipes

# Get newer packages, try learning something for once.

library(terra)
library(tidyterra)

# Get raster.

dat_raster = 
  "data/ARDEMv2.0.nc" %>% 
  rast %>% 
  project("EPSG:3338")

# Get vectors.

#  Alaska's state boundaries retrieved from marinecadastre.gov, provided by BOEM and NOAA.

vec_alaska = 
  "data/Coastal_States" %>% 
  read_sf %>% 
  filter(statename == "Alaska") %>% 
  transmute(state = "Alaska") %>%  
  st_transform("EPSG:3338") %>% 
  st_union

#  Alaska's EEZ boundaries retrieved from marineregions.org, provided by the Flanders Marine Institute.

vec_eez = 
  "data/World_EEZ_v12_20231025" %>% 
  read_sf(layer = "eez_v12") %>% 
  filter(SOVEREIGN1 == "United States") %>% 
  filter(stringr::str_sub(GEONAME, -7, -2) == "Alaska") %>% 
  transmute(state = "Alaska") %>%  
  st_transform("EPSG:3338")

# Get rasters from vectors to crop and mask the raster of interest.

dat_mask = 
  st_union(vec_alaska, vec_eez) %>% 
  st_buffer(1) %>% 
  st_remove_holes %>% # nngeo
  vect %>% # terra
  rasterize(dat_raster,
            touches = TRUE) %>% 
  trim

dat_crop = dat_mask %>% ext

# Crop and mask. This sets up the out-of-bounds layer for ggplot.

dat_oob = 
  dat_raster %>% 
  crop(dat_crop) %>% 
  mask(dat_mask, inverse = TRUE)

# Crop and mask.

dat_less = 
  dat_raster %>% 
  crop(dat_crop) %>% 
  mask(dat_mask)

# Get raster set up for ggplot2.

dat_frame = dat_less %>% as.data.frame(xy = TRUE)

# Get visualization.

elevation_max = 6000 # dat_frame$z %>% max
elevation_min = -8000 # dat_frame$z %>% min
elevation_vec = c(elevation_min, 0, 1, elevation_max)
elevation_col = c("white", "#0F204B", "#FFB612", "white")

vis = 
  ggplot() +
  geom_spatraster(data = dat_less,
                  maxcell = Inf) + # Resampling breaks sea level, which is silly.
  scale_fill_gradientn(limits = c(elevation_min, elevation_max),
                       breaks = c(elevation_min, 0, elevation_max),
                       values = scales::rescale(elevation_vec),
                       colors = elevation_col,
                       na.value = "transparent",
                       guide = 
                         guide_colorbar(direction = "horizontal",
                                        position = "bottom",
                                        # barheight = ,
                                        # barwidth = ,
                                        frame.colour = "black",
                                        ticks.colour = NA)) +
  labs(fill = "Meters From Sea Level") +
  theme_void()

# geom_spatraster(data = dat_oob %>% clamp(upper = 0, value = FALSE),
#                 fill = "grey75",
#                 na.value = "transparent",
#                 maxcell = 100) +
# geom_spatraster(data = dat_oob %>% clamp(lower = 0, value = FALSE),
#                 fill = "grey25",
#                 maxcell = 100) +

# layer over ggplots

# trick: add third layer of masking on latitude, longitude to get mask following Ardem folded globe visualization

ggsave("vis.png",
       vis,
       dpi = 300,
       width = 6.5)
