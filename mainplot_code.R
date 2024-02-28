#core
library(fpp3)
library(tidyverse)

#For Spatial Data
library(sf)
library(stars)
library(terra)

#For Visualisation
library(rayshader)
library(colorspace)
library(MetBrewer)

#for data prep
library(rgeoboundaries)
library(remotes)
#remotes::install_github("wmgeolab/rgeoboundaries")
#remotes::install_github("crazycapivara/h3-r")
library(h3)

#read gpkg file
#for many general mapping and analysis tasks, EPSG 3035 can provide satisfactory results.
#EPSG 3035 corresponds to the ETRS89 / LAEA Europe projection.
population_sf <- read_sf("kontur_population_MY_20231101.gpkg") %>% 
  st_transform("EPSG:3035")

#West Malaysia Boundaries Recognition
Malaysia_boundaries <- geoboundaries(c("Malaysia"))
Malaysia_boundaries

ggplot(data = Malaysia_boundaries) +
  geom_sf()

#remove any entry that has the latitude of > 108
pop <- population_sf
control = 1

while (TRUE) {
  x = pop$h3[control]
  h3index = x
  bbox1 <- h3_to_geo_sf(h3_index = h3index)
  
  lat_min = st_bbox(bbox1)[1]
  
  
  # Check if iteration has completed
  if (lat_min < 108) {
    print(paste("west malaysia data starts from row: ", control))
    break  # Exit the loop
  }
  else{
    control <- control+1
  }
}

######################################################################
#SPIKEPLOT

#filter for west malaysia data
population_sf <- population_sf %>%  slice(77069:nrow(population_sf)) %>% st_transform("EPSG:3035")

#creating a function to get the width and height (bounding box)
get_raster_size <- function(bbox){
  
  #Get height and width in CRS units
  height <- as.vector(bbox[4]-bbox[2]) #max latitude minus min latitude
  width <- as.vector(bbox[3]-bbox[1]) #max longitude minus min longitude
  
  #get the ratio between height and width
  if(height > width){
    height_ratio <- 1
    width_ratio <- width/height
  }
  else{
    width_ratio <- 1
    height_ratio <- height/width
  }
  
  return(list(
    width = width_ratio,
    height = height_ratio
  ))
  
}

#get height and width ratio of the bounding box in CRS
hw_ratio <- get_raster_size(st_bbox(population_sf))
hw_ratio

#resolution size
size = 3000

#configuring resolution of the raster grid
population_stars <- population_sf %>%
  select(population) %>% st_rasterize(nx = floor(size*hw_ratio$width), 
                                      ny = floor(size*hw_ratio$height))
#floor gives the lowest whole number to a decimal number

#converting the stars into matrix that allows for rayshader 3d_plot
population_matrix <- 
  population_stars %>% 
  rast() %>% 
  raster_to_matrix()

## Define palette
pal <- met.brewer("OKeeffe2", n = 10, "continuous")
## Define texture
population_texture <- colorRampPalette(
  colors = pal
)(256)
## Visualize the palette
swatchplot(population_texture)

#3d plot (rayshader)
#size 1000, zscale=120
#size 3000, zscale=40
population_matrix %>% 
  height_shade(texture = population_texture) %>% 
  plot_3d(
    heightmap       = population_matrix,
    solid           = FALSE, 
    soliddepth      = 0,      
    zscale          = 40,
    shadowdepth     = 0,      
    shadow_darkness = .95,    
    windowsize      = c(1000, 1000),
    zoom            = .6,   
    phi             = 50,    
    theta           = 30,   
    background      = "white"
  )

#adjust the angle according to preference
render_camera(
  zoom  = 0.58,
  theta = 70,
  phi   = 30
)

#render
rayshader::render_highquality(
  filename       = "WestMalaysia_Spikeplot.png",
  preview        = TRUE,
  light          = TRUE,
  lightdirection = c(240, 320),
  lightaltitude  = c(20, 80), 
  lightintensity = c(600, 100),
  lightcolor     = c(lighten(pal[7], 0.75), "white"),
  interactive    = FALSE,
  width          = dim(population_stars)[1], 
  height         = dim(population_stars)[2]
)
