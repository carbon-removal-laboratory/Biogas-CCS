### Cleaning sequestration sites ###
### 3/1/2019 ###

# Last updated: 11/16/2019: Getting depth to basement ratio & cutting shapefile by city shapefiles

# Preamble
library(rgeos)
library(rgdal)
library(dplyr)
library(sf)
library(readxl)
library(geosphere)
library(mapview)
`%notin%` <- Negate(`%in%`)

# Loading sequestration and CA county shapefile 
county <- st_read("geospatial_data/county_wgs84", "county_wgs84")
seq_sites <- st_read("geospatial_data/CCS_potential", "NATCARB_10k_saline_v1502_cost") %>% st_transform(crs=4326)

# Overlaying the two layers
overlap <- st_intersects(seq_sites, county)

# Creating identifiers 
num <- 1:NROW(overlap@data)
overlap@data$ID <- paste("S", num, sep = "")

# Writing new shapefile
st_write(overlap, dsn = "geospatial_data/ca_ccs_potential", layer = "ca_ccs_potential", driver = "ESRI Shapefile")

###############################################
### Cropping injection sites by urban areas ###
###############################################

# Read in shapefiles
city1 <- st_read("geospatial_data/urban_area1", "Adjusted_Urban_Area")
seq <- st_read("geospatial_data/ca_ccs_potential", "ca_ccs_potential") %>% 
  st_transform(st_crs(city1))
depth_basement <- st_read("geospatial_data/Basement_master", "BasementMaster_TealeAlbers_polyline") %>% 
  st_transform(st_crs(city1))
city2 <- st_read(dsn="geospatial_data/urban_area2", layer="calfire_cities") %>% 
  st_transform(st_crs(city1))

# Calculate area of each cell
seq <- seq %>% mutate(seq_area=st_area(.) %>% as.numeric())

# Intersect contour with basin
joined <- st_join(seq, depth_basement, left=T) %>% 
  group_by(ID) %>% 
  mutate(DEPTH = ifelse(is.na(DEPTH), 0, DEPTH)) %>% 
  slice(which.min(DEPTH)) %>% 
  ungroup() %>% 
  mutate(requirement = ifelse(DEPTH>=1500, 1, 0),
         ID = as.character(ID))
#mapview(joined, zcol='requirement') + mapview(depth_basement, col.regions='gray')# manual check

# Manually add in missed contours
missed_sites <- c(1430, 523, 535, 1441, 526, 446, 1239, 878, 803, 1151, 940, 293, 989, 440, 297, 363, 300, 307, 373, 553, 555, 561, 383, 155, 398, 396, 479, 481, 400, 402, 487, 402, 487, 404, 407, 491, 309, 884, 360)
for (i in missed_sites){
  miss <- paste0("S", i)
  joined <- mutate(joined, requirement=ifelse(ID==miss, 1, requirement))
}
joined <- mutate(joined, requirement=ifelse(ID=="S590", 0, requirement)) # False positive
#mapview(joined, zcol='requirement')

# Clip the sequestration layer by city
city_cuts <- list()
count = 1
for (i in list(city1, city2)) {
  pi <- st_intersection(i, joined)
  area <- pi %>% mutate(area=st_area(.) %>% as.numeric(),
                        share = area/seq_area,
                        ID = as.character(ID)) %>% 
    filter(share>0.1) #Area share filter
  nonurban_sites <- joined[joined$ID %notin% area$ID,]
  city_cuts[[count]] <- area
  count = count +1
}

# Find the areas clipped by the areas
areacut1 <- joined[joined$ID %in% city_cuts[[1]]$ID,]
areacut2 <- joined[joined$ID %in% city_cuts[[2]]$ID,]

# View a map
map <- mapview(nonurban_sites, zcol='requirement') + mapview(areacut1, col.regions='steelblue4') + mapview(areacut2, col.regions='steelblue4')
mapshot(map, url = "test/seqmap.html")

# Save the layer
seqsites <- filter(nonurban_sites, requirement==1)
st_write(seqsites, dsn = "geospatial_data/ca_nonurban_ccs", layer = "ca_nonurban_ccs", driver="ESRI Shapefile")
