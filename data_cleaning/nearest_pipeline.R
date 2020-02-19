### Finding closest natural gas pipelines to points ###
### 9/26/2019 ###

# This script creates a csv of the minimum cost of connecting to the closest existing natural gas pipeline.
# Inputs are a pipeline shapefile and facility shapefile.

# Preamble
library(rgdal)
library(dplyr)
library(rgeos)
library(sp)
library(geotools)
library(geosphere)

######################################
### Subset pipeline shp to CA only ###
######################################

pipelines <- readOGR("Pre_analysis/Raw Data/NaturalGas_Pipelines_US_201804", "NaturalGas_Pipelines_US_201804")
county <- readOGR("geospatial_data/county_wgs84", "county_wgs84")

# ensuring same CRS 
pipelines <- spTransform(pipelines, crs(county))

# intersecting the two shapefiles
overlap <- intersect(pipelines, county)

# Creating identifiers 
num <- 1:NROW(overlap@data)
overlap$ID <- paste("P", num, sep="")

# write shapefile
writeOGR(overlap, dsn = "geospatial_data/CA_NatGas_Pipelines", layer = "CA_NatGas_Pipelines", driver = "ESRI Shapefile", overwrite_layer=T)

##############################
### Find closest neighbors ###
##############################

# capacity
ad_ex <- st_read("geospatial_data/AD_point", "AD_point")
lmop_ex <- st_read("geospatial_data/lmop_existing", "lmop_existing")
wwtp <- st_read("geospatial_data/wwtp", "wwtp")
city_po <- st_read("geospatial_data/potential_sites", "potential_sites")
lmop_po <- st_read("geospatial_data/lmop_candidate", "lmop_candidate")
manure_20 <- st_read("geospatial_data/manure_2020", "manure_2020")
manure_50 <- st_read("geospatial_data/manure_2050", "manure_2050")
# pipeline
pipeline <- st_read("geospatial_data/CA_NatGas_Pipelines", "CA_NatGas_Pipelines")
# sequestration sites
seq <- st_read("geospatial_data/ca_ccs_potential", "ca_ccs_potential")

# Combining shapefiles for candidate sites
site20 <- rbind.fill(ad_ex, wwtp, city_po, manure_20, lmop_ex, lmop_po)
site20 <- site20[,c("ID", "geometry")]
site20 <- st_as_sf(site20)
site50 <- rbind.fill(ad_ex, lmop_ex, lmop_po, wwtp, city_po, manure_50)
site50 <- site50[,c("ID", "geometry")]
site50 <- st_as_sf(site50)

# Reproject
site20 <- st_transform(site20, crs=3857)
site50 <- st_transform(site50, crs=3857)
pipeline <- st_transform(pipeline, crs=3857)

# then, use 'st_nn' to find nearest neighbor, and return the distance to the nearest feature
require(nngeo)
pipe_n20 <- st_nn(site20, pipeline, k = 1, maxdist = Inf, returnDist=T)
pipe_n50 <- st_nn(site50, pipeline, k = 1, maxdist=Inf, returnDist=T)

## Turning results into df
results = list(pipe_n20=pipe_n20, pipe_n50 = pipe_n50)
listi <- list()
for (i in names(results)){
  list2env(results[[i]], envir=.GlobalEnv)
  df <- cbind(data.frame(unlist(nn)), data.frame(dist))
  listi[[i]] <-  df
}
list2env(listi, envir=.GlobalEnv)

## Merging with ID and geometry
pipeline$row <- as.integer(base::row.names(pipeline))
names(pipe_n20) <- c("row", "distance")
pipeline_id <- data.frame(pipeline[,c("ID", "row")])
pipe_n20 <- cbind(site20$ID, pipe_n20)
pipe_n20 <- left_join(pipe_n20, pipeline_id, by = "row")

names(pipe_n50) <- c("row", "distance")
pipe_n50 <- cbind(site50$ID, pipe_n50)
pipe_n50 <- left_join(pipe_n50, pipeline_id, by = "row")

## Calculating capital and o&m cost
results = list(pipe_n20=pipe_n20, pipe_n50=pipe_n50)
for (i in names(results)){
  results[[i]]$capital <- results[[i]]$distance * 1000000 / 1000 * 0.621
  results[[i]]$om <- 0.05 * results[[i]]$capital
  results[[i]]$geometry <- NULL
}
list2env(results, envir=.GlobalEnv)

## Writing into csv
library(data.table)
fwrite(pipe_n20, file = "Pre_analysis/Raw Data/nearest_pipe20.csv")
fwrite(pipe_n50, file = "Pre_analysis/Raw Data/nearest_pipe50.csv")