### OSRM Directions and Routing ###
### 12/08/2019 ###

# Preamble
library(tidyverse)
library(sf)
library(mapview)
library(osrm)

# Functions
osrm_query <- function(source, destination) {
    # OSRM Query
  listi_distance <- list()  
  listi_duration <- list()
  listj_distance <- list()
  listj_duration <- list()
  for (i in names(source)){ #loop through all 100x100 frames of source
    for(j in names(destination)){ #loop each frame of source for all frames of refinery
      mat <- NULL
      while (is.null(mat) ){
        try (
          mat <- osrmTable(src=source[[i]], dst=destination[[j]], measure = c("duration", "distance")) #note this is in minutes
        )
        message = paste("Sample duration is", mat[['durations']][1,1], "and sample distance is", mat[['distances']][1,1])
        print(message)
      }
      
      listj_distance[[j]] <- mat[['distances']] #save results from loop for all refinery for 1 source frame
      listj_duration[[j]] <- mat[['durations']]
    }
    listi_distance[[i]] <- listj_distance #save results from each source frame for all refinery frames
    listi_duration[[i]] <- listj_duration
  }
  
  blah = list(listi_distance, listi_duration)
}

unlist_n_save <- function(osrm_queries) {
  dd_list <- lapply(osrm_queries, function(x) {
    columns <- list()
    rows <- list()
    for ( i in names(x)){
      for (j in names(x[[i]])){
        columns[[j]] <- x[[i]][[j]]
      }
      c.matrix <- do.call(cbind, columns)
      rows[[i]] <- c.matrix
    }
    flat_mat <- do.call(rbind, rows)
    return(flat_mat)
  })
  names(dd_list) <- c("dist", "time")
  
  return(dd_list)
}

#########################
### Process locations ###
#########################

# Read data
shp <- lapply(list("msw", "crop", "manure", "process", "AD_point", "lmop_existing", "wwtp", "potential_sites", "lmop_candidate", "ca_nonurban_ccs"), function(x) {
  if (x=="msw" | x=="crop" | x=="manure" | x=="process") {
    name <- paste(x, "2020", sep="_") 
  } else {
    name <- x
  }
  path <- paste0("geospatial_data/", name)
  st_read(path, name)
})
names(shp) <- c("msw", "crop", "manure", "process", "AD_point", "lmop_existing", "wwtp", "potential_sites", "lmop_candidate", "ca_nonurban_ccs")

# Get coordinates
coord_list <- lapply(names(shp), function(x) {
  if (x == "wwtp") {
    data.frame(ID=shp[[x]]$ID, AD=shp[[x]]$AD_, st_coordinates(shp[[x]]))
  } else {
    if (x == "ca_nonurban_ccs") {
      seq <- shp[[x]] %>% st_transform(crs=3857) %>% st_centroid() %>% st_transform(crs=4326)
      data.frame(ID=seq$ID, st_coordinates(seq))
    } else {
        data.frame(ID=shp[[x]]$ID, st_coordinates(shp[[x]]))
    }
  }
})
names(coord_list) <- names(shp)
list2env(coord_list, envir=.GlobalEnv)

# Sorting data sets
wwtp_ex <- wwtp %>% filter(AD=="Y") %>% select(ID, X, Y) #filter only for existing AD's at wwtp
feedstock <- rbind(crop, msw, manure, process, lmop_existing, lmop_candidate, wwtp_ex) %>% 
  rename(lon=X, lat=Y) %>% mutate(ID = as.character(ID)) %>% distinct(ID, .keep_all = T) %>% 
  filter(ID!="M20807" & ID!= "CR206046" & ID!= "MW207978"& ID!= "MW207846" & ID!= "CR202981" & ID!= "MW201258" & ID!= "MW203099"& ID!= "FP2038")  #these are on weird islands
refinery <- rbind(AD_point, potential_sites, wwtp_ex, lmop_existing, manure, lmop_candidate) %>% 
  rename(lon=X, lat=Y) %>% mutate(ID = as.character(ID)) %>% distinct(ID, .keep_all = T) %>% 
  filter(ID!="M20807") #these are on weird islands
sequestration <- ca_nonurban_ccs %>% rename(lon=X, lat=Y) %>% select(ID, lon, lat)

# Get accurate coordinates for landfills
for (i in list(feedstock, refinery)) {
  i[i$ID=="LO1", "lon"] <- "-122.083127"
  i[i$ID=="LO1", "lat"] <- "38.018899"
  i[i$ID=="LO6", "lon"] <- "-117.91294"
  i[i$ID=="LO6", "lat"] <- "34.0373"
  i[i$ID=="LO10", "lon"] <- "-122.53431"
  i[i$ID=="LO10", "lat"] <- "38.57673"
  i[i$ID=="LO18", "lon"] <- "-117.92694"
  i[i$ID=="LO18", "lat"] <- "34.02003"
  i[i$ID=="LO21", "lon"] <- "-117.418596"
  i[i$ID=="LO21", "lat"] <- "34.142199"
  i[i$ID=="LO25", "lon"] <- "-117.15227"
  i[i$ID=="LO25", "lat"] <- "32.83663"
  i[i$ID=="LO26", "lon"] <- "-118.10645"
  i[i$ID=="LO26", "lat"] <- "34.03389"
  i[i$ID=="LO28", "lon"] <- "-117.00504"
  i[i$ID=="LO28", "lat"] <- "32.6055"
  i[i$ID=="LO29", "lon"] <- "-122.40626"
  i[i$ID=="LO29", "lat"] <- "37.48021"
  i[i$ID=="LO30", "lon"] <- "-122.13125"
  i[i$ID=="LO30", "lat"] <- "37.44553"
  i[i$ID=="LO36", "lon"] <- "-117.209504"
  i[i$ID=="LO36", "lat"] <- "33.095884"
  i[i$ID=="LO46", "lon"] <- "-120.834209"
  i[i$ID=="LO46", "lat"] <- "38.655503"
  i[i$ID=="LO50", "lon"] <- "-117.15227"
  i[i$ID=="LO50", "lat"] <- "32.83663"
  i[i$ID=="LP1", "lon"] <- "-115.52505"
  i[i$ID=="LP1", "lat"] <- "32.85467"
  i[i$ID=="LP4", "lon"] <- "-122.36093"
  i[i$ID=="LP4", "lat"] <- "40.41739"
  i[i$ID=="LP13", "lon"] <- "-116.30184"
  i[i$ID=="LP13", "lat"] <- "33.25754"
  i[i$ID=="LP18", "lon"] <- "-120.53155"
  i[i$ID=="LP18", "lat"] <- "35.66313"
  i[i$ID=="LP25", "lon"] <- "-122.191387"
  i[i$ID=="LP25", "lat"] <- "39.6323"
  i[i$ID=="LP29", "lon"] <- "-121.323819"
  i[i$ID=="LP29", "lat"] <- "36.8215"
  i[i$ID=="LP35", "lon"] <- "-120.22162"
  i[i$ID=="LP35", "lat"] <- "39.6731"
  i[i$ID=="LP40", "lon"] <- "-121.10132"
  i[i$ID=="LP40", "lat"] <- "38.10342"
  i[i$ID=="LP41", "lon"] <- "-116.0699297"
  i[i$ID=="LP41", "lat"] <- "33.4407794"
  i[i$ID=="LP55", "lon"] <- "-122.29187"
  i[i$ID=="LP55", "lat"] <- "40.20119"
  i[i$ID=="LP58", "lon"] <- "-117.26795"
  i[i$ID=="LP58", "lat"] <- "34.59308"
  i[i$ID=="LP61", "lon"] <- "-121.02757"
  i[i$ID=="LP61", "lat"] <- "40.31282"
} 

# Make 100x100 lists 
source.list <- split(feedstock, (seq(nrow(feedstock))-1) %/% 100)
refin.list <- split(refinery, (seq(nrow(refinery))-1) %/% 100)
seques.list <- split(sequestration, (seq(nrow(sequestration))-1) %/% 100)

# Clean up the environment for memory management
rm(list = setdiff(ls(), c("source.list", "refin.list", "seques.list", "osrm_query", "unlist_n_save")))

####################
### OSRM Queries ###
####################

# If you are setting up a local server, uncomment the following. Note that using the demo server does not give you distance, only time.
#options(osrm.server = "http://0.0.0.0:5000/", osrm.profile="driving")

# Source -> AD sites
fs_refq <- osrm_query(source.list, refin.list)
fs_ref <- unlist_n_save(fs_refq)
rm(fs_refq)
save(fs_ref, file = "Pre_analysis/Directions_data/fsref_list.RData")

# AD sites -> sequestration sites
ref_seqq <- osrm_query(refin.list, seques.list)
ref_seq <- unlist_n_save(ref_seqq)
rm(ref_seqq)
save(ref_seq, file = "Pre_analysis/Directions_data/refseq_list.RData")
