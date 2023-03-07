# Imports -------

library(tidyverse)
library(lubridate)
library(tidycensus)

library(tigris)
library(sf)
library(h3jsr)

source("utils/helpers.R")


# Get block groups for a county using FIPS -----------
get.block.groups <- function(state, county, year) {
  block_groups(state=state, county=county, cb=T, year=year) %>% 
    st_transform(crs='WGS84') %>% 
    dplyr::select(GEOID, ALAND, AWATER) # returns cook.bg
}


# Get boundaries or neighborhoods for chi or cook --------
get.geographies <- function(area, type, return.geojson=F){
  # type is either boundaries or neighborhoods
  # area is either chi or cook
  
  fname=paste0('data/',area,'-',type,'.geojson')

  if(return.geojson){
    readLines(fname) %>% paste(collapse = "\n") # returns area.bounds.geojson / area.nbh.geojson
  } else{
    read_sf(fname) # returns area.bounds / area.nbh
  }
}

# Find all h3 cells in a bounded area ------
convert.to.h3 <- function(area.bounds, hex.size=8){
  #  hex length of 10 corresponds to about 1 block, about 6-7 hexes of res 9 fit into the loop, one of hex 8 covers most of loop
  area.h3.all <- polygon_to_cells(area.bounds, res=hex.size, simple=F)
  area.h3 <- cell_to_polygon(unlist(area.h3.all$h3_addresses), simple=F)
  area.h3
}


# Map neighborhood names for each area -------- 
get.chi.matches <- function(cook.area.centroid, chi.nbh, id_cols){
  st_join(cook.area.centroid, chi.nbh, join=st_within, left=T) %>% 
    dplyr::select(all_of(id_cols), NAME_NEIGH=pri_neigh, NAME_NEIGH_BACKUP=sec_neigh) %>% 
    st_drop_geometry() %>% as_tibble()
}

get.cook.matches <- function(cook.area.centroid, cook.nbh, id_cols){
  st_join(cook.area.centroid, cook.nbh, join=st_within, left=T) %>% 
    dplyr::select(all_of(id_cols), NAME_TOWN=township_name) %>% 
    st_drop_geometry() %>% as_tibble()
}

match.neighborhoods <- function(cook.area, chi.nbh, cook.nbh, id_cols = c('GEOID')){
  # 1. find centroid of area
  cook.area.centroid <- st_centroid(cook.area)
  
  # 2. find which neighborhood that centroid falls into
  chi.area.matches <- get.chi.matches(cook.area.centroid, chi.nbh, id_cols)
  cook.area.matches <- get.cook.matches(cook.area.centroid, cook.nbh, id_cols)
  
  area.matches <- full_join(chi.area.matches, cook.area.matches, by=id_cols) %>%
    mutate(DISPLAY_NAME = coalesce(NAME_NEIGH, NAME_TOWN),
           IS_CITY = !is.na(NAME_NEIGH)
    ) %>%
    dplyr::select(-NAME_NEIGH, -NAME_NEIGH_BACKUP, -NAME_TOWN)
  
  area.matches
}

