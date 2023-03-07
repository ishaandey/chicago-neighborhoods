# ----Import packages----

# standard packages for analysis
library(tidyverse)
library(lubridate)

# for pulling census easily (no api key) 
library(tidycensus)
library(RSocrata)

# some geo tools
library(tigris)
library(sf)
library(h3jsr)

# some map and display tools
library(leaflet)
library(leaflet.extras)
library(leafsync)

library(htmltools)
library(widgetframe)

# utilities
source("utils/helpers.R")
source("utils/metrics.R")
source("utils/visualization.R")
source("utils/geography.R")


# ----Define geometries --------


#  get geoms, neighborhoods and boundaries
ACS_YEAR = 2021
cook.bg <- get.block.groups(state=17, county=031, year=ACS_YEAR) 

chi.bounds  <- get.geographies('chi', 'boundaries')
cook.bounds <- get.geographies('cook', 'boundaries')

chi.nbh  <- get.geographies('chi', 'neighborhoods')
cook.nbh <- get.geographies('cook', 'neighborhoods')

HEX_SIZE = 8
chi.h3 <- convert.to.h3(chi.bounds, HEX_SIZE)
cook.h3 <- convert.to.h3(cook.bounds, HEX_SIZE)

bg.matches <- match.neighborhoods(cook.bg, chi.nbh, cook.nbh, id_cols=c('GEOID'))
h3.matches <- match.neighborhoods(cook.h3, chi.nbh, cook.nbh, id_cols=c('h3_address'))

# ---- ACS data workflow -----

# Define.race.vars 
# https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html
# B02001_001: Total
# B03002_003: White alone (Not Hispanic or Latino)
# B03002_004: Black or African American alone (Not Hispanic or Latino)
# B03002_012: Hispanic or Latino
# B03002_005: Native American alone (Not Hispanic or Latino)
# B03002_006: Asian alone (Not Hispanic or Latino)
# B03002_007: Native Hawaiian or Pacific Islander alone (Not Hispanic or Latino)
# B03002_009: Multiple Races (Not Hispanic or Latino)
# B03002_008: Other (Not Hispanic or Latino)

race.vars <- c(pop.TOT = "B02001_001", 
               pop.WHT = "B03002_003", 
               pop.BLK = "B03002_004",
               pop.HLT = "B03002_012", 
               pop.NAM = "B03002_005", 
               pop.ASN = "B03002_006", 
               pop.HPI = "B03002_007", 
               pop.MUL = "B03002_009", 
               pop.OTH = "B03002_008")


other.vars <- c() # income, poverty, housing characteristics, industry

# get raw ACS data
cook.acs.raw <- get_acs(geography = 'block group', year = ACS_YEAR, 
                    variables = race.vars, 
                    state=17, county=031, output = 'wide')

# remove margin (error) estimates
cook.acs <- cook.acs.raw %>%
  dplyr::select(-ends_with('M')) 


# make full acs table at bg level
cook.acs.bg <- cook.acs %>% 
  left_join(bg.matches) %>% 
  left_join(cook.bg, by=c('GEOID'), suffix=c('','_BG')) %>%
  st_as_sf() %>%
  st_transform(4326) # not needed i think


# make full acs table at h3 level (interpolate hex data from bg data)

cook.acs.h3.ext <- st_interpolate_aw(cook.acs.bg %>% dplyr::select(starts_with('pop.'), ALAND, AWATER), 
                                     cook.h3, extensive = T, keep_NA=T)
# cook.acs.h3.int <- st_interpolate_aw(cook.acs.bg %>% dplyr::select(starts_with('prp.')),
#                                      cook.h3, extensive = F, keep_NA=T)
cook.acs.h3 <- bind_cols(cook.h3, 
                         cook.acs.h3.ext %>% st_drop_geometry() %>% dplyr::select(-`Group.1`), 
                         # cook.acs.h3.int %>% st_drop_geometry() %>% dplyr::select(-`Group.1`)
                         ) %>% as_tibble() %>% st_as_sf() %>% st_transform(4326) %>% 
  left_join(h3.matches) 




# add crime data

# chi.crime <- read.socrata(
#   "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
#   # app_token = "YOURAPPTOKENHERE",
#   # email     = "user@example.com",
#   # password  = "fakepassword"
# )

chi.crime <- read_csv('data/chi-crimes-2022.csv') %>%
  na.omit %>%
  st_as_sf(coords=c('Longitude', 'Latitude'), crs='wgs84')


# create datasets for bg and h3s
cook.data.bg <- cook.acs.bg %>% 
  get.race.proportions %>% 
  get.DI.score %>% # ID to add shannon index or smth
  get.prv.dif.scores(id_cols = c('GEOID','NAME')) %>% 
  get.density() %>%
  get.crime.counts(chi.crime) %>%
  filter(IS_CITY)

cook.data.h3 <- cook.acs.h3 %>% 
  get.race.proportions %>% 
  get.DI.score %>% 
  get.prv.dif.scores(id_cols = c('h3_address')) %>% 
  get.density() %>% 
  get.crime.counts(chi.crime) %>% 
  filter(IS_CITY)


# filter to chi only
# create chi.data.bg and chi.data.h3 by filtering geoms within city boundaries


# remove leftover data
rm(cook.acs.h3.ext, cook.acs.h3.int)
# rm(cook.acs.bg, cook.acs.h3)

# ----Crime data-----





# ----Visualize shit----

# make the base map
basemap.bg <- leaflet(data=cook.data.bg) %>%
  setView(lat=41.881832,lng=-87.623177, zoom = 10) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addMyResetMapButton()

basemap.h3 <- leaflet(data=cook.data.h3) %>%
  setView(lat=41.881832,lng=-87.623177, zoom = 10) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addMyResetMapButton() 

  
## ----cmap------------------------------------------------------------------------------------------------------------------
# https://coolors.co/2a7a9b-2e86ab-68618f-a23b72-f18f01-c73e1d-a43721-812f24-3b1f2b-4d333e

N_BINS = 10

max.alpha = .5 # controls seethrough strength, where 1 is fully opaque
color.NA <- "#D3DADC" # CartoDB gray
color.primary <- "#527CA2"
color.accent <- "#68618F"
color.warning <- "#f18f01"
# color.base <- "#F1E3D3"
color.base <- "#ffffff"
palette.diverging <- .c('#780000', '#c1121f', '#fdf0d5', '#003049', '#669bbc')
palette.list <- c("#ca7016","#74aa9f","#5096b9","#bf7381", "#a292c8","#a292c8","#db9080","#5E6D97")


cmap.scale <- colorNumeric(c(color.base,color.primary), min.max(1:101), na.color = color.NA) # for pcts
cmap.scale2 <- colorNumeric(c(color.base,color.accent), min.max(1:101), na.color = color.NA) # for pcts
cmap.scale3 <- colorNumeric(c(color.base,color.warning), min.max(1:101), na.color = color.NA) # for pcts

cmap.decile <- colorNumeric(c(color.base,color.primary), 1:N_BINS, na.color = color.NA) # for deciles
cmap.decile2 <- colorNumeric(c(color.base,color.accent), 1:N_BINS, na.color = color.NA) # for deciles
cmap.decile3 <- colorNumeric(c(color.base,color.warning), 1:N_BINS, na.color = color.NA) # for deciles

cmap.den.bg <- colorNumeric(c(color.base,color.primary), log10(cook.data.bg$DENSITY), na.color = color.NA)
cmap.den.h3 <- colorNumeric(c(color.base,color.primary), cook.data.h3$DENSITY, na.color = color.NA)
# for population

race.labels <- c('White', 'Black', 'Hispanic/Latino', 'Asian', 'Native American', 'Pacific Islander', 'Multiple', 'Other')
# race.labels <- c('White only', 'Black only', 'Hispanic/Latino', 'Asian only', 'Native American only', 'Pacific Islander only', 'Multiple', 'Other')
race.codes <-  c('WHT', 'BLK', 'HLT', 'ASN', 'NAM', 'HPI', 'MUL', 'OTH')
race.lookup <- data.frame(cd = race.codes, lbl = race.labels)

cmap.race <- colorFactor(palette = palette.list, levels = race.codes, na.color = color.NA)
cmap.race.legend <- colorFactor(palette = palette.list, levels = race.labels, na.color = color.NA)



## ----bg.pop----------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Neighborhood/Township:</strong> ", cook.data.bg$DISPLAY_NAME,
                 "<br><strong>Selected Population:</strong> ", prettify(cook.data.bg$pop.TOTE),
                 "<br><strong>Selected Pop. Density:</strong> ", prettify(cook.data.bg$DENSITY), " persons/mi<sup>2</sup>"
                 ) %>% lapply(htmltools::HTML)

m.pop.bg <- basemap.bg %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.den.bg(log10(cook.data.bg$DENSITY)),
    opacity = max.alpha,
    label = ~ labels
  ) %>%
  addLegend(pal = cmap.den.bg, values = log10(cook.data.bg$DENSITY), 
            title = htmltools::HTML("Population<br>Density<br>(log persons/mi<sup>2</sup>)"),
            opacity = max.alpha, na.label='No Data',  bins=4, data=10*c(1:5),
            labFormat = labelFormat(transform = function(x) 10^x, 
                                    digits=0)
  )


## ----h3.pop----------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Neighborhood/Township:</strong> ", cook.data.h3$DISPLAY_NAME,
                 "<br><strong>Selected Population:</strong> ", prettify(cook.data.h3$pop.TOTE),
                 "<br><strong>Selected Pop. Density:</strong> ", prettify(cook.data.h3$DENSITY), " persons/mi<sup>2</sup>"
                 ) %>% lapply(htmltools::HTML)

m.pop.h3 <- basemap.h3 %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.den.h3(cook.data.h3$DENSITY),
    opacity = max.alpha,
    fillOpacity = max.alpha,
    label = ~ labels
  ) %>%
  addLegend(pal = cmap.den.h3, values = cook.data.h3$DENSITY, 
            title = htmltools::HTML("Population<br>Density<br>(persons/mi<sup>2</sup>)"),
            opacity = max.alpha, na.label='No Data',  bins=5, data=0:1*1e6,
            labFormat = labelFormat(digits=0)
  )


## ----bg.di-----------------------------------------------------------------------------------------------------------------
cook.data.bg$DI.decile <- ntile(cook.data.bg$DI, 10)

labels <- paste0("<strong>Area:</strong> ", cook.data.bg$DISPLAY_NAME,
                 "<br><strong>DI:</strong> ", prettify.pct(cook.data.bg$DI), 
                 "<br><strong>Decile:</strong> ", prettify.rank(cook.data.bg$DI.decile),
                 "<br><strong>Total Pop.:</strong> ", prettify(cook.data.bg$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)

m.di.bg <- basemap.bg %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.decile2(cook.data.bg$DI.decile),
    opacity = max.alpha,
    fillOpacity = ~ min.max(cook.data.bg$DI, max.alpha),
    label = ~ labels
  ) %>%
  addLegend(title = htmltools::HTML("Diversity Index <br>(Decile)"),
            # pal = cmap.decile, values = cook.data.bg$DI.decile, 
            opacity = max.alpha,
            colors = cmap.decile2(c(1,5,10)), labels = c('1st','5th','10th'),
  )


## ----h3.di-----------------------------------------------------------------------------------------------------------------
cook.data.h3$DI.decile <- ntile(cook.data.h3$DI, 10)

labels <- paste0("<strong>Area:</strong> ", cook.data.h3$DISPLAY_NAME,
                 "<br><strong>DI:</strong> ", prettify.pct(cook.data.h3$DI), 
                 "<br><strong>Decile:</strong> ", prettify.rank(cook.data.h3$DI.decile),
                 "<br><strong>Population:</strong> ", prettify(cook.data.h3$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)

m.di.h3 <- basemap.h3 %>%
  addPolygons(
    weight = 1,
    # color = ~ cmap.scale(cook.data.h3$DI),
    color = ~ cmap.decile2(cook.data.h3$DI.decile),
    opacity = max.alpha,
    fillOpacity = ~ min.max(cook.data.h3$DI, max.alpha),
    label = ~ labels
  ) %>%
  addLegend(title = htmltools::HTML("Diversity<br>Index<br>(Decile)"),
            opacity = max.alpha,  
            colors = cmap.decile2(c(1,5,10)), labels = c('1st','5th','10th'),
            # pal = cmap.decile, values = cook.data.h3$DI.decile,
            # na.label='No Data', bins=5, data=1:10,
            # labFormat = labelFormat(digits=0, suffix='th')
  )


## ----bg.top1---------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Area:</strong> ", cook.data.bg$DISPLAY_NAME,
                 "<br><strong>Race:</strong> ", cook.data.bg$RACE_PRV_TOP1 %>% prettify.race(race.lookup),
                 "<br><strong>Pct.:</strong> ", prettify.pct(cook.data.bg$PRP_PRV_TOP1),
                 "<br><strong>Total Pop.:</strong> ", prettify(cook.data.bg$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)

m.top1.bg <- basemap.bg %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.race(RACE_PRV_TOP1),
    opacity = max.alpha/2,
    # opacity = ~ min.max(PRP_PRV_TOP1, max.alpha),
    fillOpacity = ~ min.max(PRP_PRV_TOP1, max.alpha),
    label = ~ labels
    
  ) %>%
  addLegend(pal = cmap.race.legend, values = ~race.labels[1:4], 
            title = htmltools::HTML("Prevailing Race"),
            opacity = max.alpha,
  )


## ----bg.top2---------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Area:</strong> ", cook.data.bg$DISPLAY_NAME,
                 "<br><strong>Race:</strong> ", cook.data.bg$RACE_PRV_TOP2 %>% prettify.race(race.lookup),
                 "<br><strong>Pct.:</strong> ", prettify.pct(cook.data.bg$PRP_PRV_TOP2,'%'),
                 "<br><strong>Total Pop.:</strong> ", prettify(cook.data.bg$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)


m.top2.bg <- basemap.bg %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.race(RACE_PRV_TOP2),
    opacity = max.alpha/2,
    fillOpacity = ~ min.max(PRP_PRV_TOP2, max.alpha),
    label = ~ labels
    
  ) %>%
  addLegend(pal = cmap.race, values = ~race.codes, 
            title = htmltools::HTML("2nd-Most <br>Prevailing <br>Race"),
            opacity = .8,
  )




## ----h3.top1---------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Area:</strong> ", cook.data.h3$DISPLAY_NAME,
                 "<br><strong>Race:</strong> ", cook.data.h3$RACE_PRV_TOP1,
                 "<br><strong>Pct.:</strong> ", sprintf("%.2f", cook.data.h3$PRP_PRV_TOP1),
                 "<br><strong>Pop.:</strong> ", sprintf("%.0f",cook.data.h3$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)

m.top1.h3 <- basemap.h3 %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.race(RACE_PRV_TOP1),
    opacity = max.alpha/2,
    fillOpacity = ~ min.max(PRP_PRV_TOP1, .2, .8),
    label = ~ labels
    
  ) %>%
  addLegend(pal = cmap.race.legend, values = ~race.codes, 
            title = htmltools::HTML("Prevailing Race"),
            opacity = .8,
  )


## ----h3.top2---------------------------------------------------------------------------------------------------------------
labels <- paste0("<strong>Area:</strong> ", cook.data.h3$DISPLAY_NAME,
                 "<br><strong>Race:</strong> ", cook.data.h3$RACE_PRV_TOP2,
                 "<br><strong>Pct.:</strong> ", sprintf("%.2f", cook.data.h3$PRP_PRV_TOP2),
                 "<br><strong>Pop.:</strong> ", sprintf("%.0f",cook.data.h3$pop.TOTE)
                 ) %>% lapply(htmltools::HTML)

m.top2.h3 <- basemap.h3 %>%
  addPolygons(
    weight = 1,
    color = ~ cmap.race(RACE_PRV_TOP2),    
    opacity = max.alpha/2,
    fillOpacity = ~ min.max(PRP_PRV_TOP2, .2, .8),
    label = ~ labels
    
  ) %>%
  addLegend(pal = cmap.race, values = ~race.codes, 
            title = htmltools::HTML("2nd-Most Prevailing Race"),
            opacity = .8,
  )

## ---- crime.h3 ------
cook.data.h3$crime.total.decile <- ntile(cook.data.h3$crime.total, 10)

labels <- paste0("<strong>Area:</strong> ", cook.data.h3$DISPLAY_NAME,
                 "<br><strong>Total Crime (2022):</strong> ", sprintf('%.0f',cook.data.h3$crime.total),
                 "<br><strong>Crime per 1,000 residents (2022):</strong> ", sprintf('%.0f',cook.data.h3$crime.per.capita),
                 "<br><strong>Decile: </strong>", prettify.rank(cook.data.h3$crime.total.decile)
) %>% lapply(htmltools::HTML)

m.crime.h3 <- basemap.h3 %>% 
  addPolygons(
    weight = 1,
    color = ~ cmap.decile3(cook.data.h3$crime.total.decile),
    opacity = max.alpha,
    fillOpacity = ~ min.max(cook.data.h3$crime.total.decile, max.alpha),
    label = ~ labels
  ) %>% 
  addLegend(title = htmltools::HTML("Crime Rate<br>(Decile)"),
            opacity = max.alpha,  
            colors = cmap.decile3(c(1,5,10)), labels = c('1st','5th','10th'),
            # pal = cmap.decile, values = cook.data.h3$DI.decile,
            # na.label='No Data', bins=5, data=1:10,
            # labFormat = labelFormat(digits=0, suffix='th')
  )



## ---- crime.bg ------
cook.data.bg$crime.total.decile <- ntile(cook.data.bg$crime.total, 10)

labels <- paste0("<strong>Area:</strong> ", cook.data.bg$DISPLAY_NAME,
                 "<br><strong>Total Crime (2022):</strong> ", sprintf('%.0f',cook.data.bg$crime.total),
                 "<br><strong>Crime per 1,000 residents (2022):</strong> ", sprintf('%.0f',cook.data.bg$crime.per.capita),
                 "<br><strong>Decile: </strong>", prettify.rank(cook.data.bg$crime.total.decile)
) %>% lapply(htmltools::HTML)

m.crime.bg <- basemap.bg %>% 
  addPolygons(
    weight = 1,
    color = ~ cmap.decile3(cook.data.bg$crime.total.decile),
    opacity = max.alpha,
    fillOpacity = ~ min.max(cook.data.bg$crime.total.decile, max.alpha),
    label = ~ labels
  ) %>% 
  addLegend(title = htmltools::HTML("Crime Rate<br>(Decile)"),
            opacity = max.alpha,  
            colors = cmap.decile3(c(1,5,10)), labels = c('1st','5th','10th'),
            # pal = cmap.decile, values = cook.data.bg$DI.decile,
            # na.label='No Data', bins=5, data=1:10,
            # labFormat = labelFormat(digits=0, suffix='th')
  )


## ---- out.width='100%'-----------------------------------------------------------------------------------------------------
sync(m.top1.h3,
     m.top2.h3,
     # m.top3.h3,
     sync.cursor = F, ncol = 2
     )




