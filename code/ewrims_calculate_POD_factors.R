library(tidyverse)
library(fs)
library(here)
library(sf)

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))


# this script joins the POD factors to the parcels. then writes the factors to POD_fraction_of_water_rights.csv
# this file is used for the ewrims processing. it dictates how much of a surface water diversion goes to each
# parcel. it only calculates the factors.
# date 05/16/2023
# author arich

# parcels from public sonoma county data portal
# https://gis-sonomacounty.hub.arcgis.com/pages/data
parcel <- st_read(path(data_path, "general/parcel/CDR_PARCEL_PUB_SHP_vw.shp")) %>% 
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  as("Spatial") %>%
  st_as_sf(  )


print('loading surface water data')

jsonpath <- path(data_path, "general/ewrims", 'water_rights_v3','output',
                 'water_rights_reported_sonoma.geojson')
print('Loading pre-processed Water Right usages and spatial locations')
ewrims <- jsonpath %>%
  st_read() %>%
  st_transform(epsg) %>% 
  st_make_valid() %>%
  st_as_sf(  )


# add surface water use (AF/year) to parcels, but be careful, as some
# parcels have MULTIPLE ewrims points and these must be summarized
ewrims_key <-  sf::st_join(parcel, ewrims) %>%
  st_drop_geometry() %>%
  group_by(APN, application_number) %>%
  summarise(Fraction_of_Water_Right = sum(
    frac, na.rm = TRUE),
    Total_diversion = sum(
      Average*frac, ra.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Fraction_of_Water_Right>0) 


fraction_path <- path(data_path, "general/ewrims", 'water_rights_v3','input',
                 'POD_fraction_of_water_rights.csv')

write_csv(ewrims_key, fraction_path)
