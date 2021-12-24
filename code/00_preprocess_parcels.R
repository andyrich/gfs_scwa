library(tidyverse)
library(sf)
library(here)
library(mapview)
library(fs)
library(furrr)

# load data ---------------------------------------------------------------
# b118 data: https://water.ca.gov/programs/groundwater-management/bulletin-118
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsas: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
mapview(list(son, pet, srp))

# union of all GSAs
gsas <- reduce(list(son, pet, srp), st_union) %>% as("Spatial")

# parcels from public sonoma county data portal
# https://gis-sonomacounty.hub.arcgis.com/pages/data
parcel <- st_read(path(data_path, "general/parcel/CDR_PARCEL_PUB_SHP_vw.shp")) %>% 
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  # some acres are wrong: re-calculate at APN level
  mutate(LndSzAcre = st_area(geometry) %>% 
           units::set_units(acres) %>% 
           as.numeric()) %>% 
  as("Spatial")


# parcels intersected to GSA area -----------------------------------------
gsa_parcel <- parcel[gsas, ]
cat(round(nrow(gsa_parcel@data) / nrow(parcel@data) * 100, 2), 
    "% of Sonoma Co parcels within GSAs.")

# intersect parcels to GSA boundaries and write for later use
future::plan(multisession, workers = 3)
furrr::future_walk2(list(son, pet, srp), 
      glue::glue('{c("son", "pet", "srp")}_parcel.rds'), 
      ~st_intersection(st_as_sf(gsa_parcel), .x) %>% 
        write_rds(path(data_path, "data_output", .y))
      )
