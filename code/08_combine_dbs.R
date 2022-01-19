library(tidyverse)
library(sf)
library(here)
library(fs)

# read complete DBs
psrp <- read_rds(path(data_path, "data_output/srp_parcel_complete.rds"))
pson <- read_rds(path(data_path, "data_output/son_parcel_complete.rds")) 
ppet <- read_rds(path(data_path, "data_output/pet_parcel_complete.rds"))

# ensure same crs
map_dbl(list(psrp, pson, ppet), ~st_crs(.x)$epsg) 

# combine
all <- bind_rows(psrp, pson, ppet)

# write to shp and csv
all %>% 
  st_drop_geometry() %>% 
  write_csv(here("data_output/soco_gsas_parcel.csv"))

st_write(all, here("data_output/shp/soco_gsas_parcel.shp"))
st_write(all, here("data_output/soco_gsas_parcel.geojson"))
