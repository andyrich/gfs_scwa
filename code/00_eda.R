# eda
library(tidyverse)
library(sf)
library(here)
library(mapview)

# source all functions
walk(list.files(here("code", "functions"), full.names = TRUE), ~source(.x))

# load env vars used across modules as objects in .GlobalEnv
f_load_dot_env()

# load b118 basins and remove file path
# data from https://water.ca.gov/programs/groundwater-management/bulletin-118
b118 <- file.path(data_path, "general", "b118", "i08_B118_v6-1.shp") %>% 
  f_load_b118()

# gsas: petaluma, sonoma valley, santa rosa plain
son <- filter(b118, Basin_Su_1 == "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- filter(b118, Basin_Su_1 == "PETALUMA VALLEY")
srp <- filter(b118, Basin_Su_1 == "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
mapview(list(son, pet, srp))

# load pumpage
p <- file.path(data_path, "srp", "pumpage", "SRP_future_baseline_qpercell_shallow.shp") %>% 
  st_read() %>% 
  st_centroid() %>%
  st_transform(epsg)

# load parcels from public sonoma county data portal
# https://gis-sonomacounty.hub.arcgis.com/pages/data
parcel <- st_read(file.path(data_path, "general", "parcel", "Parcels_Public_Shapefile.shp"))

# guide data
g <- readxl::read_xlsx(file.path(data_path, "srp", "parcel", "Santa Rosa Plain GSA Qualified Parcel List 2021March9.xlsx"))

# filter outliers to improve vis
quantile(p$q, 0.95)
p <- filter(p, q <= quantile(p$q, 0.95))

ggplot() +
  geom_sf(data = srp) + 
  geom_sf(data = p, aes(fill = q), alpha = 0.5, cex = 0.5)
  
