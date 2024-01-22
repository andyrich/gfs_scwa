library(tidyverse)
library(sf)
library(here)
library(mapview)
library(fs)
library(furrr)


dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))

data_path <- Sys.getenv("DATA_PATH")

# load data ---------------------------------------------------------------
# b118 data: https://water.ca.gov/programs/groundwater-management/bulletin-118
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsas: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")


# union of all GSAs
gsas <- reduce(list(son, pet, srp), st_union) %>% as("Spatial")

# parcels from public sonoma county data portal
# https://gis-sonomacounty.hub.arcgis.com/pages/data
parcel <- st_read(path(data_path, "general/parcel/GSA Deliverable Data 2023/GSA Deliverable Data 2023/Data/Data_GSA_2023.gdb")) %>% 
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  as("Spatial") 


# parcels intersected to GSA area -----------------------------------------
gsa_parcel <- parcel[gsas, ]
cat(round(nrow(gsa_parcel@data) / nrow(parcel@data) * 100, 2), 
    "% of Sonoma Co parcels within GSAs.")

# some acres are wrong: re-calculate at APN level
gsa_parcel <- st_as_sf(gsa_parcel) %>% 
  mutate(LndSzAcre = st_area(geometry) %>% 
         units::set_units(acres) %>% 
         as.numeric()) %>%
  rename(UseCDesc=UseCode_Description,
         UseCType=UseCode_Category)

print(colnames(gsa_parcel))
# add field of parcels that intersect edge of any basin
gsa_j = bind_rows(pet, srp, son)
gsa_jb <-  st_geometry(st_cast(st_as_sf(gsa_j),'MULTILINESTRING'))
f = st_intersects(st_geometry(gsa_parcel), gsa_jb, sparse=TRUE)
gsa_parcel <- mutate(gsa_parcel,
               edge = ifelse(lengths(f)>0, 'Yes','No'))


# intersect parcels to GSA boundaries and write for later use
aoi_in <- list(son, pet, srp)
file_out <- glue::glue('{c("son", "pet", "srp")}_parcel.rds')
for(i in seq_along(aoi_in)){
  st_intersection(gsa_parcel, aoi_in[[i]]) %>% 
    write_rds(path(data_path, "data_output", file_out[i]))
  cat("Wrote", file_out[i], "\n")
}

# # SRP data from Shelly ----------------------------------------------------
# 
# srp_gdb_path <- path(data_path, "srp", "SRP_GSA_RevisedSchema.gdb")
# cat("Reading in explicit connection data for:\n", 
#     paste(rgdal::ogrListLayers(srp_gdb_path), collapse = "\n "))
# 
# psrp <- rgdal::ogrListLayers(srp_gdb_path)[2] %>% 
#   rgdal::readOGR(dsn = srp_gdb_path, layer = .) %>% 
#   st_as_sf() %>% 
#   st_transform(epsg) %>% 
#   st_make_valid() %>% 
#   as("Spatial")
# 
# # add field of parcels that intersect edge of any basin
# f = st_intersects(st_geometry(st_as_sf(psrp)), gsa_jb, sparse=TRUE)
# psrp <- st_as_sf(psrp) %>%
#   mutate(
#     edge = ifelse(lengths(f)>0, 'Yes','No')) %>%
#   as("Spatial")
# 
# # crop shelly's SRP parcels to GSA boundaries, intersect with SRP and write
# gsa_psrp <- psrp[gsas, ]
# 
# # some acres are wrong: re-calculate at APN level
# gsa_psrp <- gsa_psrp %>% 
#   st_as_sf() %>% 
#   mutate(LandSizeAcres = st_area(geometry) %>% 
#          units::set_units(acres) %>% 
#          as.numeric()) 
# 
# st_intersection(gsa_psrp, srp) %>% 
#   write_rds(path(data_path, "data_output/srp_parcel_shelly.rds"))
