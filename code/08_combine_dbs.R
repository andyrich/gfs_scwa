library(tidyverse)
library(sf)
library(here)
library(fs)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))

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
  write_csv(path(data_path, "data_output/soco_gsas_parcel.csv"))

gjson_out <- path(data_path, "data_output/soco_gsas_parcel.geojson")
shp_out   <- path(data_path, "data_output/shp/soco_gsas_parcel.shp")

if(file_exists(gjson_out)) file_delete(gjson_out)
st_write(all, gjson_out)

if(file_exists(shp_out)) file_delete(shp_out)
st_write(all, shp_out)


# swap field names with SCI field names and write
sci <- read_csv(path(data_path, "general/sci_key.csv"))
names(all) <- sci$new[match(names(all), sci$old)]

shp_sci_out <- path(data_path, "data_output/shp/soco_gsas_parcel_sci.shp")

if(file_exists(shp_sci_out)) file_delete(shp_sci_out)
st_write(all, shp_sci_out)
