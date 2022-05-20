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

# assign JurisDiction, then selet record below
psrp$GSA_Jurisdiction_Prelim <- "Santa Rosa Plain"
ppet$GSA_Jurisdiction_Prelim <- "Petaluma Valley"
pson$GSA_Jurisdiction_Prelim <- "Sonoma Valley"

# combine
all <- bind_rows(psrp, pson, ppet)

# find parcel with record with biggest area -- assign that basin to Juris

#find duplicated and non-dupliacted APN's
nondup <- all[!(duplicated(all$APN) |duplicated(all$APN, fromLast=TRUE)) ,]
dup <- all[duplicated(all$APN) | duplicated(all$APN, fromLast = TRUE),]

# sorting the data by the column
# required in descending order
dup <- dup[order(dup$LandSizeAcres,
                 decreasing = TRUE), ]

# select top 1 values from each group
dup <- Reduce(rbind,                                
              by(dup,
                 st_drop_geometry(dup["APN"]),
                 head,
                 n = 1))

all <- bind_rows(dup, nondup)

all$GSA_Jurisdiction_Modified <-'No'
all$GSA_Jurisdiction_Mod_Value <-NA
all$GSA_Jurisdiction <-all$GSA_Jurisdiction_Prelim

# set to zero parcels with less than 0.1 AF
all <- mutate(all,Total_Groundwater_Use_Ac_Ft = 
              ifelse(Total_Groundwater_Use_Ac_Ft<0.1, 0,
                     Total_Groundwater_Use_Ac_Ft))

# write to shp and csv
print('done writing csv output')
all %>% 
  st_drop_geometry() %>% 
  write_csv(path(data_path, "data_output/soco_gsas_parcel.csv"))

gjson_out <- path(data_path, "data_output/soco_gsas_parcel.geojson")
shp_out   <- path(data_path, "data_output/shp/soco_gsas_parcel.shp")




print('writing geojson')
if(file_exists(gjson_out)) file_delete(gjson_out)
# st_write(all, gjson_out)

# print('writing shapefile')
# if(file_exists(shp_out)) file_delete(shp_out)
# st_write(all, shp_out)

all_og_labels <- all
# write prmd formatted files
all <- relable_parc(all)

# round numeric values
cols<- colnames(select_if(all,is.numeric))
cols = cols[! cols %in% c('geometry')]

fill_round <- function(x) replace(x, is.na(x),0.)
all[,cols] <- lapply(all[,cols], fill_round)
all <- all %>% mutate_if(is.numeric, ~round(., 3))
#remove # from all columns
all <- all %>% mutate_if(is.character, ~gsub('#','',.))


# write to shp and csv
all %>% 
  st_drop_geometry() %>% 
  write_csv(path(data_path, "data_output/soco_gsas_parcel_prmd.csv"))

# shp_out_prmd   <- path(data_path, "data_output/shp/soco_gsas_parcel_prmd.shp")
# if(file_exists(shp_out_prmd)) file_delete(shp_out_prmd)
# st_write(head(all,10), shp_out_prmd)

gjson_out_geom <- path(data_path, "data_output/soco_parcel_geom_only.geojson")
print('writing geojson')
if(file_exists(gjson_out_geom)) file_delete(gjson_out_geom)
st_write(all[,c('geometry','APN')], gjson_out_geom)

# swap field names with SCI field names and write
sci <- read_csv(path(data_path, "general/sci_key.csv"))
names(all_og_labels) <- sci$new[match(names(all_og_labels), sci$old)]

shp_sci_out <- path(data_path, "data_output/soco_gsas_parcel_sci.csv")
print('writing sci shapefile')
if(file_exists(shp_sci_out)) file_delete(shp_sci_out)

# write to csv
all_og_labels %>% 
  st_drop_geometry() %>% 
  write_csv(shp_sci_out)
print('done writing shapefile')
