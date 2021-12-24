# analyze completed spatial databases

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)

# SON database
pson <- read_rds(path(data_path, "data_output/son_parcel_complete.rds")) %>% 
  mutate(UseCode_Category2 = ifelse(
    UseCode_Category != "Agricultural" | is.na(UseCode_Category), 
    "M&I + domestic", "Agricultural"))

# GW pumping per user category
pson %>% 
  st_drop_geometry() %>% 
  group_by(UseCode_Category2) %>% 
  summarise(total_gw_use_ac_ft = sum(Total_Groundwater_Use_Ac_Ft, 
                                     na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(total_gw_use_ac_ft))

# total groundwater use per parcel spatial view
pson %>% 
  group_split(UseCode_Category2) %>% 
  mapview(layer.name = c("Agricultural", "M&I + domestic"),
          zcol = "Total_Groundwater_Use_Ac_Ft",
          at = seq(0, 500, 100), legend = c(TRUE, FALSE))


