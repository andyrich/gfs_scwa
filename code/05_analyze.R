# analyze completed spatial databases

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)

# groundwater budgets from groundwater flow models
# pumping per user based on models, postprocessed by 01_avg_annual...R, 
pump <- path(data_path, "tables/pump.csv") %>% 
  read_csv()


# parcel water budget -----------------------------------------------------

# SON database
pson <- read_rds(path(data_path, "data_output/son_parcel_complete.rds")) %>% 
  mutate(
    type = ifelse(
      UseCode_Category != "Agricultural" | is.na(UseCode_Category), 
      "M&I plus domestic", "agriculture"),
    basin = "Sonoma")

# PET database
ppet <- read_rds(path(data_path, "data_output/pet_parcel_complete.rds")) %>% 
  mutate(
    type = case_when(
      UseCode_Category == "Agricultural" ~ "agriculture",
      UseCode_Category == "Commercial" ~ "M&I", 
      UseCode_Category %in% c("MultiFamily", "Residential") ~ "Domestic", 
      TRUE ~ UseCode_Category),
    basin = "Petaluma")


# summarise annual pumping averages per user category ---------------------

# GW pumping per user category
son_bucket <- pson %>% 
  st_drop_geometry() %>% 
  group_by(type) %>% 
  summarise(pumpage_af = sum(Total_Groundwater_Use_Ac_Ft, 
                             na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(pumpage_af))

pet_bucket <- ppet %>% 
  st_drop_geometry() %>% 
  group_by(type) %>% 
  summarise(pumpage_af = sum(Total_Groundwater_Use_Ac_Ft, 
                             na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(pumpage_af)) %>% 
  filter(!is.na(type))


# comparison of groundwater model and parcel bucket model -----------------

# SON groundwater model
son_gwm <- pump %>% filter(gsa == "SON")

ggplot() +
  geom_jitter(data = son_gwm, aes(type, pumpage_af), height = 0) +
  geom_point(data = son_bucket, aes(type, pumpage_af), 
             color = "red") 
  

# spatial distribution of pumping -----------------------------------------

# total groundwater use per parcel spatial view
pson %>% 
  group_split(type) %>% 
  mapview(layer.name = c("Agricultural", "M&I + domestic"),
          zcol = "pumpage_af",
          at = seq(0, 500, 100), legend = c(TRUE, FALSE))


