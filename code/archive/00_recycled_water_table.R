library(tidyverse)
library(sf)
library(here)
library(fs)
library(mapview)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")

# read complete DBs
psrp <- read_rds(path(data_path, "data_output/srp_parcel_complete.rds"))
pson <- read_rds(path(data_path, "data_output/son_parcel_complete.rds")) 
ppet <- read_rds(path(data_path, "data_output/pet_parcel_complete.rds"))


# recycled water for son and pet
recy_son <- path(data_path, 
             "son/recycled_water/SVCSD Recycled Water Use APNs.xlsx") %>% 
  readxl::read_xlsx(sheet = 4) %>% 
  rename(Recycled_Water_Use_Ac_Ft = af_yr_2016) %>% 
  mutate(basin = "SON")

recy_pet <- path(
  data_path, 
  "pet/recycled_water/Petaluma_2016_Recyled Water_APN.xlsx") %>% 
  readxl::read_xlsx(sheet = 3) %>%
  select(APN, cubic_feet_adj) %>% 
  group_by(APN) %>% 
  summarise(cubic_feet_adj = sum(cubic_feet_adj, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # convert feet^3 to AF
  mutate(Recycled_Water_Use_Ac_Ft = cubic_feet_adj * 2.29569e-5) %>% 
  select(-cubic_feet_adj) %>% 
  mutate(basin = "PET")

# table of recycled water per area
recycled_water_deliveries <- bind_rows(recy_son, recy_pet) %>% 
  filter(APN %in% c(pson$APN, ppet$APN)) %>% 
  group_by(basin) %>% 
  summarise(sum_gw_af = sum(Recycled_Water_Use_Ac_Ft))

recycled_water_deliveries %>% 
  write_csv(path(data_path, "data_output/recycled_water_deliveries.csv"))
