library(tidyverse)
library(fs)
library(here)

# ddw reported data
l <- fs::dir_ls(path(data_path, "general/pws_water_use"), glob = "*.csv") %>% 
  map(~read_tsv(.x) %>% 
        select(pwsid = PWSID, unit = `WP Units of Measure`, gw_af = `WP Annual GW`) %>% 
        # convert gallons and million gallons to acre-feet
        filter(unit %in% c("AF","G","MG")) %>% 
        mutate(gw_af = case_when(
          unit == "G"  ~ gw_af * 3.06889e-6,
          unit == "MG" ~ gw_af * 3.06889,
          TRUE ~ gw_af)
        )
      )

d <- bind_rows(l)

# all water systems we need data for
dw <- all$CA_DrinkingWater_SvcArea_Name %>% unique()

# pwsids of systems we need data for
pwsid_key <- path(data_path, "general", "water_system_boundaries",
            "SABL_Public_083121/SABL_Public_083121.shp") %>%
  st_read() %>%
  st_drop_geometry() %>% 
  filter(WATER_SY_1 %in% dw) %>% 
  select(name = WATER_SY_1, pwsid = SABL_PWSID)

# calcualte average annual gw use from 2013-2019
muni_gw <- d %>% 
  left_join(pwsid_key) %>% 
  filter(name %in% dw & !is.na(name)) %>% 
  # incorrect units create errors for two entries - remove them
  filter(gw_af < 11000) %>% 
  group_by(name) %>% 
  summarise(gw_af = mean(gw_af)) %>% 
  ungroup() 

tibble(name = dw[! dw %in% muni_gw$name]) %>% 
  left_join(pwsid_key) %>% 
  filter(!is.na(name))
