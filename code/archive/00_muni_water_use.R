library(tidyverse)
library(fs)
library(here)
library(sf)

data_path <- Sys.getenv("DATA_PATH")

# ddw reported data 2013-2019
l <- fs::dir_ls(path(data_path, "general/pws_water_use"), glob = "*.csv") %>% 
  map(~read_tsv(.x) %>% 
        mutate(year = str_remove_all(basename(.x), "EAR|LWS.csv|SWS.csv")) %>% 
        select(pwsid = PWSID, 
               unit = `WP Units of Measure`, 
               gw_af = `WP Annual GW`,
               year) %>% 
        # convert gallons and million gallons to acre-feet
        filter(unit != "-" & !is.na(unit)) %>%
        mutate(gw_af = case_when(
          unit == "G"   ~ gw_af * 3.06889e-6,
          unit == "TG"  ~ gw_af * 3.06889e-3,
          unit == "MG"  ~ gw_af * 3.06889,
          unit == "CCF" ~ gw_af * 0.00229569,
          TRUE ~ gw_af)
        )
      )

# ddw data for 2020
l2 <- fs::dir_ls(path(data_path, "general/pws_water_use"), glob = "*xlsx") %>% 
  readxl::read_xlsx() %>% 
  select(pwsid = PwsID, 
         unit  = WPUnitsofMeasure, 
         gw_af = WPAnnualGW) %>% 
  mutate(year = "2020") %>% 
  # convert gallons, million gallons, ccf to acre-feet
  filter(unit != "-" & !is.na(unit)) %>%
  mutate(gw_af = case_when(
    unit == "G"   ~ gw_af * 3.06889e-6,
    unit == "TG"  ~ gw_af * 3.06889e-3,
    unit == "MG"  ~ gw_af * 3.06889,
    unit == "CCF" ~ gw_af * 0.00229569,
    TRUE ~ gw_af)
  )

d <- bind_rows(bind_rows(l), l2) %>% 
  # IMPORTANT: control for incorrect data with reasonable use assumption
  filter(gw_af <= 2500 | pwsid == "CA4910017") %>% 
  # Windsor only has one well, so we correct to Raftelis' 50 AF/yr
  mutate(gw_af = ifelse(pwsid == "CA4910017", 50, gw_af)) 

# all water systems we need data for
psrp <- read_rds(path(data_path, "data_output/srp_parcel_complete.rds"))
pson <- read_rds(path(data_path, "data_output/son_parcel_complete.rds")) 
ppet <- read_rds(path(data_path, "data_output/pet_parcel_complete.rds"))
all <- bind_rows(psrp, pson, ppet)

# unique names in soco
dw <- all$CA_DrinkingWater_SvcArea_Name %>% 
  unique() %>% 
  paste(collapse = "; ") %>% 
  str_split("; ") %>% 
  unlist() %>% 
  unique()

# unique pwsids and names of systems we need data for
pwsid_key <- path(data_path, "general", "water_system_boundaries",
                  "SABL_Public_083121/SABL_Public_083121.shp") %>%
  st_read() %>%
  st_drop_geometry() %>% 
  filter(WATER_SY_1 %in% dw) %>% 
  select(name = WATER_SY_1, pwsid = SABL_PWSID)

# add pwsid to water systems in soco
dw <- tibble(name = dw) %>% 
  left_join(pwsid_key) %>% 
  filter(!is.na(pwsid))

# full gw use (all years) for appendix C
d <- d %>% 
  filter(pwsid %in% dw$pwsid) %>% 
  left_join(dw) %>% 
  # all units are either AF, CCF, G, or MG at this point, so drop units
  select(-unit)
  
write_csv(d, here("data_output/ddw_muni_pumping_all_years.csv"))

# calculate average annual gw use from 2013-2020
muni_gw <- d %>%
  group_by(name, pwsid) %>% 
  summarise(gw_af = mean(gw_af, na.rm = TRUE)) %>% 
  ungroup() 

# deduplicate sanity check
length(muni_gw$name) == length(unique(muni_gw$name))

# add gsas per water system
gsa_key <- all %>% 
  select(gsa  = GSA_Jurisdiction_Prelim, 
         name = CA_DrinkingWater_SvcArea_Name) %>% 
  st_drop_geometry() %>% 
  # separate semicolon column and do some finessing to get a GSA:pswid key
  separate(name, into = letters[1:20], sep = "; ") %>% 
  janitor::remove_empty(which = "cols") %>% 
  pivot_longer(-gsa, names_to = "rm", values_to = "name") %>% 
  select(-rm) %>% 
  filter(!is.na(name))

# remove Cotati upon request from Permit Sonoma
ddw_gw <- muni_gw %>% 
  anti_join(
    tibble(name = "COTATI, CITY OF",
           gsa  = "Petaluma Valley")
    ) %>% 
  left_join(gsa_key, by = "name") %>% 
  select(pwsid, everything()) %>% 
  distinct()

write_csv(ddw_gw, here("data_output/ddw_muni_pumping.csv"))


# per GSP muni totals to add ----------------------------------------------

ddw_muni_pumping <- ddw_gw %>% 
  group_by(gsa) %>% 
  summarise(sum_gw_af = sum(gw_af)) %>% 
  ungroup()

write_csv(ddw_muni_pumping, here("data_output/ddw_muni_pumping_summary.csv"))


# missing water systems ---------------------------------------------------

muni_missing <- dw %>% 
  filter(
    ! pwsid %in% muni_gw$pwsid & 
    !is.na(name) & 
    name != "NA" & 
    name != "NAPA, CITY OF"
  ) %>% 
  arrange(pwsid, name) %>% 
  filter(name != "WINDSOR, TOWN OF")

write_csv(muni_missing, here("data_output/ddw_muni_missing.csv"))
