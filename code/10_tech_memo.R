library(tidyverse)
library(sf)
library(here)
library(fs)
library(patchwork)
library(ggsflabel)
library(ggmap)

# setup
dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsa spatial data: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY") %>% 
  mutate(Basin_Name = "SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN") %>% 
  mutate(Basin_Name = "SANTA ROSA PLAIN")

srp <- st_transform(srp, 4269)
son <- st_transform(son, 4269)
pet <- st_transform(pet, 4269)

# all parcel data
all <- st_read(path(data_path, "data_output/soco_gsas_parcel.geojson"))


# figure 1: basin setting -------------------------------------------------

# need to `register_google("my_key")`
basemap <- get_map(location = c(lon = -122.6284, lat = 38.33129), 
                   zoom = 10, maptype = 'terrain-background', 
                   source = 'stamen') 

p1 <- ggmap(basemap) +
  geom_sf(data = srp, aes(fill = Basin_Name), 
          alpha = 0.8, inherit.aes = FALSE) +
  geom_sf(data = son, aes(fill = Basin_Name), 
          alpha = 0.8, inherit.aes = FALSE) +
  geom_sf(data = pet, aes(fill = Basin_Name), 
          alpha = 0.8, inherit.aes = FALSE) +
  geom_sf_label_repel(data = srp, aes(label = Basin_Name),
                      force = 100, nudge_x = -2, nudge_y = 0.05, seed = 10,
                      inherit.aes = FALSE) +
  geom_sf_label_repel(data = son, aes(label = Basin_Name),
                      force = 100, nudge_y = 0.1, nudge_x = 1, seed = 10,
                      inherit.aes = FALSE) +
  geom_sf_label_repel(data = pet, aes(label = Basin_Name),
                      force = 100, nudge_y = -0.1, nudge_x = -0.1, seed = 10,
                      inherit.aes = FALSE) +
  rcartocolor::scale_fill_carto_d(palette = "Bold") +
  guides(fill = "none") +
  labs(x = "", y = "")
p1
ggsave(here("results/01_study_area.png"), p1, width = 7, height = 5)


# figure 2: crop map ------------------------------------------------------

# DWR crop data
crop <- path(data_path, "general/crops/i15_Crop_Mapping_2018.shp") %>% 
  st_read() %>% 
  filter(COUNTY == "Sonoma") %>% 
  st_transform(4269) %>% 
  st_make_valid() %>% 
  st_intersection(st_union(bind_rows(son, srp, pet))) %>% 
  select(crop_class = CLASS2) %>% 
  mutate(crop_class = case_when(
    crop_class == "C" ~ "Citrus & Subtropical",
    crop_class == "D" ~ "Deciduous Fruit & Nuts",
    crop_class == "T" ~ "Truck & BerryCrops",
    crop_class == "V" ~ "Vine",
    crop_class == "G" ~ "Grain",
    crop_class == "P" ~ "Pasture",
    TRUE ~ "Other"
  ))

# map of crops
p2a <- ggplot() +
  geom_sf(data = srp, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = son, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = pet, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = crop, aes(fill = crop_class), lwd = 0,
          alpha = 0.8, inherit.aes = FALSE) +
  rcartocolor::scale_fill_carto_d(palette = "Pastel") +
  labs(fill = "Crop Class") +
  labs(x = "", y = "") +
  guides(fill = "none") + 
  theme_void()
  
p2a

# calculate crop areas
crop_area <- crop %>% 
  st_join(select(bind_rows(son, pet, srp), Basin_Name)) %>% 
  rmapshaper::ms_simplify(keep_shapes = TRUE) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  group_by(crop_class, Basin_Name) %>% 
  summarise(area_m2 = sum(area)) %>% 
  ungroup() %>% 
  select(Basin_Name, everything()) %>% 
  arrange(Basin_Name, crop_class)

p2b <- crop_area %>% 
  # convert m2 to acres
  mutate(area_acres = as.numeric(area_m2 * 0.000247105)) %>% 
  ggplot(aes(fct_reorder(crop_class, area_acres), area_acres)) +
  geom_col(aes(fill = crop_class)) +
  scale_y_continuous(label = scales::comma) +
  coord_flip() +
  rcartocolor::scale_fill_carto_d(palette = "Pastel") +
  labs(x = "", y = "Acres") +
  guides(fill = "none") + 
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p2b 

p2 <- p2b + p2a + plot_layout(widths = c(0.3, 0.7))
ggsave(here("results/02_crop_map.png"), p2, width = 11, height = 5)


# figure 3: water service areas -------------------------------------------

# water service areas in SON
wsa <- path(data_path, "general", "water_system_boundaries",
            "SABL_Public_083121/SABL_Public_083121.shp") %>% 
  st_read() %>% 
  st_transform(4269) %>% 
  st_make_valid() %>% 
  st_intersection(st_union(bind_rows(son, srp, pet))) %>% 
  select(name  = WATER_SY_1,
         pwsid = SABL_PWSID) %>% 
  mutate(area = st_area(geometry))

top_10_wsa <- wsa %>% slice_max(area, n = 10) %>% pull(name)

# rewrite names for plotting so that only the top 10 show up
wsa <- wsa %>% 
  mutate(name = ifelse(name %in% top_10_wsa, name, "other"))

p3a <- ggplot() +
  geom_sf(data = srp, alpha = 0.5, inherit.aes = FALSE) +
  geom_sf(data = son, alpha = 0.5, inherit.aes = FALSE) +
  geom_sf(data = pet, alpha = 0.5, inherit.aes = FALSE) +
  geom_sf(data = wsa, aes(fill = name), lwd = 0, 
          alpha = 0.5, inherit.aes = FALSE) +
  geom_sf_label_repel(data = filter(wsa, name != "other"), 
                      aes(label = name, fill = name), alpha = 0.7) +
  rcartocolor::scale_fill_carto_d(palette = "Bold") +
  labs(x = "", y = "") +
  guides(fill = "none") +
  theme_void()  

p3a

ggsave(here("results/03_wsa.png"), p3a, width = 22, height = 10)


# figure 4: parcel groundwater use ----------------------------------------

breaks <- seq(0, 400, 50)

p4 <- ggplot() +
  geom_sf(data = srp, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = son, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = pet, alpha = 0.5, inherit.aes = FALSE, fill = "white") +
  geom_sf(data = all, 
          aes(fill = Total_Groundwater_Use_Ac_Ft), lwd = 0,
          alpha = 0.8, inherit.aes = FALSE) +
  rcartocolor::scale_fill_carto_c(palette = "Sunset") +
  labs(fill = "Total Groundwater\nUse (AF/yr)") +
  labs(x = "", y = "") +
  theme_void()

ggsave(here("results/04_gw_use_parcel.png"), p4, width = 11, height = 5)
