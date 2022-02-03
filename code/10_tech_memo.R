library(tidyverse)
library(sf)
library(here)
library(fs)
library(mapview)
library(patchwork)
library(ggsflabel)
library(ggmap)
library(ggsn)

source(here("code/setup.R"))

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

ggsave(here("results/01_study_area.png"), p1, width = 7, height = 5)
