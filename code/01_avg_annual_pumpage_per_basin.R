library(tidyverse)
library(sf)
library(here)
library(mapview)
library(fs)


# load general data -------------------------------------------------------
# load pumpage data for each GSA
f_srp <- path(data_path, "srp/pumpage/SRP_future_baseline_qpercell.shp")
f_son <- dir_ls(path(data_path, "son/pumpage"), regexp = ".csv")
f_pet <- dir_ls(path(data_path, "pet/pumpage"), regexp = ".csv")

# gsa spatial boundaries
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")


# SRP pumpage -------------------------------------------------------------
p_srp <- f_srp %>% 
  st_read() %>% 
  st_centroid() %>%
  st_transform(epsg) %>% 
  st_intersection(srp)

ggplot() + 
  geom_sf(data = srp) + 
  geom_sf(data = filter(p_srp, date == 2021), aes(color = log(q)))

# annual average pumping
p_srp_sum <- p_srp %>% 
  group_by(date) %>% 
  summarise(total_pumpage_af = sum(q)) %>% 
  ungroup() 

cat("SRP average annual (", glue::glue_collapse(range(p_srp$date), "-"),
    ") pumping in entire aquifer is", 
    mean(p_srp_sum$total_pumpage_af), "AF.")
  
p_srp_sum %>% 
  ggplot(aes(date, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mean(p_srp_sum$total_pumpage_af), color = "red")


# SON pumpage -------------------------------------------------------------

p_son <- map2_df(f_son, c("farm", "m&i_domestic"),
                 ~read_csv(.x) %>% 
                   mutate(type = .y) %>% 
                   tidyr::separate(date, into = c("year", "month"), 
                                   sep = "-", convert = TRUE) %>% 
                   group_by(year, type) %>% 
                   summarise(total_pumpage_af = sum(QAF)) %>% 
                   ungroup()
                 )

cat("SON average annual (", glue::glue_collapse(range(p_son$year), "-"),
    ") pumping in entire aquifer:")
p_son %>% 
  group_by(type) %>% 
  summarise(mean_pumpage_af = mean(total_pumpage_af))

p_son %>% 
  ggplot(aes(year, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type)


# PET pumpage -------------------------------------------------------------

p_pet <- map2_df(f_pet, c("ag", "m&i", "domestic"),
                 ~read_csv(.x) %>% 
                   mutate(type = .y) %>% 
                   tidyr::separate(date, into = c("year", "month"), 
                                   sep = "-", convert = TRUE) %>% 
                   group_by(year, type) %>% 
                   summarise(total_pumpage_af = sum(QAF)) %>% 
                   ungroup()
                 )

cat("PET average annual (", glue::glue_collapse(range(p_pet$year), "-"),
    ") pumping in entire aquifer:")
p_pet %>% 
  group_by(type) %>% 
  summarise(mean_pumpage_af = mean(total_pumpage_af))

p_pet %>% 
  ggplot(aes(year, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type)


