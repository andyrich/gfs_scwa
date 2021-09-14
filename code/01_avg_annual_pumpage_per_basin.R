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
  
p1 <- p_srp_sum %>% 
  ggplot(aes(date, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mean(p_srp_sum$total_pumpage_af), color = "red") +
  labs(y = "Pumping (AF/yr)", x = "", 
       title = "Santa Rosa Plain", 
       subtitle = "all users, shallow & deep aquifer zones (future baseline scenario, 2021-2070)")


# SON pumpage -------------------------------------------------------------

p_son <- map2_df(f_son, c("agriculture", "M&I plus domestic"),
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

# mean pumping per user
son_means <- group_by(p_son, type) %>% 
  summarise(mean = mean(total_pumpage_af)) %>% 
  ungroup()

p2 <- p_son %>% 
  ggplot(aes(year, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  geom_hline(data = son_means, aes(yintercept = mean), color = "red") +
  facet_wrap(~type) +
  labs(y = "Pumping (AF/yr)", x = "", 
       title = "Sonoma Valley", 
       subtitle = "(2011-2018)")


# PET pumpage -------------------------------------------------------------

p_pet <- map2_df(f_pet, c("agriculture", "M&I", "domestic"),
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

# mean pumping per user
pet_means <- group_by(p_pet, type) %>% 
  summarise(mean = mean(total_pumpage_af)) %>% 
  ungroup()

p3 <- p_pet %>% 
  ggplot(aes(year, total_pumpage_af)) +
  geom_point() +
  geom_line() +
  geom_hline(data = pet_means, aes(yintercept = mean), color = "red") +
  facet_wrap(~type) +
  labs(y = "Pumping (AF/yr)", x = "", 
       title = "Petaluma", 
       subtitle = "(2011-2018)")


# write plots and table ---------------------------------------------------

# plots
files <- path(data_path, "figures", paste0(c("srp", "son", "pet"), "_pump.png"))
walk2(list(p1, p2, p3), files, ~ggsave(.y, .x))

# table
bind_rows(
  tibble(type = "all", mean = mean(p_srp_sum$total_pumpage_af), gsa = "SRP"),
  mutate(pet_means, gsa = "PET"),
  mutate(son_means, gsa = "SON")
  ) %>% 
  rename(pumpage_af = mean) %>% 
  write_csv(path(data_path, "tables/pump.csv"))
