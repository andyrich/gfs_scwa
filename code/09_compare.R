library(tidyverse)
library(here)
library(fs)

df <- tibble(
  basin  = c("SRP", "SRP", "SRP", "SON", "SON", "SON", "PET", "PET", "PET"),
  use    = rep(c("agriculture", "M&I + domestic", "all"), times = 3),
  pmin   = c(8700, 7300, 16000, 3100, 1400, 4500, 1900, 300, 2200),
  pmax   = c(11400, 11200, 22600, 4300, 2000, 6300, 2150, 700, 2850),
  parcel = c(8328, 11617, 19945, 4145, 2602, 6747, 1582, 993, 2575)
)

p <- df %>% 
  filter(use == "all") %>% 
  ggplot() +
  geom_errorbar(aes(x = basin, ymin = pmin, ymax = pmax), width= 0.2, color = "blue") +
  geom_point(aes(x = basin, y = parcel), color = "red") +
  coord_flip() +
  labs(y = "Estimated Groundwater pumping (AF/year)", x = "") + 
  scale_y_continuous(label = scales::comma) +
  theme_grey(base_size = 13)

dir_create(here("results"))
ggsave(p, filename = here("results/compare_parcel_gwf.png"), 
       height = 4, width = 9)

p2 <- df %>% 
  filter(use != "all") %>% 
  ggplot() +
  geom_errorbar(aes(x = basin, ymin = pmin, ymax = pmax), width= 0.2, color = "blue") +
  geom_point(aes(x = basin, y = parcel), color = "red") +
  coord_flip() +
  facet_wrap(~use) + 
  labs(y = "Estimated Groundwater pumping (AF/year)", x = "") + 
  scale_y_continuous(label = scales::comma) +
  theme_grey(base_size = 13)

ggsave(p2, filename = here("results/compare_parcel_gwf_by_use.png"), 
       height = 4, width = 9)
