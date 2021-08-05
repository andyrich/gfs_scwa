# reversed, sorted table of a vector
tsr <- function(x){
  rev(sort(table(x)))
}

# source all functions
purrr::walk(list.files(here::here("code", "functions"), full.names = TRUE), 
            ~source(.x))

# load env vars used across modules as objects in .GlobalEnv
f_load_dot_env()

# set some themes
library(ggplot2)
library(ggtext)
theme_project <- theme_minimal() + 
  theme(panel.grid.minor = element_blank())

theme_map <- theme_void()

theme_markdown <- theme(
  plot.title = element_markdown(face = "bold"),
  plot.caption = element_markdown(margin = margin(t = 15)), 
  axis.title.x = element_markdown(),
  axis.title.y = element_markdown()
)
