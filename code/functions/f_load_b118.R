# load GSA boundaries
f_load_b118_basin <- function(path, basin = NULL, names = NULL){

  b <- sf::st_read(path) %>% 
    sf::st_transform(epsg)
  
  if(!is.null(basin)){
    b <- filter(b, Basin_Su_1 %in% basin)
  }
  
  if(is.null(basin)){
    b <- pull(b, Basin_Su_1) %>% 
      sort() %>% 
      glue::glue_collapse(sep = "\n ")
  }
  
  return(b)
}
