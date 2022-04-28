# load GSA boundaries
load_land_size <- function(data_path, parcel){
  print('loading parcel area')
  # parcels from public sonoma county data portal
  # https://gis-sonomacounty.hub.arcgis.com/pages/data
  area <- st_read(path(data_path, "general/parcel/CDR_PARCEL_PUB_SHP_vw.shp") ) %>%
    mutate(  LandSizeParcelAcres = LndSzSF/43560.,) %>%
    select(c('APN','LandSizeParcelAcres')) %>%
    st_drop_geometry()
  
  parcel <- left_join(parcel, area, by='APN')
  
  return(parcel)
}

