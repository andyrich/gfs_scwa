# Load modified GW uses for individual parcels. These values will be assigned to their GW use components


load_modified_single <- function(sheetname, col1, col2){
  # function to load modified table
  df <-  readxl::read_xlsx( path(data_path, "general/modified_values",
                                 paste(sheetname,  '.xlsx', sep ='') ))
  
  
  # assigning new names to the columns of the data frame
  colnames(df) <- c('APN', unlist(col1), unlist(col2))
  df[[sheetname]] = 'Yes'
  
  #extra row with APN='APN' is being added. so removing here.
  df <- filter(df,!df$APN=='APN')
  print(colnames(df))
  
  return(df)}

reduce_dfs <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  xxx = merge(..., by = 'APN', all=TRUE)
  return(xxx)
}



load_all_modified <- function(){
  
  # load all of the modified values from all of the sheets, then join together
  print('loading all modified values')
 
  h <- list(
    Ag_GW_Use_Modified= c('Ag_GW_Use_Modified_Ac_Ft',
                          'Ag_GW_Use_Comment'),
    Commercial_GW_Use_Modified=c('Commercial_GW_Use_Modified_Ac_Ft',
                                 'Commercial_GW_Use_Comment'),
    Res_GW_Use_Modified=c('Res_GW_Use_Modified_Ac_Ft',
                          'Res_GW_Use_Comment'),
    School_Golf_Modified=c('School_Golf_GW_Use_Modified_Ac_Ft',
                           'School_Golf_GW_Use_Comment'),
    Urban_Well_Modified=c('Urban_Well_Modified_Ac_Ft',
                                'Urban_Well_Comment' ),

    Surface_Water_Connection_Modified=c('Surface_Water_Connection_Modified',
                                'Surface_Water_Connection_Comment' ),
    Surface_Water_Use_Modified=c('Surface_Water_Use_Modified_Ac_Ft',
                                'Surface_Water_Use_Comment' ),
    Recycled_Water_Use_Modified=c('Recycled_Water_Use_Modified_Ac_Ft',
                                'Recycled_Water_Use_Comment' )

    )
  
  df1 = load_modified_single(names(h[1]),h[1][1], h[1][2])
  df2 = load_modified_single(names(h[2]),h[2][1], h[2][2])
  df3 = load_modified_single(names(h[3]),h[3][1], h[3][2])
  df4 = load_modified_single(names(h[4]),h[4][1], h[4][2])
  # df5 = load_modified_single(names(h[5]),h[5][1], h[5][2])
  # df6 = load_modified_single(names(h[6]),h[6][1], h[6][2])
  # df7 = load_modified_single(names(h[7]),h[7][1], h[7][2])
  # df8 = load_modified_single(names(h[8]),h[8][1], h[8][2])
  
  df = list(df1, df2, df3, df4)
  # df = list(df1, df2, df3, df4, df5,  df6, df7, df8)

  dfall = Reduce( reduce_dfs, df)

  
  return(dfall)}

join_with_modified <- function(parcel){
  # join modified fields to the parcel database
  modified <- load_all_modified()
  
  cols = colnames(modified)
  print("These are the colnames of the modified database")
  print(cols)
  
  parcel = left_join(parcel, modified) %>%
    mutate(Ag_GW_Use_Modified = ifelse(is.na(Ag_GW_Use_Modified),'No','Yes'),
           Commercial_GW_Use_Modified = ifelse(is.na(Commercial_GW_Use_Modified),'No','Yes'),
           Res_GW_Use_Modified = ifelse(is.na(Res_GW_Use_Modified),'No','Yes'),
           School_Golf_Modified = ifelse(is.na(School_Golf_Modified),'No','Yes'),
           )
  
  return(parcel)
}


replace_use_code <- function(parcel) {
  
  df <- load_modified_single('UseCode_Modified', 'UseCode_Modified_Value', 'UseCode_Comment')
  
  df <- df %>% 
    mutate(UseCode_Modified_Value = str_pad(UseCode_Modified_Value, 4, "left", "0"))
  
  parcel <- left_join(parcel, df) %>%
      mutate(UseCode_Modified = ifelse(is.na(UseCode_Modified),'No','Yes'),
             UseCode_prelim = UseCode, 
             UseCode = if_else(UseCode_Modified=='Yes', UseCode_Modified_Value, UseCode)) 
  
  
  return(parcel)
}

add_urban_irrigation_modified <- function(parcel) {
  print('loading urban irrigation modified')
  df <- load_modified_single('Urban_Irrigation_Modified', 'Urban_Irrigation_Modified_Ac_Ft', 'Urban_Irrigation_GW_Use_Comment')


  parcel <- left_join(parcel, df)  %>%
    mutate(
      Urban_Irrigation_GW_Use_Prelim_Ac_Ft = Urban_Irrigation_GW_Use_Ac_Ft,
      Urban_Irrigation_GW_Use_Ac_Ft   = ifelse(
        Urban_Irrigation_Modified == "Yes",
        Urban_Irrigation_Modified_Ac_Ft,
        Urban_Irrigation_GW_Use_Prelim_Ac_Ft))

  return(parcel)
}

add_surface_water_connection_modified <- function(parcel) {
  print('loading surface water connection')
  df <- load_modified_single('Surface_Water_Connection_Modified', 'Surface_Water_Connection_Modified_Value', 'Surface_Water_Connection_Modified_Comment')

  parcel <- left_join(parcel, df) %>%
    mutate(Surface_Water_Connection_Prelim = Surface_Water_Connection,
           Surface_Water_Connection = if_else(!is.na(Surface_Water_Connection_Modified),
                                             Surface_Water_Connection_Modified_Value,
                                             Surface_Water_Connection_Prelim),
           
           Surface_Water_Use_Ac_Ft = if_else(Surface_Water_Connection=='Yes', 
                                             Surface_Water_Use_Ac_Ft, 
                                             0)) 
  
  return(parcel)
}

add_surface_water_modified <- function(parcel) {
  print('loading surface water modified')
  df <- load_modified_single('Surface_Water_Use_Modified', 'Surface_Water_Use_Modified_Ac_Ft', 'Surface_Water_Comment')

  df <-   mutate(df, 
             Surface_Water_Connection = ifelse(
            !is.na(Surface_Water_Use_Modified_Ac_Ft) & Surface_Water_Use_Modified_Ac_Ft > 0,
            "Yes", "No"))
  
  parcel <- left_join(parcel, df) %>%
    mutate(Surface_Water_Use_Modified = ifelse(is.na(Surface_Water_Use_Modified),'No','Yes'),
           Surface_Water_Use_Ac_Ft_prelim = Surface_Water_Use_Ac_Ft,
           Surface_Water_Use_Ac_Ft_prelim = if_else(is.na(Surface_Water_Use_Ac_Ft_prelim),0,Surface_Water_Use_Ac_Ft_prelim), #fix na values
           Surface_Water_Use_Ac_Ft = if_else(Surface_Water_Use_Modified=='Yes', 
                                             Surface_Water_Use_Modified_Ac_Ft, 
                                             Surface_Water_Use_Ac_Ft_prelim)) 
  
  return(parcel)
}
  
add_recycled_water_connection_modified <- function(parcel) {
  print('loading recycled water connection')
  df <- load_modified_single('Recycled_Water_Connection_Modified', 'Recycled_Water_Connection_Modified_Value', 'Recycled_Water_Connection_Modified_Comment')
  
  parcel <- left_join(parcel, df) %>%
    mutate(Recycled_Water_Connection_Prelim = Recycled_Water_Connection,
           Recycled_Water_Connection = if_else(!is.na(Recycled_Water_Connection_Modified),
                                               Recycled_Water_Connection_Modified_Value,
                                               Recycled_Water_Connection_Prelim),
           
           Recycled_Water_Use_Ac_Ft = if_else(Recycled_Water_Connection=='Yes', 
                                              Recycled_Water_Use_Ac_Ft, 
                                             0)) 
  return(parcel)
}

add_recycled_water_modified <- function(parcel) {
  print('loading recycled water modified')
  df <- load_modified_single('Recycled_Water_Use_Modified', 'Recycled_Water_Use_Modified_Ac_Ft', 'Recycled_Water_Use_Comment')
  
  df <-   mutate(df, 
                 Recycled_Water_Connection = ifelse(
                   !is.na(Recycled_Water_Use_Modified_Ac_Ft) & Recycled_Water_Use_Modified_Ac_Ft > 0,
                   "Yes", "No"))
  
  parcel <- left_join(parcel, df) %>%
    mutate(Recycled_Water_Use_Modified = ifelse(is.na(Recycled_Water_Use_Modified),'No','Yes'),
           Recycled_Water_Use_Modified_Ac_Ft_prelim = Recycled_Water_Use_Ac_Ft,
           Recycled_Water_Use_Modified_Ac_Ft_prelim = if_else(is.na(Recycled_Water_Use_Modified_Ac_Ft_prelim),
                                                              0,
                                                              Recycled_Water_Use_Modified_Ac_Ft_prelim), #fix na values
           Recycled_Water_Use_Ac_Ft = if_else(Recycled_Water_Use_Modified=='Yes', 
                                              Recycled_Water_Use_Modified_Ac_Ft, 
                                              Recycled_Water_Use_Modified_Ac_Ft_prelim)) 
  
  return(parcel)
}

replace_active_well_modified <- function(parcel) {
  
  df <- load_modified_single('Active_Well_Modified', 'Active_Well_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  parcel <- left_join(parcel, df) %>%
    mutate(Active_Well_Modified = ifelse(is.na(Active_Well_Modified),'No','Yes'),
           Active_Well = if_else(Active_Well_Modified=='Yes', Active_Well_Modified_Value, Active_Well)) 
  
  return(parcel)
}

replace_shared_well_modified <- function(parcel) {
  
  df <- load_modified_single('Shared_Well_Modified', 'Shared_Well_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  if ('Shared_Well' %in% colnames(parcel)){
    print('--------------changing shared well-=-------------bbbbbbbbbbbbbbbbbb') # should only be done for SRP
    parcel <-mutate(parcel, 
                    Shared_Well = replace_na(Shared_Well, 'No'))}
  else {    parcel <-mutate(parcel, 
                            Shared_Well = 'No')
  }
  

  
  parcel <- left_join(parcel, df) %>%
    mutate(Shared_Well_Modified = ifelse(is.na(Shared_Well_Modified),'No','Yes'),
           Shared_Well = if_else(Shared_Well_Modified=='Yes', Shared_Well_Modified_Value, Shared_Well)) 
  
  
  return(parcel)
}

replace_shared_well_APN_modified <- function(parcel) {
  
  df <- load_modified_single('Shared_Well_APN_Modified', 'Shared_Well_APN_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  if ('Shared_Well_APN' %in% colnames(parcel)){
    print('changing Shared_Well_APN') # should only be done for SRP
    parcel <-mutate(parcel, 
                    Shared_Well_APN = replace_na(Shared_Well_APN, '')) }
  else {    parcel <-mutate(parcel, 
                            Shared_Well_APN = '')
  }

  
  parcel <- left_join(parcel, df) %>%
    mutate(Shared_Well_APN_Modified = ifelse(is.na(Shared_Well_APN_Modified),'No','Yes'),
           Shared_Well_APN = if_else(Shared_Well_APN_Modified=='Yes', Shared_Well_APN_Modified_Value, Shared_Well_APN)
           ) 
  
  return(parcel)
}

replace_Well_Records_Available_modified <- function(parcel) {
  
  df <- load_modified_single('Well_Records_Available_Modified', 'Well_Records_Available_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  parcel <- left_join(parcel, df) %>%
    mutate(Well_Records_Available_Modified = ifelse(is.na(Well_Records_Available_Modified),'No','Yes'),
           Well_Records_Available = if_else(Well_Records_Available_Modified=='Yes', Well_Records_Available_Modified_Value, Well_Records_Available)) 
  
  return(parcel)
}

replace_Onsite_Well_modified <- function(parcel) {
  
  df <- load_modified_single('Onsite_Well_Modified', 'Onsite_Well_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  parcel <- left_join(parcel, df) %>%
    mutate(Onsite_Well_Modified = ifelse(is.na(Onsite_Well_Modified),'No','Yes'),
           Onsite_Well = if_else(Onsite_Well_Modified=='Yes', Onsite_Well_Modified_Value, Onsite_Well)) 
  
  return(parcel)
}

replace_urban_well_modified <- function(parcel) {
  
  df <- load_modified_single('Urban_Well_Modified', 'Urban_Well_Modified_Value', 'Comment')
  
  df$Comment <- NULL
  
  parcel <- left_join(parcel, df) %>%
    mutate(Urban_Well_Modified = ifelse(is.na(Urban_Well_Modified),'No','Yes'),
           Urban_Well = if_else(Urban_Well_Modified=='Yes', Urban_Well_Modified_Value, Urban_Well)) 
  
  return(parcel)
}