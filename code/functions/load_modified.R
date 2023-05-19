# Load modified GW uses for individual parcels. These values will be assigned to their GW use components


load_modified_single <- function(sheetname, col1, col2){
  # function to load modified table
  df <-  readxl::read_xlsx( path(data_path, "general/modified_values",
                                 paste(sheetname,  '.xlsx', sep ='') ))
  
  
  # assigning new names to the columns of the data frame
  colnames(df) <- c('APN', unlist(col1), unlist(col2))
  df[[sheetname]] = 'Yes'
  
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
  df5 = load_modified_single(names(h[5]),h[5][1], h[5][2])
  df6 = load_modified_single(names(h[6]),h[6][1], h[6][2])
  df7 = load_modified_single(names(h[7]),h[7][1], h[7][2])
  df8 = load_modified_single(names(h[8]),h[8][1], h[8][2])
  
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
           
           # Urban_Irrigation_Modified = ifelse(is.na(Urban_Irrigation_Modified),'No','Yes'), #TODO check Urban_Irrigation_Modified
           # Surface_Water_Connection_Modified = ifelse(is.na(Surface_Water_Connection_Modified),'No','Yes'), #TODO check Surface_Water_Connection_Modified
           # Surface_Water_Use_Modified = ifelse(is.na(Surface_Water_Use_Modified),'No','Yes'), #TODO check Surface_Water_Use_Modified
           # Recycled_Water_Use_Modified = ifelse(is.na(Recycled_Water_Use_Modified),'No','Yes'), #TODO check Recycled_Water_Use_Modified
           )
  
  return(parcel)
}


replace_with_modified <- function(parcel,original_column, modified_column, comment_column){
  

  
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