# load GSA boundaries
relable_parc <- function(parc){
  print('loading database names')
  
  # s_ <- path(data_path, "schema", 
  #                          'schema_layout_temp.xlsx')
  # print('Loading pre-processed list of urban wells')
  # schema <- s_ %>%
  #   readxl::read_xlsx(sheet = 'Sheet1') %>%
  #   select(c('Field_Name', 'Field_Name_PRMD'))
  
  # s_ <- path(data_path, "schema", 
  #            '2022_07_21 GSA Schema from RP.xlsx')
  # print('Loading pre-processed list of urban wells')
  # schema <- s_ %>%
  #   readxl::read_xlsx(sheet = 'Sheet1')
  
  schema <-get_schema(data_path)
  
  colnames(schema) <- gsub(" ", "_", colnames(schema))
  
  schema <- schema %>%
    subset(!is.na(PRMD_Public_Database_Order)) %>% #drop na's
    arrange(PRMD_Public_Database_Order) %>% # sort value based on 
    rename(Field_Name = 'Field_Name_(proposed)',
           Field_Name_PRMD = PRMD_Public_Database_Field_Name) %>%
    select(c('Field_Name', 'Field_Name_PRMD'))
  
  new_row <- c('geometry', 'geometry')
  schema <- rbind(schema, new_row)
  print('here is the schema file')
  print(schema)
  print('\n')
  print('colnames from parc before changing names')
  print(colnames(parc))
  print('Done loading schema')
  
  # # swap field names with field_names_prmd
  names(parc) <- schema$Field_Name_PRMD[match(names(all), schema$Field_Name)]
  print('Done re-naming parcel layer')
  
  nonmatches <- schema$Field_Name_PRMD[!(schema$Field_Name_PRMD %in% colnames(parc))]
  print('these are fields that are missing from the schema')
  print(nonmatches)
  
  print('colnames from parc after changing names')
  print(colnames(parc))
  print('Done loading schema')
  
  #reorder columns
  parc <- parc[,schema$Field_Name_PRMD]
  print('Done re-ordering parcel layer')
  print(colnames(parc))
  
  # # swap field names with SCI field names and write
  # sci <- read_csv(path(data_path, "general/sci_key.csv"))
  # names(all) <- sci$new[match(names(all), sci$old)]

  
  return(parc)
}



