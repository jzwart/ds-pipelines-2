process_data <- function(nwis_data){
  
  data_out <- data.frame(agency_cd = c(), site_no = c(), dateTime = c(), 
                         X_00010_00000 = c(), X_00010_00000_cd = c(), tz_cd = c())
  # loop through files to download 
  for (download_file in nwis_data$filepath){
    these_data <- read_csv(download_file, col_types = 'ccTdcc')
    data_out <- rbind(data_out, these_data)
  }

  nwis_data_clean <- rename(data_out, water_temperature = X_00010_00000) %>% 
    select(-agency_cd, -X_00010_00000_cd, -tz_cd)
  
  site_no <- unique(data_out$site_no)
  site_info <- dataRetrieval::readNWISsite(site_no)
  
  annotated_data <- left_join(nwis_data_clean, site_info, by = "site_no") %>% 
    select(station_name = station_nm, site_no, dateTime, water_temperature, latitude = dec_lat_va, longitude = dec_long_va) %>% 
    mutate(station_name = as.factor(station_name))
  
  return(annotated_data) 
}


combine_into_df = function(...){
  
  site_files = c(...) 
  
  out_df = tibble(filepath = site_files, 
                  hash = scipiper::get_remake_status(site_files)$hash)
  
  return(out_df) 
}



annotate_data <- function(site_data_clean, site_filename){
  site_info <- read_csv(site_filename)
  annotated_data <- left_join(site_data_clean, site_info, by = "site_no") %>% 
    select(station_name = station_nm, site_no, dateTime, water_temperature, latitude = dec_lat_va, longitude = dec_long_va)
  
  return(annotated_data)
}


style_data <- function(site_data_annotated){
  mutate(site_data_annotated, station_name = as.factor(station_name))
}