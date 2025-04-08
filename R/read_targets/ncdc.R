ncdc_targets <- list(
  
  #' Target for raw source file
  #' 
  #' Path for data file downloaded from NCDC.
  #' Last pulled 4-8-25 with data from 2010-2024
  #' See https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf for information
  #'
  #' @return A file path.
  tar_target(ncdc_file, "raw_data/other/ncdc_4-8-25.csv"),
  
  
  #' Read NCDC file
  #' 
  #' @param ncdc_file 
  #'
  #' @return data frame of ncdc daily summaries
  tar_target(ncdc_raw, read_csv(ncdc_file)),
  
  
  #' Format ncdc data
  #' 
  #' Change column names, drop flag columns, sort
  #' 
  #' @param ncdc_raw 
  #'
  #' @return data frame of formatted ncdc daily summaries
  tar_target(ncdc, ncdc_raw %>% 
               select(site = NAME, latitude = LATITUDE, longitude = LONGITUDE, date = DATE,
                      precip = PRCP, snow = SNOW, snow_depth = SNWD, temp_max = TMAX, temp_min = TMIN) %>% 
               mutate(site = case_when(site == "SUPERIOR, WI US" ~ "superior",
                                       site == "BRULE RANGER STATION, WI US" ~ "brule",
                                       site == "BAYFIELD FISH HATCHERY, WI US" ~ "bayfield",
                                       .default = site)) %>% 
               arrange(date))
  
)