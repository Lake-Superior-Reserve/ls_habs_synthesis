#helper functions
dnr_rename <- Vectorize(function(param) {
  if (param == "530") return("tss")
  else if (param == "600") return("tn")
  else if (param == "608") return("nh3")
  else if (param == "625") return("tkn")
  else if (param == "631") return("no3")
  else if (param == "665") return("tp")
  else if (param == "671") return("po4")
  else if (param == "99717") return("chl")
  else return(param)
})

dnr_fix_dates <- Vectorize(function(station, date) {
  if (station == "10040814" & date == "8/20/2019 12:15:00 PM") return("8/22/2019 12:15:00 PM")
  else if (station == "10052503" & date == "8/11/2019 12:00:00 PM") return("8/12/2019 10:50:00 AM")
  else if (station == "10052505" & date == "8/22/2019 10:00:00 AM") return("8/22/2019 10:30:00 AM")
  else if (station == "10052513" & date == "7/8/2019 12:10:00 PM") return("7/8/2019 12:00:00 PM")
  else if (station == "10052514" & date == "7/22/2019 1:00:00 PM") return("7/25/2019 1:00:00 PM")
  else if (station == "10052587" & date == "8/22/2019 9:55:00 AM") return("8/22/2019 9:40:00 AM")
  else return(date)
})

s1_targets <- list(
  
  #raw data files
  tar_target(dnr_swims_19_file, "raw_data/wdnr/2019LSNSHABs_SWIMS.xlsx", format = "file"),
  tar_target(dnr_swims_21_file, "raw_data/wdnr/2021NSData_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_22_file, "raw_data/wdnr/2022NSHAB_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_23_file, "raw_data/wdnr/2023NSHABs_SWIMS.xlsx", format = "file"),
  
  #reading functions
  tar_target(dnr_swims_19, read_xlsx(dnr_swims_19_file)),
  tar_target(dnr_swims_21, read_xlsx(dnr_swims_21_file)),
  tar_target(dnr_swims_22, read_xlsx(dnr_swims_22_file)),
  tar_target(dnr_swims_23, read_xlsx(dnr_swims_23_file)),
  
  #pull out coordinates from 2019 dnr file since other years are missing it
  tar_target(dnr_stations, dnr_swims_19 %>% 
               group_by(StationID) %>%
               summarise(StationLatitude = first(StationLatitude), StationLongitude = first(StationLongitude))
             ),
  
  #cleaning functions
  
  tar_target(dnr_swims_19_clean, dnr_swims_19 %>%
               filter(!is.na(DNRParameterCode) & DNRParameterCode != "625") %>% #dropping tkn in addition to nas, tkn only has 1 round of results, all ND
               mutate(StartDateTime = dnr_fix_dates(StationID, StartDateTime),
                      StartDateTime = mdy_hms(StartDateTime, tz = "America/Chicago"),
                      DNRParameterCode = unname(dnr_rename(DNRParameterCode)),
                      ResultValueNo = if_else(ResultValueNo == "ND", as.character(0.5 * as.numeric(LODAmount)), ResultValueNo),
                      ResultValueNo = as.numeric(ResultValueNo)) %>% #separating the conversion to prevent warning message
               select(StartDateTime, StationID, DNRParameterCode, ResultValueNo) %>%
               pivot_wider(names_from = DNRParameterCode, values_from = ResultValueNo, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               arrange(StartDateTime, StationID) %>%
               left_join(dnr_stations)
               )
  

)

