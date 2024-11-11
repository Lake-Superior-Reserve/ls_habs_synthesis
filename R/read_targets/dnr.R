dnr_targets <- list(
  
  
  tar_target(dnr_rename, Vectorize(function(param) {
    if (is.na(param)) return(NA)
    else if (param == "10") return("temp")
    else if (param == "94") return("cond")
    else if (param == "95") return("cond")
    else if (param == "300") return("do")
    else if (param == "301") return("do_sat")
    else if (param == "400") return("ph")
    else if (param == "530") return("tss")
    else if (param == "600") return("tn")
    else if (param == "608") return("nh3")
    else if (param == "625") return("tkn")
    else if (param == "631") return("no3")
    else if (param == "665") return("tp")
    else if (param == "671") return("po4")
    else if (param == "681") return("doc")
    else if (param == "940") return("cl")
    else if (param == "955") return("si")
    else if (param == "61190") return("trans_tube")
    else if (param == "82079") return("turb")
    else if (param == "99530") return("tss")
    else if (param == "99717") return("chl")
    else return(NA)
  })),
  
  tar_target(dnr_fix_dates, Vectorize(function(station, date) {
    # confirmed changes with Ellen C 9/9/24
    if (station == "10040814" & date == "8/20/2019") return("8/22/2019") # chl is on a different day from rest of analytes, chl start and end sample dates don’t match
    else if (station == "10052503" & date == "8/11/2019") return("8/12/2019") # chl is on a different day from rest of analytes
    else if (station == "10052514" & date == "7/22/2019") return("7/25/2019") # chl is on a different day from rest of analytes, other analytes start and end sample dates don’t match
    else if (station == "101" & date == "2021-07-09") return("2021-07-08") # po4 and chl are one a different day from the rest of the analytes, site 3 was usually on the first day of sampling in 2021, so I moved po4 and chl to be on the same day as the other analytes
    else if (is.na(station) & date == "2023-08-01") return("2023-09-05") # wrong date for bloom sample on 9/5/23, confirmed with Ellen C 9/17/24
    else return(date)
  })),
  
  
  tar_target(dnr_swims_19_file, "raw_data/wdnr/2019LSNSHABs_SWIMS.xlsx", format = "file"),
  tar_target(dnr_swims_21_file, "raw_data/wdnr/2021NSData_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_22_file, "raw_data/wdnr/2022NSHAB_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_23_file, "raw_data/wdnr/2023NSHABs_SWIMS.xlsx", format = "file"),
  
  tar_target(dnr_hydro_19_file, "raw_data/wdnr/2019_Hydro.xlsx", format = "file"),
  tar_target(dnr_hydro_21_file, "raw_data/wdnr/2021_Hydro.csv", format = "file"),
  tar_target(dnr_hydro_22_file, "raw_data/wdnr/AllDepths2022Hydro.csv", format = "file"),
  tar_target(dnr_hydro_23_file, "raw_data/wdnr/2023LSNSHABsHydro_alldepth.csv", format = "file"),
  
  tar_target(dnr_swims_19, read_xlsx(dnr_swims_19_file)),
  tar_target(dnr_swims_21, read_xlsx(dnr_swims_21_file)),
  tar_target(dnr_swims_22, read_xlsx(dnr_swims_22_file)),
  tar_target(dnr_swims_23, read_xlsx(dnr_swims_23_file)),
  
  tar_target(dnr_hydro_19, read_xlsx(dnr_hydro_19_file)),
  tar_target(dnr_hydro_21, read_csv(dnr_hydro_21_file)),
  tar_target(dnr_hydro_22, read_csv(dnr_hydro_22_file)),
  tar_target(dnr_hydro_23, read_csv(dnr_hydro_23_file)),
  
  
  # pull out coordinates from 2019 dnr file since other years are missing it
  # add additional coordinates provided by Ellen C 9/9/24
  tar_target(dnr_stations, dnr_swims_19 %>% 
               group_by(StationID) %>%
               summarise(StationLatitude = first(StationLatitude), StationLongitude = first(StationLongitude)) %>% 
               bind_rows(tibble(StationID = c("10054863", "104", "BLOOM_2023-09-05", "BLOOM_2023-09-21"),
                                StationLatitude = c("46.88067000", "46.75940790", "46.792122", "46.723286"),
                                StationLongitude = c("-91.06150000", "-91.61504730", "-91.392734", "-92.064256")))
  ),
  
  # pull out site numbers (1-15) from 2023 dnr file so that we can easily merge hydro data
  tar_target(dnr_sites, dnr_swims_23 %>%
               filter(!is.na(`Id #`) & !str_detect(`Field #`, "DUP") & !str_detect(`Field #`, "BL") & str_detect(`Field #`, "HABS")) %>% 
               mutate(site = str_split_i(`Field #`, "-", 2),
                      site = as.numeric(site)) %>% 
               group_by(`Id #`) %>% 
               summarise(SiteID = first(site)) %>% 
               rename(StationID = `Id #`) 
  ),
  
  # pull out block numbers from 2019 file to join sonde data
  tar_target(dnr_blocks, dnr_swims_19 %>% 
               mutate(StartDateTime = str_split_i(StartDateTime, " ", 1),
                      StartDateTime = dnr_fix_dates(StationID, StartDateTime),
                      StartDateTime = mdy(StartDateTime, tz = "America/Chicago"),
                      Block = str_trim(str_split_i(FieldNo, "K", 2)),
                      Block = str_split_i(Block, "\\D", 1),
                      Block = as.numeric(Block)) %>% 
               group_by(StationID, StartDateTime) %>% 
               summarise(Block = mean(Block)) %>% 
               filter(!is.na(Block))),
  
  #cleaning functions
  
  tar_target(dnr_swims_19_clean, dnr_swims_19 %>%
               filter(!is.na(DNRParameterCode) & DNRParameterCode != "625") %>% #dropping tkn in addition to nas, tkn only has 1 round of results, all ND
               mutate(StartDateTime = str_split_i(StartDateTime, " ", 1),
                      StartDateTime = dnr_fix_dates(StationID, StartDateTime),
                      StartDateTime = mdy(StartDateTime, tz = "America/Chicago"),
                      DNRParameterCode = unname(dnr_rename(DNRParameterCode)),
                      ResultValueNo = if_else(ResultValueNo == "ND", as.character(0.5 * as.numeric(LODAmount)), ResultValueNo), #set NDs to 0.5 * LOD
                      ResultValueNo = as.numeric(ResultValueNo)) %>% #separating the conversion to prevent warning message
               select(StartDateTime, StationID, DNRParameterCode, ResultValueNo) %>%
               pivot_wider(names_from = DNRParameterCode, values_from = ResultValueNo, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               arrange(StartDateTime, StationID) 
  ),
  tar_target(dnr_swims_21_clean, dnr_swims_21 %>% 
               filter(!str_detect(`Field #`, "BL")) %>% #drop field blanks
               mutate(`DNR Parameter Code` = unname(dnr_rename(`DNR Parameter Code`)),
                      `Numeric Value` = if_else(`Result value` == "ND", 0.5 * as.numeric(LOD), `Numeric Value`),
                      `Collection Start Date/Time` = str_split_i(`Collection Start Date/Time`, " ", 1),
                      `Collection Start Date/Time` = dnr_fix_dates(`Id #`, `Collection Start Date/Time`),
                      `Collection Start Date/Time` = ymd(`Collection Start Date/Time`, tz = "America/Chicago")) %>% 
               select(StartDateTime = `Collection Start Date/Time`, StationID = `Id #`, `DNR Parameter Code`, `Numeric Value`) %>% 
               pivot_wider(names_from = `DNR Parameter Code`, values_from = `Numeric Value`, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               arrange(StartDateTime, StationID)),
  tar_target(dnr_swims_22_clean, dnr_swims_22 %>% 
               filter(!str_detect(`Field #`, "BL")) %>% #drop field blanks, also drops rows missing field id, which conveniently drops all the bacteria species counts
               mutate(`DNR Parameter Code` = unname(dnr_rename(`DNR Parameter Code`)),
                      `Numeric Value` = if_else(`Result value` == "ND", 0.5 * as.numeric(LOD), `Numeric Value`),
                      `Collection Start Date/Time` = str_split_i(`Collection Start Date/Time`, " ", 1),
                      `Collection Start Date/Time` = dnr_fix_dates(`Id #`, `Collection Start Date/Time`),
                      `Collection Start Date/Time` = ymd(`Collection Start Date/Time`, tz = "America/Chicago")) %>% 
               select(StartDateTime = `Collection Start Date/Time`, StationID = `Id #`, `DNR Parameter Code`, `Numeric Value`) %>% 
               pivot_wider(names_from = `DNR Parameter Code`, values_from = `Numeric Value`, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               arrange(StartDateTime, StationID)),
  tar_target(dnr_swims_23_clean, dnr_swims_23 %>% 
               filter(!(str_detect(`Field #`, "BL") & !str_detect(`Field #`, "BLOOM")) & str_detect(`Field #`, "HABS")) %>% # 23 has two blooms samples (keeps blooms) and 3 random trib sites (drops trib sites)
               mutate(`DNR Parameter Code` = unname(dnr_rename(`DNR Parameter Code`)),
                      `Numeric Value` = if_else(`Result value` == "ND", 0.5 * as.numeric(LOD), `Numeric Value`),
                      `Collection Start Date/Time` = str_split_i(`Collection Start Date/Time`, " ", 1),
                      `Collection Start Date/Time` = dnr_fix_dates(`Id #`, `Collection Start Date/Time`),
                      `Collection Start Date/Time` = ymd(`Collection Start Date/Time`, tz = "America/Chicago"),
                      `Id #` = if_else(is.na(`Id #`), str_c('BLOOM', `Collection Start Date/Time`, sep = "_"), `Id #`)) %>% 
               select(StartDateTime = `Collection Start Date/Time`, StationID = `Id #`, `DNR Parameter Code`, `Numeric Value`) %>% 
               pivot_wider(names_from = `DNR Parameter Code`, values_from = `Numeric Value`, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
               mutate(po4 = 0.0007) %>% # per Ellen C 9/9/24, po4 samples were analyzed at Pace Analytical Duluth and were all NDs. Pace has a LOD of 0.0014 for po4, so setting all po4 results to 0.5*0.0014=0.0007
               arrange(StartDateTime, StationID)),
  
  tar_target(dnr_hydro_19_clean, dnr_hydro_19 %>% 
               mutate(`SpCond uS/cm` = if_else(is.na(`SpCond uS/cm`), 1000 * `SpCond mS/cm`, `SpCond uS/cm`),
                      `SpCond uS/cm` = if_else(`SpCond uS/cm` < 50, NA, `SpCond uS/cm`), #remove bad conductivity values
                      Block = as.numeric(str_split_i(Block, " ", 2)),
                      `Date (MM/DD/YYYY)` = force_tz(`Date (MM/DD/YYYY)`, tzone = "America/Chicago")) %>%
               left_join(dnr_blocks, by = join_by(Block, `Date (MM/DD/YYYY)` == StartDateTime)) %>%
               mutate(StationID = if_else(is.na(StationID) & Block == 6, "103", StationID),
                      StationID = if_else(is.na(StationID) & Block == 7, "10052505", StationID),
                      StationID = if_else(is.na(StationID) & Block == 9, "10052509", StationID),
                      StationID = if_else(is.na(StationID) & Block == 13, "10038057", StationID),
                      StationID = if_else(is.na(StationID) & Block == 14, "10052512", StationID)) %>% 
               left_join(dnr_sites, by = join_by(StationID)) %>%
               select(block = Block, station = StationID, site = SiteID, date = `Date (MM/DD/YYYY)`, depth = `Depth m`, temp = `Temp °C`, do_sat = `ODO % sat`, do = `ODO mg/L`, 
                      cond = `SpCond uS/cm`, ph = pH, turb = `Turbidity FNU`, chl_f = `Chlorophyll µg/L`)),
  tar_target(dnr_hydro_21_clean, dnr_hydro_21 %>% 
               filter(`Depth (m)` != "above") %>% 
               mutate(`Depth (m)` = as.numeric(`Depth (m)`),
                      Date = mdy(Date)) %>% 
               left_join(dnr_sites, by = join_by(Site == SiteID)) %>% 
               select(date = Date, station = StationID, site = Site, depth = `Depth (m)`, temp = `Temp (C)`, do_sat = `DO %`, do = `DO (mg/L)`, cond = `Specific Conductivity (uS/cm)`,
                      ph = pH, turb = `Turbidity (NTU)`)),
  tar_target(dnr_hydro_22_clean, dnr_hydro_22 %>%  
               mutate(Date = mdy(Date),
                      `pH (SU)` = if_else(`pH (SU)` < 6.5, NA, `pH (SU)`)) %>% # drop suspicious pH values
               left_join(dnr_sites, by = join_by(Site == SiteID)) %>% 
               select(date = Date, station = StationID, site = Site, depth = `Depth (m)`, temp = `Temp (C)`, do_sat = `DO %`, do = `DO (mg/L)`, cond = `Specific Conductivity (uS/cm)`,
                      ph = `pH (SU)`, turb = `Turbidity (NTU)`)),
  tar_target(dnr_hydro_23_clean, dnr_hydro_23 %>% 
               mutate(Date = mdy(Date),
                      `Temp (C)` = NA) %>% # 2023 temp values are all suspiciously high, dropping
               left_join(dnr_sites, by = join_by(Site == SiteID)) %>% 
               select(date = Date, station = StationID, site = Site, depth = `Depth (m)`, temp = `Temp (C)`, do_sat = `DO %`, do = `DO (mg/L)`, cond = `Specific Conductivity (uS/cm)`,
                      ph = `pH (SU)`, turb = `Turbidity (NTU)`)),
  
  #surface files
  tar_target(dnr_hydro_19_surf, dnr_hydro_19_clean %>% 
               filter(depth <= 2 & !is.na(station)) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE), chl_field = mean(chl_f, na.rm = TRUE))),
  tar_target(dnr_hydro_21_surf, dnr_hydro_21_clean %>% 
               filter(depth <= 2) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE))),
  tar_target(dnr_hydro_22_surf, dnr_hydro_22_clean %>% 
               filter(depth <= 2) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE))),
  tar_target(dnr_hydro_23_surf, dnr_hydro_23_clean %>% 
               filter(depth <= 2) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE))),
  
  tar_target(dnr_swims_clean, bind_rows(dnr_swims_19_clean, dnr_swims_21_clean, dnr_swims_22_clean, dnr_swims_23_clean)),
  tar_target(dnr_hydro_clean, bind_rows(dnr_hydro_19_clean, dnr_hydro_21_clean, dnr_hydro_22_clean, dnr_hydro_23_clean)),
  tar_target(dnr_hydro_surf, bind_rows(dnr_hydro_19_surf, dnr_hydro_21_surf, dnr_hydro_22_surf, dnr_hydro_23_surf)), 
  
  #combine dnr files
  tar_target(dnr, dnr_swims_clean %>% 
               left_join(dnr_stations, by = join_by(StationID)) %>% 
               left_join(dnr_sites, by = join_by(StationID)) %>% 
               rename(date = StartDateTime, station = StationID, site = SiteID) %>% 
               mutate(site = if_else(station == "104", 6, site)) %>% # Per Ellen C 9/9/24, 104 is very close to 103, and 103 was not sampled that round, so assuming 104 is also site 6
               left_join(dnr_hydro_surf, by = join_by(station, date)) %>%
               mutate(depth = 0, source = "WDNR", type = "Lake",
                      site = str_c("site ", site, " - ", station, sep = ""),
                      site = if_else(is.na(site), station, site),
                      latitude = as.numeric(StationLatitude),
                      longitude = as.numeric(StationLongitude)) %>% 
               select(-c(station, StationLatitude, StationLongitude)))
  
  
)