s1_targets <- list(
  
  #helper functions
  tar_target(dnr_rename, Vectorize(function(param) {
    if (param == "530") return("tss")
    else if (param == "600") return("tn")
    else if (param == "608") return("nh3")
    else if (param == "625") return("tkn")
    else if (param == "631") return("no3")
    else if (param == "665") return("tp")
    else if (param == "671") return("po4")
    else if (param == "681") return("doc")
    else if (param == "99717") return("chl")
    else return(param)
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
  
  tar_target(qc_lsnerr, Vectorize(function(value, flag) {
    if (is.na(flag)) return(value)
    else if (str_detect(flag, "1") | str_detect(flag, "-3")) return(NA)
    else return(value)
  })),
  
  #raw data files
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(nerr_stations_file, "ref/nerr_sampling_stations.csv", format = "file"),
  
  tar_target(dnr_swims_19_file, "raw_data/wdnr/2019LSNSHABs_SWIMS.xlsx", format = "file"),
  tar_target(dnr_swims_21_file, "raw_data/wdnr/2021NSData_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_22_file, "raw_data/wdnr/2022NSHAB_LDES.xlsx", format = "file"),
  tar_target(dnr_swims_23_file, "raw_data/wdnr/2023NSHABs_SWIMS.xlsx", format = "file"),
  
  tar_target(dnr_hydro_19_file, "raw_data/wdnr/2019_Hydro.xlsx", format = "file"),
  tar_target(dnr_hydro_21_file, "raw_data/wdnr/2021_Hydro.csv", format = "file"),
  tar_target(dnr_hydro_22_file, "raw_data/wdnr/AllDepths2022Hydro.csv", format = "file"),
  tar_target(dnr_hydro_23_file, "raw_data/wdnr/2023LSNSHABsHydro_alldepth.csv", format = "file"),
  
  tar_target(umd_1721_file, "raw_data/umd/SouthShoreArchive.csv", format = "file"),
  tar_target(umd_2123_file, "raw_data/umd/SouthShoreArchive2021to2023.csv", format = "file"),
  
  tar_target(get_lkswq_file, function() {
    dir <- "raw_data/lsnerr/lkswq"
    lkswq <- data.frame()
    for (file in list.files(dir)) {
      lkswq <- bind_rows(lkswq, read_csv(str_c(dir,file,sep = "/")))
    }
    return(lkswq[1:30])
  }),
  
  #reading functions
  tar_target(ls_shp, read_sf(ls_shp_file)),
  tar_target(nerr_stations, read_csv(nerr_stations_file) %>% 
               select(site = `Station Code`, latitude = Latitude, longitude = Longitude)),
  
  tar_target(dnr_swims_19, read_xlsx(dnr_swims_19_file)),
  tar_target(dnr_swims_21, read_xlsx(dnr_swims_21_file)),
  tar_target(dnr_swims_22, read_xlsx(dnr_swims_22_file)),
  tar_target(dnr_swims_23, read_xlsx(dnr_swims_23_file)),
  
  tar_target(dnr_hydro_19, read_xlsx(dnr_hydro_19_file)),
  tar_target(dnr_hydro_21, read_csv(dnr_hydro_21_file)),
  tar_target(dnr_hydro_22, read_csv(dnr_hydro_22_file)),
  tar_target(dnr_hydro_23, read_csv(dnr_hydro_23_file)),
  
  tar_target(umd_1721, read_csv(umd_1721_file)),
  tar_target(umd_2123, read_csv(umd_2123_file)),
  
  tar_target(lkswq, get_lkswq_file()),
  
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
               mutate(Date = mdy(Date)) %>% 
               left_join(dnr_sites, by = join_by(Site == SiteID)) %>% 
               select(date = Date, station = StationID, site = Site, depth = `Depth (m)`, temp = `Temp (C)`, do_sat = `DO %`, do = `DO (mg/L)`, cond = `Specific Conductivity (uS/cm)`,
                      ph = `pH (SU)`, turb = `Turbidity (NTU)`)),
  tar_target(dnr_hydro_23_clean, dnr_hydro_23 %>% 
               mutate(Date = mdy(Date)) %>% 
               left_join(dnr_sites, by = join_by(Site == SiteID)) %>% 
               select(date = Date, station = StationID, site = Site, depth = `Depth (m)`, temp = `Temp (C)`, do_sat = `DO %`, do = `DO (mg/L)`, cond = `Specific Conductivity (uS/cm)`,
                      ph = `pH (SU)`, turb = `Turbidity (NTU)`)),

  #surface files
  tar_target(dnr_hydro_19_surf, dnr_hydro_19_clean %>% 
               filter(depth <= 1 & !is.na(station)) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE), chl_field = mean(chl_f, na.rm = TRUE))),
  tar_target(dnr_hydro_21_surf, dnr_hydro_21_clean %>% 
               filter(depth <= 1) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE))),
  tar_target(dnr_hydro_22_surf, dnr_hydro_22_clean %>% 
               filter(depth <= 1) %>% 
               group_by(date, station) %>%
               summarise(temp = mean(temp, na.rm = TRUE), do_sat = mean(do_sat, na.rm = TRUE), do = mean(do, na.rm = TRUE),
                         cond = mean(cond, na.rm = TRUE), ph = mean(ph, na.rm = TRUE), turb = mean(turb, na.rm = TRUE))),
  tar_target(dnr_hydro_23_surf, dnr_hydro_23_clean %>% 
               filter(depth <= 1) %>% 
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
                      site = if_else(is.na(site), station, site)) %>% 
               select(-station) %>% 
               st_as_sf(coords = c("StationLongitude", "StationLatitude"), crs = st_crs(ls_shp))),
  
  # umd clean and combine
  tar_target(umd_1721_clean, umd_1721 %>% 
               mutate(Date = mdy(Date, tz = "America/Chicago"),
                      NO3 = if_else(NO3Flag == "bdl", 0.001, NO3), # set to 0.5 * detection limit if below detection limit
                      PON = if_else(PONFlag == "bdl", 0.0009, PON), # NO3 LOD = 0.002, PON LOD = 0.0018, SRP LOD = 0.0015 per Sandy B 9/17/24
                      SRP = if_else(SRPFlag == "bdl", 0.0008, SRP)) %>% 
               group_by(Date, Site) %>% 
               summarise(latitude = mean(Latitude), longitude = mean(Longitude), depth = mean(Depth), source = first(Project), type = first(Type),
                         doc = mean(DOC, na.rm = T), poc = mean(POC, na.rm = T), poc_filt = mean(POC2, na.rm = T), no3 = mean(NO3, na.rm = T),
                         nh3 = mean(NH3, na.rm = T), tdn = mean(TDN, na.rm = T), pon = mean(PON, na.rm = T), pon_filt = mean(PON2, na.rm = T),
                         po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), srsi = mean(SRSi, na.rm = T), 
                         chl = mean(Chl, na.rm = T), chl_filt = mean(Chl2, na.rm = T), phyco = mean(Phyco, na.rm = T), tss = mean(TSS, na.rm = T)) %>% 
               mutate(srsi = srsi * .001 * 60.084, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011, poc_filt = poc_filt * .001 * 12.011, #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, pon_filt = pon_filt * .001 * 14.007,
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974) %>% 
               rename(date = Date, site = Site)),
  tar_target(umd_2123_clean, umd_2123 %>% 
               mutate(Date = ymd(Date, tz = "America/Chicago"),
                      NO3 = if_else(NO3Flag == "bdl", 0.001, NO3), # set to 0.5 * detection limit if below detection limit
                      PON = if_else(PONFlag == "bdl", 0.0009, PON) # NO3 LOD = 0.002, PON LOD = 0.0018, SRP LOD = 0.0015 per Sandy B 9/17/24
                      ) %>% 
               group_by(Date, Site) %>% 
               summarise(latitude = mean(Latitude), longitude = mean(Longitude), depth = 0, source = "NPS/BRICO", type = first(Type),
                         doc = mean(DOC, na.rm = T), poc = mean(POC, na.rm = T), no3 = mean(NO3, na.rm = T), nh3 = mean(NH3, na.rm = T), 
                         tdn = mean(TDN, na.rm = T), pon = mean(PON, na.rm = T), po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), 
                         pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), srsi = mean(SRSi, na.rm = T), chl = mean(Chl, na.rm = T),  
                         tss = mean(TSS, na.rm = T)) %>% 
               mutate(srsi = srsi * .001 * 60.084, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011,  #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, 
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974) %>% 
               rename(date = Date, site = Site)),
  
  tar_target(umd, bind_rows(umd_1721_clean, umd_2123_clean) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  #lsnerr cleaning
  tar_target(lkswq_clean, lkswq %>%
               filter(!(is.na(Temp) & is.na(SpCond) &is.na(Sal) & is.na(DO_Pct) & is.na(DO_mgl) & is.na(Depth) & is.na(cDepth) & is.na(Level) & is.na(cLevel) & is.na(pH) & is.na(Turb) & is.na(ChlFluor))) %>% #drops almost half the rows
               mutate(DateTimeStamp = mdy_hm(DateTimeStamp, tz = "America/Chicago"),
                      Temp = qc_lsnerr(Temp, F_Temp),
                      SpCond = qc_lsnerr(SpCond, F_SpCond),
                      Sal = qc_lsnerr(Sal, F_Sal),
                      DO_Pct = qc_lsnerr(DO_Pct, F_DO_Pct),
                      DO_mgl = qc_lsnerr(DO_mgl, F_DO_mgl),
                      Depth = qc_lsnerr(Depth, F_Depth),
                      cDepth = qc_lsnerr(cDepth, F_cDepth),
                      Level = qc_lsnerr(Level, F_Level),
                      cLevel = qc_lsnerr(cLevel, F_cLevel),
                      pH = qc_lsnerr(pH, F_pH),
                      Turb = qc_lsnerr(Turb, F_Turb),
                      ChlFluor = qc_lsnerr(ChlFluor, F_ChlFluor)) %>%
               filter(!(is.na(Temp) & is.na(SpCond) &is.na(Sal) & is.na(DO_Pct) & is.na(DO_mgl) & is.na(Depth) & is.na(cDepth) & is.na(Level) & is.na(cLevel) & is.na(pH) & is.na(Turb) & is.na(ChlFluor))) # filter again to drop more than 10k rows
  ),

  tar_target(lkswq_dv, lkswq_clean %>%
               mutate(date = date(DateTimeStamp),
                      site = StationCode) %>%
               group_by(date, site) %>%
               summarise(temp = mean(Temp, na.rm = T),
                         cond = mean(SpCond, na.rm = T),
                         sal = mean(Sal, na.rm = T),
                         do_sat = mean(DO_Pct, na.rm = T),
                         do = mean(DO_mgl, na.rm = T),
                         depth = mean(cDepth, na.rm = T),
                         ph = mean(pH, na.rm = T),
                         turb = mean(Turb, na.rm = T),
                         chl_field = mean(ChlFluor, na.rm = T)) %>%
               left_join(nerr_stations) %>%
               mutate(source = "LSNERR", type = "Estuary") %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  
  
  
  tar_target(lake_full, dnr %>%
               bind_rows(filter(umd, type == "Lake")) %>% 
               select(date, site, source, depth, chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, poc, tn, tdn, pon, no3, nh3, tp, tdp, pp, po4, srsi, phyco) %>% 
               arrange(date)),
  
  tar_target(trib_full, filter(umd, type == "Watershed") %>% 
               select(date, site, source, depth, chl, tss, # reorder, drop station and type (all lake)
                      doc, poc, tdn, pon, no3, nh3, tp, tdp, pp, po4, srsi, phyco) %>% 
               arrange(date))
  
)

