source("R/wqp_clean_fn.R")

s1_targets <- list(
  
  #helper functions
  tar_target(drop_na_cols, function(df) {
    na_cols <- c()
    for (n in 1:length(df)) {
      cur_name <- colnames(df)[n]
      if (all(is.na(df[n]))) na_cols <- append(na_cols, cur_name)
    } 
    print("Dropping:")
    print(na_cols)
    return(select(df, -all_of(na_cols)))
  }),
  
  tar_target(replace_nan, function(val){
    return(if_else(is.nan(val), NA, val))
  }),
  
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
  
  tar_target(qc_lsnerr_wq, Vectorize(function(value, flag) {
    if (is.na(flag)) return(value)
    else if (str_detect(flag, "1") | str_detect(flag, "-3")) return(NA) # remove values that are suspect (1) or did not pass QC (-3)
    else return(value)
  })),
  
  tar_target(qc_lsnerr_nut, Vectorize(function(value, flag) {
    if (is.na(flag)) return(value)
    else if (str_detect(flag, "-4")) return(value * 0.5) # set NDs to be .5 the MDL, reported values are the MDL for that year
    else if (str_detect(flag, "G") | str_detect(flag, "-3")) return(NA) # remove values that are suspect (1) or rejected (-3) due to QC issues, but leave in samples that were suspect due to delayed analysis
    else return(value)
  })),
  
  wqp_clean_fns,
  
  #raw data files
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(nerr_stations_file, "ref/nerr_sampling_stations.csv", format = "file"),
  tar_target(cb_stations_file, "ref/CB_SiteCoordinates.csv", format = "file"),
  tar_target(wqp_synref_file, "ref/wqp_Synonym_Reference_Table.csv", format = "file"),
  
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
  
  tar_target(umd20_troll_file, "raw_data/umd/Troll 2020.xlsx", format = "file"),
  
  tar_target(get_umd23_file, function() {
    dir <- "raw_data/umd/2023"
    troll <- data.frame()
    for (file in list.files(dir)) {
      site <- str_split_i(file, "_", 3)
      site <- str_split_i(site, ".x", 1)
      new_troll <- read_xlsx(str_c(dir,file,sep = "/"))
      new_troll$site <- site
      troll <- bind_rows(troll, new_troll)
    }
    return(troll)
  }),
  
  tar_target(nps16_sonde_file, "raw_data/nps/Apostle Islands Sonde Data.csv", format = "file"),
  tar_target(nps_sonde_file, "raw_data/nps/sonde.csv", format = "file"),
  tar_target(nps_chem_file, "raw_data/nps/Apostle Islands Water Chemistry.csv", format = "file"),
  tar_target(nps_wetlab_file, "raw_data/nps/wetlab.csv", format = "file"),
  
  tar_target(cbnut_file, "raw_data/ncbc/Compiled_Bay_Data_2014-2022.xlsx", format = "file"),
  
  tar_target(lksnut12_file, "raw_data/lsnerr/lksnut/lksnut2012.csv", format = "file"),
  tar_target(lksnut13_file, "raw_data/lsnerr/lksnut/lksnut2013.csv", format = "file"),
  tar_target(lksnut14_file, "raw_data/lsnerr/lksnut/lksnut2014.csv", format = "file"),
  tar_target(lksnut15_file, "raw_data/lsnerr/lksnut/lksnut2015.csv", format = "file"),
  tar_target(lksnut16_file, "raw_data/lsnerr/lksnut/lksnut2016.csv", format = "file"),
  tar_target(lksnut17_file, "raw_data/lsnerr/lksnut/lksnut2017.csv", format = "file"),
  tar_target(lksnut18_file, "raw_data/lsnerr/lksnut/lksnut2018.csv", format = "file"),
  tar_target(lksnut19_file, "raw_data/lsnerr/lksnut/lksnut2019.csv", format = "file"),
  tar_target(lksnut20_file, "raw_data/lsnerr/lksnut/lksnut2020.csv", format = "file"),
  tar_target(lksnut21_file, "raw_data/lsnerr/lksnut/lksnut2021.csv", format = "file"),
  tar_target(lksnut22_file, "raw_data/lsnerr/lksnut/lksnut2022.csv", format = "file"),
  tar_target(lksnut23_file, "raw_data/lsnerr/lksnut/lksnut2023.csv", format = "file"),
  #tar_target(lksnut24_file, "raw_data/lsnerr/lksnut/lksnut2024.csv", format = "file"),
  
  tar_target(get_lkswq_file, function() {
    dir <- "raw_data/lsnerr/lkswq"
    lkswq <- data.frame()
    for (file in list.files(dir)) {
      lkswq <- bind_rows(lkswq, read_csv(str_c(dir,file,sep = "/")))
    }
    return(lkswq[1:30])
  }),
  
  tar_target(ncca20_sites_file, "raw_data/ncca/ncca20-siteinfo-great-lakes-data_revision-1_0.csv", format = "file"),
  tar_target(ncca15_sites_file, "raw_data/ncca/ncca_2015_site_information_great_lakes-data.csv", format = "file"),
  tar_target(ncca10_sites_file, "raw_data/ncca/assessed_ncca2010_siteinfo.revised.06212016.csv", format = "file"),
  tar_target(ncca10na_sites_file, "raw_data/ncca/not_assessed_ncca2010_siteinfo.revised.06212016.csv", format = "file"),
  
  tar_target(ncca20_secchi_file, "raw_data/ncca/ncca20_secchi_data.csv", format = "file"),
  tar_target(ncca15_secchi_file, "raw_data/ncca/ncca_2015_secchi_great_lakes-data.csv", format = "file"),
  
  tar_target(ncca20_hydro_file, "raw_data/ncca/ncca20_hydroprofile_data.csv", format = "file"),
  tar_target(ncca15_hydro_file, "raw_data/ncca/ncca_2015_hydrographic_profile_great_lakes-data.csv", format = "file"),
  tar_target(ncca10_hydro_file, "raw_data/ncca/assessed_ncca2010_hydrolab.csv", format = "file"),
  tar_target(ncca10na_hydro_file, "raw_data/ncca/not_assessed_ncca2010_hydrolab.csv", format = "file"),
  
  tar_target(ncca20_chem_file, "raw_data/ncca/ncca20_waterchem_data.csv", format = "file"),
  tar_target(ncca15_chem_file, "raw_data/ncca/ncca_2015_water_chemistry_great_lakes-data.csv", format = "file"),
  tar_target(ncca10_chem_file, "raw_data/ncca/assessed_ncca2010_waterchem.csv", format = "file"),
  tar_target(ncca10na_chem_file, "raw_data/ncca/not_assessed_ncca2010_waterchem.csv", format = "file"),
  
  
  #reading functions
  tar_target(ls_shp, read_sf(ls_shp_file)),
  tar_target(nerr_stations, read_csv(nerr_stations_file) %>%
               select(site = `Station Code`, latitude = Latitude, longitude = Longitude) %>% 
               filter(str_detect(site, "lks")) %>% 
               mutate(site = str_sub(site, end = 5),
                      longitude = 0 - as.numeric(longitude)) %>% 
               filter(!duplicated(site))),
  tar_target(cb_stations, read_csv(cb_stations_file) %>% 
               select(site = Site_ID, latitude = Lat, longitude = Long)),
  tar_target(wqp_synref, read_csv(wqp_synref_file)),
  
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
  
  tar_target(umd20_troll, read_xlsx(umd20_troll_file)),
  tar_target(umd23_troll, get_umd23_file()),
  
  tar_target(nps16_sonde, read_csv(nps16_sonde_file, skip = 4)),
  tar_target(nps_sonde, read_csv(nps_sonde_file)),
  tar_target(nps_chem, read_csv(nps_chem_file, skip = 5)),
  tar_target(nps_wetlab, read_csv(nps_wetlab_file)),
  
  tar_target(cbnut, read_xlsx(cbnut_file)),
  
  tar_target(lksnut12, read_csv(lksnut12_file, 
                                col_types = cols(NH4F = col_double(), NO2F = col_double(), 
                                                 NO3F = col_double(), F_CHLA_N = col_character()))),
  tar_target(lksnut13, read_csv(lksnut13_file, 
                                col_types = cols(F_Record = col_character(), TP = col_double(), 
                                                 F_TP = col_character(), NO2F = col_double(), 
                                                 F_NO2F = col_character(), NO3F = col_double(), 
                                                 F_NO3F = col_character(), TN = col_double(), 
                                                 F_TN = col_character()))),
  tar_target(lksnut14, read_csv(lksnut14_file,
                                col_types = cols(F_Record = col_character()))),
  tar_target(lksnut15, read_csv(lksnut15_file,
                                col_types = cols(F_Record = col_character()))),
  tar_target(lksnut16, read_csv(lksnut16_file,
                                col_types = cols(F_Record = col_character()))),
  tar_target(lksnut17, read_csv(lksnut17_file,
                                col_types = cols(F_CHLA_N = col_character()))),
  tar_target(lksnut18, read_csv(lksnut18_file)),
  tar_target(lksnut19, read_csv(lksnut19_file,
                                col_types = cols(F_Record = col_character(),
                                                 F_CHLA_N = col_character()))),
  tar_target(lksnut20, read_csv(lksnut20_file,
                                col_types = cols(F_Record = col_character(),
                                                 NH4F = col_double()))),
  tar_target(lksnut21, read_csv(lksnut21_file,
                                col_types = cols(F_Record = col_character()))),
  tar_target(lksnut22, read_csv(lksnut22_file,
                                col_types = cols(F_Record = col_character()))),
  tar_target(lksnut23, read_csv(lksnut23_file)),
  #tar_target(lksnut24, read_csv(lksnut24_file),
  
  tar_target(lksnut, bind_rows(lksnut12, lksnut13, lksnut14, lksnut15, lksnut16, lksnut17, lksnut18, lksnut19, lksnut20, lksnut21, lksnut22, lksnut23)),
  tar_target(lkswq, get_lkswq_file()),
  
  tar_target(ncca20_sites, read_csv(ncca20_sites_file) %>% 
               filter(GREAT_LAKE == "Lake Superior" & LAT_DD83 < 47.5 & LON_DD83 < -90 & VISIT_NO == 1)),
  tar_target(ncca15_sites, read_csv(ncca15_sites_file) %>% 
               filter(FEAT_NM == "Lake_Superior" & LAT_DD83 < 47.5 & LON_DD83 < -90 & VISIT_NO == 1)),
  tar_target(ncca10_sites, read_csv(ncca10_sites_file) %>% 
               filter(WTBDY_NM == "Lake Superior" & ALAT_DD < 47.5 & ALON_DD < -90 & VISIT_NO == 1)),
  tar_target(ncca10na_sites, read_csv(ncca10na_sites_file) %>% 
               filter(WTBDY_NM == "Lake Superior" & ALAT_DD < 47.5 & ALON_DD < -90 & VISIT_NO == 1)),
  
  tar_target(ncca20_secchi, read_csv(ncca20_secchi_file) %>% 
               filter(SITE_ID %in% ncca20_sites$SITE_ID)),
  tar_target(ncca15_secchi, read_csv(ncca15_secchi_file) %>% 
               filter(SITE_ID %in% ncca15_sites$SITE_ID)),
  
  tar_target(ncca20_hydro, read_csv(ncca20_hydro_file) %>% 
               filter(SITE_ID %in% ncca20_sites$SITE_ID)),
  tar_target(ncca15_hydro, read_csv(ncca15_hydro_file) %>% 
               filter(SITE_ID %in% ncca15_sites$SITE_ID)),
  tar_target(ncca10_hydro, read_csv(ncca10_hydro_file) %>% 
               filter(SITE_ID %in% ncca10_sites$SITE_ID)),
  tar_target(ncca10na_hydro, read_csv(ncca10na_hydro_file) %>% 
               filter(SITE_ID %in% ncca10na_sites$SITE_ID)),
  
  tar_target(ncca20_chem, read_csv(ncca20_chem_file) %>% 
               filter(SITE_ID %in% ncca20_sites$SITE_ID)),
  tar_target(ncca15_chem, read_csv(ncca15_chem_file) %>% 
               filter(SITE_ID %in% ncca15_sites$SITE_ID)),
  tar_target(ncca10_chem, read_csv(ncca10_chem_file) %>% 
               filter(SITE_ID %in% ncca10_sites$SITE_ID)),
  tar_target(ncca10na_chem, read_csv(ncca10na_chem_file) %>% 
               filter(SITE_ID %in% ncca10na_sites$SITE_ID)),
  
  #pull wqp data
  tar_target(wqp_pull_trib, TADA_BigDataRetrieval(huc = c("04010102", "04010201", "04010202", "04010301", "04010302"), startDate = "2010-01-01", endDate = "2023-12-31")),
  tar_target(wqp_pull_ls, TADA_BigDataRetrieval(huc = "04020300", startDate = "2010-01-01", endDate = "2023-12-31")),
  
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
                         po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), si = mean(SRSi, na.rm = T), 
                         chl = mean(Chl, na.rm = T), chl_filt = mean(Chl2, na.rm = T), phyco = mean(Phyco, na.rm = T), tss = mean(TSS, na.rm = T)) %>% 
               mutate(si = si * .001 * 28.086, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011, poc_filt = poc_filt * .001 * 12.011, #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, pon_filt = pon_filt * .001 * 14.007,
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974,
                      toc = poc + doc) %>% 
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
                         pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), si = mean(SRSi, na.rm = T), chl = mean(Chl, na.rm = T),  
                         tss = mean(TSS, na.rm = T)) %>% 
               mutate(si = si * .001 * 28.086, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011,  #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, 
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974,
                      toc = poc + doc) %>% 
               rename(date = Date, site = Site)),
  
  tar_target(umd20_troll_profile, umd20_troll %>% 
               filter(`Profile or Transect` == "P") %>% 
               select(date = `Date Time`, site = Station,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth = `Depth (ft) (525639)`,
                      chl_field = `Chlorophyll-a Concentration (µg/L) (652536)`, temp = `Water Temperature (°C) (519767)`,
                      cond = `Specific Conductivity (µS/cm) (675375)`, turb = `Turbidity (NTU) (695981)`, 
                      tds = `Total Dissolved Solids (ppt) (675375)`, tss = `Total Suspended Solids (mg/L) (695981)`) %>% 
               mutate(tds = tds * 1000,
                      depth = depth * 0.3048,
                      date = force_tz(date, tzone = "America/Chicago")) %>% 
               filter(depth > 0 & cond > 80)), #remove measurements above water/suspected above water
  tar_target(umd20_troll_transect, umd20_troll %>% 
               filter(`Profile or Transect` == "T") %>% 
               select(date = `Date Time`, site = Station,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth = `Depth (ft) (525639)`,
                      chl_field = `Chlorophyll-a Concentration (µg/L) (652536)`, temp = `Water Temperature (°C) (519767)`,
                      cond = `Specific Conductivity (µS/cm) (675375)`, turb = `Turbidity (NTU) (695981)`, 
                      tds = `Total Dissolved Solids (ppt) (675375)`, tss = `Total Suspended Solids (mg/L) (695981)`) %>% 
               mutate(tds = tds * 1000,
                      depth = depth * 0.3048,
                      date = force_tz(date, tzone = "America/Chicago")) %>% 
               filter(depth > 0 & cond > 80)), #remove measurements above water/suspected above water
  tar_target(umd20_troll_profile_surf, umd20_troll_profile %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_transect_surf, umd20_troll_transect %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_surf, umd20_troll_profile_surf %>% 
               bind_rows(umd20_troll_transect_surf)),
  
  tar_target(umd23_troll_clean, umd23_troll %>% 
               mutate(date = force_tz(`Date Time`, tzone = "America/Chicago"),
                      tds = `Total Dissolved Solids (ppt) (803397)` * 1000,
                      depth = `Depth (ft) (785112)` * 0.3048) %>% 
               select(date, site,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth,
                      chl_field = `Chlorophyll-a Fluorescence (RFU) (804408)`, temp = `Temperature (°C) (804550)`, # based on the troll 2020 file, chl RFU is equivalent to ug/L for this sensor
                      cond = `Specific Conductivity (µS/cm) (803397)`, turb = `Turbidity (NTU) (803671)`, tds) %>% 
               mutate(cond = if_else(cond < 50, NA, cond),
                      tds = if_else(tds < 40, NA, tds),
                      turb = if_else(turb > 10000, NA, turb))),
  tar_target(umd23_troll_surf, umd23_troll_clean %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup() %>% 
               mutate(site = sapply(site, function(site){
                 if (site == "BBay") return("Bark Bay")
                 else if (site == "MB5a") return("Mawikwe Bay")
                 else if (site == "MB5c") return("Mawikwe Bay c")
                 else if (site == "SBay") return("Siskiwit Bay")
                 else if (site == "SBay2") return("Siskiwit Bay 2")
                 else if (site == "SBay3") return("Siskiwit Bay 3")
                 else if (site == "SBay1") return("Siskiwit Bay")
                 else if (site == "SCav") return("Sea Caves")
                 else return(site)
               }))),
  
  tar_target(umd_troll_surf, bind_rows(umd20_troll_surf, umd23_troll_surf)),
  
  tar_target(umd, bind_rows(umd_1721_clean, umd_2123_clean) %>%
               mutate(across(-c(source, type, site, depth, latitude, longitude), replace_nan)) %>%
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp)) %>% 
               left_join(select(umd_troll_surf, -c(depth, tss, latitude, longitude)))),
  
  #nps sensor cleaning
  tar_target(nps16_sonde_deploy_clean, nps16_sonde %>% # continuous data at consistent depth
               mutate(site = Station,
                      date = parse_date_time(`Date/Time`, c("mdy HM", "mdy HMS"), tz = "America/Chicago"),
                      depth = if_else(is.na(`Depth m`), Depth, `Depth m`),
                      Latitude = if_else(str_detect(site, "West"), Latitude[1], Latitude), #fixing coordinates, on 7/7/16 SIW starts having has coordinates for SIE
                      Longitude = if_else(str_detect(site, "West"), Longitude[1], Longitude)) %>%
               filter(!is.na(date) & depth > 8) %>% #conveniently, types of data (cont vs discrete) have different date formats, so we can parse one type and remove the ones that fail to parse; also drop measurements during deployment, indicated by depth
               select(date, site, latitude = Latitude, longitude = Longitude, depth,
                      temp = `Temp °C`, do = `ODO mg/L`, do_sat = `ODO % sat`, ph = pH, cond = `SpCond µS/cm`, turb = `Turbidity NTU`)),
  tar_target(nps16_sonde_profile_clean, nps16_sonde %>% # discrete data at multiple depths
               mutate(site = paste("SIW", Transect_Station, sep = " "),
                      date = parse_date_time(`Date/Time`, c("%m/%d/%Y"), exact = TRUE, tz = "America/Chicago"),
                      depth = if_else(is.na(`Depth m`), Depth, `Depth m`)) %>%
               filter(!is.na(date)) %>%
               select(date, site, latitude = Latitude, longitude = Longitude, depth,
                      temp = `Temp °C`, do = `ODO mg/L`, do_sat = `ODO % sat`, ph = pH, cond = `SpCond µS/cm`, turb = `Turbidity NTU`)),
  tar_target(nps_chem_clean, nps_chem %>%
               mutate(site = if_else(!is.na(Transect_Station), str_c("SIW ", Transect_Station), Station),
                      Latitude = as.numeric(str_trim(Latitude)),
                      Longitude = as.numeric(str_trim(Longitude)),
                      `SRP (ug/L)` = if_else(`SRP (ug/L)` == "BDL" | `SRP (ug/L)` == "0", 0.0008, as.numeric(`SRP (ug/L)`)), # using 0.5 * UMD MDL
                      `NO3 -` = if_else(`NO3 -` == "BDL", 0.001, as.numeric(`NO3 -`)), # using 0.5 * UMD MDL
                      `SO43-` = as.numeric(`SO43-`),
                      po4 = `SRP (ug/L)` * 0.001,
                      tdp = `TDP (ug/L)` * 0.001, # convert to mg/L
                      pp = `PP (ug/L)` * 0.001,
                      tp = tdp + pp,
                      date = mdy(Date, tz = "America/Chicago")) %>%
               filter(!is.na(Latitude)) %>%
               select(date, site, latitude = Latitude, longitude = Longitude, depth = `Sample Depth (m)`,
                      chl = `Chl-a (ug/L)`, tp, tdp, pp, po4, no3 = `NO3 -`, cl = `Cl-`, so4 = `SO43-`)),
  tar_target(nps_wetlab_clean, nps_wetlab %>% 
               mutate(date = mdy_hm(DateTime, tz = "America/Chicago"),
                      date = ceiling_date(date, unit = "minute")) %>%
               filter(!is.na(date)) %>% 
               select(date, site = Station, depth = StationDepth, chl = CHL, cdom = CDOM) %>%
               mutate(across(c(chl, cdom), ~if_else(.x < 0, NA, .x))) %>% 
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE)))),
  tar_target(nps_sonde_clean, nps_sonde %>% 
               mutate(SpCond = if_else(is.na(SpCond), Cond, SpCond)) %>% # values in wrong column
               select(date = DateTime, site = Station, latitude = Latitude, longitude = Longitude, depth = SensorDepth,
                      chl = Chlorophyll, temp = Temp, do = ODOmg, do_sat = ODOsat, cond = SpCond, ph = pH, turb = Turbidity, tds = TDS) %>% 
               mutate(date = mdy_hm(date, tz = "America/Chicago"),
                      cond = if_else(cond < 70, NA, cond),
                      across(c(chl, temp, turb), ~if_else(.x < 0, NA, .x))) %>% 
               filter(date > ymd("2016-01-01")) %>% 
               bind_rows(nps16_sonde_deploy_clean) %>% 
               arrange(date)),
  
  tar_target(nps_wetlab_hourly, nps_wetlab_clean %>% 
               mutate(date = floor_date(date, unit = "hour")) %>%
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_hourly, nps_sonde_clean %>%
               mutate(date = floor_date(date, unit = "hour")) %>%
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(depth, chl, temp, do, do_sat, ph, cond, turb, tds), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_daily, nps_sonde_hourly %>%
               mutate(date = date(date)) %>%
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(depth, chl, temp, do, do_sat, ph, cond, turb, tds), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_wetlab_daily, nps_wetlab_hourly %>%
               mutate(date = date(date)) %>%
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_wetlab, nps_wetlab_hourly %>%
               inner_join(nps_sonde_hourly, by = join_by(date, site)) %>% 
               filter(depth.y != 5) %>% 
               select(date, site, latitude, longitude, depth = depth.y,
                      chl = chl.x, cdom, turb, temp, do, do_sat, ph, cond, tds) %>% 
               arrange(date)),
  
  tar_target(nps_sonde_surf, nps16_sonde_profile_clean %>%
               filter(depth <= 2) %>% # matching with chem depth of 2 m
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(temp, do, do_sat, ph, cond, turb), ~mean(.x, na.rm = TRUE)))),
  tar_target(nps_chem_surf, nps_chem_clean %>%
               filter(is.na(depth) | depth < 8)),
  
  tar_target(nps, nps_chem_surf %>%
               left_join(nps_sonde_surf, by = join_by(date, site)) %>%
               rename(latitude = latitude.x, # using coordinates from chem file, since they're consistent with other sonde data
                      longitude = longitude.x) %>%
               select(-c(latitude.y, longitude.y)) %>%
               mutate(source = "NPS", type = "Lake") %>%
               arrange(date) %>%
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  #ncbc cleaning
  tar_target(cbnut_clean, cbnut %>% 
               select(date = Date, site = Station, tp = TP_mgL, chl = Chla_ugL, pp = PP_ugL, po4 = SRP_mgL, tdp = `TDP_ug/L`,
                      poc = POC_ugL, pon = PON_ugL, no3 = `NO3_mg/L`, nh3 = `NH3_ug/L`, tss = `TSS _mgL`, temp = mn_temp,
                      do = mn_HDOmgl, do_sat = mn_HDOsat, ph = mn_pHunits, cond = mn_spcond, turb = mn_turb, chl_field = mn_chl) %>% 
               mutate(pp = 0.001 * pp, # convert to mg/L
                      tdp = 0.001 * tdp,
                      poc = 0.001 * poc,
                      pon = 0.001 * pon,
                      nh3 = 0.001 * nh3,
                      turb = if_else(turb < 0 | turb > 1000, NA, turb),
                      do = if_else(do > 20, NA, do),
                      ph = if_else(ph > 14, NA, ph),
                      date = force_tz(date, tzone = "America/Chicago"),
                      date = date(date)) %>% 
               left_join(cb_stations) %>% 
               mutate(source = "NCBC", type = "Lake") %>%
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  #lsnerr cleaning
  tar_target(lkswq_clean, lkswq %>%
               filter(!(is.na(Temp) & is.na(SpCond) &is.na(Sal) & is.na(DO_Pct) & is.na(DO_mgl) & is.na(Depth) & is.na(cDepth) & is.na(Level) & is.na(cLevel) & is.na(pH) & is.na(Turb) & is.na(ChlFluor))) %>% #drops almost half the rows
               mutate(DateTimeStamp = mdy_hm(DateTimeStamp, tz = "America/Chicago"),
                      Temp = qc_lsnerr_wq(Temp, F_Temp),
                      SpCond = qc_lsnerr_wq(SpCond, F_SpCond),
                      SpCond = 1000 * SpCond, # convert to us/cm
                      Sal = qc_lsnerr_wq(Sal, F_Sal),
                      DO_Pct = qc_lsnerr_wq(DO_Pct, F_DO_Pct),
                      DO_mgl = qc_lsnerr_wq(DO_mgl, F_DO_mgl),
                      Depth = qc_lsnerr_wq(Depth, F_Depth),
                      cDepth = qc_lsnerr_wq(cDepth, F_cDepth),
                      Level = qc_lsnerr_wq(Level, F_Level),
                      cLevel = qc_lsnerr_wq(cLevel, F_cLevel),
                      pH = qc_lsnerr_wq(pH, F_pH),
                      Turb = qc_lsnerr_wq(Turb, F_Turb),
                      ChlFluor = qc_lsnerr_wq(ChlFluor, F_ChlFluor)) %>%
               filter(!(is.na(Temp) & is.na(SpCond) &is.na(Sal) & is.na(DO_Pct) & is.na(DO_mgl) & is.na(Depth) & is.na(cDepth) & is.na(Level) & is.na(cLevel) & is.na(pH) & is.na(Turb) & is.na(ChlFluor))) # filter again to drop more than 10k rows
  ),
  
  tar_target(lksnut_clean, lksnut %>% 
               mutate(PO4F = qc_lsnerr_nut(PO4F, F_PO4F),
                      TP = qc_lsnerr_nut(TP, F_TP),
                      NH4F = qc_lsnerr_nut(NH4F, F_NH4F),
                      NO2F = qc_lsnerr_nut(NO2F, F_NO2F),
                      NO3F = qc_lsnerr_nut(NO3F, F_NO3F),
                      NO23F = qc_lsnerr_nut(NO23F, F_NO23F),
                      TN = qc_lsnerr_nut(TN, F_TN),
                      SiO4F = qc_lsnerr_nut(SiO4F, F_SiO4F),
                      CHLA_N = qc_lsnerr_nut(CHLA_N, F_CHLA_N),
                      TSS = qc_lsnerr_nut(TSS, F_TSS),
                      UncCHLa_N = qc_lsnerr_nut(UncCHLa_N, F_UncCHLa_N),
                      DIN = qc_lsnerr_nut(DIN, F_DIN),
                      UncCHLa_N = if_else(UncCHLa_N > 40, NA, UncCHLa_N), # drop suspect chl values from 2/3/2015 (abnormally high)
                      CHLA_N = if_else(is.na(CHLA_N), UncCHLa_N, CHLA_N), # this is mislabeled, confirmed with Hannah R 9/19/24
                      NO3F = if_else(is.na(NO3F) & str_detect(F_NO3F, "-4"), NO23F - NO2F, NO3F), # fill in NO3 values that weren't included due to NO2 or NO23 being NDs - if both are ND, we know NO3 is also ND, if NO2 is ND, we know NO3 is basically NO23
                      NO3F = abs(NO3F) # make sure we don't have any negative values - this makes them very small values below detection limit, but still not 0, which is unlikely
               )),

  tar_target(lkswq_dv, lkswq_clean %>%
               mutate(date = date(DateTimeStamp),
                      site = StationCode,
                      site = str_sub(site, end = 5)) %>%
               group_by(date, site) %>%
               summarise(temp = mean(Temp, na.rm = T),
                         cond = mean(SpCond, na.rm = T),
                         sal = mean(Sal, na.rm = T),
                         do_sat = mean(DO_Pct, na.rm = T),
                         do = mean(DO_mgl, na.rm = T),
                         depth = mean(cDepth, na.rm = T),
                         ph = mean(pH, na.rm = T),
                         turb = mean(Turb, na.rm = T),
                         chl_field = mean(ChlFluor, na.rm = T))),
  
  tar_target(lksnut_dv, lksnut_clean %>% 
             mutate(date = str_split_i(DateTimeStamp, " ", 1),
                    date = mdy(date, tz = "America/Chicago"),
                    site = `Station Code`,
                    site = str_sub(site, end = 5)) %>%
             group_by(date, site) %>%
             summarise(po4 = mean(PO4F, na.rm = T),
                       tp = mean(TP, na.rm = T),
                       nh3 = mean(NH4F, na.rm = T),
                       no3 = mean(NO23F, na.rm = T), # note that we're calling nitrate/nitrite no3
                       tn = mean(TN, na.rm = T),
                       si = mean(SiO4F, na.rm = T),
                       chl = mean(CHLA_N, na.rm = T),
                       tss = mean(TSS, na.rm = T),
                       din = mean(DIN, na.rm = T))),
  
  tar_target(lsnerr, full_join(lkswq_dv, lksnut_dv) %>%
               left_join(nerr_stations) %>%
               mutate(source = "LSNERR", type = "Estuary") %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  #ncca cleaning
  tar_target(ncca20_sites_clean, ncca20_sites %>% 
               select(ncca = UNIQUE_ID, site = SITE_ID, latitude = LAT_DD83, longitude = LON_DD83) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  tar_target(ncca15_sites_clean, ncca15_sites %>% 
               select(ncca = UNIQUE_ID, site = SITE_ID, latitude = LAT_DD83, longitude = LON_DD83) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  tar_target(ncca10_sites_clean, ncca10_sites %>% 
               select(site = SITE_ID, latitude = ALAT_DD, longitude = ALON_DD) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp)) %>%
               st_join(ncca20_sites_clean, st_is_within_distance, dist = 100) %>% # adding unique ID to sites if they're within 100 meters of a known site in 2015/2020
               st_join(ncca15_sites_clean, st_is_within_distance, dist = 100) %>%
               mutate(ncca.x = if_else(is.na(ncca.x) & !is.na(ncca.y), ncca.y, ncca.x)) %>% 
               select(ncca = ncca.x, site = site.x)),
  tar_target(ncca10na_sites_clean, ncca10na_sites %>% 
               select(site = SITE_ID, latitude = ALAT_DD, longitude = ALON_DD) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp)) %>%
               st_join(ncca20_sites_clean, st_is_within_distance, dist = 100) %>%
               st_join(ncca15_sites_clean, st_is_within_distance, dist = 100) %>%
               mutate(ncca.x = if_else(is.na(ncca.x) & !is.na(ncca.y), ncca.y, ncca.x)) %>% 
               select(ncca = ncca.x, site = site.x)),
  
  tar_target(ncca20_secchi_clean, ncca20_secchi %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>% 
               filter(REP == 1) %>% 
               select(date = DATE_COL, site = SITE_ID, secchi = MEAN_SECCHI_DEPTH)),
  tar_target(ncca15_secchi_clean, ncca15_secchi %>% 
               mutate(DATE_COL = as.Date(DATE_COL-2, origin = mdy("1-1-1900"))) %>%
               filter(REP == 0) %>% 
               select(date = DATE_COL, site = SITE_ID, secchi = MEAN_SECCHI_DEPTH)),
  tar_target(ncca10_secchi_clean, ncca10_hydro %>% 
               filter(PARAMETER == "SECCHI_MEAN") %>%
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(date = DATE_COL, site = SITE_ID, PARAMETER, RESULT) %>%
               mutate(RESULT = if_else(RESULT < 0, NA, RESULT)) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               rename(secchi = SECCHI_MEAN)),
  tar_target(ncca10na_secchi_clean, ncca10na_hydro %>% 
               filter(PARAMETER == "SECCHI_MEAN") %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(date = DATE_COL, site = SITE_ID, PARAMETER, RESULT) %>%
               mutate(RESULT = if_else(RESULT < 0, NA, RESULT)) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               rename(secchi = SECCHI_MEAN)),
  
  tar_target(ncca20_hydro_clean, ncca20_hydro %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(date = DATE_COL, site = SITE_ID, depth = DEPTH, cond = CONDUCTIVITY, do = DO, ph = PH, temp = TEMPERATURE)),
  tar_target(ncca15_hydro_clean, ncca15_hydro %>% 
               mutate(DATE_COL = as.Date(DATE_COL-2, origin = mdy("1-1-1900"))) %>%
               select(date = DATE_COL, site = SITE_ID, depth = DEPTH, cond = CONDUCTIVITY, do = DO, ph = PH, temp = TEMPERATURE)),
  tar_target(ncca10_hydro_clean, ncca10_hydro %>% 
               filter(CAST != "IM_CALC") %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(DATE_COL, SITE_ID, CAST, SDEPTH, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               select(date = DATE_COL, site = SITE_ID, depth = SDEPTH, cond = COND, do = DO, ph = pH, temp = TEMP, turb = TURB)),
  tar_target(ncca10na_hydro_clean, ncca10na_hydro %>% 
               filter(CAST != "IM_CALC") %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(DATE_COL, SITE_ID, CAST, SDEPTH, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               select(date = DATE_COL, site = SITE_ID, depth = SDEPTH, cond = COND, do = DO, ph = pH, temp = TEMP)),
  
  tar_target(ncca20_chem_clean, ncca20_chem %>% 
               mutate(RESULT = if_else(is.na(RESULT) & NARS_FLAG == "ND", 0.5 * MDL, RESULT),
                      DATE_COL = mdy(DATE_COL)) %>% 
               select(DATE_COL, SITE_ID, ANALYTE, RESULT) %>% 
               pivot_wider(names_from = ANALYTE, values_from = RESULT) %>% 
               mutate(NITRATE_N = if_else(is.na(NITRATE_N), NITRATE_NITRITE_N, NITRATE_N)) %>% 
               select(date = DATE_COL, site = SITE_ID, chl = CHLA, tn = NTL, no3 = NITRATE_N, no2 = NITRITE_N,
                      nh3 = AMMONIA_N, din = DIN, tp = PTL, po4 = SRP, cl = CHLORIDE, so4 = SULFATE)),
  tar_target(ncca15_chem_clean, ncca15_chem %>% 
               mutate(RESULT = if_else(is.na(RESULT) & NARS_FLAG == "ND", 0.5 * MDL, RESULT),
                      DATE_COL = dmy(DATE_COL)) %>% 
               select(DATE_COL, SITE_ID, ANALYTE, RESULT) %>% 
               pivot_wider(names_from = ANALYTE, values_from = RESULT) %>% 
               select(date = DATE_COL, site = SITE_ID, chl = CHLA, tn = NTL, no3 = NITRATE_N, no2 = NITRITE_N,
                      nh3 = AMMONIA_N, din = DIN, tp = PTL, po4 = SRP, cl = CHLORIDE, so4 = SULFATE, si = SILICA)),
  tar_target(ncca10_chem_clean, ncca10_chem %>% 
               mutate(RESULT = if_else(!is.na(QACODE) & QACODE == "N", 0.5 * MDL, RESULT),
                      DATE_COL = mdy(DATE_COL)) %>% 
               select(DATE_COL, SITE_ID, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               select(date = DATE_COL, site = SITE_ID, chl = CHLA, tn = NTL, no3 = NO3NO2, tkn = TKN,
                      nh3 = NH3, din = DIN, tp = PTL, po4 = SRP)),
  tar_target(ncca10na_chem_clean, ncca10na_chem %>% 
               mutate(RESULT = if_else(!is.na(QACODE) & QACODE == "N", 0.5 * MDL, RESULT),
                      DATE_COL = mdy(DATE_COL)) %>% 
               select(DATE_COL, SITE_ID, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               select(date = DATE_COL, site = SITE_ID, chl = CHLA, tn = NTL, no3 = NO3NO2,
                      nh3 = NH3, din = DIN, tp = PTL, po4 = SRP)),
  
  tar_target(ncca_sites, bind_rows(ncca20_sites_clean, ncca15_sites_clean, ncca10_sites_clean, ncca10na_sites_clean)),
  tar_target(ncca_secchi, bind_rows(ncca20_secchi_clean, ncca15_secchi_clean, ncca10_secchi_clean, ncca10na_secchi_clean)),
  tar_target(ncca_hydro, bind_rows(ncca20_hydro_clean, ncca15_hydro_clean, ncca10_hydro_clean, ncca10na_hydro_clean)),
  tar_target(ncca_chem, bind_rows(ncca20_chem_clean, ncca15_chem_clean, ncca10_chem_clean, ncca10na_chem_clean)),
  
  tar_target(ncca_hydro_surf, ncca_hydro %>% 
               filter(depth <= 2) %>% 
               group_by(date, site) %>% 
               summarise(across(c(depth, cond, do, ph, temp, turb), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(c(cond, do, ph, temp, turb), replace_nan)) %>% 
               select(-depth)),
  
  tar_target(ncca, ncca_chem %>% 
               left_join(ncca_hydro_surf) %>% 
               left_join(ncca_secchi) %>% 
               left_join(ncca_sites) %>% 
               mutate(site = if_else(!is.na(ncca), ncca, site),
                      source = "NCCA",
                      type = if_else(site %in% c("NGL_MN-10014", "NGL_MN-10017", "NGL_MN-10012", "NGL_MN-10011", "NCCAGL10-2001",
                                                 "NCCAGL10-GLBA10-118", "NGL_MN-10019", "NGL_MN-10015", "NCCAGL10-GLBA10-134"),
                                     "Estuary", "Great Lake")) %>% 
               select(-ncca) %>% 
               arrange(date)),
  
  
  # cleaning WQP data
  # first just trim sites outside area, which requires autoclean
  tar_target(wqp_trib, wqp_pull_trib %>%
               TADA_AutoClean() %>%
               filter(TADA.LatitudeMeasure > 46)), #drop sites missing lat/long
  tar_target(wqp_ls, wqp_pull_ls %>%
               TADA_AutoClean() %>%
               filter(TADA.LatitudeMeasure < 47.3 & TADA.LongitudeMeasure < -90.0)), #remove LS data outside of western lobe
  
  #run all other recommended cleaning functions
  tar_target(wqp_s1, bind_rows(wqp_trib, wqp_ls) %>%
               TADA_AnalysisDataFilter(clean = TRUE, surface_water = TRUE) %>%
               filter(!(ActivityMediaSubdivisionName %in% c("Surface Water Sediment", "Interstitial Water", "Drinking Water", "Stormwater", "Ambient Air", "Snowmelt"))) %>% #drops sediment samples from surface waters
               TADA_RunKeyFlagFunctions() %>% #also drops ~160k NAs
               TADA_FindPotentialDuplicatesSingleOrg() %>% 
               TADA_FindPotentialDuplicatesMultipleOrgs(org_hierarchy = c("NARS_WQX", "NALMS")) %>%
               filter(TADA.SingleOrgDup.Flag == 'Unique' & TADA.ResultSelectedMultipleOrgs == 'Y') %>% #drops ~800k duplicated temp records, spot checked and seemed to be accurate
               TADA_FlagMeasureQualifierCode(clean = T) %>% 
               TADA_SimpleCensoredMethods() %>% 
               filter(TADA.CensoredData.Flag != "Detection condition is missing and required for censored data ID.") %>%
               TADA_AutoFilter() %>% 
               TADA_FlagDepthCategory()),
  
  #harmonizing parameters and removing unnecessary parameters
  tar_target(wqp_s2, wqp_s1 %>% 
               filter(TADA.CharacteristicName %in% unique(wqp_synref$TADA.CharacteristicName)) %>%
               TADA_HarmonizeSynonyms(ref = wqp_synref) %>% 
               filter(!(TADA.ComparableDataIdentifier %in% wqp_invalid_units)) %>%
               mutate(TADA.ResultMeasureValue = wqp_fix_values(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode, TADA.ResultMeasureValue),
                      TADA.ResultMeasure.MeasureUnitCode = wqp_fix_units(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode),
                      TADA.CharacteristicName = if_else(TADA.CharacteristicName == "DISSOLVED OXYGEN (DO)" & TADA.ResultMeasure.MeasureUnitCode == "%", "DISSOLVED OXYGEN SATURATION", TADA.CharacteristicName)) %>%
               TADA_CreateComparableID()),
  
  #relabel site types and remove inland lakes/wetlands/etc.
  tar_target(wqp_s3, wqp_s2 %>%
               mutate(MonitoringLocationTypeName = renameMonitType(MonitoringLocationTypeName,MonitoringLocationIdentifier)) %>%
               filter(MonitoringLocationTypeName %in% c("Great Lake", "Estuary", "River/Stream"))),
  # could potentially filter out River/Stream sites in hucs 04010201/04010202, since they drain into the estuary before the lake
  # make sure everything is correctly labeled - remove filter above and then run:
  # filter(wqp_s3, MonitoringLocationTypeName == "Great Lake") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "Estuary") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "River/Stream") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "Lake") %>%
  #   TADA_OverviewMap()

  tar_target(wqp_long, wqp_s3 %>%
               select(-all_of(wqp_drop_cols(wqp_s3))) %>%
               TADA_RetainRequired()),
  
  tar_target(wqp_wide, wqp_long %>% 
               filter(!(TADA.DepthCategory.Flag %in% c("Bottom", "Middle"))) %>% #assume surface if not specified
               select(TADA.ComparableDataIdentifier, ActivityStartDate, TADA.ResultMeasureValue, OrganizationIdentifier, MonitoringLocationTypeName,
                      TADA.MonitoringLocationIdentifier, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>% 
               pivot_wider(names_from = TADA.ComparableDataIdentifier, values_from = TADA.ResultMeasureValue, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
               mutate(date = ymd(ActivityStartDate, tz = "America/Chicago"),
                      nh3 = rowMeans(across(c(`AMMONIA_FILTERED_AS N_MG/L`, `AMMONIA_UNFILTERED_AS N_MG/L`)), na.rm = TRUE), #combining filtered and unfiltered parameters where they shouldn't be split (ions, mislabeled chl)
                      cl = rowMeans(across(c(`CHLORIDE_FILTERED_NA_MG/L`, `CHLORIDE_UNFILTERED_NA_MG/L`)), na.rm = TRUE),
                      chl = rowMeans(across(c(`CHLOROPHYLL A_UNFILTERED_NA_UG/L`, `CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN_UNFILTERED_NA_MG/L`, `CHLOROPHYLL A_FILTERED_NA_UG/L`, `CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN_FILTERED_NA_MG/L`))), # all chl sensor measurements (ie buoys) were removed when filtering to surface samples only
                      pheo = rowMeans(across(c(`PHEOPHYTIN A_FILTERED_NA_UG/L`, `PHEOPHYTIN A_UNFILTERED_NA_UG/L`)), na.rm = TRUE),
                      no23 = rowMeans(across(c(`NITRATE + NITRITE_FILTERED_AS N_MG/L`, `NITRATE + NITRITE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      no3 = rowMeans(across(c(`NITRATE_FILTERED_AS N_MG/L`, `NITRATE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      no2 = rowMeans(across(c(`NITRITE_FILTERED_AS N_MG/L`, `NITRITE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      po4 = rowMeans(across(c(`ORTHOPHOSPHATE_FILTERED_AS P_MG/L`, `ORTHOPHOSPHATE_UNFILTERED_AS P_MG/L`)), na.rm = TRUE),
                      si = rowMeans(across(c(`SILICA_FILTERED_AS SI_MG/L`, `SILICA_UNFILTERED_AS SI_MG/L`)), na.rm = TRUE),
                      across(c(nh3, cl, chl, pheo, no23, no3, no2, po4, si), replace_nan),
                      no23 = if_else(no3 > no23, no3, no23),
                      no23 = if_else(is.na(no23) & !is.na(no3) & !is.na(no2), no3 + no2, no23),
                      no23 = if_else(is.na(no23) & !is.na(no3) & is.na(no2), no3, no23),
                      temp = if_else(`TEMPERATURE_NA_NA_DEG C` > -1 & `TEMPERATURE_NA_NA_DEG C` < 30, `TEMPERATURE_NA_NA_DEG C`, NA), # remove obviously wrong temp data
                      do = if_else(`DISSOLVED OXYGEN (DO)_NA_NA_MG/L` > 0 & `DISSOLVED OXYGEN (DO)_NA_NA_MG/L` < 20, `DISSOLVED OXYGEN (DO)_NA_NA_MG/L`, NA), # remove obviously wrong/outlier do data
                      do_sat = if_else(`DISSOLVED OXYGEN SATURATION_NA_NA_%` > 0 & `DISSOLVED OXYGEN SATURATION_NA_NA_%` < 140, `DISSOLVED OXYGEN SATURATION_NA_NA_%`, NA), # remove obviously wrong/outlier do data
                      ph = if_else(`PH_NA_NA_NA` > 5 & `PH_NA_NA_NA` < 11, `PH_NA_NA_NA`, NA), # remove obviously wrong/outlier ph data
                      cond = if_else(`CONDUCTIVITY_NA_NA_US/CM` > 10000, `CONDUCTIVITY_NA_NA_US/CM` * 0.001, `CONDUCTIVITY_NA_NA_US/CM`), # fixing obviously mislabeled cond units
                      cond = if_else(cond > 2000, NA, cond),
                      turb = if_else(`TURBIDITY_NA_NA_NTU` < 0, NA, `TURBIDITY_NA_NA_NTU`),
                      trans_tube = if_else(`TRANSPARENCY, TUBE WITH DISK_NA_NA_IN` > 120, NA, `TURBIDITY_NA_NA_NTU`),
                      secchi = `SECCHI DEPTH_NA_NA_M`,
                      flow = if_else(`STREAM FLOW_NA_NA_CFS` < 0, NA, `STREAM FLOW_NA_NA_CFS`),
                      tss = if_else(`TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_MG/L` < 0, NA, `TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_MG/L`),
                      tds = if_else(`TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L` > 0 & `TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L` < 1000, `TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L`, NA),
                      toc = `ORGANIC CARBON_UNFILTERED_NA_MG/L`,
                      doc = if_else(`ORGANIC CARBON_UNFILTERED_NA_MG/L` < `ORGANIC CARBON_FILTERED_NA_MG/L`, `ORGANIC CARBON_UNFILTERED_NA_MG/L`, `ORGANIC CARBON_FILTERED_NA_MG/L`), # if DOC is greater than TOC, set equal to TOC 
                      ton = `ORGANIC NITROGEN_UNFILTERED_AS N_MG/L`,
                      don = if_else(`ORGANIC NITROGEN_UNFILTERED_AS N_MG/L` < `ORGANIC NITROGEN_FILTERED_AS N_MG/L`, `ORGANIC NITROGEN_UNFILTERED_AS N_MG/L`, `ORGANIC NITROGEN_FILTERED_AS N_MG/L`), # if DON is greater than TON, set equal to TON 
                      tkn = `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L`,
                      tdkn = if_else(`TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L` < `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_FILTERED_AS N_MG/L`, `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L`, `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_FILTERED_AS N_MG/L`),
                      tn = `TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L`,
                      tdn = if_else(`TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L` < `TOTAL NITROGEN, MIXED FORMS_FILTERED_AS N_MG/L`, `TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L`, `TOTAL NITROGEN, MIXED FORMS_FILTERED_AS N_MG/L`),  
                      tp = `TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L`,
                      tdp = if_else(`TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L` < `TOTAL PHOSPHORUS, MIXED FORMS_FILTERED_AS P_MG/L`, `TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L`, `TOTAL PHOSPHORUS, MIXED FORMS_FILTERED_AS P_MG/L`)  
               ) %>% 
               select(date, source = OrganizationIdentifier, type = MonitoringLocationTypeName, site = TADA.MonitoringLocationIdentifier,
                      latitude = TADA.LatitudeMeasure, longitude = TADA.LongitudeMeasure, temp, do, do_sat, ph, cond, turb, trans_tube, secchi, flow,
                      tss, tds, chl, pheo, toc, doc, ton, don, tdkn, tkn, tdn, tn, nh3, no3 = no23, tdp, tp, po4, cl, si) %>% #note that were calling nitrate/nitrite no3
               filter(!if_all(-c(date, source, type, site, latitude, longitude, temp), is.na)) %>% # drop rows with only temp
               arrange(date) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  
  # making full files by type
  tar_target(lake_full, dnr %>%
               bind_rows(filter(umd, type == "Lake")) %>% 
               bind_rows(cbnut_clean) %>% 
               bind_rows(filter(wqp_wide, type == "Great Lake")) %>%
               bind_rows(filter(ncca, type == "Great Lake")) %>%
               bind_rows(nps) %>% 
               mutate(chl = rowMeans(across(c(chl, chl_field)), na.rm = TRUE), #combine chl and chl_field
                      chl = if_else(is.nan(chl), NA, chl)) %>% 
               select(date, site, source, depth, chl, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl) %>% 
               arrange(date)),
  
  tar_target(trib_full, filter(umd, type == "Watershed") %>% 
               bind_rows(filter(wqp_wide, type == "River/Stream")) %>%
               select(date, site, source, depth, flow, chl, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl) %>% 
               arrange(date)),
  
  tar_target(est_full, lsnerr %>%
               bind_rows(filter(wqp_wide, type == "Estuary")) %>%
               bind_rows(filter(ncca, type == "Estuary")) %>%
               filter(source != "NARS_WQX") %>% # dropping duplicates
               mutate(chl = rowMeans(across(c(chl, chl_field)), na.rm = TRUE), #combine chl and chl_field
                      chl = if_else(is.nan(chl), NA, chl)) %>%
               select(date, site, source, depth, flow, chl, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, si, cl) %>% 
               arrange(date))
  
)

