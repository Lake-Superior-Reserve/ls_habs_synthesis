lsnerr_targets <- list(
  
  # Helper functions ---------------------------------------------------------
  
  #' QC water quality data
  #' 
  #' Function to drop values based on NERR QC codes (1 and -3). Meant to be used on whole columns.
  #' Also used to QC meteorological data, as the flags are the same
  #'
  #' @param value parameter numerical value column
  #' @param flag corresponding flag column for value
  #'
  #' @returns value or NA if QC fails
  tar_target(qc_lsnerr_wq, Vectorize(function(value, flag) {
    if (is.na(flag)) return(value)
    else if (str_detect(flag, "1") | str_detect(flag, "-3")) return(NA) # remove values that are suspect (1) or did not pass QC (-3)
    else return(value)
  })),
  
  #' QC nutrient chemistry data
  #' 
  #' Function to drop values based on NERR QC codes (1 and -3). Also sets NDs as 0.5*LOD. Meant to be used on whole columns.
  #'
  #' @param value parameter numerical value column
  #' @param flag corresponding flag column for value
  #'
  #' @returns value or NA if QC fails
  tar_target(qc_lsnerr_nut, Vectorize(function(value, flag) {
    if (is.na(flag)) return(value)
    else if (str_detect(flag, "-4")) return(value * 0.5) # set NDs to be .5 the MDL, reported values are the MDL for that year
    else if (str_detect(flag, "G") | str_detect(flag, "-3")) return(NA) # remove values that are suspect (1) or rejected (-3) due to QC issues, but leave in samples that were suspect due to delayed analysis
    else return(value)
  })),
  
  #' Combine raw water quality files
  #' 
  #' Function to stack water quality files. There are a bunch and are structured identically, so it didn't make sense to list them all.
  #'
  #' @returns Data frame with all raw LSNERR water quality data
  tar_target(get_lkswq_file, function() {
    dir <- "raw_data/lsnerr/lkswq"
    lkswq <- data.frame()
    for (file in list.files(dir)) {
      lkswq <- bind_rows(lkswq, read_csv(str_c(dir,file,sep = "/")))
    }
    return(lkswq[1:30])
  }),  
  
  #' Combine raw meteorological files
  #' 
  #' Function to stack water quality files. There are a bunch and are structured identically, so it didn't make sense to list them all.
  #'
  #' @returns Data frame with all raw LSNERR water quality data
  tar_target(get_lksmet_file, function() {
    dir <- "raw_data/lsnerr/lksmet"
    lkswq <- data.frame()
    for (file in list.files(dir)) {
      lkswq <- bind_rows(lkswq, read_csv(str_c(dir,file,sep = "/")))
    }
    return(lkswq[1:28])
  }), 

  # Raw files --------------------------------------------------------
  
  #' Targets for raw source files
  #' 
  #' These targets list the paths for all of the LSNERR source files.
  #'
  #' @return A file path.
  tar_target(nerr_stations_file, "ref/nerr_sampling_stations.csv", format = "file"),
  
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
  
  
  # Read files ------------------------------------------------------------
  
  #' Read LSNERR station list
  #' 
  #' Reads in file of all NERR stations, filters to just LSNERR stations, changes column names, cleans site name
  #' 
  #' @param nerr_stations_file csv with all NERR station locations
  #'
  #' @return Data frame of LSNERR stations and coordinates.
  tar_target(nerr_stations, read_csv(nerr_stations_file) %>%
             select(site = `Station Code`, latitude = Latitude, longitude = Longitude) %>% 
             filter(str_detect(site, "lks")) %>% 
             mutate(site = str_sub(site, end = 5),
                    longitude = 0 - as.numeric(longitude)) %>% 
             filter(!duplicated(site))),
  
  #' Read LSNERR nutrient files
  #' 
  #' Reads in raw files of nutrient data, fixing column types as needed and bind together.
  #' 
  #' @param lksnut<year> csv with LSNERR nutrient data.
  #'
  #' @return Data frame of uncleaned nutrient data.
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
  
  #' Read LSNERR water quality files
  #' 
  #' Using `get_lkswq_file()`, Reads in raw files of water quality data, and binds together.
  #'
  #' @return Data frame of uncleaned water quality data.
  tar_target(lkswq, get_lkswq_file()),
  
  #' Read LSNERR meteorological files
  #' 
  #' Using `get_lksmet_file()`, Reads in raw files of meteorological data, and binds together.
  #'
  #' @return Data frame of uncleaned meteorological data.
  tar_target(lksmet, get_lksmet_file()),
  
  
  # Clean files ---------------------------------------------------------------------
  
  #' Clean LSNERR meteorological files
  #' 
  #' Cleans raw meteorological data, removing rows with no data, fixing dates, dropping bad data using `qc_lsnerr_wq()`, correcting a few other data issues
  #' 
  #' @param lksmet data frame of LSNERR meteorological data.
  #'
  #' @return Data frame of cleaned meteorological data.
  tar_target(lksmet_clean, lksmet %>%
               filter(!(is.na(ATemp) & is.na(RH) &is.na(BP) & is.na(WSpd) & is.na(MaxWSpd) & is.na(Wdir) & is.na(TotPAR) & is.na(TotPrcp) & is.na(TotSoRad))) %>%
               mutate(DatetimeStamp = mdy_hm(DatetimeStamp, tz = "America/Chicago"),
                      ATemp = qc_lsnerr_wq(ATemp, F_ATemp),
                      RH = qc_lsnerr_wq(RH, F_RH),
                      BP = qc_lsnerr_wq(BP, F_BP),
                      WSpd = qc_lsnerr_wq(WSpd, F_WSpd),
                      MaxWSpd = qc_lsnerr_wq(MaxWSpd, F_MaxWSpd),
                      Wdir = qc_lsnerr_wq(Wdir, F_Wdir),
                      SDWDir = qc_lsnerr_wq(SDWDir, F_SDWDir),
                      TotPAR = qc_lsnerr_wq(TotPAR, F_TotPAR),
                      TotPrcp = qc_lsnerr_wq(TotPrcp, F_TotPrcp),
                      TotSoRad = qc_lsnerr_wq(TotSoRad, F_TotSoRad)) %>%
               filter(!(is.na(ATemp) & is.na(RH) &is.na(BP) & is.na(WSpd) & is.na(MaxWSpd) & is.na(Wdir) & is.na(TotPAR) & is.na(TotPrcp) & is.na(TotSoRad))) 
  ),
  
  #' Clean LSNERR water quality files
  #' 
  #' Cleans raw water quality data, removing rows with no data, fixing dates, dropping bad data using `qc_lsnerr_wq()`, correcting a few other data issues
  #' 
  #' @param lkswq data frame of LSNERR water quality data.
  #'
  #' @return Data frame of cleaned water quality data.
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
  
  #' Clean LSNERR nutrient files
  #' 
  #' Cleans raw nutrient data, removing rows with no data, fixing dates, dropping bad data using `qc_lsnerr_nut()`, correcting a few other data issues
  #' 
  #' @param lksnut data frame of LSNERR nutrient data.
  #'
  #' @return Data frame of cleaned nutrient data.
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
  
  
  # Daily versions --------------------------------------------------------
  
  #' Make daily version of LSNERR meteorological data
  #' 
  #' Get daily averages of meteorological parameters and rename columns.
  #' 
  #' @param lksmet_clean data frame of LSNERR meteorological data.
  #'
  #' @return Data frame of daily meteorological data.
  tar_target(lksmet_dv, lksmet_clean %>%
               mutate(date = date(DatetimeStamp),
                      site = StationCode,
                      site = str_sub(site, end = 5)) %>%
               group_by(date, site) %>%
               summarise(airtemp = mean(ATemp, na.rm = T),
                         humidity = mean(RH, na.rm = T),
                         pressure = mean(BP, na.rm = T),
                         windspeed = mean(WSpd, na.rm = T),
                         winddir = mean(Wdir, na.rm = T),
                         par = sum(TotPAR, na.rm = T),
                         precip = sum(TotPrcp, na.rm = T),
                         sorad = sum(TotSoRad, na.rm = T)) %>% 
               ungroup()),
  
  #' Make daily version of LSNERR water quality data
  #' 
  #' Get daily averages of water quality parameters and rename columns.
  #' 
  #' @param lkswq_clean data frame of LSNERR water quality data.
  #'
  #' @return Data frame of daily water quality data.
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
                         chl_field = mean(ChlFluor, na.rm = T)) %>% 
               ungroup()),
  
  #' Make daily version of LSNERR nutrient data
  #' 
  #' Get daily averages of nutrient parameters and rename columns.
  #' 
  #' @param lkswq_clean data frame of LSNERR nutrient data.
  #'
  #' @return Data frame of daily nutrient data.
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
                         din = mean(DIN, na.rm = T)) %>% 
               ungroup()),
  
  
  # Make full file -------------------------------------------------------
  
  #' Make full LSNERR data frame
  #' 
  #' Combine daily water quality and nutrient data.
  #' 
  #' @param lkswq_dv data frame of daily LSNERR water quality data.
  #' @param lksnut_dv data frame of daily LSNERR nutrient data.
  #'
  #' @return Data frame of all LSNERR daily data.
  tar_target(lsnerr, full_join(lkswq_dv, lksnut_dv) %>%
               left_join(nerr_stations) %>%
               mutate(source = "LSNERR", type = "Estuary"))
  
)