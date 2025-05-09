ncca_targets <- list(
  
  # raw files -------------------------------------------------------------
  
  #' Targets for raw source files
  #' 
  #' These targets list the paths for all of the NCCA source files.
  #'
  #' @return A file path.
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
  
  
  # read files --------------------------------------------------------------
  
  #' Read NCCA site files
  #' 
  #' Reads in files with site info and limit to Western Lake Superior
  #' 
  #' @param ncca<year>_sites_file site data file
  #'
  #' @return Data frame with uncleaned site info, particularly coordinates
  tar_target(ncca20_sites, read_csv(ncca20_sites_file) %>% 
               filter(GREAT_LAKE == "Lake Superior" & LAT_DD83 < 47.5 & LON_DD83 < -90 & VISIT_NO == 1)),
  tar_target(ncca15_sites, read_csv(ncca15_sites_file) %>% 
               filter(FEAT_NM == "Lake_Superior" & LAT_DD83 < 47.5 & LON_DD83 < -90 & VISIT_NO == 1)),
  tar_target(ncca10_sites, read_csv(ncca10_sites_file) %>% 
               filter(WTBDY_NM == "Lake Superior" & ALAT_DD < 47.5 & ALON_DD < -90 & VISIT_NO == 1)),
  tar_target(ncca10na_sites, read_csv(ncca10na_sites_file) %>% 
               filter(WTBDY_NM == "Lake Superior" & ALAT_DD < 47.5 & ALON_DD < -90 & VISIT_NO == 1)),
  
  #' Read NCCA files
  #' 
  #' Reads in data files (secchi, hydro, chem) and filter to only sites in site data frames
  #' 
  #' @param ncca<year>_<type>_file data file
  #'
  #' @return Data frame with uncleaned data.
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
  
  
  # clean files ------------------------------------------------------------------
  
  #' Clean NCCA site files
  #' 
  #' Renames columns, converts to SF data so that we can add unique_ID column to 2010 data based on site location.
  #' 
  #' @param ncca<year>_sites raw site data
  #'
  #' @return Data frame with cleaned site info
  tar_target(ncca20_sites_clean, ncca20_sites %>% 
               select(ncca = UNIQUE_ID, site = SITE_ID, latitude = LAT_DD83, longitude = LON_DD83) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp), remove = FALSE)),
  tar_target(ncca15_sites_clean, ncca15_sites %>% 
               select(ncca = UNIQUE_ID, site = SITE_ID, latitude = LAT_DD83, longitude = LON_DD83) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp), remove = FALSE)),
  tar_target(ncca10_sites_clean, ncca10_sites %>% 
               select(site = SITE_ID, latitude = ALAT_DD, longitude = ALON_DD) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp), remove = FALSE) %>%
               st_join(ncca20_sites_clean, st_is_within_distance, dist = 100) %>% # adding unique ID to sites if they're within 100 meters of a known site in 2015/2020
               st_join(ncca15_sites_clean, st_is_within_distance, dist = 100) %>%
               mutate(ncca.x = if_else(is.na(ncca.x) & !is.na(ncca.y), ncca.y, ncca.x)) %>% 
               select(ncca = ncca.x, site = site.x, latitude = latitude.x, longitude = longitude.x)),
  tar_target(ncca10na_sites_clean, ncca10na_sites %>% 
               select(site = SITE_ID, latitude = ALAT_DD, longitude = ALON_DD) %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp), remove = FALSE) %>%
               st_join(ncca20_sites_clean, st_is_within_distance, dist = 100) %>%
               st_join(ncca15_sites_clean, st_is_within_distance, dist = 100) %>%
               mutate(ncca.x = if_else(is.na(ncca.x) & !is.na(ncca.y), ncca.y, ncca.x)) %>% 
               select(ncca = ncca.x, site = site.x, latitude = latitude.x, longitude = longitude.x)),
  
  #' Clean secchi data
  #' 
  #' Clean secchi data by formatting dates, dropping duplicates and bad data, renaming columns, and converting to wide format.
  #' 
  #' @param ncca<year>_secchi secchi data frame
  #'
  #' @return Data frame with cleaned secchi data.
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
               mutate(RESULT = if_else(RESULT < 0, NA, RESULT)) %>% # can't have a negative secchi depth
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               rename(secchi = SECCHI_MEAN)),
  tar_target(ncca10na_secchi_clean, ncca10na_hydro %>% 
               filter(PARAMETER == "SECCHI_MEAN") %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(date = DATE_COL, site = SITE_ID, PARAMETER, RESULT) %>%
               mutate(RESULT = if_else(RESULT < 0, NA, RESULT)) %>% # can't have a negative secchi depth
               pivot_wider(names_from = PARAMETER, values_from = RESULT) %>% 
               rename(secchi = SECCHI_MEAN)),
  
  #' Clean hydro data
  #' 
  #' Clean hydro data by formatting dates, renaming columns, and converting to wide format.
  #' 
  #' @param ncca<year>_hydro hydro data frame
  #'
  #' @return Data frame with cleaned hydro data.
  tar_target(ncca20_hydro_clean, ncca20_hydro %>% 
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(date = DATE_COL, site = SITE_ID, depth = DEPTH, cond = CONDUCTIVITY, do = DO, ph = PH, temp = TEMPERATURE)),
  tar_target(ncca15_hydro_clean, ncca15_hydro %>% 
               mutate(DATE_COL = as.Date(DATE_COL-2, origin = mdy("1-1-1900"))) %>%
               select(date = DATE_COL, site = SITE_ID, depth = DEPTH, cond = CONDUCTIVITY, do = DO, ph = PH, temp = TEMPERATURE)),
  tar_target(ncca10_hydro_clean, ncca10_hydro %>% 
               filter(CAST != "IM_CALC") %>% # drop secchi data
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(DATE_COL, SITE_ID, CAST, SDEPTH, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               select(date = DATE_COL, site = SITE_ID, depth = SDEPTH, cond = COND, do = DO, ph = pH, temp = TEMP, turb = TURB)),
  tar_target(ncca10na_hydro_clean, ncca10na_hydro %>% 
               filter(CAST != "IM_CALC") %>% # drop secchi data
               mutate(DATE_COL = mdy(DATE_COL)) %>%
               select(DATE_COL, SITE_ID, CAST, SDEPTH, PARAMETER, RESULT) %>% 
               pivot_wider(names_from = PARAMETER, values_from = RESULT, values_fn = ~ mean(.x, na.rm = TRUE)) %>% 
               select(date = DATE_COL, site = SITE_ID, depth = SDEPTH, cond = COND, do = DO, ph = pH, temp = TEMP)),
  
  #' Clean chemistry data
  #' 
  #' Clean chemistry data by formatting dates, setting NDs to 0.5 * LOD, renaming columns, and converting to wide format.
  #' 
  #' @param ncca<year>_chem chemistry data frame
  #'
  #' @return Data frame with cleaned chemistry data.
  tar_target(ncca20_chem_clean, ncca20_chem %>% 
               mutate(RESULT = if_else(is.na(RESULT) & NARS_FLAG == "ND", 0.5 * MDL, RESULT),
                      DATE_COL = mdy(DATE_COL)) %>% 
               select(DATE_COL, SITE_ID, ANALYTE, RESULT) %>% 
               pivot_wider(names_from = ANALYTE, values_from = RESULT) %>% 
               mutate(NITRATE_N = if_else(is.na(NITRATE_N), NITRATE_NITRITE_N, NITRATE_N)) %>% # count nitrate/nitrite as nitrate
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
  
  #' Combine data across years
  #' 
  #' Stack together different years of each type of data.
  #' Removes SF data from sites (but keeps coordinates)
  #' 
  #' @param ncca<year>_<type>_clean year-level data
  #'
  #' @return Data frame with all NCCA data of a specific type.
  tar_target(ncca_sites, bind_rows(ncca20_sites_clean, ncca15_sites_clean, ncca10_sites_clean, ncca10na_sites_clean) %>% 
               st_drop_geometry()),
  tar_target(ncca_secchi, bind_rows(ncca20_secchi_clean, ncca15_secchi_clean, ncca10_secchi_clean, ncca10na_secchi_clean)),
  tar_target(ncca_hydro, bind_rows(ncca20_hydro_clean, ncca15_hydro_clean, ncca10_hydro_clean, ncca10na_hydro_clean)),
  tar_target(ncca_chem, bind_rows(ncca20_chem_clean, ncca15_chem_clean, ncca10_chem_clean, ncca10na_chem_clean)),
  
  
  # make full file ---------------------------------------------------------------
  
  #' Make surface-only hydro data
  #' 
  #' Averages surface (<=2 m depth) observations so that there is only one row per date and site.
  #'
  #' @param ncca_hydro R object of all years cleaned hydro data.
  #'
  #' @return Data frame of cleaned NCCA surface hydro data
  tar_target(ncca_hydro_surf, ncca_hydro %>% 
               filter(depth <= 2) %>% 
               group_by(date, site) %>% 
               summarise(across(c(depth, cond, do, ph, temp, turb), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(c(cond, do, ph, temp, turb), replace_nan)) %>% 
               select(-depth)),
  
  #' Combine NCCA data
  #' 
  #' Joins all types of NCCA data.
  #' Formats the combined data frame to match the style used in the full core datasets.
  #' Adds type and source columns
  #'
  #' @param ncca_<type> NCCA cleaned data (surface level for hydro)
  #'
  #' @return Data frame of combined NCCA data, formatted for joining with core data
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
               arrange(date))
  
)