ncbc_targets <- list(
  
  tar_target(cb_stations_file, "ref/CB_SiteCoordinates.csv", format = "file"),
  tar_target(cb_gage_file, "ref/CBGageSites_Coordinates.xlsx", format = "file"),
  
  tar_target(cbnut_file, "raw_data/ncbc/Compiled_Bay_Data_2014-2022.xlsx", format = "file"),
  tar_target(cbtrib_tp_file, "raw_data/ncbc/TP_Tribs_14-16.csv", format = "file"),
  tar_target(cbtrib_ssc_file, "raw_data/ncbc/SSC_Tribs_14-16.csv", format = "file"),
  
  tar_target(cbq_pco2_file, "raw_data/ncbc/discharge/pco2.xlsx", format = "file"),
  tar_target(cbq_tcbs_file, "raw_data/ncbc/discharge/tcbs.csv", format = "file"),
  tar_target(cbq_sftc_file, "raw_data/ncbc/discharge/sftc.csv", format = "file"),
  tar_target(cbq_nfo2_file, "raw_data/ncbc/discharge/nfo2.xlsx", format = "file"),
  tar_target(cbq_bcbr_file, "raw_data/ncbc/discharge/bcbr.csv", format = "file"),
  tar_target(cbq_nf2i_file, "raw_data/ncbc/discharge/nf2i.csv", format = "file"),
  tar_target(cbq_bcc_file, "raw_data/ncbc/discharge/bcc.xlsx", format = "file"),
  tar_target(cbq_sfcr_file, "raw_data/ncbc/discharge/sfcr.csv", format = "file"),
  tar_target(cbq_ltls_file, "raw_data/ncbc/discharge/ltls.csv", format = "file"),
  tar_target(cbq_nft2_file, "raw_data/ncbc/discharge/nft2.csv", format = "file"),
  
  tar_target(cb_stations, read_csv(cb_stations_file) %>% 
               select(site = Site_ID, latitude = Lat, longitude = Long)),
  tar_target(cb_gages, read_xlsx(cb_gage_file)),
  
  tar_target(cbnut, read_xlsx(cbnut_file)),
  tar_target(cbtrib_tp, read_csv(cbtrib_tp_file)),
  tar_target(cbtrib_ssc, read_csv(cbtrib_ssc_file)),
  
  tar_target(cbq_pco2, read_xlsx(cbq_pco2_file, skip = 7) %>% 
               mutate(site = "PCO2")),
  tar_target(cbq_tcbs, read_csv(cbq_tcbs_file, skip = 2) %>% 
               mutate(site = "TCBS")),
  tar_target(cbq_sftc, read_table(cbq_sftc_file, skip = 2, col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                                                            Value = col_double(),
                                                                            Grade = col_character(), Approval = col_character(), 
                                                                            InterpolationCode = col_character())) %>% 
               mutate(site = "SFTC")),
  tar_target(cbq_nfo2, read_xlsx(cbq_nfo2_file, skip = 14) %>% 
               mutate(site = "NFO2")),
  tar_target(cbq_bcbr, read_csv(cbq_bcbr_file, skip = 2, col_types = cols(Date = col_date(format = "%m/%d/%y"), 
                                                                          Grade = col_character(), Approval = col_character(), 
                                                                          InterpolationCode = col_character())) %>% 
               mutate(site = "BCBR")),
  tar_target(cbq_nf2i, read_csv(cbq_nf2i_file, skip = 2, col_types = cols(Date = col_date(format = "%m/%d/%y"), Grade = col_character(), Approval = col_character(), 
                                                                          InterpolationCode = col_character())) %>% 
               mutate(site = "NF2I")),
  tar_target(cbq_bcc, read_xlsx(cbq_bcc_file, skip = 14) %>% 
               mutate(site = "BCC")),
  tar_target(cbq_sfcr, read_csv(cbq_sfcr_file, skip = 14, col_types = cols(`ISO 8601 UTC` = col_character(),
                                                                           `Timestamp (UTC-05:00)` = col_datetime(format = "%m/%d/%Y %H:%M"))) %>% 
               mutate(site = "SFCR")),
  tar_target(cbq_ltls, read_csv(cbq_ltls_file, skip = 2, col_types = cols(Date = col_date(format = "%m/%d/%y"), 
                                                                          Grade = col_character(), Approval = col_character(), 
                                                                          InterpolationCode = col_character())) %>% 
               mutate(site = "LTLS")),
  tar_target(cbq_nft2, read_csv(cbq_nft2_file, skip = 2, col_types = cols(Date = col_date(format = "%m/%d/%y"), 
                                                                          Grade = col_character(), Approval = col_character(), 
                                                                          InterpolationCode = col_character())) %>% 
               mutate(site = "NFT2")),
  
  
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
               mutate(source = "NCBC", type = "Lake")),
  
  tar_target(cbtrib_tp_clean, cbtrib_tp %>% 
               mutate(date = str_c(Date, `Time (CDT)`, sep = " "),
                      date = mdy_hms(date, tz = "America/Chicago")) %>% 
               select(date, site = `Site ID`, discharge = `Discharge (CFS)`, tp = `Total Phosphorus (mg/L)`)),
  tar_target(cbtrib_ssc_clean, cbtrib_ssc %>% 
               mutate(date = str_c(Date, `Time (CDT)`, sep = " "),
                      date = mdy_hms(date, tz = "America/Chicago"),
                      tss = if_else(`Suspended Sediment (1.5 um filter) Concentration (mg/L)` == "ND", "0", `Suspended Sediment (1.5 um filter) Concentration (mg/L)`),
                      tss = as.numeric(tss)) %>% 
               select(date, site = `Site ID`, discharge = `Discharge (CFS)`, tss)),
  tar_target(cbtrib_ssc_daily, cbtrib_ssc_clean %>% 
               mutate(date = date(date)) %>%
               group_by(date, site) %>%
               summarise(across(c(discharge, tss), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(cbtrib_tp_daily, cbtrib_tp_clean %>% 
               mutate(date = date(date)) %>%
               group_by(date, site) %>%
               summarise(across(c(discharge, tp), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(cbtrib, cbtrib_tp_daily %>% 
               full_join(cbtrib_ssc_daily, join_by(date, site)) %>% 
               mutate(discharge = rowMeans(across(c(discharge.x, discharge.y)), na.rm = TRUE)) %>% 
               select(-c(discharge.x, discharge.y)) %>% 
               arrange(date) %>% 
               inner_join(cb_gages, by = join_by(site == Site_ID)) %>% #remove sites without discharge data
               select(date, site, station_nm = Site_Name, discharge, tp, tss, latitude = LAT_WGS84, longitude = LONG_WGS84) %>% 
               mutate(source = "NCBC")),
  
  tar_target(cbq_s1, bind_rows(cbq_pco2, cbq_nfo2, cbq_bcc, cbq_sfcr) %>% 
               select(date = `Timestamp (UTC-05:00)`, site, discharge = Value) %>% 
               mutate(date = force_tz(date, tzone = "America/Chicago"))),
  tar_target(cbq_s2, bind_rows(cbq_sftc, cbq_bcbr, cbq_nf2i, cbq_ltls, cbq_nft2) %>% 
               select(date = Date, site, discharge = Value) %>% 
               mutate(date = force_tz(date, tzone = "America/Chicago"))),
  tar_target(cbq_s3, bind_rows(cbq_tcbs) %>% 
               select(date = Date, site, discharge = Value) %>% 
               mutate(date = mdy(date, tz = "America/Chicago")) %>% 
               group_by(date, site) %>% 
               summarise(discharge = mean(discharge, na.rm = TRUE)) %>% 
               ungroup()),
  
  tar_target(cbq_clean, bind_rows(cbq_s1, cbq_s2, cbq_s3) %>% 
               filter(!is.na(discharge)) %>% 
               left_join(cb_gages, by = join_by(site == Site_ID)) %>% 
               select(date, site, station_nm = Site_Name, discharge, latitude = LAT_WGS84, longitude = LONG_WGS84) %>% 
               mutate(source = "NCBC"))
  
  
  
)