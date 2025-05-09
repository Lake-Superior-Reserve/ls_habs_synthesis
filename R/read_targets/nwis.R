nwis_targets <- list(
  
  # identify relevant NWIS sites ----------------------------------------------------
  
  #' Get NWIS site list
  #' 
  #' Retrieves a list of sites in the Western Lake Superior watershed, based on huc8 code.
  #' Removes groundwater sites and those without daily or instantaneous data. Also those without data since 2010.
  #' Removes sites I visually ID'ed as being inappropriate
  #'
  #' @return Data frame of metadata for relevant sites
  tar_target(nwis_metadata, whatNWISdata(huc = c("04010102", "04010201", "04010202", "04010301", "04010302", "04020300")) %>% 
               filter(site_tp_cd != "GW") %>% 
               filter(data_type_cd %in% c("dv", "uv")) %>% 
               filter(end_date > ymd("2010-01-01")) %>% 
               filter(!(site_no %in% c("04015438", "465735092504901", "470535092570801", "471149092360901", "04021520", # these 5 are upstream in St Louis watershed
                                       "04027200", "040265935", "040265981", "04026740", "04026450", "04026511", "04026561", "04026900", # these are too far upstream in bad river area
                                       "04029990", "04001000") # too far east; isle royale
               ))),
  
  #' Polish site list
  #' 
  #' Remove unnecessary columns and duplicate rows from our site list
  #'
  #' @return Data frame of basic info for relevant sites
  tar_target(nwis_sites, nwis_metadata %>% 
               select(site = site_no, station_nm, huc = huc_cd, latitude = dec_lat_va, longitude = dec_long_va, type = site_tp_cd) %>% 
               filter(!duplicated(site)) %>% 
               mutate(source = "USGS")),
  
  
  # download NWIS data --------------------------------------------------------------
  
  
  #' Download daily and instantaneous data
  #' 
  #' Using our site list, download data using `readNWISdata()`.
  #' From my exploration, not all sites have dv data, so we also need to pull instantaneous values (uv/iv), and we'll merge them back together later
  #' Years are split out for the "iv" pull to make each pull more reasonably sized. Joined together in the last target of this chunk.
  #'
  #' @return Data frame of all raw NWIS data for relevant sites
  tar_target(nwis_pull_dv, readNWISdata(service = "dv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "dv")$site_no), startDate = "2010-01-01", endDate = "2024-12-31", tz = "America/Chicago")),
  
  tar_target(nwis_pull_uv24, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2024-01-01", endDate = "2024-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv23, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2023-01-01", endDate = "2023-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv22, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2022-01-01", endDate = "2022-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv21, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2021-01-01", endDate = "2021-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv20, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2020-01-01", endDate = "2020-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv19, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2019-01-01", endDate = "2019-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv18, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2018-01-01", endDate = "2018-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv17, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2017-01-01", endDate = "2017-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv16, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2016-01-01", endDate = "2016-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv15, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2015-01-01", endDate = "2015-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv14, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2014-01-01", endDate = "2014-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv13, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2013-01-01", endDate = "2013-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv12, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2012-01-01", endDate = "2012-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv11, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2011-01-01", endDate = "2011-12-31", tz = "America/Chicago")),
  tar_target(nwis_pull_uv10, readNWISdata(service = "iv", siteNumbers = unique(filter(nwis_metadata, data_type_cd == "uv")$site_no), startDate = "2010-01-01", endDate = "2010-12-31", tz = "America/Chicago")),
  
  tar_target(nwis_pull_uv, bind_rows(nwis_pull_uv10, nwis_pull_uv11, nwis_pull_uv12, nwis_pull_uv13, nwis_pull_uv14, nwis_pull_uv15, nwis_pull_uv16, nwis_pull_uv17, nwis_pull_uv18, nwis_pull_uv19,
                                     nwis_pull_uv20, nwis_pull_uv21, nwis_pull_uv22, nwis_pull_uv23, nwis_pull_uv24)),
  
  
  # Clean data --------------------------------------------------------
  
  #' Clean daily and instant data
  #' 
  #' Rename columns, format dates, drop rows without any data
  #' 
  #' @param nwis_pull_<type> raw NWIS data
  #'
  #' @return Data frame of clean NWIS data for relevant sites
  tar_target(nwis_dv_clean, nwis_pull_dv %>% 
               select(site = site_no, date = dateTime, discharge = X_00060_00003, temp = X_00010_00003, cond = X_00095_00003, do = X_00300_00003, ph = X_00400_00008, turb = X_63680_00003,
                      tss = X_00530_00003, tkn = X_00625_00003, no3 = X_00631_00003, tp = X_00665_00003) %>% 
               mutate(date = as.character(date),
                      date = ymd(date, tz = "America/Chicago"),
                      tn = tkn + no3) %>% 
               filter(!if_all(c(discharge, temp, cond, do, ph, turb, tss, tkn, no3, tp), is.na))),
  tar_target(nwis_uv_clean, nwis_pull_uv %>% 
               select(site = site_no, date = dateTime, discharge = X_00060_00000, temp = X_00010_00000, cond = X_00095_00000, do = X_00300_00000, ph = X_00400_00000, turb = X_63680_00000) %>% 
               mutate(date = force_tz(date, tzone = "America/Chicago")) %>% 
               filter(!if_all(c(discharge, temp, cond, do, ph, turb), is.na))),
  
  #' Make instant values daily
  #' 
  #' Get daily averages of instant data
  #' 
  #' @param nwis_uv_clean clean NWIS instant data
  #'
  #' @return Data frame of daily averages of instant NWIS data 
  tar_target(nwis_uv_daily, nwis_uv_clean %>% 
               mutate(date = date(date)) %>% 
               group_by(date, site) %>% 
               summarise(across(c(discharge, temp, cond, do, ph, turb), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(c(discharge, temp, cond, do, ph, turb), replace_nan))),
  
  #' Make instant values daily
  #' 
  #' Combine data pulled as daily averages and calculated daily average from instant data.
  #' Remove duplicates, add in site info
  #' Adding the uv data only got us ~300 extra values
  #' 
  #' @param nwis_dv_clean clean NWIS daily data
  #' @param nwis_uv_daily daily averages of instant uv data
  #' @param nwis_sites site info for each site
  #'
  #' @return Data frame of daily averages of NWIS data with site info included
  tar_target(nwis, bind_rows(nwis_dv_clean, nwis_uv_daily) %>% 
               mutate(date_site = str_c(date, site, sep = "_")) %>% 
               filter(!duplicated(date_site)) %>% 
               select(-date_site) %>% 
               arrange(date) %>% 
               left_join(nwis_sites))
  
)