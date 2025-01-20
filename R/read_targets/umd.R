umd_targets <- list(
  
  # raw files ----------------------------------------------------------
  
  #' Targets for raw source files
  #' 
  #' These targets list the paths for all of the UMD source files.
  #'
  #' @return A file path.
  tar_target(umd_1721_file, "raw_data/umd/SouthShoreArchive.csv", format = "file"),
  tar_target(umd_2123_file, "raw_data/umd/SouthShoreArchive2021to2023.csv", format = "file"),
  
  tar_target(umd20_troll_file, "raw_data/umd/Troll 2020.xlsx", format = "file"),
  
  
  # helper function ----------------------------------------------------
  
  #' Combine raw 2023 troll files
  #' 
  #' Function to stack 2023 troll files. There are a bunch and are structured identically, so it didn't make sense to list them all.
  #'
  #' @returns Data frame with all raw 2023 troll data
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
  
  
  # read files ----------------------------------------------------------
  
  #' Read UMD files
  #' 
  #' First two files are chemistry only; troll files are water quality only
  #' 
  #' @param umd_<years>_file raw chemistry data file
  #' @param umd<years>_troll_file raw water quality data file
  #'
  #' @return Data frame with nutrient/water quality data
  tar_target(umd_1721, read_csv(umd_1721_file)),
  tar_target(umd_2123, read_csv(umd_2123_file)),
  
  tar_target(umd20_troll, read_xlsx(umd20_troll_file)),
  tar_target(umd23_troll, get_umd23_file()),
  
  
  # Clean files ---------------------------------------------------------
  
  #' Clean chemistry files
  #' 
  #' Cleans raw chemistry data, converting units, handling NDs, renaming columns, and averaging duplicates.
  #' 
  #' @param umd_<year> data frame of chemistry data.
  #'
  #' @return Data frame of cleaned chemistry data.
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
                      toc = poc + doc,
                      latitude = if_else(Site == "CB1", 46.59708, latitude), # make coords match NCBC CB stations coords
                      latitude = if_else(Site == "CB3", 46.62188, latitude),
                      latitude = if_else(Site == "CB10", 46.71722, latitude),
                      latitude = if_else(Site == "CB11", 46.74633, latitude),
                      longitude = if_else(Site == "CB1", -90.91349, longitude),
                      longitude = if_else(Site == "CB3", -90.90092, longitude),
                      longitude = if_else(Site == "CB10", -90.84085, longitude),
                      longitude = if_else(Site == "CB11", -90.81973, longitude),
                      source = if_else(str_detect(Site, "CB"), "NCBC", source)) %>% 
               rename(date = Date, site = Site)),
  tar_target(umd_2123_clean, umd_2123 %>% 
               mutate(Date = ymd(Date, tz = "America/Chicago"),
                      NO3 = if_else(NO3Flag == "bdl", 0.001, NO3), # set to 0.5 * detection limit if below detection limit
                      PON = if_else(PONFlag == "bdl", 0.0009, PON) # NO3 LOD = 0.002, PON LOD = 0.0018, SRP LOD = 0.0015 per Sandy B 9/17/24
               ) %>% 
               group_by(Date, Site) %>% 
               summarise(latitude = mean(Latitude), longitude = mean(Longitude), depth = 0, source = "NPS", type = first(Type),
                         doc = mean(DOC, na.rm = T), poc = mean(POC, na.rm = T), no3 = mean(NO3, na.rm = T), nh3 = mean(NH3, na.rm = T), 
                         tdn = mean(TDN, na.rm = T), pon = mean(PON, na.rm = T), po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), 
                         pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), si = mean(SRSi, na.rm = T), chl = mean(Chl, na.rm = T),  
                         tss = mean(TSS, na.rm = T)) %>% 
               mutate(si = si * .001 * 28.086, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011,  #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, 
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974,
                      toc = poc + doc,
                      source = if_else(longitude > -91.7, "NPS", "UMD")
                      ) %>% 
               rename(date = Date, site = Site)),
  
  #' Clean 2020 troll files
  #' 
  #' Cleans raw 2020 troll (water quality) data, converting units, formatting dates, dropping bad data, and renaming columns.
  #' Also splits data innto two parts: depth profiles and surface transects
  #' 
  #' @param umd20_troll data frame of troll data.
  #'
  #' @return Data frame of cleaned depth profile or surface transect troll data.
  tar_target(clean_umd20_troll, function(df){
    df %>% 
      select(date = `Date Time`, site = Station,
             latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth = `Depth (ft) (525639)`,
             chl_field = `Chlorophyll-a Concentration (µg/L) (652536)`, temp = `Water Temperature (°C) (519767)`,
             cond = `Specific Conductivity (µS/cm) (675375)`, turb = `Turbidity (NTU) (695981)`, 
             tds = `Total Dissolved Solids (ppt) (675375)`, tss = `Total Suspended Solids (mg/L) (695981)`) %>% 
      mutate(tds = tds * 1000, #convert to uS/cm
             depth = depth * 0.3048, # convert to m
             date = force_tz(date, tzone = "America/Chicago")) %>% 
      filter(depth > 0 & cond > 80) #remove measurements above water/suspected above water
  }),
  tar_target(umd20_troll_profile, umd20_troll %>% 
               filter(`Profile or Transect` == "P") %>% 
               clean_umd20_troll()),
  tar_target(umd20_troll_transect, umd20_troll %>% 
               filter(`Profile or Transect` == "T") %>% 
               clean_umd20_troll()), 
  
  #' Clean 2023 troll file
  #' 
  #' Cleans raw 2023 troll (water quality) data, converting units, formatting dates, dropping bad data, and renaming columns.
  #' Also splits data innto two parts: depth profiles and surface transects
  #' 
  #' @param umd20_troll data frame of troll data.
  #'
  #' @return Data frame of cleaned troll data.
  tar_target(umd23_troll_clean, umd23_troll %>% 
               mutate(date = force_tz(`Date Time`, tzone = "America/Chicago"),
                      tds = `Total Dissolved Solids (ppt) (803397)` * 1000, #convert to uS/cm
                      depth = `Depth (ft) (785112)` * 0.3048) %>% # convert to m
               select(date, site,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth,
                      chl_field = `Chlorophyll-a Fluorescence (RFU) (804408)`, temp = `Temperature (°C) (804550)`, # based on the troll 2020 file, chl RFU is equivalent to ug/L for this sensor
                      cond = `Specific Conductivity (µS/cm) (803397)`, turb = `Turbidity (NTU) (803671)`, tds) %>% 
               mutate(cond = if_else(cond < 50, NA, cond), # drop very low/high data
                      tds = if_else(tds < 40, NA, tds),
                      turb = if_else(turb > 10000, NA, turb))),
  
  
  # Make surface-only files ---------------------------------------------------------
  
  #' Make surface-only troll data
  #' 
  #' Average surface (<=2 m depth) observations in troll data so that there is only one row per date and site.
  #' Fix site names in 2023 file
  #' Bind together all troll data across years
  #'
  #' @param umd<year>_troll_<type>_surf cleaned troll data.
  #'
  #' @return Data frame of all UMD surface water quality data 
  tar_target(umd20_troll_profile_surf, umd20_troll_profile %>% 
               filter(depth <= 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_transect_surf, umd20_troll_transect %>% 
               filter(depth <= 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_surf, umd20_troll_profile_surf %>% 
               bind_rows(umd20_troll_transect_surf)),
  
  tar_target(umd23_troll_surf, umd23_troll_clean %>% 
               filter(depth <= 2) %>% # only want surface measurements
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
  
  
  # Make full file -----------------------------------------------------------------
  
  #' Make combined UMD file
  #' 
  #' Join Chemistry and surface water quality data, add type, source, and huc columns
  #'
  #' @param umd_<year>_clean clean chemistry data
  #' @param umd_troll_surf clean, surface-only troll data
  #'
  #' @return Data frame of all UMD data, ready to join with core data. 
  tar_target(umd, bind_rows(umd_1721_clean, umd_2123_clean) %>%
               mutate(across(-c(source, type, site, depth, latitude, longitude), replace_nan)) %>%
               left_join(select(umd_troll_surf, -c(depth, tss, latitude, longitude))) %>% 
               mutate(type = if_else(str_detect(site, "Barkers") | site == "Nemadji River ISCO", "Estuary", type),
                      huc = if_else(type == "Watershed", "04010301", NA)))
  
)