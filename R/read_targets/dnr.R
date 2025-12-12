dnr_targets <- list(
  # Helper functions ------------------------------------------------------

  #' Rename variables based on WDNR parameter codes
  #'
  #' This function returns a properly formatted (for this dataset) variable name for the passed in parameter code.
  #'
  #' @param param A character string parameter code
  #'
  #' @return A character string variable name, or NA
  tar_target(
    dnr_rename,
    Vectorize(function(param) {
      if (is.na(param)) {
        return(NA)
      } else if (param == "10") {
        return("temp")
      } else if (param == "94") {
        return("cond")
      } else if (param == "95") {
        return("cond")
      } else if (param == "300") {
        return("do")
      } else if (param == "301") {
        return("do_sat")
      } else if (param == "400") {
        return("ph")
      } else if (param == "530") {
        return("tss")
      } else if (param == "600") {
        return("tn")
      } else if (param == "608") {
        return("nh3")
      } else if (param == "625") {
        return("tkn")
      } else if (param == "631") {
        return("no3")
      } else if (param == "665") {
        return("tp")
      } else if (param == "671") {
        return("po4")
      } else if (param == "681") {
        return("doc")
      } else if (param == "940") {
        return("cl")
      } else if (param == "955") {
        return("si")
      } else if (param == "61190") {
        return("trans_tube")
      } else if (param == "82079") {
        return("turb")
      } else if (param == "99530") {
        return("tss")
      } else if (param == "99717") {
        return("chl")
      } else {
        return(NA)
      }
    })
  ),

  #' Fix dates
  #'
  #' This function fixes known incorrectly entered dates in the data. Changes were confirmed via email with Ellen C on 9/9/24.
  #'
  #' @param station A character string WDNR station ID code.
  #' @param date A character string date. %m/%d/%Y for 2019 data, %Y-%m-%d for other data.
  #'
  #' @return A character string date. %m/%d/%Y for 2019 data, %Y-%m-%d for other data.
  tar_target(
    dnr_fix_dates,
    Vectorize(function(station, date) {
      if (station == "10040814" & date == "8/20/2019") {
        # chl is on a different day from rest of analytes, chl start and end sample dates don’t match
        return("8/22/2019")
      } else if (station == "10052503" & date == "8/11/2019") {
        # chl is on a different day from rest of analytes
        return("8/12/2019")
      } else if (station == "10052514" & date == "7/22/2019") {
        # chl is on a different day from rest of analytes, other analytes start and end sample dates don’t match
        return("7/25/2019")
      } else if (station == "101" & date == "2021-07-09") {
        # po4 and chl are one a different day from the rest of the analytes, site 3 was usually on the first day of sampling in 2021, so I moved po4 and chl to be on the same day as the other analytes
        return("2021-07-08")
      } else if (is.na(station) & date == "2023-08-01") {
        # wrong date for bloom sample on 9/5/23, confirmed with Ellen C 9/17/24
        return("2023-09-05")
      } else {
        return(date)
      }
    })
  ),

  # Raw file list --------------------------------------------------------

  #' Targets for raw source files
  #'
  #' These targets list the paths for all of the WDNR source files.
  #'
  #' @return A file path.
  tar_target(
    dnr_swims_19_file,
    "raw_data/wdnr/2019LSNSHABs_SWIMS.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_swims_21_file,
    "raw_data/wdnr/2021NSData_LDES.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_swims_22_file,
    "raw_data/wdnr/2022NSHAB_LDES.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_swims_23_file,
    "raw_data/wdnr/2023NSHABs_SWIMS.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_swims_24_file,
    "raw_data/wdnr/2024WaterChemDataLDES.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_swims_25_file,
    "raw_data/wdnr/2025WaterChemDataLDES.csv",
    format = "file"
  ),

  tar_target(
    dnr_hydro_19_file,
    "raw_data/wdnr/2019_Hydro.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_hydro_21_file,
    "raw_data/wdnr/2021_Hydro.csv",
    format = "file"
  ),
  tar_target(
    dnr_hydro_22_file,
    "raw_data/wdnr/AllDepths2022Hydro.csv",
    format = "file"
  ),
  tar_target(
    dnr_hydro_23_file,
    "raw_data/wdnr/2023LSNSHABsHydro_alldepth.csv",
    format = "file"
  ),
  tar_target(
    dnr_hydro_24_file,
    "raw_data/wdnr/2024_LS_NS_HABs_HydroData_alldepth.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_hydro_24_cb_file,
    "raw_data/wdnr/Compiled_NC_Nearshore_Sites_2024.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_hydro_25_cb_file,
    "raw_data/ncbc/BCER_NearshoreData_Compiled_2025.xlsx",
    format = "file"
  ),
  tar_target(
    dnr_hydro_25_file,
    "raw_data/wdnr/2025_LS_NS_HABs_HydroData_alldepth.csv",
    format = "file"
  ),

  tar_target(
    dnr_bac_23_file,
    "raw_data/wdnr/AlgalID2023.csv",
    format = "file"
  ),

  tar_target(
    dnr_bac_24_file,
    "raw_data/wdnr/2024AlgalData_full.xlsx",
    format = "file"
  ),

  tar_target(dnr_sites_file, "ref/DNRStationLocations.xlsx", format = "file"),
  tar_target(dnr_nc_sites_file, "ref/NC_DNR_sites.csv", format = "file"),

  # Read in raw files -----------------------------------------------------------------

  #' Read source files
  #'
  #' These targets read in source data files using `read_xlsx()` and `read_csv()`
  #'
  #' @param Filepath to source file
  #'
  #' @return Data frame of source data
  tar_target(dnr_swims_19, read_xlsx(dnr_swims_19_file)),
  tar_target(dnr_swims_21, read_xlsx(dnr_swims_21_file)),
  tar_target(dnr_swims_22, read_xlsx(dnr_swims_22_file)),
  tar_target(dnr_swims_23, read_xlsx(dnr_swims_23_file)),
  tar_target(dnr_swims_24, read_xlsx(dnr_swims_24_file)),
  tar_target(dnr_swims_25, read_csv(dnr_swims_25_file)),

  tar_target(dnr_hydro_19, read_xlsx(dnr_hydro_19_file)),
  tar_target(dnr_hydro_21, read_csv(dnr_hydro_21_file)),
  tar_target(dnr_hydro_22, read_csv(dnr_hydro_22_file)),
  tar_target(dnr_hydro_23, read_csv(dnr_hydro_23_file)),
  tar_target(dnr_hydro_24, read_xlsx(dnr_hydro_24_file)),
  tar_target(dnr_hydro_24_cb, read_xlsx(dnr_hydro_24_cb_file)),
  tar_target(dnr_hydro_25_cb, read_xlsx(dnr_hydro_25_cb_file)),
  tar_target(dnr_hydro_25, read_csv(dnr_hydro_25_file)),

  tar_target(dnr_bac_23_raw, read_csv(dnr_bac_23_file)),
  tar_target(dnr_bac_24_raw, read_xlsx(dnr_bac_24_file)),

  # Create join tables for station, site, and block number --------------------------------

  #' Northland College site information
  #'
  #' Table of site information for sites East of WDNR main sites that are sampled by Northland College,
  #' but are still analyzed by WSLH and the data are maintained by WDNR.
  #' Includes lat, long, WDNR station ID, WDNR site ID, NC site ID
  #'
  #' @param filepath to site reference file
  #'
  #' @return Data frame of site info
  tar_target(
    dnr_nc_sites,
    read_csv(dnr_nc_sites_file) %>%
      mutate(
        WDNR_Site_ID = str_split_i(WDNR_Site_ID, "-", 2),
        across(c(Latitude, Longitude, WDNR_Station_ID), as.character)
      )
  ),

  #' Station Locations
  #'
  #' Table of lat/long coordinates for each station. Extracted from 2019 file to join with other years.
  #' Also add additional coordinates provided by email from Ellen C 9/9/24
  #'
  #' @param dnr_swims_19 R object of (uncleaned) data from 2019
  #'
  #' @return Data frame of station locations
  tar_target(
    dnr_stations,
    dnr_swims_19 %>%
      group_by(StationID) %>%
      summarise(
        StationLatitude = first(StationLatitude),
        StationLongitude = first(StationLongitude)
      ) %>%
      bind_rows(tibble(
        StationID = c(
          "10054863",
          "104",
          "BLOOM_2023-09-05",
          "BLOOM_2023-09-21"
        ),
        StationLatitude = c(
          "46.88067000",
          "46.75940790",
          "46.792122",
          "46.723286"
        ),
        StationLongitude = c(
          "-91.06150000",
          "-91.61504730",
          "-91.392734",
          "-92.064256"
        )
      )) %>%
      bind_rows(select(
        dnr_nc_sites,
        StationID = WDNR_Station_ID,
        StationLatitude = Latitude,
        StationLongitude = Longitude
      )) %>%
      bind_rows(tibble_row(
        StationID = "10060164",
        StationLatitude = "46.90407",
        StationLongitude = "-91.03325"
      ))
  ),
  tar_target(
    dnr_stations_24,
    read_xlsx(dnr_sites_file) %>%
      mutate(
        StationID = as.character(`SWIMS Station ID`),
        StationName = str_to_upper(`SWIMS Station Name`),
        StationName = str_replace(StationName, fixed("R."), "R")
      ) %>%
      select(StationID, StationName, Latitude, Longitude)
  ),

  #' Station/site join table
  #'
  #' Table of site numbers for each station ID. Extracted from 2023 file to join with other years.
  #' Needed because hydro profile data are only identified by site number.
  #'
  #' @param dnr_swims_23 R object of (uncleaned) data from 2023
  #'
  #' @return Data frame of station-site pairs
  tar_target(
    dnr_sites,
    dnr_swims_23 %>%
      filter(
        !is.na(`Id #`) &
          !str_detect(`Field #`, "DUP") &
          !str_detect(`Field #`, "BL") &
          str_detect(`Field #`, "HABS")
      ) %>%
      mutate(site = str_split_i(`Field #`, "-", 2)) %>%
      group_by(`Id #`) %>%
      summarise(SiteID = first(site)) %>%
      rename(StationID = `Id #`) %>%
      bind_rows(select(
        dnr_nc_sites,
        StationID = WDNR_Station_ID,
        SiteID = WDNR_Site_ID
      )) %>%
      bind_rows(tibble_row(StationID = "10060164", SiteID = "SEACAVES"))
  ),

  #' Station/block join table
  #'
  #' Table of block numbers for each station ID/date. Extracted from 2019 file to join with other years.
  #' Needed because hydro profile data in 2019 are only identified by block number.
  #'
  #' @param dnr_swims_19 R object of (uncleaned) data from 2019
  #'
  #' @return Data frame of station-date-block matches
  tar_target(
    dnr_blocks,
    dnr_swims_19 %>%
      mutate(
        StartDateTime = str_split_i(StartDateTime, " ", 1),
        StartDateTime = dnr_fix_dates(StationID, StartDateTime),
        StartDateTime = mdy(StartDateTime, tz = "America/Chicago"),
        Block = str_trim(str_split_i(FieldNo, "K", 2)),
        Block = str_split_i(Block, "\\D", 1),
        Block = as.numeric(Block)
      ) %>%
      group_by(StationID, StartDateTime) %>%
      summarise(Block = mean(Block)) %>%
      filter(!is.na(Block))
  ),

  # Clean raw files -----------------------------------------------------------------------

  #' Clean 2019 chemistry file
  #'
  #' Target to clean 2019 chemistry data. Doesn't have its own function because it is uniquely formatted.
  #' Main tasks are formatting dates, formatting parameter names, setting ND values to 0.5 * LOD, and making data wide.
  #'
  #' @param dnr_swims_19 R object of (uncleaned) data from 2019
  #'
  #' @return Cleaned data frame of 2019 chemistry data
  tar_target(
    dnr_swims_19_clean,
    dnr_swims_19 %>%
      filter(!is.na(DNRParameterCode) & DNRParameterCode != "625") %>% # dropping tkn in addition to nas, tkn only has 1 round of results, all ND
      mutate(
        StartDateTime = str_split_i(StartDateTime, " ", 1),
        StartDateTime = dnr_fix_dates(StationID, StartDateTime),
        StartDateTime = mdy(StartDateTime, tz = "America/Chicago"),
        DNRParameterCode = unname(dnr_rename(DNRParameterCode)),
        ResultValueNo = if_else(
          ResultValueNo == "ND",
          as.character(0.5 * as.numeric(LODAmount)),
          ResultValueNo
        ), # set NDs to 0.5 * LOD
        ResultValueNo = as.numeric(ResultValueNo)
      ) %>% # separating the conversion to prevent warning message
      select(StartDateTime, StationID, DNRParameterCode, ResultValueNo) %>%
      pivot_wider(
        names_from = DNRParameterCode,
        values_from = ResultValueNo,
        values_fn = ~ mean(.x, na.rm = TRUE)
      ) %>%
      arrange(StartDateTime, StationID)
  ),

  #' Clean WDNR chemistry files after 2019
  #'
  #' Function to clean most WDNR chemistry data.
  #' Main tasks are formatting dates, formatting parameter names, setting ND values to 0.5 * LOD, and making data wide.
  #'
  #' @param dnr_swims_<year> R object of (uncleaned) data, filtered to remove field blanks
  #'
  #' @return Cleaned data frame of WDNR chemistry data
  tar_target(clean_swims, function(df) {
    df %>%
      mutate(
        `DNR Parameter Code` = unname(dnr_rename(`DNR Parameter Code`)),
        `Numeric Value` = if_else(
          `Result value` == "ND",
          0.5 * as.numeric(LOD),
          `Numeric Value`
        ),
        `Collection Start Date/Time` = str_split_i(
          `Collection Start Date/Time`,
          " ",
          1
        ),
        `Collection Start Date/Time` = dnr_fix_dates(
          `Id #`,
          `Collection Start Date/Time`
        ),
        `Collection Start Date/Time` = ymd(
          `Collection Start Date/Time`,
          tz = "America/Chicago"
        ),
        `Id #` = if_else(
          is.na(`Id #`),
          str_c('BLOOM', `Collection Start Date/Time`, sep = "_"),
          `Id #`
        )
      ) %>% # sets station ID for 2023 bloom samples
      select(
        StartDateTime = `Collection Start Date/Time`,
        StationID = `Id #`,
        `DNR Parameter Code`,
        `Numeric Value`
      ) %>%
      pivot_wider(
        names_from = `DNR Parameter Code`,
        values_from = `Numeric Value`,
        values_fn = ~ mean(.x, na.rm = TRUE)
      ) %>%
      arrange(StartDateTime, StationID)
  }),

  #' Clean specific WDNR chemistry files after 2019
  #'
  #' Targets to clean most WDNR chemistry data.
  #' Filter out field blanks, then pass to `clean_swims()`.
  #' Add 2023 po4 data, which was all NDs
  #'
  #' @param dnr_swims_<year> R object of (uncleaned) data, filtered to remove field blanks
  #'
  #' @return Cleaned data frame of WDNR chemistry data
  tar_target(
    dnr_swims_21_clean,
    dnr_swims_21 %>%
      filter(!str_detect(`Field #`, "BL")) %>% #drop field blanks
      clean_swims()
  ),
  tar_target(
    dnr_swims_22_clean,
    dnr_swims_22 %>%
      filter(!str_detect(`Field #`, "BL")) %>% # drop field blanks, also drops rows missing field id, which conveniently drops all the bacteria species counts
      clean_swims()
  ),
  tar_target(
    dnr_swims_23_clean,
    dnr_swims_23 %>%
      filter(
        !(str_detect(`Field #`, "BL") & !str_detect(`Field #`, "BLOOM")) &
          str_detect(`Field #`, "HABS")
      ) %>% # 23 has two bloom samples (keeps blooms) and 3 random trib sites (drops trib sites, which are included in WQP data)
      clean_swims() %>%
      mutate(po4 = 0.0007)
  ), # per Ellen C 9/9/24, po4 samples were analyzed at Pace Analytical Duluth and were all NDs. Pace has a LOD of 0.0014 for po4, so setting all po4 results to 0.5*0.0014=0.0007
  tar_target(
    dnr_swims_24_clean,
    dnr_swims_24 %>%
      filter(!str_detect(`Field #`, "BL") & !str_detect(`Field #`, "FB")) %>% # drop field blanks
      clean_swims()
  ),
  tar_target(
    dnr_swims_25_clean,
    dnr_swims_25 %>%
      filter(!str_detect(`Field #`, "BL")) %>%
      mutate(`Id #` = if_else(`Id #` == "ELLEN COONEY", "10052505", `Id #`)) %>%
      mutate(
        `DNR Parameter Code` = unname(dnr_rename(`DNR Parameter Code`)),
        `Numeric Value` = if_else(
          `Result value` == "ND",
          0.5 * as.numeric(LOD),
          `Numeric Value`
        ),
        `Collection Start Date/Time` = str_split_i(
          `Collection Start Date/Time`,
          " ",
          1
        ),
        `Collection Start Date/Time` = mdy(
          `Collection Start Date/Time`,
          tz = "America/Chicago"
        )
      ) %>%
      select(
        StartDateTime = `Collection Start Date/Time`,
        StationID = `Id #`,
        `DNR Parameter Code`,
        `Numeric Value`
      ) %>%
      pivot_wider(
        names_from = `DNR Parameter Code`,
        values_from = `Numeric Value`,
        values_fn = ~ mean(.x, na.rm = TRUE)
      ) %>%
      arrange(StartDateTime, StationID)
  ),

  #' Clean 2019 hydro file
  #'
  #' Target to clean 2019 hydro profile data. Doesn't have its own function because it is uniquely formatted.
  #' Main tasks are formatting dates, formatting parameter names, adding station IDs, and removing bad data.
  #'
  #' @param dnr_hydro_19 R object of (uncleaned) data from 2019
  #'
  #' @return Cleaned data frame of 2019 hydro profiles data
  tar_target(
    dnr_hydro_19_clean,
    dnr_hydro_19 %>%
      mutate(
        `SpCond uS/cm` = if_else(
          is.na(`SpCond uS/cm`),
          1000 * `SpCond mS/cm`,
          `SpCond uS/cm`
        ),
        `SpCond uS/cm` = if_else(`SpCond uS/cm` < 50, NA, `SpCond uS/cm`), #remove bad conductivity values
        Block = as.numeric(str_split_i(Block, " ", 2)),
        `Date (MM/DD/YYYY)` = force_tz(
          `Date (MM/DD/YYYY)`,
          tzone = "America/Chicago"
        )
      ) %>%
      left_join(
        dnr_blocks,
        by = join_by(Block, `Date (MM/DD/YYYY)` == StartDateTime)
      ) %>%
      mutate(
        StationID = if_else(is.na(StationID) & Block == 6, "103", StationID),
        StationID = if_else(
          is.na(StationID) & Block == 7,
          "10052505",
          StationID
        ),
        StationID = if_else(
          is.na(StationID) & Block == 9,
          "10052509",
          StationID
        ),
        StationID = if_else(
          is.na(StationID) & Block == 13,
          "10038057",
          StationID
        ),
        StationID = if_else(
          is.na(StationID) & Block == 14,
          "10052512",
          StationID
        )
      ) %>%
      left_join(dnr_sites, by = join_by(StationID)) %>%
      select(
        block = Block,
        station = StationID,
        site = SiteID,
        date = `Date (MM/DD/YYYY)`,
        depth = `Depth m`,
        temp = `Temp °C`,
        do_sat = `ODO % sat`,
        do = `ODO mg/L`,
        cond = `SpCond uS/cm`,
        ph = pH,
        turb = `Turbidity FNU`,
        chl_field = `Chlorophyll µg/L`
      )
  ),

  #' Clean WDNR hydro files 2021-2023
  #'
  #' Function to clean most WDNR hydro data.
  #' Main tasks are formatting dates, formatting parameter names, and removing suspicious data.
  #'
  #' @param dnr_hydro_<year> R object of (uncleaned) data
  #'
  #' @return Cleaned data frame of WDNR hydro data
  tar_target(clean_dnr_hydro, function(df) {
    col_select <- c(
      date = "Date",
      station = "StationID",
      site = "Site",
      depth = "Depth (m)",
      temp = "Temp (C)",
      do_sat = "DO %",
      do = "DO (mg/L)",
      cond = "Specific Conductivity (uS/cm)",
      ph = "pH (SU)",
      turb = "Turbidity (NTU)",
      par = "PAR"
    )
    df %>%
      filter(`Depth (m)` != "above") %>%
      mutate(
        `Depth (m)` = as.numeric(`Depth (m)`),
        Date = mdy(Date, tz = "America/Chicago"),
        `pH (SU)` = if_else(`pH (SU)` < 6.5, NA, `pH (SU)`), # drop suspicious pH values
        `DO %` = if_else(year(Date) == 2023, NA, `DO %`), # 2023 DO values are all suspiciously high, dropping
        `DO (mg/L)` = if_else(year(Date) == 2023, NA, `DO (mg/L)`),
        Site = as.character(Site)
      ) %>%
      left_join(dnr_sites, by = join_by(Site == SiteID)) %>%
      select(any_of(col_select))
  }),
  #' Clean specific WDNR chemistry files 2021-2023
  #'
  #' Targets to clean WDNR hydro data 2021-2023.
  #' Main tasks are formatting dates, formatting parameter names, and removing suspicious data.
  #'
  #' @param dnr_hydro_<year> R object of (uncleaned) data
  #'
  #' @return Cleaned data frame of WDNR hydro data
  tar_target(
    dnr_hydro_21_clean,
    dnr_hydro_21 %>%
      clean_dnr_hydro()
  ),
  tar_target(
    dnr_hydro_22_clean,
    dnr_hydro_22 %>%
      clean_dnr_hydro()
  ),
  tar_target(
    dnr_hydro_23_clean,
    dnr_hydro_23 %>%
      clean_dnr_hydro()
  ),
  tar_target(
    dnr_hydro_25_clean,
    dnr_hydro_25 %>%
      rename(
        `Specific Conductivity (uS/cm)` = `Specific Conductivity (μS/cm)`
      ) %>%
      mutate(
        `Turbidity (NTU)` = as.numeric(`Turbidity (NTU)`), # will get warnings about NAs
        PAR = as.numeric(PAR)
      ) %>%
      clean_dnr_hydro() %>%
      bind_rows(dnr_hydro_25_cb_clean) %>%
      arrange(date)
  ),

  #' Clean 2024 and 2025 NC hydro data
  #'
  #' Function to clean NC site hydro data. Needs its own function since it has a unique format
  #' Main tasks are formatting dates, formatting parameter names, handling NDs, and removing suspicious data.
  #'
  #' @param dnr_hydro_<year> R object of (uncleaned) hydro data from NC sites.
  #'
  #' @return Cleaned data frame of WDNR hydro data
  tar_target(
    dnr_hydro_24_cb_clean,
    dnr_hydro_24_cb %>%
      mutate(
        site = str_split_i(WDNR_Site_ID, "-", 3),
        Date = force_tz(Date, tzone = "America/Chicago"),
        Turb_NTU = if_else(Turb_NTU == "NA", NA, Turb_NTU),
        Turb_NTU = if_else(Turb_NTU == "<0.1", 0.05, as.numeric(Turb_NTU)), # has NAs, so will get warning about NAs introduced by coercion
        `Chl_ug/l` = if_else(`Chl_ug/l` == "<0.1", 0.05, as.numeric(`Chl_ug/l`))
      ) %>%
      left_join(dnr_sites, by = join_by(site == SiteID)) %>%
      select(
        date = Date,
        station = StationID,
        site,
        depth = Depth,
        temp = Temp_deg_C,
        do_sat = `HDO_%Sat`,
        do = `HDO_mg/l`,
        cond = `SpCond_uS/cm`,
        ph = pH_units,
        turb = Turb_NTU,
        chl_field = `Chl_ug/l`
      )
  ),
  tar_target(
    dnr_hydro_25_cb_clean,
    dnr_hydro_25_cb %>%
      mutate(
        site = str_split_i(WDNR_Site_ID, "-", 3),
        site = if_else(site == "SeaCaves", "SEACAVES", site),
        Date = if_else(Date == "45832", "06/24/2025", Date),
        Date = mdy(Date, tz = "America/Chicago")
      ) %>%
      left_join(dnr_sites, by = join_by(site == SiteID)) %>%
      select(
        date = Date,
        station = StationID,
        site,
        depth = Depth,
        temp = Temp_deg_C,
        do_sat = `HDO_%Sat`,
        do = `HDO_mg/l`,
        cond = `SpCond_uS/cm`,
        ph = pH_units,
        chl_field = `Chl_ug/l`,
        PAR = PAR_uMol
      )
  ),

  #' Clean 2024 WDNR hydro data
  #'
  #' Function to clean WNDR 2024 hydro data. Needs its own function to handle different date format and join in NC site data
  #' Main tasks are formatting dates and formatting parameter names.
  #'
  #' @param dnr_hydro_<year> R object of (uncleaned) hydro data.
  #'
  #' @return Cleaned data frame of WDNR hydro data
  tar_target(
    dnr_hydro_24_clean,
    dnr_hydro_24 %>%
      mutate(
        Date = ymd(Date, tz = "America/Chicago"),
        Site = as.character(Site)
      ) %>%
      left_join(dnr_sites, by = join_by(Site == SiteID)) %>%
      select(
        date = Date,
        station = StationID,
        site = Site,
        depth = `Depth (m)`,
        temp = `Temp (C)`,
        do_sat = `DO %`,
        do = `DO (mg/L)`,
        cond = `Specific Conductivity (μS/cm)`,
        ph = `pH (SU)`,
        turb = `Turbidity (NTU)`
      ) %>%
      bind_rows(dnr_hydro_24_cb_clean) %>%
      arrange(date)
  ),

  # Create surface sample-only versions of depth profile data ------------------------

  #' Make surface-only hydro data
  #'
  #' Function to average surface (<=2 m depth) observations so that there is only one row per date and site.
  #'
  #' @param dnr_hydro_<year>_clean R object of cleaned hydro data.
  #'
  #' @return Data frame of cleaned WDNR surface hydro data
  tar_target(make_dnr_surf, function(df) {
    data_cols = c(
      "temp",
      "do_sat",
      "do",
      "cond",
      "ph",
      "turb",
      "par",
      "chl_field"
    )
    df %>%
      filter(depth <= 2 & !is.na(station)) %>%
      group_by(date, station) %>%
      summarise(across(any_of(data_cols), ~ mean(., na.rm = TRUE))) %>%
      mutate(across(where(is.numeric), replace_nan))
  }),

  #' Make surface-only hydro data
  #'
  #' Targets to average surface (<=2 m depth) observations so that there is only one row per date and site.
  #'
  #' @param dnr_hydro_<year>_clean R object of cleaned hydro data.
  #'
  #' @return Data frame of cleaned WDNR surface hydro data
  tar_target(
    dnr_hydro_19_surf,
    dnr_hydro_19_clean %>%
      make_dnr_surf()
  ),
  tar_target(
    dnr_hydro_21_surf,
    dnr_hydro_21_clean %>%
      make_dnr_surf()
  ),
  tar_target(
    dnr_hydro_22_surf,
    dnr_hydro_22_clean %>%
      make_dnr_surf()
  ),
  tar_target(
    dnr_hydro_23_surf,
    dnr_hydro_23_clean %>%
      make_dnr_surf()
  ),
  tar_target(
    dnr_hydro_24_surf,
    dnr_hydro_24_clean %>%
      make_dnr_surf()
  ),
  tar_target(
    dnr_hydro_25_surf,
    dnr_hydro_25_clean %>%
      make_dnr_surf()
  ),

  # Join together single-year files --------------------------------------------------

  tar_target(
    dnr_swims_clean,
    bind_rows(
      dnr_swims_19_clean,
      dnr_swims_21_clean,
      dnr_swims_22_clean,
      dnr_swims_23_clean,
      dnr_swims_24_clean,
      dnr_swims_25_clean
    )
  ),
  tar_target(
    dnr_hydro_clean,
    bind_rows(
      dnr_hydro_19_clean,
      dnr_hydro_21_clean,
      dnr_hydro_22_clean,
      dnr_hydro_23_clean,
      dnr_hydro_24_clean,
      dnr_hydro_25_clean
    )
  ),
  tar_target(
    dnr_hydro_surf,
    bind_rows(
      dnr_hydro_19_surf,
      dnr_hydro_21_surf,
      dnr_hydro_22_surf,
      dnr_hydro_23_surf,
      dnr_hydro_24_surf,
      dnr_hydro_25_surf
    )
  ),

  # Combine chemistry and surface hydro data ------------------------------------------

  #' Add together chemistry, hydro, and location data
  #'
  #' Combines multi-year chemistry and hydro files as well as coordinates for each site.
  #'
  #' @param dnr_hydro/swims_clean data frames of combined DNR data.
  #' @param dnr_stations Locations for each DNR station/site
  #'
  #' @return Data frame of combined DNR data
  tar_target(
    dnr_s1,
    dnr_swims_clean %>%
      left_join(dnr_stations, by = join_by(StationID)) %>%
      left_join(dnr_sites, by = join_by(StationID)) %>%
      rename(date = StartDateTime, station = StationID, site = SiteID) %>%
      mutate(site = if_else(station == "104", "6", site)) %>% # Per Ellen C 9/9/24, 104 is very close to 103, and 103 was not sampled that round, so assuming 104 is also site 6
      left_join(dnr_hydro_surf, by = join_by(station, date))
  ),

  #' Format combined WDNR data
  #'
  #' Formats the combined data frame to match the style used in the full core datasets.
  #' Adds depth, type, and source columns; merges site and station columns; renames coordinate columns
  #'
  #' @param dnr_s1 combined data frame of DNR data
  #'
  #' @return Data frame of combined DNR data, formatted for joining with core data
  tar_target(
    dnr,
    dnr_s1 %>%
      mutate(
        depth = 0,
        source = "WDNR",
        type = if_else(station == "BLOOM_2023-09-21", "Estuary", "Lake"),
        site = str_c("site ", site, " - ", station, sep = ""),
        site = if_else(is.na(site), station, site),
        latitude = as.numeric(StationLatitude),
        longitude = as.numeric(StationLongitude)
      ) %>%
      select(-c(station, StationLatitude, StationLongitude)) %>%
      relocate(date, site)
  ),

  # Additional objects for working with bacteria counts (not part of core data) -------------

  #' Extract bacteria counts from 2022 DNR chemistry data
  #'
  #' Keeps only bacteria counts from 2022 data file, formats date and parameter name.
  #'
  #' @param dnr_swims_22 Uncleaned 2022 chemistry data
  #'
  #' @return Data frame of bacteria counts for 2022
  tar_target(
    dnr_bac_22,
    dnr_swims_22 %>%
      filter(Units == "cells/mL") %>%
      mutate(
        `Collection Start Date/Time` = str_split_i(
          `Collection Start Date/Time`,
          " ",
          1
        ),
        `Collection Start Date/Time` = ymd(
          `Collection Start Date/Time`,
          tz = "America/Chicago"
        ),
        `DNR Parameter Description` = str_split_i(
          `DNR Parameter Description`,
          " ",
          1
        ),
        `DNR Parameter Description` = str_to_lower(`DNR Parameter Description`),
        `Sample Location` = str_sub(`Sample Location`, -9, -2)
      ) %>%
      select(
        StartDateTime = `Collection Start Date/Time`,
        StationID = `Sample Location`,
        `DNR Parameter Description`,
        `Numeric Value`
      )
  ),

  #' Process 2023 bacteria counts
  #'
  #' Formats date and parameter name.
  #'
  #' @param dnr_bac_23_raw Uncleaned 2023 bacteria data
  #'
  #' @return Data frame of bacteria counts for 2023
  tar_target(
    dnr_bac_23,
    dnr_bac_23_raw %>%
      # drop empty rows
      filter(!is.na(Site)) %>%
      mutate(
        StartDateTime = date(mdy_hm(`Collection Start Date/Time`)),
        `DNR Parameter Description` = str_split_i(
          `DNR Parameter Description`,
          " ",
          1
        ),
        `DNR Parameter Description` = str_to_lower(`DNR Parameter Description`),
        Site = as.character(Site)
      ) %>%
      left_join(dnr_sites, by = join_by(Site == SiteID)) %>%
      select(
        StartDateTime,
        StationID,
        `DNR Parameter Description`,
        `Numeric Value` = Value
      )
  ),

  #' Process 2024 bacteria counts
  #'
  #' Matches formatting of 2024 data to that from 2022-23
  #'
  #' @param dnr_bac_24_raw Uncleaned 2023 chemistry data
  #'
  #' @return Data frame of bacteria counts for 2024
  tar_target(
    dnr_bac_24,
    dnr_bac_24_raw %>%
      mutate(
        StartDateTime = date(`Collection Start Date/Time`),
        StationName = str_split_i(`Sample Location`, fixed(" ("), 1),
        StationName = str_replace(StationName, "R-", "R -"),
        `DNR Parameter Description` = str_split_i(
          `DNR Parameter Description`,
          " ",
          1
        ),
        `DNR Parameter Description` = str_to_lower(`DNR Parameter Description`)
      ) %>%
      left_join(dnr_stations_24, by = join_by(StationName)) %>%
      select(
        StartDateTime,
        StationID,
        `DNR Parameter Description`,
        `Numeric Value`
      )
  ),

  #' Combine bacteria counts
  #'
  #' Join bacteria count data frames, make wide, and fix some column names
  #'
  #' @param dnr_bac_22/23/24 Bacteria counts from 2022, 2023, 2024 WDNR sampling
  #'
  #' @return Wide data frame of bacteria counts
  tar_target(
    dnr_bac,
    bind_rows(dnr_bac_22, dnr_bac_23, dnr_bac_24) %>%
      arrange(`DNR Parameter Description`) %>%
      pivot_wider(
        names_from = `DNR Parameter Description`,
        values_from = `Numeric Value`,
        values_fn = ~ mean(.x, na.rm = TRUE)
      ) %>%
      arrange(StartDateTime, StationID) %>%
      rename(date = StartDateTime, station = StationID) %>%
      # make dates align with chem data
      mutate(
        date = case_when(
          station == "10052510" & date == ymd("2023-08-01") ~ ymd("2023-07-31"),
          station == "101" & date == ymd("2023-07-31") ~ ymd("2023-08-01"),
          station %in%
            c("101", "103", "10040814", "10052587") &
            date == ymd("2024-10-04") ~ ymd("2024-09-30"),
          .default = date
        )
      )
  ),

  #' Add other parameters to bacteria counts
  #'
  #' Joins in combined dnr file to bacteria counts
  #'
  #' @param dnr_s1 combined data frame of DNR data
  #' @param dnr_bac data frame of dnr bacteria counts
  #'
  #' @return Data frame of bacteria counts and chemistry and hydro data.
  tar_target(
    dnr_bac_plus,
    inner_join(dnr_s1, dnr_bac, by = join_by(date, station)) %>%
      relocate(
        date,
        site,
        station,
        latitude = StationLatitude,
        longitude = StationLongitude
      )
  )
)
