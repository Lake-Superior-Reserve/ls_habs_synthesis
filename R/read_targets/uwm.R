uwm_targets <- list(
  # raw files --------------------------------------------------------

  #' Targets for raw source files
  #'
  #' These targets list the paths for all of the UWM source files.
  #' Data from 2022 were a loss due to poor cell reception and a corrupt SD card.
  #' Buoy was not deployed in 2025 due to staffing and administrative changes within NPS and UWM
  #'
  #' @return A file path.
  tar_target(
    uwm21_file,
    "raw_data/uwm/MWKE2021.csv",
    format = "file"
  ),
  tar_target(
    uwm23_file,
    "raw_data/uwm/MWKE2023.csv",
    format = "file"
  ),
  tar_target(
    uwm24_file,
    "raw_data/uwm/MWKE2024.csv",
    format = "file"
  ),

  # read files -------------------------------------------------------

  #' Read NPS files
  #'
  #' Reads in data files
  #'
  #' @param nps_<type>_file raw data file
  #'
  #' @return Data frame with uncleaned data.
  tar_target(uwm21, read_csv(uwm21_file)),
  tar_target(uwm23, read_csv(uwm23_file)),
  tar_target(uwm24, read_csv(uwm24_file)),

  # clean files ------------------------------------------------------

  #' Clean data
  #'
  #' Format dates, rename columns
  #' Removes erroenously low data (< 0 in most cases; <100 for conductivity)
  #' Remove suspiciously high chlorophyll (> 50 ug/L)
  #'
  #' @param uwm<year> raw data
  #'
  #' @return Data frame with cleaned data.
  tar_target(
    uwm21_clean,
    uwm21 %>%
      mutate(
        temp = temp1, # Based on 2023, exo temp is closes to temp1
        date = force_tz(Dates, tzone = "America/Chicago")
      ) %>%
      select(
        date,
        roll,
        pitch,
        heading,
        latitude = lat,
        longitude = lon,
        wind = ws,
        phy,
        chl,
        temp0,
        temp1,
        temp2,
        temp3,
        temp4,
        temp5,
        temp6,
        temp7,
        temp8,
        temp
      )
  ),

  tar_target(
    uwm23_clean,
    uwm23 %>%
      mutate(
        date = force_tz(Dates, tzone = "America/Chicago"),
        lat = if_else(lat == 0, median(lat), lat),
        lon = if_else(lon == 0, median(lon), lon),
        chl = if_else(chl > 50, NA, chl)
      ) %>%
      select(
        date,
        roll,
        pitch,
        heading,
        latitude = lat,
        longitude = lon,
        wind = ws,
        air_temp = airt,
        phy,
        chl,
        temp0,
        temp1,
        temp2,
        temp3,
        temp4,
        temp5,
        temp6,
        temp7,
        temp8,
        temp = extemp,
        do_sat = exdos,
        do = exdomgl,
        cond = excond,
        turb = exturb,
        chl_exo = exchl,
        phy_exo = exphy
      )
  ),
  tar_target(
    uwm24_clean,
    uwm24 %>%
      mutate(
        date = force_tz(datetime, tzone = "America/Chicago"),
        lat = if_else(lat == 0, median(lat), lat),
        lon = if_else(lon == 0, median(lon), lon),
        chl = if_else(chlavg > 50, NA, chlavg),
        wind = if_else(ws < 0, NA, ws),
        air_temp = if_else(atmoairt < 0, NA, atmoairt),
        cond = if_else(exospcond < 100, NA, exospcond),
        turb = if_else(exoturb < 0, NA, exoturb),
      ) %>%
      select(
        date,
        roll,
        pitch,
        heading,
        latitude = lat,
        longitude = lon,
        wind,
        wind_dir = truewd,
        air_temp,
        phy = phyavg,
        chl = chl,
        temp = exotempc,
        do_sat = exodosat,
        do = exodomgl,
        cond,
        turb,
        phy_exo = exophy
      )
  ),

  # Aggregate data by hour and day -----------------------------------------

  #' Aggregate data by hour and day
  #'
  #' Get hourly and daily averages for data.
  #' Data were not measured at consistent time gaps each year, so this makes it consistent (hourly)
  #' Combined dataset is daily, so this preps for that
  #'
  #' @param nps<year>_clean clean data; hourly data for the daily averages
  #'
  #' @return Data frame with hourly/daily mean data.
  tar_target(
    uwm_hourly,
    bind_rows(uwm21_clean, uwm23_clean, uwm24_clean) %>%
      select(
        date,
        latitude,
        longitude,
        wind,
        air_temp,
        chl_field = chl,
        phyco = phy,
        temp,
        do_sat,
        do,
        cond,
        turb
      ) %>%
      mutate(date = floor_date(date, "hour")) %>%
      summarize(
        across(everything(), ~ mean(., na.rm = TRUE)),
        .by = c(date)
      )
  ),
  tar_target(
    uwm_daily,
    uwm_hourly %>%
      mutate(date = date(date)) %>%
      summarize(
        across(everything(), ~ mean(., na.rm = TRUE)),
        .by = c(date)
      ) %>%
      mutate(site = "Mawike Buoy", source = "UWM")
  )
)
