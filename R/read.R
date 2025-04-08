source("R/read_targets/dnr.R")
source("R/read_targets/umd.R")
source("R/read_targets/nps.R")
source("R/read_targets/ncbc.R")
source("R/read_targets/lsnerr.R")
source("R/read_targets/ncca.R")
source("R/read_targets/wqp.R")
source("R/read_targets/nwis.R")
source("R/read_targets/nbdc.R")
source("R/read_targets/ncdc.R")


read_targets <- list(

  # Helper functions ---------------------------------------------------

  #' Replace `NaN`s with `NA`s
  #'
  #' This function replaces `NaN` values with `NA` values to make visually identifying `NA` values easier.
  #'
  #' @param val R object to pass to `is.nan()`
  #' 
  #' @returns Original R object or NA
  tar_target(replace_nan, function(val) {
    return(if_else(is.nan(val), NA, val))
  }),

  #' Write object to a csv file
  #'
  #' This function takes a data frame and writes it to the `/out` directory with the supplied name.
  #'
  #' @param df Data frame to write out.
  #' @param name Name to use for the new file.
  #'
  #' @returns Path to new file (not used)
  tar_target(write_file, function(df, name) {
    filepath <- str_c("out/", name, ".csv")
    df <- df %>%
      mutate(date = as.character(date)) # necessary to prevent conversion to UTC
    write_csv(df, filepath, na = "")
    return(filepath)
  }),

  #' Add stoichiometric ratios to full files
  #' 
  #' Calculates 6 stoichiometric ratios and adds to passed in data frame. TN:TP, TOC:TN, TOC:TP, PON:PP, POC:PON, POC:PP.
  #'
  #' @param df Data frame with columns tn, tp, toc, pon, pp, poc
  #'
  #' @returns Data frame with 6 stoichiometric ratio columns added
  tar_target(add_ratios, function(df){
    df %>%
      mutate(
        npr = tn / tp,
        cnr = toc / tn,
        cpr = toc / tp,
        pnpr = pon / pp,
        pcnr = poc / pon,
        pcpr = poc / pp
      )
  }),
  
  #' Remove duplicate rows by averaging values
  #' 
  #' This function removes duplicates samples (same date at exact same latitude and longitude) by averaging rows. 
  #' This is preferred over dropping one of the rows because there is no way to know which row is more accurate.
  #' This function also removes unwanted parameters - those not sampled consistently enough across sources to warrant inclusion.
  #' Finally, as a part of the call to `summarise()` this function arranges the data frame by date.
  #'
  #' @param df Data frame resulting from cleaning functions earlier in pipeline. 
  #' Must have columns site, source, date, longitude, and latitude for grouping. See function for required data columns.
  #' @param type String specifying what type of cleaned data are being passed in. Used to choose what columns to include.
  #'
  #' @returns Data frame with duplicates rows averaged and unwanted parameters removed.
  tar_target(average_duplicates, function(df, type){
    id_cols = c("site", "source")
    if (type == "trib") id_cols <- c(id_cols, "huc")
    
    data_cols <- c()
    if (type == "trib") data_cols <- c(data_cols, "discharge")
    data_cols <- c(data_cols, "chl")
    if (type != "trib") data_cols <- c(data_cols, "chl_field") # no tribs have chl sensor data
    data_cols <- c(data_cols, "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                   "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                   "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl")
    
    df %>%
      group_by(date, latitude, longitude) %>%
      summarise(
        across(all_of(id_cols), ~ first(.x)),
        across(all_of(data_cols), ~ mean(.x, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      mutate(across(everything(), replace_nan))
  }),


  # Add targets organized by data source -------------------------------
  dnr_targets,
  umd_targets,
  nps_targets,
  ncbc_targets,
  lsnerr_targets,
  ncca_targets,
  wqp_targets,
  nwis_targets,
  nbdc_targets,
  ncdc_targets,


  # Make full files by type ---------------------------------------------
  
  #' Assemble core datasets
  #' 
  #' These 4 targets (for Western Lake Superior, the St Louis River Estuary, tributaries, and tributary discharge only) 
  #' combine relevant rows from cleaned versions of source data, add stoichiometric ratios, remove duplicates, and drop unwanted parameters.
  #'
  #' @params Cleaned versions of source data
  #'
  #' @returns Core dataset R objects
  tar_target(lake_full, filter(dnr, type == "Lake") %>%
    bind_rows(filter(umd, type == "Lake" & depth <= 2)) %>%
    bind_rows(filter(nps, type == "Lake")) %>%
    bind_rows(cbnut_clean) %>%
    bind_rows(filter(ncca, type == "Great Lake")) %>%
    bind_rows(filter(wqp_wide, type == "Great Lake")) %>%
    add_ratios() %>%
    average_duplicates("lake")),
  tar_target(est_full, lsnerr %>%
    bind_rows(filter(dnr, type == "Estuary")) %>%
    bind_rows(filter(umd, type == "Estuary")) %>%
    bind_rows(filter(ncca, type == "Estuary")) %>%
    bind_rows(filter(wqp_wide, type == "Estuary")) %>%
    add_ratios() %>%
    average_duplicates("est")),
  tar_target(trib_full, filter(umd, type == "Watershed") %>%
    bind_rows(cbtrib) %>%
    bind_rows(filter(nps, type == "Tributary")) %>%
    bind_rows(filter(nwis, !if_all(c(cond, do, ph, turb, tss, tkn, no3, tp), is.na))) %>% # only add rows with more than just discharge
    bind_rows(filter(wqp_wide, type == "River/Stream")) %>%
    add_ratios() %>%
    average_duplicates("trib")),
  tar_target(trib_q, nwis %>%
    bind_rows(cbq_clean) %>%
    select(date, site, latitude, longitude, huc, source, discharge) %>%
    arrange(date)),
  
  
  # Write CSV files ---------------------------------------------------------------
  
  #' Save core data to csv for distribution
  #' 
  #' These 4 targets (for Western Lake Superior, the St Louis River Estuary, tributaries, and tributary discharge only) 
  #' write out each R object to a CSV file in the `/out` directory.
  #'
  #' @params Core data objects
  #'
  #' @returns Path to CSV of core data file  
  tar_target(lake_file, write_file(lake_full, "lake_core"), format = "file"),
  tar_target(est_file, write_file(est_full, "estuary_core"), format = "file"),
  tar_target(trib_file, write_file(trib_full, "tributary_core"), format = "file"),
  tar_target(trib_q_file, write_file(trib_q, "trib_q_core"), format = "file")
)
