analysis_targets <- list(
  
  #sf versions of main files
  tar_target(make_sf, function(df){
    df %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))
  }),
  tar_target(lake_sf, make_sf(lake_full)),
  tar_target(trib_sf, make_sf(trib_full)),
  tar_target(est_sf, make_sf(est_full)),
  tar_target(trib_q_sf, make_sf(trib_q)),
  
  #assign region to sites
  tar_target(get_region, Vectorize(function(lat, long, huc = ""){
    if (huc == "04010102" | 
        (huc == "" & 
         ((lat > 46.78 & long < -91.63) |
          (lat > 47 & long < -91.1)))) {
      if (long < -92) return("les")
      else if (lat < 46.9 & long < -91.99) return("les")
      else if (long < -91.83) return("suc")
      else if (lat < 46.9) return("") #LLO buoy
      else if (long < -91.72) return("kni")
      else if (lat < 47.15 & long < -91.55) return("two")
      else if (lat < 47.24 & long < -91.42) return("goo")
      else if (lat < 47.24 & long < -91.39) return("spl")
      else if (lat < 47.3 & long < -91.44) return("spl")
      else if (lat < 47.1) return("") #far offshore mnpca site
      else if (long < -91.25) return("bea")
      else if (long < -91.2) return("pal")
      else return("")
    }
    else if (huc %in% c("04010201", "04010202") | 
             (huc == "" & 
              (lat > 46.72 & long < -91.99))) {
      if (lat < 46.9 & long > -92.7) return("stl")
      else return("")
    }
    else if (huc == "04010301" | 
             (huc == "" & 
              (long < -90.73))) {
      if (long > -91.2 & lat < 46.76) return("cb")
      else if (lat > 46.7 & long < -92.02) return("stl")
      else if (long < -92.3) return("") # drop way upstream nemadji sites
      else if (long < -92) return("nem")
      else if (lat > 46.55 & long < -91.95) return("nem")
      else if (lat > 46.6 & long < -91.92) return("dut")
      else if (long < -91.9) return("amn")
      else if (lat > 46.6 & long < -91.84) return("amn")
      else if (long < -91.75) return("pop")
      else if (long < -91.65) return("pea")
      else if (long < -91.5) return("bb")
      else if (long < -91.4 & lat > 46.74) return("iro")
      else if (long < -91.3 & lat < 46.72) return("") # drop sites upstream of orienta flowage
      else if (long < -91.38 & lat < 46.79) return("") # drop bibon lake
      else if (long < -91.28) return("fla")
      else if (long < -91.21) return("cra")
      else if (long < -91.195 & lat > 46.845) return("") # drop bark bay slough
      else if (long < -91.14) return("bar")
      else if (lat < 46.802 & long < -91.02) return("") # drop siskit lakes
      else if (long < -91.02 & lat < 46.96) return("sis")
      else if (long < -90.86 & lat < 46.96) return("san")
      else if (long < -90.81 & lat > 46.91 & lat < 46.96) return("ras")
      else if (long > -90.6) return("")
      else if (lat > 46.9 & lat < 46.94) return("fro")
      else if (lat > 46.8 & lat < 46.9) return("red")
      else return("")
    }
    else if (huc == "04010302" | 
             (huc == "" & 
              (long > -90.73 & lat < 46.75))) {
      if (lat > 46.65 & long < -90.65) return("")
      else if (lat > 46.485 & long > -90.91 & long < -90.6) return("bad")
      else return("") # drops way upstream bad river sites
    }
    else return("")
  })),
  tar_target(get_tpload_region, Vectorize(function(river){
    if (river == "Amnicon River") return("amn")
    else if (river == "Bad") return("bad")
    else if (river == "Bois Brule River") return("bb")
    else if (river == "Nemadji River") return("nem")
    else if (river == "StLouis") return("stl")
    else return("")
  })),
  tar_target(lake_reg, lake_full %>% 
               mutate(region = get_region(latitude, longitude)) %>% 
               filter(region != "")),
  tar_target(trib_reg, trib_full %>% 
               mutate(region = get_region(latitude, longitude, huc)) %>% 
               filter(region != "")),
  tar_target(trib_q_reg, trib_q %>% 
               mutate(region = get_region(latitude, longitude, huc))  %>% 
               filter(region != "")),
  
  
  # trib load
  tar_target(trib_load, trib_reg %>% 
               filter(!is.na(discharge)) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl), 
                             ~ discharge * 28.31685 * .)) %>% 
               select(-c(turb, cond, ph, temp, do, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr))),

  
  #scaled versions of main files -- don't scale if <5 observations of param at site (<10 per region)
  tar_target(make_scaled, function(df, cols, region = FALSE){
    id_cols <- c("date", "site", "latitude", "longitude", "source")
    if ("huc" %in% colnames(df)) id_cols <- c(id_cols, "huc")
    if ("region" %in% colnames(df)) id_cols <- c(id_cols, "region")
    if (region) {
      n <- 10
      group_cols <- c("region")
    }
    else {
      n <- 5
      group_cols <- c("latitude", "longitude")
    }
    df %>% 
      group_by(pick(all_of(group_cols))) %>%
      mutate(across(all_of(cols), ~ sum(!is.na(.)) >=n, .names = "{.col}2")) %>% 
      ungroup() %>% 
      mutate(across(all_of(cols), ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
      group_by(pick(all_of(group_cols))) %>%
      mutate(across(all_of(cols), ~(scale(.) %>% as.vector))) %>% 
      ungroup() %>% 
      select(all_of(id_cols), all_of(cols)) %>% 
      filter(!if_all(-all_of(id_cols), is.na))
  }),
  tar_target(lake_scaled, make_scaled(lake_full, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                   "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                   "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  tar_target(est_scaled, make_scaled(est_full, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                   "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                   "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  tar_target(trib_scaled, make_scaled(trib_full, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                 "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                 "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  tar_target(trib_q_scaled, make_scaled(trib_q, c("discharge"))),
  tar_target(trib_load_scaled, make_scaled(trib_load, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),

  tar_target(lake_scaled_reg, make_scaled(lake_reg, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                   "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                   "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"), TRUE)),
  tar_target(trib_scaled_reg, make_scaled(trib_reg, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                   "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                   "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"), TRUE)),
  tar_target(trib_q_scaled_reg, make_scaled(trib_q_reg, c("discharge"), TRUE)),
  tar_target(trib_load_scaled_reg, make_scaled(trib_load, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"), TRUE)),
  
  tar_target(tpload_scaled, ls_tpload %>% 
               mutate(region = get_tpload_region(river)) %>% 
               group_by(region) %>%
               mutate(across(c(tp_load),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, region, tp_load)),
  
  
  
  # week, month and year averages of scaled files
  tar_target(group_time, function(df, time = c("year", "month", "week"), reg = FALSE){
    remove_cols <- c("date")
    if ("latitude" %in% colnames(df)) remove_cols = c(remove_cols, "latitude", "longitude")
    if (time == "year") {
      df <- df %>% 
        mutate(year = year(date))
    } else if (time == "month") {
      df <- df %>% 
        mutate(month = floor_date(date, "month")) 
    } else {
      df <- df %>% 
        mutate(week = floor_date(date, "week"))
    }
    if (reg){
      df <- df %>% group_by(pick(all_of(time)), region)
    }else{
      df <- df %>% group_by(pick(all_of(time)))
    }
    df <- df %>% 
      summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
      mutate(across(where(is.double), replace_nan)) %>% 
      select(-all_of(remove_cols)) %>% 
      ungroup()
  }),
  tar_target(lake_year, group_time(lake_scaled, "year")),
  tar_target(lake_month, group_time(lake_scaled, "month")),
  tar_target(lake_week, group_time(lake_scaled, "week")),
  tar_target(lake_year_reg, group_time(lake_scaled_reg, "year", TRUE)),
  tar_target(lake_month_reg, group_time(lake_scaled_reg, "month", TRUE)),
  tar_target(lake_week_reg, group_time(lake_scaled_reg, "week", TRUE)),
  
  tar_target(est_year, group_time(est_scaled, "year")),
  tar_target(est_month, group_time(est_scaled, "month")),
  tar_target(est_week, group_time(est_scaled, "week")),
  
  tar_target(trib_year, group_time(trib_scaled, "year")),
  tar_target(trib_month, group_time(trib_scaled, "month")),
  tar_target(trib_week, group_time(trib_scaled, "week")),
  tar_target(trib_year_reg, group_time(trib_scaled_reg, "year", TRUE)),
  tar_target(trib_month_reg, group_time(trib_scaled_reg, "month", TRUE)),
  tar_target(trib_week_reg, group_time(trib_scaled_reg, "week", TRUE)),
  
  tar_target(trib_q_year, group_time(trib_q_scaled, "year")),
  tar_target(trib_q_month, group_time(trib_q_scaled, "month")),
  tar_target(trib_q_week, group_time(trib_q_scaled, "week")),
  tar_target(trib_q_year_reg, group_time(trib_q_scaled_reg, "year", TRUE)),
  tar_target(trib_q_month_reg, group_time(trib_q_scaled_reg, "month", TRUE)),
  tar_target(trib_q_week_reg, group_time(trib_q_scaled_reg, "week", TRUE)),
  
  tar_target(trib_load_year, group_time(trib_load_scaled, "year")),
  tar_target(trib_load_month, group_time(trib_load_scaled, "month")),
  tar_target(trib_load_week, group_time(trib_load_scaled, "week")),
  tar_target(trib_load_year_reg, group_time(trib_load_scaled_reg, "year", TRUE)),
  tar_target(trib_load_month_reg, group_time(trib_load_scaled_reg, "month", TRUE)),
  tar_target(trib_load_week_reg, group_time(trib_load_scaled_reg, "week", TRUE)),

  tar_target(tpload_year, group_time(tpload_scaled, "year")),
  tar_target(tpload_month, group_time(tpload_scaled, "month")),
  tar_target(tpload_week, group_time(tpload_scaled, "week")),
  tar_target(tpload_year_reg, group_time(tpload_scaled, "year", TRUE)),
  tar_target(tpload_month_reg, group_time(tpload_scaled, "month", TRUE)),
  tar_target(tpload_week_reg, group_time(tpload_scaled, "week", TRUE)),
  
  
  
  
  # lagged versions of trib from scaled file
  tar_target(make_lag, function(df, cols, n = 4){
    df <- df %>% 
      mutate(across(all_of(cols), ~lag(.), .names = "{.col}_lag1")) %>% 
      mutate(across(all_of(cols), ~lag(., 2), .names = "{.col}_lag2")) %>%
      mutate(across(all_of(cols), ~lag(., 3), .names = "{.col}_lag3")) %>%
      mutate(across(all_of(cols), ~lag(., 4), .names = "{.col}_lag4")) 
    
    if (n > 4) {
      df <- df %>% 
        mutate(across(all_of(cols), ~lag(., 5), .names = "{.col}_lag5")) %>% 
        mutate(across(all_of(cols), ~lag(., 6), .names = "{.col}_lag6")) %>%
        mutate(across(all_of(cols), ~lag(., 7), .names = "{.col}_lag7")) %>%
        mutate(across(all_of(cols), ~lag(., 8), .names = "{.col}_lag8"))
    }
    
    df %>%
      select(-all_of(cols))
  }),
  
  tar_target(trib_week_lag, make_lag(trib_week, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                  "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                  "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  
  tar_target(trib_load_week_lag, make_lag(trib_load_week, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  
  tar_target(trib_q_week_lag, make_lag(trib_q_week, c("discharge"), 8)),
  
  tar_target(tpload_week_lag, make_lag(tpload_week, c("tp_load"), 8)),
  
  tar_target(trib_week_lag_reg, make_lag(trib_week_reg, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                  "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                  "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  
  tar_target(trib_load_week_lag_reg, make_lag(trib_load_week_reg, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  
  tar_target(trib_q_week_lag_reg, make_lag(trib_q_week_reg, c("discharge"), 8)),
  
  tar_target(tpload_week_lag_reg, make_lag(tpload_week_reg, c("tp_load"), 8)),
  
  
  
  # get degree day temp from buoy file
  tar_target(nbdc_temp, nbdc_daily %>% 
               filter(site == "45028") %>% 
               group_by(date) %>% 
               summarise(temp = mean(wtemp_c, na.rm = TRUE)) %>% 
               mutate(across(temp, replace_nan))),
  
  tar_target(nbdc_temp_month, group_time(nbdc_temp, "month")),
  tar_target(nbdc_temp_week, group_time(nbdc_temp, "week")),
  
  tar_target(temp_week_lag, make_lag(nbdc_temp_week, c("temp"), 8) %>% 
               mutate(temp_lag2_cum = rowMeans(data.frame(temp_lag1, temp_lag2), na.rm = TRUE),
                      temp_lag3_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3), na.rm = TRUE),
                      temp_lag4_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4), na.rm = TRUE),
                      temp_lag5_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5), na.rm = TRUE),
                      temp_lag6_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6), na.rm = TRUE),
                      temp_lag7_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6, temp_lag7), na.rm = TRUE),
                      temp_lag8_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6, temp_lag7, temp_lag8), na.rm = TRUE))),
  
  tar_target(nbdc_dd_year, nbdc_temp %>% 
               mutate(year = year(date),
                      temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
               group_by(year) %>% 
               mutate(dd = cumsum(temp)) %>% 
               ungroup()),
  tar_target(nbdc_dd_month, nbdc_temp %>% 
               mutate(month = floor_date(date, "month"),
                      temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
               group_by(month) %>% 
               mutate(dd = cumsum(temp)) %>% 
               ungroup()),
  tar_target(nbdc_dd_week, nbdc_temp %>% 
               mutate(week = floor_date(date, "week"),
                      temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
               group_by(week) %>% 
               mutate(dd = cumsum(temp)) %>% 
               ungroup()),
  
  tar_target(nbdc_dd_year_sum, nbdc_dd_year %>%
               filter(year != 2010) %>% 
               group_by(year) %>% 
               summarise(dd = max(dd), n = sum(temp != 0))),
  tar_target(nbdc_dd_month_sum, nbdc_dd_month %>% 
               group_by(month) %>% 
               summarise(dd = max(dd), n = sum(temp != 0))),
  tar_target(nbdc_dd_week_sum, nbdc_dd_week %>% 
               group_by(week) %>% 
               summarise(dd = max(dd), n = sum(temp != 0))),
  
  tar_target(dd_month_lag, make_lag(nbdc_dd_month_sum, c("dd"), 4) %>% 
               select(-n)),
  tar_target(dd_week_lag, make_lag(nbdc_dd_week_sum, c("dd"), 8) %>% 
               mutate(dd_lag2_cum = dd_lag1 + dd_lag2,
                      dd_lag3_cum = dd_lag2_cum + dd_lag3,
                      dd_lag4_cum = dd_lag3_cum + dd_lag4,
                      dd_lag5_cum = dd_lag4_cum + dd_lag5,
                      dd_lag6_cum = dd_lag5_cum + dd_lag6,
                      dd_lag7_cum = dd_lag6_cum + dd_lag7,
                      dd_lag8_cum = dd_lag7_cum + dd_lag8) %>% 
               select(-n)),
  
  
  #joining together
  
  tar_target(week_reg, left_join(lake_week_reg, trib_week_reg, by = join_by(week, region)) %>% 
               left_join(trib_q_week_reg, by = join_by(week, region)) %>% 
               left_join(tpload_week_reg, by = join_by(week, region))),
  tar_target(week_reg_load, left_join(lake_week_reg, trib_week_reg, by = join_by(week, region)) %>% 
               left_join(trib_q_week_reg, by = join_by(week, region)) %>% 
               left_join(tpload_week_reg, by = join_by(week, region)) %>% 
               left_join(nbdc_dd_week_sum, by = join_by(week)) %>% 
               select(-n) %>% 
               mutate(dd = if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, dd))),
  tar_target(week_reg_lag, left_join(lake_week_reg, trib_week_lag_reg, by = join_by(week, region))),
  tar_target(week_reg_lag_big, left_join(lake_week_reg, trib_week_lag_reg, by = join_by(week, region)) %>% 
               left_join(trib_q_week_lag_reg, by = join_by(week, region)) %>% 
               left_join(tpload_week_lag_reg, by = join_by(week, region)) %>% 
               left_join(dd_week_lag, by = join_by(week)) %>% 
               mutate(across(contains("dd"), ~if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, .))))

  
  
  


  
  
)