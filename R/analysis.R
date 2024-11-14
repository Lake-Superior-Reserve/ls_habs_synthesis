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
  
  #CB and WI coast versions of files
  tar_target(make_lake_west, function(df){
    df %>% 
      filter(longitude < -91 | (latitude > 46.75 & longitude < -90.85)) %>% 
      filter(!str_detect(site, "MN") & year(date) > 2010)
  }),
  tar_target(make_lake_cb, function(df){
    df %>% 
      filter(longitude > -91 & latitude < 46.75 & longitude < -90.73)
  }),
  tar_target(make_trib_west, function(df){
    df %>% 
      filter(huc == "04010301" & longitude > -92.3) %>% 
      filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))
  }),
  tar_target(make_trib_cb, function(df){
    df %>% 
      filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76) 
  }),
  tar_target(make_trib_stl, function(df){
    df %>% 
      filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)
  }),
  tar_target(lake_west, make_lake_west(lake_full)),
  tar_target(lake_cb, make_lake_cb(lake_full)),
  tar_target(trib_west, make_trib_west(trib_full)),
  tar_target(trib_cb, make_trib_cb(trib_full)),
  tar_target(trib_stl, make_trib_stl(trib_full)),
  tar_target(trib_q_west, make_trib_west(trib_q)),
  tar_target(trib_q_cb, make_trib_cb(trib_q)),
  tar_target(trib_q_stl, make_trib_stl(trib_q)),
  
  # trib load
  tar_target(trib_load, trib_full %>% 
               filter(!is.na(discharge)) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl), 
                             ~ discharge * 28.31685 * .)) %>% 
               select(-c(turb, cond, ph, temp, do, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr))),
  tar_target(trib_load_west, make_trib_west(trib_load)),
  tar_target(trib_load_cb, make_trib_cb(trib_load)),
  tar_target(trib_load_stl, make_trib_stl(trib_load)),
  
  #scaled versions of main files -- don't scale if <5 observations of param at site
  tar_target(make_scaled, function(df, cols){
    id_cols <- c("date", "site", "latitude", "longitude", "source")
    if ("huc" %in% colnames(df)) id_cols <- c(id_cols, "huc")
    df %>% 
      group_by(latitude, longitude) %>%
      mutate(across(all_of(cols), ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
      ungroup() %>% 
      mutate(across(all_of(cols), ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
      group_by(latitude, longitude) %>%
      mutate(across(all_of(cols), ~(scale(.) %>% as.vector))) %>% 
      ungroup() %>% 
      select(all_of(id_cols), all_of(cols))
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
  
  tar_target(tpload_scaled, ls_tpload %>% 
               group_by(river) %>%
               mutate(across(c(tp_load),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, river, tp_load)),
  tar_target(tpload_west_scaled, tpload_scaled %>% 
               filter(river %in% c("Amnicon River", "Bois Brule River", "Nemadji River"))),
  tar_target(tpload_stl_scaled, tpload_scaled %>% 
               filter(river %in% c("StLouis"))),
  
  tar_target(lake_west_scaled, make_lake_west(lake_scaled)),
  tar_target(lake_cb_scaled, make_lake_cb(lake_scaled)),
  tar_target(trib_west_scaled, make_trib_west(trib_scaled)),
  tar_target(trib_cb_scaled, make_trib_cb(trib_scaled)),
  tar_target(trib_stl_scaled, make_trib_stl(trib_scaled)),
  tar_target(trib_q_west_scaled, make_trib_west(trib_q_scaled)),
  tar_target(trib_q_cb_scaled, make_trib_cb(trib_q_scaled)),
  tar_target(trib_q_stl_scaled, make_trib_stl(trib_q_scaled)),
  tar_target(trib_load_west_scaled, make_trib_west(trib_load_scaled)),
  tar_target(trib_load_cb_scaled, make_trib_cb(trib_load_scaled)),
  tar_target(trib_load_stl_scaled, make_trib_stl(trib_load_scaled)),
  
  
  # week, month and year averages of scaled files
  tar_target(make_year, function(df){
    remove_cols <- c("date")
    if ("latitude" %in% colnames(df)) remove_cols = c(remove_cols, "latitude", "longitude")
    df %>% 
      mutate(year = year(date)) %>% 
      group_by(year) %>% 
      summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
      mutate(across(where(is.double), replace_nan)) %>% 
      select(-all_of(remove_cols))
  }),
  tar_target(make_month, function(df){
    remove_cols <- c("date")
    if ("latitude" %in% colnames(df)) remove_cols = c(remove_cols, "latitude", "longitude")
    df %>% 
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>% 
      summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
      mutate(across(where(is.double), replace_nan)) %>%
      select(-all_of(remove_cols))
  }),
  tar_target(make_week, function(df){
    remove_cols <- c("date")
    if ("latitude" %in% colnames(df)) remove_cols = c(remove_cols, "latitude", "longitude")
    df %>% 
      mutate(week = floor_date(date, "week")) %>%
      group_by(week) %>% 
      summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
      mutate(across(where(is.double), replace_nan)) %>%
      select(-all_of(remove_cols))
  }),
  
  tar_target(lake_year, make_year(lake_scaled)),
  tar_target(lake_month, make_month(lake_scaled)),
  tar_target(lake_week, make_week(lake_scaled)),
  tar_target(lake_west_year, make_year(lake_west_scaled)),
  tar_target(lake_west_month, make_month(lake_west_scaled)),
  tar_target(lake_west_week, make_week(lake_west_scaled)),
  tar_target(lake_cb_year, make_year(lake_cb_scaled)),
  tar_target(lake_cb_month, make_month(lake_cb_scaled)),
  tar_target(lake_cb_week, make_week(lake_cb_scaled)),
  
  tar_target(est_year, make_year(est_scaled)),
  tar_target(est_month, make_month(est_scaled)),
  tar_target(est_week, make_week(est_scaled)),
  
  tar_target(trib_year, make_year(trib_scaled)),
  tar_target(trib_month, make_month(trib_scaled)),
  tar_target(trib_week, make_week(trib_scaled)),
  tar_target(trib_west_year, make_year(trib_west_scaled)),
  tar_target(trib_west_month, make_month(trib_west_scaled)),
  tar_target(trib_west_week, make_week(trib_west_scaled)),
  tar_target(trib_cb_year, make_year(trib_cb_scaled)),
  tar_target(trib_cb_month, make_month(trib_cb_scaled)),
  tar_target(trib_cb_week, make_week(trib_cb_scaled)),
  tar_target(trib_stl_year, make_year(trib_stl_scaled)),
  tar_target(trib_stl_month, make_month(trib_stl_scaled)),
  tar_target(trib_stl_week, make_week(trib_stl_scaled)),
  
  tar_target(trib_q_year, make_year(trib_q_scaled)),
  tar_target(trib_q_month, make_month(trib_q_scaled)),
  tar_target(trib_q_week, make_week(trib_q_scaled)),
  tar_target(trib_q_west_year, make_year(trib_q_west_scaled)),
  tar_target(trib_q_west_month, make_month(trib_q_west_scaled)),
  tar_target(trib_q_west_week, make_week(trib_q_west_scaled)),
  tar_target(trib_q_cb_year, make_year(trib_q_cb_scaled)),
  tar_target(trib_q_cb_month, make_month(trib_q_cb_scaled)),
  tar_target(trib_q_cb_week, make_week(trib_q_cb_scaled)),
  tar_target(trib_q_stl_year, make_year(trib_q_stl_scaled)),
  tar_target(trib_q_stl_month, make_month(trib_q_stl_scaled)),
  tar_target(trib_q_stl_week, make_week(trib_q_stl_scaled)),
  
  tar_target(trib_load_year, make_year(trib_load_scaled)),
  tar_target(trib_load_month, make_month(trib_load_scaled)),
  tar_target(trib_load_week, make_week(trib_load_scaled)),
  tar_target(trib_load_west_year, make_year(trib_load_west_scaled)),
  tar_target(trib_load_west_month, make_month(trib_load_west_scaled)),
  tar_target(trib_load_west_week, make_week(trib_load_west_scaled)),
  tar_target(trib_load_cb_year, make_year(trib_load_cb_scaled)),
  tar_target(trib_load_cb_month, make_month(trib_load_cb_scaled)),
  tar_target(trib_load_cb_week, make_week(trib_load_cb_scaled)),
  tar_target(trib_load_stl_year, make_year(trib_load_stl_scaled)),
  tar_target(trib_load_stl_month, make_month(trib_load_stl_scaled)),
  tar_target(trib_load_stl_week, make_week(trib_load_stl_scaled)),

  tar_target(tpload_year, make_year(tpload_scaled)),
  tar_target(tpload_month, make_month(tpload_scaled)),
  tar_target(tpload_week, make_week(tpload_scaled)),
  tar_target(tpload_west_year, make_year(tpload_west_scaled)),
  tar_target(tpload_west_month, make_month(tpload_west_scaled)),
  tar_target(tpload_west_week, make_week(tpload_west_scaled)),
  tar_target(tpload_stl_year, make_year(tpload_stl_scaled)),
  tar_target(tpload_stl_month, make_month(tpload_stl_scaled)),
  tar_target(tpload_stl_week, make_week(tpload_stl_scaled)),
  
  
  
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
  tar_target(trib_west_week_lag, make_lag(trib_west_week, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                            "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                            "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  tar_target(trib_cb_week_lag, make_lag(trib_cb_week, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                        "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                        "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  tar_target(trib_stl_week_lag, make_lag(trib_stl_week, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                          "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                          "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))),
  
  tar_target(trib_load_week_lag, make_lag(trib_load_week, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  tar_target(trib_load_west_week_lag, make_lag(trib_load_west_week, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  tar_target(trib_load_cb_week_lag, make_lag(trib_load_cb_week, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  tar_target(trib_load_stl_week_lag, make_lag(trib_load_stl_week, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))),
  
  tar_target(trib_q_week_lag, make_lag(trib_q_week, c("discharge"), 8)),
  tar_target(trib_q_west_week_lag, make_lag(trib_q_west_week, c("discharge"), 8)),
  tar_target(trib_q_cb_week_lag, make_lag(trib_q_cb_week, c("discharge"), 8)),
  tar_target(trib_q_stl_week_lag, make_lag(trib_q_stl_week, c("discharge"), 8)),
  
  tar_target(tpload_week_lag, make_lag(tpload_week, c("tp_load"), 8)),
  tar_target(tpload_west_week_lag, make_lag(tpload_west_week, c("tp_load"), 8)),
  tar_target(tpload_stl_week_lag, make_lag(tpload_stl_week, c("tp_load"), 8)),
  
  
  # west region tp load
  tar_target(trib_west_tp_load, trib_load_west %>% 
               filter(site %in% c("04026005", "04026160") & year(date) > 2020) %>% 
               mutate(discharge = if_else(is.na(discharge), 0, discharge),
                      tp = if_else(is.na(tp), 0, tp)) %>% 
               group_by(date) %>% 
               summarise(tp_load = sum(tp), discharge_tot = sum(discharge))),
  
  tar_target(trib_west_tp_year, trib_west_tp_load %>% 
               mutate(year = year(date)) %>%
               group_by(year) %>% 
               mutate(tp_load = cumsum(tp_load), discharge_tot = cumsum(discharge_tot)) %>% 
               ungroup()),
  tar_target(trib_west_tp_month, trib_west_tp_load %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               mutate(tp_load = cumsum(tp_load), discharge_tot = cumsum(discharge_tot)) %>% 
               ungroup()),
  tar_target(trib_west_tp_week, trib_west_tp_load %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               mutate(tp_load = cumsum(tp_load), discharge_tot = cumsum(discharge_tot)) %>% 
               ungroup()),
  
  tar_target(trib_west_tp_year_sum, trib_west_tp_year %>%
               group_by(year) %>% 
               summarise(tp_load = max(tp_load), discharge_tot = max(discharge_tot), n = sum(tp_load != 0))),
  tar_target(trib_west_tp_month_sum, trib_west_tp_month %>% 
               group_by(month) %>% 
               summarise(tp_load = max(tp_load), discharge_tot = max(discharge_tot), n = sum(tp_load != 0))),
  tar_target(trib_west_tp_week_sum, trib_west_tp_week %>% 
               group_by(week) %>% 
               summarise(tp_load = max(tp_load), discharge_tot = max(discharge_tot), n = sum(tp_load != 0))),
  
  tar_target(trib_west_tp_week_lag, make_lag(trib_west_tp_week_sum, c("tp_load"), 8) %>% 
               mutate(tp_lag2_cum = tp_load_lag1 + tp_load_lag2,
                      tp_lag3_cum = tp_lag2_cum + tp_load_lag3,
                      tp_lag4_cum = tp_lag3_cum + tp_load_lag4,
                      tp_lag5_cum = tp_lag4_cum + tp_load_lag5,
                      tp_lag6_cum = tp_lag5_cum + tp_load_lag6,
                      tp_lag7_cum = tp_lag6_cum + tp_load_lag7,
                      tp_lag8_cum = tp_lag7_cum + tp_load_lag8) %>% 
               select(-n)),
  
  
  # get degree day temp from buoy file
  tar_target(nbdc_temp, nbdc_daily %>% 
               filter(site == "45028") %>% 
               group_by(date) %>% 
               summarise(temp = mean(wtemp_c, na.rm = TRUE)) %>% 
               mutate(across(temp, replace_nan))),
  
  tar_target(nbdc_temp_month, make_month(nbdc_temp)),
  tar_target(nbdc_temp_week, make_week(nbdc_temp)),
  
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
  
  
  # combined week/month means plus lag dd and q
  tar_target(lake_west_week_plus, trib_q_west_week_lag %>% 
               #left_join(tpload_west_week_lag) %>% 
               left_join(trib_west_tp_week_lag) %>% 
               left_join(dd_week_lag) %>% 
               left_join(lake_west_week)),
  tar_target(lake_cb_week_plus, trib_q_cb_week_lag %>% 
               left_join(dd_week_lag) %>% 
               left_join(lake_cb_week)),
  
  
  tar_target(lake_week_q_dd, trib_q_week_lag %>% 
               left_join(dd_week_lag) %>% 
               left_join(lake_week)),
  tar_target(lake_week_q_temp, trib_q_week_lag %>% 
               left_join(temp_week_lag) %>% 
               left_join(lake_week)),
  
  #tp lag and lake week
  tar_target(lake_week_tp, tpload_week_lag %>% 
               left_join(lake_week))

  
  
  


  
  
)