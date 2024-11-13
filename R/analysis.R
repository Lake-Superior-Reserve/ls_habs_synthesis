analysis_targets <- list(
  
  #sf versions of main files
  tar_target(lake_sf, lake_full %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  tar_target(trib_sf, trib_full %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  tar_target(est_sf, est_full %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  tar_target(trib_q_sf, trib_q %>% 
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))),
  
  #CB and WI coast versions of files
  tar_target(lake_west, lake_full %>% 
               filter(longitude < -91 | (latitude > 46.75 & longitude < -90.85)) %>% 
               filter(!str_detect(site, "MN") & year(date) > 2010)),
  tar_target(lake_cb, lake_full %>% 
               filter(longitude > -91 & latitude < 46.75 & longitude < -90.73)),
  tar_target(trib_west, trib_full %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_cb, trib_full %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_stl, trib_full %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  tar_target(trib_q_west, trib_q %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_q_cb, trib_q %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_q_stl, trib_q %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  
  # trib load
  tar_target(trib_load, trib_full %>% 
               filter(!is.na(discharge)) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl), 
                             ~ discharge * 28.31685 * .)) %>% 
               select(-c(turb, cond, ph, temp, do, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr))),
  tar_target(trib_load_west, trib_load %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_load_cb, trib_load %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_load_stl, trib_load %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  
  #scaled versions of main files -- don't scale if <5 observations of param at site
  tar_target(lake_scaled, lake_full %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
               ungroup() %>% 
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, site, latitude, longitude, source, chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl)),
  tar_target(trib_scaled, trib_full %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
               ungroup() %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, site, latitude, longitude, huc, source, discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl)),
  tar_target(est_scaled, est_full %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, npr, cnr, cpr, si, cl),
                             ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
               ungroup() %>% 
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, npr, cnr, cpr, si, cl),
                             ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, si, cl),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, site, latitude, longitude, source, chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, 
                      doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, npr, cnr, cpr, si, cl)),
  tar_target(trib_q_scaled, trib_q %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(discharge),
                             ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
               ungroup() %>% 
               mutate(across(c(discharge),
                             ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(discharge),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, site, latitude, longitude, huc, source, discharge)),
  tar_target(trib_load_scaled, trib_load %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~ sum(!is.na(.)) >=5, .names = "{.col}2")) %>% 
               ungroup() %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
               group_by(latitude, longitude) %>%
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~(scale(.) %>% as.vector))) %>% 
               ungroup() %>% 
               select(date, site, latitude, longitude, huc, source, chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl)),
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
  tar_target(lake_west_scaled, lake_scaled %>% 
               filter(longitude < -91 | (latitude > 46.75 & longitude < -90.85)) %>% 
               filter(!str_detect(site, "MN") & year(date) > 2010)),
  tar_target(lake_cb_scaled, lake_scaled %>% 
               filter(longitude > -91 & latitude < 46.75 & longitude < -90.73)),
  tar_target(trib_west_scaled, trib_scaled %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_cb_scaled, trib_scaled %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_stl_scaled, trib_scaled %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  tar_target(trib_q_west_scaled, trib_q_scaled %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_q_cb_scaled, trib_q_scaled %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_q_stl_scaled, trib_q_scaled %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  tar_target(trib_load_west_scaled, trib_load_scaled %>% 
               filter(huc == "04010301" & longitude > -92.3) %>% 
               filter(longitude < -91.2 | (latitude > 46.76 & longitude < -90.86))),
  tar_target(trib_load_cb_scaled, trib_load_scaled %>% 
               filter(huc == "04010301" & longitude > -91.2 & latitude < 46.76)),
  tar_target(trib_load_stl_scaled, trib_load_scaled %>% 
               filter(huc %in% c("04010201", "04010202") & latitude < 46.9 & longitude > -92.7)),
  
  
  # week, month and year averages of scaled files
  tar_target(lake_year, lake_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>% 
               select(-c(date, latitude, longitude))),
  tar_target(lake_month, lake_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(lake_week, lake_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(lake_west_year, lake_west_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>% 
               select(-c(date, latitude, longitude))),
  tar_target(lake_west_month, lake_west_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(lake_west_week, lake_west_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(lake_cb_year, lake_cb_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>% 
               select(-c(date, latitude, longitude))),
  tar_target(lake_cb_month, lake_cb_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(lake_cb_week, lake_cb_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  
  
  
  
  
  tar_target(est_year, est_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(est_month, est_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(est_week, est_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  
  
  
  
  tar_target(trib_year, trib_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_month, trib_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_week, trib_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_west_year, trib_west_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_west_month, trib_west_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_west_week, trib_west_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_cb_year, trib_cb_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_cb_month, trib_cb_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_cb_week, trib_cb_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_stl_year, trib_stl_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_stl_month, trib_stl_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_stl_week, trib_stl_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  
  
  
  tar_target(trib_q_year, trib_q_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_month, trib_q_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_week, trib_q_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_west_year, trib_q_west_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_west_month, trib_q_west_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_west_week, trib_q_west_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_cb_year, trib_q_cb_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_cb_month, trib_q_cb_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_cb_week, trib_q_cb_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_stl_year, trib_q_stl_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_stl_month, trib_q_stl_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  tar_target(trib_q_stl_week, trib_q_stl_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(discharge, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(discharge, replace_nan))),
  
  
  
  tar_target(trib_load_year, trib_load_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_month, trib_load_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_week, trib_load_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_west_year, trib_load_west_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_west_month, trib_load_west_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_west_week, trib_load_west_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_cb_year, trib_load_cb_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_cb_month, trib_load_cb_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_cb_week, trib_load_cb_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_stl_year, trib_load_stl_scaled %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_stl_month, trib_load_stl_scaled %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  tar_target(trib_load_stl_week, trib_load_stl_scaled %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(where(is.double), replace_nan)) %>%
               select(-c(date, latitude, longitude))),
  
  
  
  
  tar_target(tpload_year, tpload_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_month, tpload_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_week, tpload_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_west_year, tpload_west_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_west_month, tpload_west_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_west_week, tpload_west_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_stl_year, tpload_stl_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(year = year(date)) %>% 
               group_by(year) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_stl_month, tpload_stl_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  tar_target(tpload_stl_week, tpload_stl_scaled %>% 
               filter(tp_load < 5) %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(tp_load, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(tp_load, replace_nan))),
  
  
  
  
  
  
  
  # lagged versions of trib from scaled file
  tar_target(trib_week_lag, trib_week %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag2")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag3")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag4")) %>%
               select(-c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                         doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl))),
  tar_target(trib_west_week_lag, trib_west_week %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag2")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag3")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag4")) %>%
               select(-c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                         doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl))),
  tar_target(trib_cb_week_lag, trib_cb_week %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag2")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag3")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag4")) %>%
               select(-c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                         doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl))),
  tar_target(trib_stl_week_lag, trib_stl_week %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag2")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag3")) %>%
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             ~lag(., 2),
                             .names = "{.col}_lag4")) %>%
               select(-c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                         doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl))),
  
  
  tar_target(trib_load_week_lag, trib_load_week %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag2")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag3")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag4")) %>% 
               select(-c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl))),
  tar_target(trib_load_west_week_lag, trib_load_west_week %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag2")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag3")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag4")) %>% 
               select(-c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl))),
  tar_target(trib_load_cb_week_lag, trib_load_cb_week %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag2")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag3")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag4")) %>% 
               select(-c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl))),
  tar_target(trib_load_stl_week_lag, trib_load_stl_week %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag1")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag2")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag3")) %>% 
               mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl),
                             ~lag(.),
                             .names = "{.col}_lag4")) %>% 
               select(-c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl))),
  
  
  
  
  tar_target(trib_q_month_lag, trib_q_month %>% 
               mutate(q_lag1 = lag(discharge),
                      q_lag2 = lag(discharge, 2),
                      q_lag3 = lag(discharge, 3),
                      q_lag4 = lag(discharge, 4))),
  tar_target(trib_q_week_lag, trib_q_week %>% 
               mutate(q_lag1 = lag(discharge),
                      q_lag2 = lag(discharge, 2),
                      q_lag3 = lag(discharge, 3),
                      q_lag4 = lag(discharge, 4),
                      q_lag5 = lag(discharge, 5),
                      q_lag6 = lag(discharge, 6),
                      q_lag7 = lag(discharge, 7),
                      q_lag8 = lag(discharge, 8))),
  tar_target(trib_q_west_week_lag, trib_q_west_week %>% 
               mutate(q_lag1 = lag(discharge),
                      q_lag2 = lag(discharge, 2),
                      q_lag3 = lag(discharge, 3),
                      q_lag4 = lag(discharge, 4),
                      q_lag5 = lag(discharge, 5),
                      q_lag6 = lag(discharge, 6),
                      q_lag7 = lag(discharge, 7),
                      q_lag8 = lag(discharge, 8))),
  tar_target(trib_q_cb_week_lag, trib_q_cb_week %>% 
               mutate(q_lag1 = lag(discharge),
                      q_lag2 = lag(discharge, 2),
                      q_lag3 = lag(discharge, 3),
                      q_lag4 = lag(discharge, 4),
                      q_lag5 = lag(discharge, 5),
                      q_lag6 = lag(discharge, 6),
                      q_lag7 = lag(discharge, 7),
                      q_lag8 = lag(discharge, 8))),
  tar_target(trib_q_stl_week_lag, trib_q_stl_week %>% 
               mutate(q_lag1 = lag(discharge),
                      q_lag2 = lag(discharge, 2),
                      q_lag3 = lag(discharge, 3),
                      q_lag4 = lag(discharge, 4),
                      q_lag5 = lag(discharge, 5),
                      q_lag6 = lag(discharge, 6),
                      q_lag7 = lag(discharge, 7),
                      q_lag8 = lag(discharge, 8))),
  
  
  tar_target(tpload_week_lag, tpload_week %>% 
               mutate(tp_lag1 = lag(tp_load),
                      tp_lag2 = lag(tp_load, 2),
                      tp_lag3 = lag(tp_load, 3),
                      tp_lag4 = lag(tp_load, 4),
                      tp_lag5 = lag(tp_load, 5),
                      tp_lag6 = lag(tp_load, 6),
                      tp_lag7 = lag(tp_load, 7),
                      tp_lag8 = lag(tp_load, 8))),
  tar_target(tpload_west_week_lag, tpload_west_week %>% 
               mutate(tp_lag1 = lag(tp_load),
                      tp_lag2 = lag(tp_load, 2),
                      tp_lag3 = lag(tp_load, 3),
                      tp_lag4 = lag(tp_load, 4),
                      tp_lag5 = lag(tp_load, 5),
                      tp_lag6 = lag(tp_load, 6),
                      tp_lag7 = lag(tp_load, 7),
                      tp_lag8 = lag(tp_load, 8))),
  tar_target(tpload_stl_week_lag, tpload_stl_week %>% 
               mutate(tp_lag1 = lag(tp_load),
                      tp_lag2 = lag(tp_load, 2),
                      tp_lag3 = lag(tp_load, 3),
                      tp_lag4 = lag(tp_load, 4),
                      tp_lag5 = lag(tp_load, 5),
                      tp_lag6 = lag(tp_load, 6),
                      tp_lag7 = lag(tp_load, 7),
                      tp_lag8 = lag(tp_load, 8))),
  
  
  
  
  
  
  
  
  
  # get degree day temp from buoy file
  tar_target(nbdc_temp, nbdc_daily %>% 
               filter(site == "45028") %>% 
               group_by(date) %>% 
               summarise(temp = mean(wtemp_c, na.rm = TRUE)) %>% 
               mutate(across(temp, replace_nan))),
  
  tar_target(nbdc_temp_month, nbdc_temp %>% 
               mutate(month = floor_date(date, "month")) %>%
               group_by(month) %>% 
               summarise(across(temp, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(temp, replace_nan))),
  tar_target(nbdc_temp_week, nbdc_temp %>% 
               mutate(week = floor_date(date, "week")) %>%
               group_by(week) %>% 
               summarise(across(temp, ~mean(.x, na.rm = TRUE))) %>% 
               mutate(across(temp, replace_nan))),
  
  tar_target(temp_week_lag, nbdc_temp_week %>% 
               mutate(temp_lag1 = lag(temp),
                      temp_lag2 = lag(temp, 2),
                      temp_lag3 = lag(temp, 3),
                      temp_lag4 = lag(temp, 4),
                      temp_lag5 = lag(temp, 5),
                      temp_lag6 = lag(temp, 6),
                      temp_lag7 = lag(temp, 7),
                      temp_lag8 = lag(temp, 8),
                      temp_lag2_cum = rowMeans(data.frame(temp_lag1, temp_lag2), na.rm = TRUE),
                      temp_lag3_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3), na.rm = TRUE),
                      temp_lag4_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4), na.rm = TRUE),
                      temp_lag5_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5), na.rm = TRUE),
                      temp_lag6_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6), na.rm = TRUE),
                      temp_lag7_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6, temp_lag7), na.rm = TRUE),
                      temp_lag8_cum = rowMeans(data.frame(temp_lag1, temp_lag2, temp_lag3, temp_lag4, temp_lag5, temp_lag6, temp_lag7, temp_lag8), na.rm = TRUE),
                      ) %>% 
               rename(temp_b = temp)),
  
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
  
  tar_target(dd_month_lag, nbdc_dd_month_sum %>% 
               mutate(dd_lag1 = lag(dd),
                      dd_lag2 = lag(dd, 2),
                      dd_lag3 = lag(dd, 3),
                      dd_lag4 = lag(dd, 4)) %>% 
               select(-n)),
  tar_target(dd_week_lag, nbdc_dd_week_sum %>% 
               mutate(dd_lag1 = lag(dd),
                      dd_lag2 = lag(dd, 2),
                      dd_lag3 = lag(dd, 3),
                      dd_lag4 = lag(dd, 4),
                      dd_lag5 = lag(dd, 5),
                      dd_lag6 = lag(dd, 6),
                      dd_lag7 = lag(dd, 7),
                      dd_lag8 = lag(dd, 8),
                      dd_lag2_cum = dd_lag1 + dd_lag2,
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
               left_join(dd_week_lag) %>% 
               left_join(lake_west_week)%>% 
               left_join(trib_load_west_week_lag)),
  tar_target(lake_cb_week_plus, trib_q_cb_week_lag %>% 
               left_join(dd_week_lag) %>% 
               left_join(lake_cb_week) %>% 
               left_join(trib_load_cb_week_lag)),
  
  
  tar_target(lake_month_q_dd, trib_q_month_lag %>% 
               left_join(dd_month_lag) %>% 
               left_join(lake_month)),
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