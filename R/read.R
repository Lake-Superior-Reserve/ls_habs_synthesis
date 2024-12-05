source("R/read_targets/dnr.R")
source("R/read_targets/umd.R")
source("R/read_targets/nps.R")
source("R/read_targets/ncbc.R")
source("R/read_targets/lsnerr.R")
source("R/read_targets/ncca.R")
source("R/read_targets/wqp.R")
source("R/read_targets/nwis.R")
source("R/read_targets/nbdc.R")


read_targets <- list(
  
  #helper functions
  tar_target(replace_nan, function(val){
    return(if_else(is.nan(val), NA, val))
  }),
  
  tar_target(write_file, function(df, name){
    filepath <- str_c("out/", name, ".csv")
    df <- df %>% 
      mutate(date = as.character(date))
    write_csv(df, filepath, na = "")
    return(filepath)
  }),
  
  #Lake Superior Shape
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(ls_shp, read_sf(ls_shp_file)),
  
  #ls loads
  tar_target(ls_tpload_file, "raw_data/SuperiorLoads.xlsx", format = "file"),
  tar_target(ls_tpload, read_xlsx(ls_tpload_file) %>% 
               mutate(Date = force_tz(Date, tzone = "America/Chicago")) %>% 
               filter(Major_Rivers250 %in% c("Amnicon River", "Bad", "Bois Brule River", "Nemadji River", "StLouis")) %>%  #montreal river at border of mi/wi?)
                select(river = Major_Rivers250, date = Date, tp_load = Load)),
               
               
  #targets organized by data source
  dnr_targets,
  umd_targets,
  nps_targets,
  ncbc_targets,
  lsnerr_targets,
  ncca_targets,
  wqp_targets,
  nwis_targets,
  nbdc_targets,
  
  
  # making full files by type
  tar_target(lake_full, filter(dnr, type == "Lake" & year(date) < 2024) %>%
               bind_rows(filter(umd, type == "Lake" & depth <= 2)) %>% 
               bind_rows(filter(nps, type == "Lake")) %>%
               bind_rows(cbnut_clean) %>%
               bind_rows(filter(ncca, type == "Great Lake")) %>%
               bind_rows(filter(wqp_wide, type == "Great Lake")) %>%
               mutate(npr = tn/tp,
                      cnr = toc/tn,
                      cpr = toc/tp, 
                      pnpr = pon/pp,
                      pcnr = poc/pon,
                      pcpr = poc/pp) %>% 
               group_by(date, latitude, longitude) %>% 
               summarise(across(c(site, source), ~first(.x)),
                         across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                                  doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                                ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             replace_nan))),
  
  tar_target(trib_full, filter(umd, type == "Watershed") %>% 
               bind_rows(cbtrib) %>% 
               bind_rows(filter(nps, type == "Tributary")) %>%
               bind_rows(filter(nwis, !if_all(c(cond, do, ph, turb, tss, tkn, no3, tp), is.na))) %>% 
               bind_rows(filter(wqp_wide, type == "River/Stream")) %>%
               mutate(npr = tn/tp,
                      cnr = toc/tn,
                      cpr = toc/tp, 
                      pnpr = pon/pp,
                      pcnr = poc/pon,
                      pcpr = poc/pp) %>% 
               group_by(date, latitude, longitude) %>% 
               summarise(across(c(site, source, huc), ~first(.x)),
                         across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                                  doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                                ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(c(discharge, chl, tss, turb, cond, ph, temp, do, do_sat, 
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             replace_nan))),
  
  tar_target(trib_q, nwis %>% 
               bind_rows(cbq_clean) %>% 
               select(date, site, latitude, longitude, huc, source, discharge) %>% 
               arrange(date)),
  
  tar_target(est_full, lsnerr %>%
               bind_rows(filter(dnr, type == "Estuary")) %>% 
               bind_rows(filter(umd, type == "Estuary")) %>%
               bind_rows(filter(ncca, type == "Estuary")) %>%
               bind_rows(filter(wqp_wide, type == "Estuary")) %>%
               mutate(npr = tn/tp,
                      cnr = toc/tn,
                      cpr = toc/tp, 
                      pnpr = pon/pp,
                      pcnr = poc/pon,
                      pcpr = poc/pp) %>% 
               group_by(date, latitude, longitude) %>% 
               summarise(across(c(site, source), ~first(.x)),
                         across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                                  doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                                ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(c(chl, chl_field, tss, turb, cond, ph, temp, do, do_sat,
                               doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, npr, cnr, cpr, pnpr, pcnr, pcpr, si, cl),
                             replace_nan))),
  
  tar_target(lake_file, write_file(lake_full, "lake_core"), format = "file"),
  tar_target(est_file, write_file(est_full, "estuary_core"), format = "file"),
  tar_target(trib_file, write_file(trib_full, "tributary_core"), format = "file"),
  tar_target(trib_q_file, write_file(trib_q, "trib_q_core"), format = "file")
  
)

