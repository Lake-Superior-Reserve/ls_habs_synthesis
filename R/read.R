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
  
  #Lake Superior Shape
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(ls_shp, read_sf(ls_shp_file)),
   
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
  tar_target(lake_full, dnr %>%
               bind_rows(filter(umd, type == "Lake" & depth <= 2)) %>% 
               bind_rows(cbnut_clean) %>% 
               bind_rows(filter(wqp_wide, type == "Great Lake")) %>%
               bind_rows(filter(ncca, type == "Great Lake")) %>%
               bind_rows(nps) %>% 
               select(date, site, source, chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl) %>% 
               arrange(date)),
  
  tar_target(trib_full, filter(umd, type == "Watershed") %>% 
               bind_rows(cbtrib) %>% 
               bind_rows(filter(nwis, !if_all(c(cond, do, ph, turb, tss, tkn, no3, tp), is.na))) %>% 
               bind_rows(filter(wqp_wide, type == "River/Stream")) %>%
               select(date, site, source, discharge, chl, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl) %>% 
               arrange(date)),
  
  tar_target(trib_q, nwis %>% 
               bind_rows(cbq_clean) %>% 
               arrange(date)),
  
  tar_target(est_full, lsnerr %>%
               bind_rows(filter(wqp_wide, type == "Estuary")) %>%
               bind_rows(filter(ncca, type == "Estuary")) %>%
               filter(source != "NARS_WQX") %>% # dropping duplicates
               select(date, site, source, discharge, chl, chl_field, tss, turb, cond, ph, temp, do, do_sat, # reorder, drop station and type (all lake)
                      doc, toc, tn, tdn, ton, don, no3, nh3, tp, tdp, po4, si, cl) %>% 
               arrange(date))
  
)

