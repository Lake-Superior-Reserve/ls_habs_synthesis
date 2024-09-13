s2_targets <- list(
  
  # variable comparison
  
  # corrplot
  tar_target(dnr_corr_fn, function(dnr) {
    dnr_corr <- select(dnr, chl, tss, nh3, no3, tn, po4, tp, doc, temp, do, cond, ph, turb) %>% 
      st_drop_geometry()
    dnr_cor <- rcorr(as.matrix(dnr_corr), type = 'spearman')
    dnr_cor$P[is.na(dnr_cor$P)] <- 1
    dnr_cor
  }),

  tar_target(dnr_corr, dnr_corr_fn(dnr)),
  # corrplot::corrplot(tar_read(dnr_corr)$r, type = "lower", tl.cex = .5, p.mat = tar_read(dnr_corr)$P, sig.level = 0.01, insig = 'blank', outline = T)

  # random forest
  tar_target(dnr_rf_prep, dnr %>% 
               st_drop_geometry() %>% 
               mutate(month = month(date)) %>% 
               select(-c(date, station, nh3, doc, do_sat, chl_f)) %>% 
               filter(!is.na(site)) %>% 
               mutate(month = as.factor(month),
                      site = as_factor(site))),
  tar_target(dnr_rf_prep_chem, select(dnr_rf_prep, -c(month, site, temp, do, cond, ph, turb)) %>%
               na.omit()),
  tar_target(dnr_rf_prep_full, filter(dnr_rf_prep, ph > 6 & cond > 95) %>% na.omit()),
  
  tar_target(dnr_rf_chem, randomForest(chl ~ ., data = dnr_rf_prep_chem, importance = TRUE)),
  tar_target(dnr_rf_full, randomForest(chl ~ ., data = dnr_rf_prep_full, importance = TRUE)),
  # randomForestExplainer::explain_forest(tar_read(dnr_rf_full))
  
  # clustering
  
  # based on sites across years
  tar_target(dnr_site_clus_prep, dnr %>% 
               st_drop_geometry() %>%  
               select(-c(date, station, nh3, doc, do_sat, chl_f)) %>% 
               filter(!is.na(site)) %>% 
               mutate(site = as.factor(site)) %>% 
               group_by(site) %>% 
               summarise(chl = median(chl, na.rm = T),
                         tss = median(tss, na.rm = T),
                         no3 = median(no3, na.rm = T),
                         tn = median(tn, na.rm = T),
                         po4 = median(po4, na.rm = T),
                         tp = median(tp, na.rm = T),
                         temp = median(temp, na.rm = T),
                         do = median(do, na.rm = T),
                         cond = median(cond, na.rm = T),
                         ph = median(ph, na.rm = T),
                         turb = median(turb, na.rm = T)) %>% 
               column_to_rownames("site") %>% 
               scale()),
  
  tar_target(dnr_site_clus_pca, prcomp(dnr_site_clus_prep)),
  # biplot(tar_read(dnr_site_clus_pca))
  
  tar_target(dnr_site_clus_kmean, kmeans(dnr_site_clus_prep, centers = 2, nstart = 25)),
  # factoextra::fviz_cluster(tar_read(dnr_site_clus_kmean), data = tar_read(dnr_site_clus_prep))
  
  # maps

  #ls shapes
  tar_target(ls_crop, st_crop(ls_shp, st_bbox(c(xmin = -92.5, ymin = 46.5, xmax = -90, ymax = 47.5)))),
  tar_target(ls_crop_dnr, st_crop(ls_shp, st_bbox(c(xmin = -92, ymin = 46.6, xmax = -91, ymax = 46.9)))),
  
  #dnr maps
  tar_target(dnr_map_chl, ggplot() +
               theme_bw() +
               geom_sf(data = ls_crop_dnr) +
               geom_sf(data = dnr, mapping = aes(color = chl), size = 7) +
               scale_color_gradient(low = "palegreen", high = "darkgreen") +
               transition_states(date) +
               labs(title = "Date: {closest_state}"))
  # gganimate::animate(tar_read(dnr_map_chl), nframes = length(unique(tar_read(dnr)$date)), fps = 1)
  
  
  
)