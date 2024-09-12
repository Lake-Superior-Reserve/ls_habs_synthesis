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
               mutate(site = as.factor(site),
                      month = as.factor(month))),
  tar_target(dnr_rf_prep_chem, select(dnr_rf_prep, -c(temp, do, cond, ph, turb)) %>% 
               na.omit()),
  tar_target(dnr_rf_prep_full, na.omit(dnr_rf_prep)),
  
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
               summarise(chl = mean(chl, na.rm = T),
                         tss = mean(tss, na.rm = T),
                         no3 = mean(no3, na.rm = T),
                         tn = mean(tn, na.rm = T),
                         po4 = mean(po4, na.rm = T),
                         tp = mean(tp, na.rm = T),
                         temp = mean(temp, na.rm = T),
                         do = mean(do, na.rm = T),
                         cond = mean(cond, na.rm = T),
                         ph = mean(ph, na.rm = T),
                         turb = mean(turb, na.rm = T)) %>% 
               column_to_rownames("site") %>% 
               scale()),
  
  # treating each site/year combo differently
  tar_target(dnr_site_date_clus_prep, dnr %>% 
               st_drop_geometry() %>%  
               select(-c(station, nh3, doc, do_sat, chl_f)) %>% 
               filter(!is.na(site)) %>% 
               mutate(dateround = year(date),
                      site_date = str_c(dateround, site, sep = "_"),
               ) %>% 
               group_by(site_date) %>% 
               summarise(chl = mean(chl, na.rm = T),
                         tss = mean(tss, na.rm = T),
                         no3 = mean(no3, na.rm = T),
                         tn = mean(tn, na.rm = T),
                         po4 = mean(po4, na.rm = T),
                         tp = mean(tp, na.rm = T),
                         temp = mean(temp, na.rm = T),
                         do = mean(do, na.rm = T),
                         cond = mean(cond, na.rm = T),
                         ph = mean(ph, na.rm = T),
                         turb = mean(turb, na.rm = T)) %>% 
               column_to_rownames("site_date") %>% 
               na.omit() %>% 
               scale()),
  
  tar_target(dnr_site_clus_pca, prcomp(dnr_site_clus_prep)),
  # biplot(tar_read(dnr_site_clus_pca))
  
  tar_target(dnr_site_clus_kmean, kmeans(dnr_site_clus_prep, centers = 2, nstart = 25)),
  # factoextra::fviz_cluster(tar_read(dnr_site_clus_kmean), data = tar_read(dnr_site_clus_prep))
  
  tar_target(dnr_site_date_clus_pca, prcomp(dnr_site_date_clus_prep)),
  # biplot(tar_read(dnr_site_date_clus_pca))
  
  tar_target(dnr_site_date_clus_kmean, kmeans(dnr_site_date_clus_prep, centers = 3, nstart = 25)),
  # factoextra::fviz_cluster(tar_read(dnr_site_date_clus_kmean), data = tar_read(dnr_site_date_clus_prep))
  # or plotly::ggplotly(factoextra::fviz_cluster(tar_read(dnr_site_date_clus_kmean), data = tar_read(dnr_site_date_clus_prep)))
  
  
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