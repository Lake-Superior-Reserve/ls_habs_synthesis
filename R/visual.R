s2_targets <- list(
  
  #corrplot functions
  
  tar_target(dnr_corr_fn, function(dnr) {
    dnr_corr <- select(dnr, chl, tss, nh3, no3, tn, po4, tp, doc, temp, do, cond, ph, turb) %>% 
      st_drop_geometry()
    dnr_cor <- rcorr(as.matrix(dnr_corr), type = 'spearman')
    dnr_cor$P[is.na(dnr_cor$P)] <- 1
    dnr_cor
  }),

  tar_target(dnr_corr, dnr_corr_fn(dnr)),
  # corrplot::corrplot(tar_read(dnr_corr)$r, type = "lower", tl.cex = .5, p.mat = tar_read(dnr_corr)$P, sig.level = 0.01, insig = 'blank', outline = T)

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