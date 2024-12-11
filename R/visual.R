viz_targets <- list(
  
  # variable comparison
  
  # maps

  #ls shapes
  #Lake Superior Shape
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(ls_shp, read_sf(ls_shp_file)),
  
  tar_target(ls_crop, st_crop(ls_shp, st_bbox(c(xmin = -92.5, ymin = 46.5, xmax = -90, ymax = 47.5))))
  
)