analysis_targets <- list(
  #' Targets for Lake Superior Shapefile
  #'
  #' This uses the WI DNR 24k hydro polygon for Lake Superior.
  #' `ls_shp_file` is the file location
  #' `ls_shp` is the main target and has the full lake
  #' `ls_crop` is a polygon cropped to only the area of interest for this dataset
  #'
  #' @returns sf object of Lake Superior
  tar_target(ls_shp_file, "ref/ls_shp/ls.shp", format = "file"),
  tar_target(ls_shp, read_sf(ls_shp_file)),
  tar_target(
    ls_crop,
    st_crop(
      ls_shp,
      st_bbox(c(xmin = -92.5, ymin = 46.5, xmax = -90.3, ymax = 47.2))
    )
  ),

  #' SF objects of core data
  #'
  #' removes `latitude` and `longitude` columns and replace with `geometry`
  #'
  #' @returns sf object of core data sets
  tar_target(make_sf, function(df) {
    df %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp))
  }),
  tar_target(lake_sf, make_sf(lake_full)),
  tar_target(trib_sf, make_sf(trib_full)),
  tar_target(est_sf, make_sf(est_full)),
  tar_target(trib_q_sf, make_sf(trib_q)),

  #' Add stoichiometric ratios to full files (currently unused)
  #'
  #' Calculates 6 stoichiometric ratios and adds to passed in data frame. TN:TP, TOC:TN, TOC:TP, PON:PP, POC:PON, POC:PP.
  #'
  #' @param df Data frame with columns tn, tp, toc, pon, pp, poc
  #'
  #' @returns Data frame with 6 stoichiometric ratio columns added
  tar_target(add_ratios, function(df) {
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

  #' Daily TP Loads
  #'
  #' Daily estimates of TP load for major tributaries to Western Lake Superior. 2010-2020.
  #'
  #' @returns data frame of daily TP loads
  tar_target(
    ls_tpload_file,
    "raw_data/other/SuperiorLoads.xlsx",
    format = "file"
  ),
  tar_target(
    ls_tpload,
    read_xlsx(ls_tpload_file) %>%
      mutate(Date = force_tz(Date, tzone = "America/Chicago")) %>%
      filter(
        Major_Rivers250 %in%
          c(
            "Amnicon River",
            "Bad",
            "Bois Brule River",
            "Nemadji River",
            "StLouis"
          )
      ) %>% #montreal river at border of mi/wi?)
      select(river = Major_Rivers250, date = Date, tp_load = Load)
  ),

  #' Reported Blooms
  #'
  #' Reported blooms in Lake Superior/SLRE 2012-2023, including location, general size, and dominant cyano species.
  #'
  #' @returns data frame of reported blooms
  tar_target(
    ls_bloom_file,
    "raw_data/other/PublicBloomData.csv",
    format = "file"
  ),
  tar_target(ls_bloom, read_csv(ls_bloom_file))
)
