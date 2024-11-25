library(shiny)
library(tidyverse)
library(rlang)
library(reshape2)
library(plotly)
library(leaflet)

replace_nan <-  function(val){
  return(if_else(is.nan(val), NA, val))
}
get_region <-  Vectorize(function(lat, long, huc = ""){
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
})
lake_core <- read_csv("www/lake_core.csv") %>% 
  mutate(region = get_region(latitude, longitude))
estuary_core <- read_csv("www/estuary_core.csv") %>% 
  mutate(region = "stl")
tributary_core <- read_csv("www/tributary_core.csv", col_types = cols(huc = col_character())) %>% 
  mutate(region = get_region(latitude, longitude, huc))

tributary_load <- tributary_core %>% 
  filter(!is.na(discharge)) %>% 
  mutate(across(c(chl, tss, doc, poc, toc, tn, tdn, ton, don, pon, no3, nh3, tp, tdp, pp, po4, si, cl), 
                ~ discharge * 28.31685 * .)) %>% 
  select(-c(turb, cond, ph, temp, do, do_sat, npr, cnr, cpr, pnpr, pcnr, pcpr))

make_scaled <-  function(df, cols, region = FALSE){
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
  df <- df %>% 
    group_by(pick(all_of(group_cols))) %>%
    mutate(across(all_of(cols), ~ sum(!is.na(.)) >=n, .names = "{.col}2")) %>% 
    ungroup() %>% 
    mutate(across(all_of(cols), ~if_else(get(paste0(deparse(substitute(.)), "2")), ., NA))) %>% 
    group_by(pick(all_of(group_cols))) %>%
    mutate(across(all_of(cols), ~(scale(.) %>% as.vector))) %>% 
    ungroup() %>% 
    select(all_of(id_cols), all_of(cols)) %>% 
    filter(!if_all(-all_of(id_cols), is.na))
  if (region){
    return(df %>% filter(region != ""))
  } else return(df)
}

lake_scaled <-  make_scaled(lake_core, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                 "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                 "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))
estuary_scaled <-  make_scaled(estuary_core, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                               "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                               "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))
tributary_scaled <-  make_scaled(tributary_core, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                 "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                 "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"))
tributary_load_scaled <-  make_scaled(tributary_load, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"))

lake_scaled_region <-  make_scaled(lake_core, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                         "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                         "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"), TRUE)
estuary_scaled_region <-  make_scaled(estuary_core, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                           "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                           "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"), TRUE)
tributary_scaled_region <-  make_scaled(tributary_core, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                              "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                              "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl"), TRUE)
tributary_load_scaled_region <-  make_scaled(tributary_load, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl"), TRUE)

group_time <-  function(df, time = c("year", "month", "week"), reg = FALSE){
  remove_cols <- c()
  if ("latitude" %in% colnames(df) & !reg) remove_cols = c(remove_cols, "latitude", "longitude")
  if (time == "year") {
    df <- df %>% 
      mutate(date = year(date))
  } else if (time == "month") {
    df <- df %>% 
      mutate(date = floor_date(date, "month")) 
  } else {
    df <- df %>% 
      mutate(date = floor_date(date, "week"))
  }
  if (reg){
    df <- df %>% group_by(date, region)
  }else{
    df <- df %>% group_by(date)
  }
  df <- df %>% 
    summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
    mutate(across(where(is.double), replace_nan)) %>% 
    select(-all_of(remove_cols)) %>% 
    ungroup()
}

lake_year <-  group_time(lake_scaled, "year")
lake_month <-  group_time(lake_scaled, "month")
lake_week <-  group_time(lake_scaled, "week")
lake_year_region <-  group_time(lake_scaled_region, "year", TRUE)
lake_month_region <-  group_time(lake_scaled_region, "month", TRUE)
lake_week_region <-  group_time(lake_scaled_region, "week", TRUE)

estuary_year <-  group_time(estuary_scaled, "year")
estuary_month <-  group_time(estuary_scaled, "month")
estuary_week <-  group_time(estuary_scaled, "week")
estuary_year_region <-  group_time(estuary_scaled_region, "year", TRUE)
estuary_month_region <-  group_time(estuary_scaled_region, "month", TRUE)
estuary_week_region <-  group_time(estuary_scaled_region, "week", TRUE)

tributary_year <-  group_time(tributary_scaled, "year")
tributary_month <-  group_time(tributary_scaled, "month")
tributary_week <-  group_time(tributary_scaled, "week")
tributary_year_region <-  group_time(tributary_scaled_region, "year", TRUE)
tributary_month_region <-  group_time(tributary_scaled_region, "month", TRUE)
tributary_week_region <-  group_time(tributary_scaled_region, "week", TRUE)

tributary_load_year <-  group_time(tributary_load_scaled, "year")
tributary_load_month <-  group_time(tributary_load_scaled, "month")
tributary_load_week <-  group_time(tributary_load_scaled, "week")
tributary_load_year_region <-  group_time(tributary_load_scaled_region, "year", TRUE)
tributary_load_month_region <-  group_time(tributary_load_scaled_region, "month", TRUE)
tributary_load_week_region <-  group_time(tributary_load_scaled_region, "week", TRUE)

make_lag <-  function(df, cols, n = 4){
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
}


lake_week_lag_reg <-  make_lag(lake_week_region, c("chl", "chl_field", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                        "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                        "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl")) %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("lake_", .x, recycle0 = TRUE), -c(date, region))

trib_week_lag_reg <-  make_lag(tributary_week_region, c("discharge", "chl", "tss", "turb", "cond", "ph", "temp", "do", "do_sat",
                                                        "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4",
                                                        "npr", "cnr", "cpr", "pnpr", "pcnr", "pcpr", "si", "cl")) %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("trib_", .x, recycle0 = TRUE), -c(date, region))

trib_load_week_lag_reg <-  make_lag(tributary_load_week_region, c("chl", "tss", "doc", "poc", "toc", "tn", "tdn", "ton", "don", "pon", "no3", "nh3", "tp", "tdp", "pp", "po4", "si", "cl")) %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("trib_", .x, "_load", recycle0 = TRUE), -c(date, region))

llo_temp <- read_csv("www/llo_temp.csv")

dd_year <-  llo_temp %>% 
  mutate(date = year(date),
         temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
  group_by(date) %>% 
  mutate(dd = cumsum(temp)) %>% 
  ungroup() %>%
  filter(date != 2010) %>% 
  group_by(date) %>% 
  summarise(dd = max(dd))
dd_month <-  llo_temp %>% 
  mutate(date = floor_date(date, "month"),
         temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
  group_by(date) %>% 
  mutate(dd = cumsum(temp)) %>% 
  ungroup()  %>% 
  group_by(date) %>% 
  summarise(dd = max(dd))
dd_week <-  llo_temp %>% 
  mutate(date = floor_date(date, "week"),
         temp = if_else(is.na(temp) | temp < 10, 0, temp)) %>%
  group_by(date) %>% 
  mutate(dd = cumsum(temp)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  summarise(dd = max(dd))

dd_week_lag <-  make_lag(dd_week, c("dd"), 8) %>% 
  mutate(dd_lag2_cum = dd_lag1 + dd_lag2,
         dd_lag3_cum = dd_lag2_cum + dd_lag3,
         dd_lag4_cum = dd_lag3_cum + dd_lag4,
         dd_lag5_cum = dd_lag4_cum + dd_lag5,
         dd_lag6_cum = dd_lag5_cum + dd_lag6,
         dd_lag7_cum = dd_lag6_cum + dd_lag7,
         dd_lag8_cum = dd_lag7_cum + dd_lag8)

lake_week_prep <- lake_week_region %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("lake_", .x, recycle0 = TRUE), -c(date, region))

tributary_week_prep <- tributary_week_region %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("trib_", .x, recycle0 = TRUE), -c(date, region))

tributary_load_week_prep <- tributary_load_week_region %>% 
  select(-c(latitude, longitude)) %>% 
  rename_with(~ paste0("trib_", .x, "_load", recycle0 = TRUE), -c(date, region))

lake_tributary <- lake_week_prep %>% 
  full_join(tributary_week_prep, by = join_by(date, region)) %>% 
  left_join(dd_week, by = join_by(date)) %>% 
  mutate(dd = if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, dd)) %>% 
  filter(!if_all(-c(date, region, trib_discharge), is.na)) %>% 
  arrange(date)

lake_tributary_load <- lake_week_prep %>% 
  full_join(tributary_load_week_prep, by = join_by(date, region)) %>% 
  left_join(dd_week, by = join_by(date)) %>% 
  mutate(dd = if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, dd)) %>% 
  filter(!if_all(-c(date, region), is.na)) %>% 
  arrange(date)

lake_tributary_lag <- lake_week_prep %>% 
  full_join(trib_week_lag_reg, by = join_by(date, region)) %>% 
  filter(!if_all(-c(date, region, trib_discharge_lag1, trib_discharge_lag2, trib_discharge_lag3, trib_discharge_lag4), is.na)) %>% 
  arrange(date)

lake_tributary_load_lag <- lake_week_prep %>% 
  full_join(trib_load_week_lag_reg, by = join_by(date, region)) %>% 
  filter(!if_all(-c(date, region), is.na)) %>% 
  arrange(date)

lake_lag <- lake_week_prep %>% 
  full_join(dd_week, by = join_by(date)) %>% 
  full_join(lake_week_lag_reg, by = join_by(date, region)) %>% 
  full_join(dd_week_lag, by = join_by(date)) %>% 
  mutate(dd = if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, dd),
         across(contains("dd"), ~if_else(region %in% c("ras", "fro", "red", "cb", "bad"), NA, .))) %>% 
  arrange(date)

datasets <- c("lake_core", "estuary_core", "tributary_core", "tributary_load", 
              "lake_scaled", "estuary_scaled", "tributary_scaled", "tributary_load_scaled",
              "lake_scaled_region", "estuary_scaled_region", "tributary_scaled_region", "tributary_load_scaled_region",
              "lake_year", "estuary_year", "tributary_year", "tributary_load_year",
              "lake_year_region", "estuary_year_region", "tributary_year_region", "tributary_load_year_region",
              "lake_month", "estuary_month", "tributary_month", "tributary_load_month",
              "lake_month_region", "estuary_month_region", "tributary_month_region", "tributary_load_month_region",
              "lake_week", "estuary_week", "tributary_week", "tributary_load_week",
              "lake_week_region", "estuary_week_region", "tributary_week_region", "tributary_load_week_region",
              "lake_tributary", "lake_tributary_load", "lake_tributary_lag", "lake_tributary_load_lag", "lake_lag")

map_datasets <- c("lake_core", "estuary_core", "tributary_core", "tributary_load", 
                  "lake_scaled", "estuary_scaled", "tributary_scaled", "tributary_load_scaled",
                  "lake_scaled_region", "estuary_scaled_region", "tributary_scaled_region", "tributary_load_scaled_region",
                  "lake_year_region", "estuary_year_region", "tributary_year_region", "tributary_load_year_region",
                  "lake_month_region", "estuary_month_region", "tributary_month_region", "tributary_load_month_region",
                  "lake_week_region", "estuary_week_region", "tributary_week_region", "tributary_load_week_region")


site_rename <- Vectorize(function(site){
  if (site %in% c("Amnicon 2a", "site 2 - 10052502")) return("Amnicon 2a - site 2")
  else if (site %in% c("Brule 3a", "site 6 - 103")) return("Brule 3a - site 6")
  else if (site %in% c("Flag 4a", "site 9 - 10052510")) return("Flag 4a - site 9")
  else if (site %in% c("Bark Bay", "site 13 - 10052512")) return("Bark- site 13")
  else if (site %in% c("Siskiwit Bay", "site 14 - 10052513", "Siskiwit Bay OS", "Siskiwit Bay NS", "Siskiwit Outlet", "Siskiwit Outlet Transect", "Siskiwit Outlet Pier End", "Siskiwit Beach")) return("Siskiwit - site 14")
  else if (site %in% c("Mawikwe Bay", "site 15 - 10054863", "Mawikwe Bay OS", "Mawikwe Bay NS", "Mawikwe Beach")) return("Mawikwe - site 15")
  else return(site)
})

lake_clus_prep <- lake_core %>% 
  filter(source != "NCCA" & chl < 40) %>% 
  select(site, chl, no3, nh3, tp, po4) %>%
  mutate(site = site_rename(site)) %>% 
  group_by(site) %>% 
  mutate(n = n()) %>%
  filter(n > 1) %>% 
  select(-n) %>%
  summarise(across(everything(), ~median(., na.rm = T))) %>% 
  column_to_rownames("site") %>% 
  na.omit() %>% 
  scale()

lake_pca <- prcomp(lake_clus_prep)

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)
stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#using modifying functions from factoextra package
.get_withinSS <- function(d, cluster){
  d <- as.dist(d)
  cn <- max(cluster)
  clusterf <- as.factor(cluster)
  clusterl <- levels(clusterf)
  cnn <- length(clusterl)
  
  cwn <- cn
  # Compute total within sum of square
  dmat <- as.matrix(d)
  within.cluster.ss <- 0
  for (i in 1:cn) {
    cluster.size <- sum(cluster == i)
    di <- as.dist(dmat[cluster == i, cluster == i])
    within.cluster.ss <- within.cluster.ss + sum(di^2)/cluster.size
  }
  within.cluster.ss
}

opt_clus <- function(x, kmax){
  if (is.data.frame(x)) x <- as.matrix(x)
  diss <- dist(x)
  
  v <- rep(0, kmax)
  
  for(i in 1:kmax){
    clust <- kmeans(x, i, nstart = 100)
    v[i] <- .get_withinSS(diss, clust$cluster)
  }
  
  df <- data.frame(clusters = as.factor(1:kmax), y = v, stringsAsFactors = TRUE)
  
  ggplot(data = df, aes(x = clusters, y = y, group = 1)) +
    geom_point() + 
    geom_line() +
    xlab("Number of clusters k") +
    ylab("Total Within Sum of Square") +
    ggtitle("Optimal number of clusters") +
    theme_classic()
  
}

cor_pmat <- function(x, corr) {
  
  # initializing values
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  # creating the p-value matrix
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (is.na(corr[i,j]) | abs(round(corr[i,j], 10)) == 1) {
        p.mat[i, j] <- p.mat[j, i] <- 1
      } else {
        tmp <- cor.test(mat[, i], mat[, j])
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
  }
  
  # name rows and columns of the p-value matrix
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  
  # return the final matrix
  p.mat
}
ggcorrplot <- function(df) {
  cormat = cor(df, use = "pairwise.complete.obs", method = "spearman")
  pmat = cor_pmat(df, cormat)
  cormat[is.na(cormat)] <- 0
  cormat[upper.tri(cormat)] <- NA
  diag(cormat) <- NA
  pmat[upper.tri(pmat)] <- NA
  diag(pmat) <- NA
  cormat <- melt(cormat)
  colnames(cormat) <- c("Var1", "Var2", "r")
  cormat$pvalue <- rep(NA, nrow(cormat))
  cormat$signif <- rep(NA, nrow(cormat))
  pmat <- melt(pmat)
  cormat$coef <- cormat$r
  cormat$pvalue <- pmat$value
  cormat$signif <- as.numeric(pmat$value <= 0.05)
  pmat <- subset(pmat, pmat$value > 0.05)
  cormat$r <- round(cormat$r * cormat$signif, 3)
  
  
  p <- ggplot(data = cormat, mapping = aes(x = Var1, y = Var2, fill = r)) + 
    geom_tile(color = "grey") + 
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Corr", na.value = "white") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12, hjust = 1), axis.text.y = element_text(size = 12)) +
    coord_fixed() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  
  p
  
}

ui <- fluidPage(

    titlePanel("LS HABs Data Viewer"),

    sidebarLayout(
      sidebarPanel(width = 2, 
                   selectInput("corrdataset1", "Corrplot 1 Dataset:", datasets, selected = "lake_core"),
                   selectInput("corrdataset2", "Corrplot 2 Dataset:", datasets, selected = "lake_scaled")
      ),
      mainPanel(width = 10,
                fluidRow(
                  column(6, plotlyOutput("corrplot1", height = "600px")),
                  column(6, plotlyOutput("corrplot2", height = "600px"))
                )
                
      )
    ),
    
    sidebarLayout(
        sidebarPanel(width = 2,
                     tags$h4("Plot 1:"),
            selectInput("plotdataset1", "Dataset:", datasets, selected = "lake_core"),
            uiOutput("plot1choices"),
            tags$h4("Plot 2:"),
            selectInput("plotdataset2", "Dataset:", datasets, selected = "lake_scaled"),
            uiOutput("plot2choices")
        ),
        mainPanel(width = 10,
          fluidRow(
            column(6, plotlyOutput("plot1", height = "500px"), textOutput("plot1text")),
            column(6, plotlyOutput("plot2", height = "500px"), textOutput("plot2text"))
          )
        )
    ),
    
    sidebarLayout(
      sidebarPanel(width = 2, 
                   tags$h4("Map 1:"),
                   selectInput("mapdataset1", "Dataset:", map_datasets, selected = "lake_core"),
                   uiOutput("map1choices1"),
                   uiOutput("map1choices2"),
                   tags$h4("Map 2:"),
                   selectInput("mapdataset2", "Dataset:", map_datasets, selected = "lake_scaled"),
                   uiOutput("map2choices1"),
                   uiOutput("map2choices2")
      ),
      mainPanel(width = 10,
                fluidRow(
                  column(6, leafletOutput("map1", height = "600px")),
                  column(6, leafletOutput("map2", height = "600px"))
                )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(width = 2, 
                   tags$h4("PCA Site Clusters"),
                   numericInput("clusk", "# of Clusters:", 8, 2, 20, 1),
                   selectInput("clusplotchoice", "Plot Type:", c("Optimal K", "Principal Components", "Cluster Comparison")),
                   selectInput("clusvar", "Variable for Cluster Comparison", c("chl", "no3", "nh3", "tp", "po4"))
      ),
      mainPanel(width = 10,
                fluidRow(
                  column(6, leafletOutput("clusmap", height = "600px")),
                  column(6, plotlyOutput("clusplot", height = "600px"))
                )
      )
    )
    
)


server <- function(input, output) {

  plot1_data <- reactive({
    get(input$plotdataset1)
  })
  
  output$plot1choices <- renderUI({
    tagList(
      selectInput("plot1x", "X:", colnames(plot1_data())),
      selectInput("plot1y", "Y:", colnames(plot1_data()), selected = "chl")
    )
  })
  
  plot1data <- reactive({
    filter(plot1_data(), !is.na(!!sym(input$plot1x)) & !is.na(!!sym(input$plot1y))) %>% 
      mutate(across(where(is.character), ~as_factor(.)))
  })
  plot1model <- reactive({
    plottingformula <- as.formula(str_c(input$plot1y, "~", input$plot1x))
    lm(plottingformula, data = plot1data())
  })
  
  output$plot1 <- renderPlotly({
    plotdata <- plot1data()
    if ("site" %in% colnames(plotdata)) {
      plotdata <- plotdata %>% 
        mutate(id = str_c(date, site, region, sep = " "))
    } else if ("region" %in% colnames(plotdata)){
      plotdata <- plotdata %>% 
        mutate(id = str_c(date, region, sep = " "))
    } else {
      plotdata <- plotdata %>% 
        mutate(id = date)
    }
    ggplotly(
      ggplot(data = plotdata) +
        geom_point(aes(!!sym(input$plot1x), !!sym(input$plot1y), text = id)) +
        geom_abline(slope = plot1model()$coefficients[2], intercept = plot1model()$coefficients[1], linetype = "dashed")
    )
  })
  
  output$plot1text <- renderText({
    modelsum <- summary(plot1model())
    str_c("Slope: ", round(plot1model()$coefficients[2],3), " R2: ", round(modelsum$r.squared, 3), "  P: ", round(coef(modelsum)[2,4], 3))
  })
  
  plot2_data <- reactive({
    get(input$plotdataset2)
  })
  
  output$plot2choices <- renderUI({
    tagList(
      selectInput("plot2x", "X:", colnames(plot2_data())),
      selectInput("plot2y", "Y:", colnames(plot2_data()), selected = "chl")
    )
  })
  
  plot2data <- reactive({
    filter(plot2_data(), !is.na(!!sym(input$plot2x)) & !is.na(!!sym(input$plot2y))) %>% 
      mutate(across(where(is.character), ~as_factor(.)))
  })
  plot2model <- reactive({
    plot2formula <- as.formula(str_c(input$plot2y, "~", input$plot2x))
    lm(plot2formula, data = plot2data())
  })

  output$plot2 <- renderPlotly({
    plotdata <- plot2data()
    if ("site" %in% colnames(plotdata)) {
      plotdata <- plotdata %>% 
        mutate(id = str_c(date, site, region, sep = " "))
    } else if ("region" %in% colnames(plotdata)){
      plotdata <- plotdata %>% 
        mutate(id = str_c(date, region, sep = " "))
    } else {
      plotdata <- plotdata %>% 
        mutate(id = date)
    }
    ggplotly(
      ggplot(data = plotdata) +
        geom_point(aes(!!sym(input$plot2x), !!sym(input$plot2y), text = id)) +
        geom_abline(slope = plot2model()$coefficients[2], intercept = plot2model()$coefficients[1], linetype = "dashed")
    )
  })

  output$plot2text <- renderText({
    modelsum <- summary(plot2model())
    str_c("Slope: ", round(plot2model()$coefficients[2],3), " R2: ", round(modelsum$r.squared, 3), "  P: ", round(coef(modelsum)[2,4], 3))
  })
  
  map1_data <- reactive({
    get(input$mapdataset1)
  })
  
  map2_data <- reactive({
    get(input$mapdataset2)
  })
  
  output$map1choices1 <- renderUI({
    tagList(
      selectInput("map1var", "Variable:", colnames(map1_data()), selected = "chl")
    )
  })
  output$map1choices2 <- renderUI({
    tagList(
      selectInput("map1date", "Date:", map1dates())
    )
  })
  
  output$map2choices1 <- renderUI({
    tagList(
      selectInput("map2var", "Variable:", colnames(map2_data()), selected = "chl")
    )
  })
  output$map2choices2 <- renderUI({
    tagList(
      selectInput("map2date", "Date:", map2dates())
    )
  })
  
  map1data <- reactive({
    if (isTruthy(input$map1var)) {
      filter(map1_data(), !is.na(get(input$map1var)))
    }
  })
  map1dates <- reactive({
    if (isTruthy(map1data())) {
      c("all", as.character(unique(map1data()$date)))
    } else {
      c("all", as.character(unique(map1_data()$date)))
    }
  })
  map2data <- reactive({
    filter(map2_data(), !is.na(get(input$map2var)))
  })
  map2dates <- reactive({
    if (isTruthy(map2data())) {
      c("all", as.character(unique(map2data()$date)))
    } else {
      c("all", as.character(unique(map2_data()$date)))
    }
  })
  
  output$map1 <- renderLeaflet({
    if (!("site" %in% colnames(map1data()))) {
      mapdata <- map1data() %>% 
        rename(site = region)
    } else {
      mapdata <- map1data()
    }
    if (input$map1date == "all") {
      mapdata <- mapdata %>% 
        group_by(site) %>% 
        summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
        mutate(across(where(is.double), replace_nan))
    } else {
      mapdata <- filter(mapdata, date == ymd(input$map1date))
    }
    if (is.numeric(mapdata[input$map1var][[1]])) {
      pal <- colorNumeric(
        palette=colorRamp(c("white", "black"), interpolate = "spline"),
        domain=map1data()[input$map1var]
      )
      if (nrow(mapdata > 0)){
        leaflet() %>%
          addProviderTiles('Esri.WorldStreetMap') %>%
          addCircleMarkers(
            data = mapdata,
            lat = ~latitude,
            lng = ~longitude,
            label = ~str_c(site, get(input$map1var), sep = " "),
            fillColor = ~pal(get(input$map1var)),
            color="black",
            weight =.5,
            opacity=1,
            fillOpacity=1,
          ) %>% 
          addLegend(
            position = "bottomright",
            pal = pal,
            data = map1data(),
            values = ~get(input$map1var),
            bins = 3,
            title = input$map1var
          )
      }
    }
  })
  
  output$map2 <- renderLeaflet({
    if (!("site" %in% colnames(map2data()))) {
      mapdata <- map2data() %>% 
        rename(site = region)
    } else {
      mapdata <- map2data()
    }
    if (input$map2date == "all") {
      mapdata <- mapdata %>% 
        group_by(site) %>% 
        summarise(across(where(is.double), ~mean(.x, na.rm = TRUE))) %>% 
        mutate(across(where(is.double), replace_nan))
    } else {
      mapdata <- filter(mapdata, date == ymd(input$map2date))
    }
    if (is.numeric(mapdata[input$map2var][[1]])) {
      pal <- colorNumeric(
        palette=colorRamp(c("white", "black"), interpolate = "spline"),
        domain=map2data()[input$map2var]
      )
      if (nrow(mapdata > 0)){
        leaflet() %>%
          addProviderTiles('Esri.WorldStreetMap') %>%
          addCircleMarkers(
            data = mapdata,
            lat = ~latitude,
            lng = ~longitude,
            label = ~str_c(site, get(input$map2var), sep = " "),
            fillColor = ~pal(get(input$map2var)),
            color="black",
            weight =.5,
            opacity=1,
            fillOpacity=1,
          ) %>% 
          addLegend(
            position = "bottomright",
            pal = pal,
            data = map2data(),
            values = ~get(input$map2var),
            bins = 3,
            title = input$map2var
          )
      }
    }
  })
  
  corrplot1_data <- reactive({
    get(input$corrdataset1)
  })
  
  output$corrplot1 <- renderPlotly({
    data <- select(corrplot1_data(), where(is.double) & !where(is.Date)) 
    if ("latitude" %in% colnames(data)) {
      data <- data %>% 
      select(-c(latitude, longitude))
    }
    ggplotly(ggcorrplot(data))
  })
  
  corrplot2_data <- reactive({
    get(input$corrdataset2)
  })
  
  lake_kmean <- reactive({
    kmeans(lake_clus_prep, centers = input$clusk, nstart = 100)
  })
  
  lake_clus <- reactive({
    lake_kmean_clus <- data.frame(site = names(lake_kmean()$cluster), kclus = as.numeric(lake_kmean()$cluster))
    
    data.frame(lake_pca$x) %>% 
      rownames_to_column(var = "site") %>% 
      left_join(lake_kmean_clus) %>% 
      mutate(across(c(kclus), ~as_factor(.)))
  })
  
  output$corrplot2 <- renderPlotly({
    data <- select(corrplot2_data(), where(is.double) & !where(is.Date)) 
    if ("latitude" %in% colnames(data)) {
      data <- data %>% 
        select(-c(latitude, longitude))
    }
    ggplotly(ggcorrplot(data))
  })
  
  output$clusmap <- renderLeaflet({
    lake_clus_map <- select(lake_core, site, latitude, longitude) %>% 
      mutate(site = site_rename(site)) %>% 
      group_by(site) %>% 
      summarise(latitude = mean(latitude), longitude = mean(longitude))
    lake_clus_map <- lake_clus() %>% 
      left_join(lake_clus_map)
    
    
    pal <- colorFactor(
      palette="viridis",
      domain=lake_clus_map$kclus
    )
    
    leaflet() %>%
      addProviderTiles('Esri.WorldStreetMap') %>%
      addCircleMarkers(
        data = lake_clus_map,
        lat = ~latitude,
        lng = ~longitude,
        label = ~str_c(site, ", cluster #", kclus, sep = ""),
        fillColor = ~pal(kclus),
        color="black",
        weight =.5,
        opacity=1,
        fillOpacity=1,
      )
    
  })
  
  output$clusplot <- renderPlotly({
    if (input$clusplotchoice == "Optimal K") {
      ggplotly(opt_clus(lake_clus_prep, input$clusk + 5))
    } else if (input$clusplotchoice == "Principal Components") {
      ggplotly(
        ggplot(data = lake_clus(), aes(x = PC1, y = PC2, color = kclus, text = site)) +
          geom_point() +
          stat_chull(fill = NA) +
          geom_label_repel(aes(label = site)) + 
          scale_color_viridis_d()
      )
    } else {
      lake_full_clus <- lake_core %>% 
        left_join(lake_clus()) %>% 
        filter(!is.na(kclus))
      ggplot(data = lake_full_clus, aes(x = kclus, y = !!sym(input$clusvar), color = kclus)) +
        geom_boxplot() + 
        scale_color_viridis_d()
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
