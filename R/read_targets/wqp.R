

wqp_targets <- list(
  
  tar_target(drop_na_cols, function(df) {
    na_cols <- c()
    for (n in 1:length(df)) {
      cur_name <- colnames(df)[n]
      if (all(is.na(df[n]))) na_cols <- append(na_cols, cur_name)
    } 
    print("Dropping:")
    print(na_cols)
    return(select(df, -all_of(na_cols)))
  }),
  
  tar_target(wqp_invalid_units, c("CONDUCTIVITY_NA_NA_%", "DISSOLVED OXYGEN (DO)_NA_NA_%", "PH_NA_NA_%", "TEMPERATURE_NA_NA_%", "TURBIDITY_NA_NA_%", "DISSOLVED OXYGEN (DO)_NA_NA_UG/KG",
                                  "PH_NA_NA_MMOL/L", "STREAM FLOW_NA_NA_M/SEC", "TURBIDITY_NA_NA_IN", "TURBIDITY_NA_NA_NA", "TOTAL DISSOLVED SOLIDS_FILTERED_NA_LB/DAY",
                                  "CHLOROPHYLL A_UNFILTERED_NA_G/M2", "STREAM FLOW_NA_NA_LB/DAY", "TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_%", "SILICA_DISSOLVED_AS SIO2_MG/L", "SILICA_DISSOLVED_AS SI_MG/L")), #dropping two silica types that won't harmonize - 16 measurements total
  
  tar_target(wqp_fix_values, Vectorize(function(name, unit, val) {
    if (is.na(unit)) return(val)
    else if (unit == "IN") return(val*2.54)
    else if (name %in% c("CHLOROPHYLL A", "PHEOPHYTIN A")) return(val)
    else if (unit == "UG/L" | unit == "UG/KG") return(val*0.001)
    else return(val)
  })),
  
  tar_target(wqp_fix_units, Vectorize(function(name, unit) {
    if (is.na(unit)) return(unit)
    else if (name %in% c("CHLOROPHYLL A", "PHEOPHYTIN A")) return(unit)
    else if (unit == "MG N/L******") return ("MG/L")
    else if (unit == "% SATURATN") return ("%")
    else if (unit == "STD UNITS") return ("NA")
    else if (unit == "US/CM @25C") return ("US/CM")
    else if (unit == "UG/L" | unit == "UG/KG") return ("MG/L")
    else return(unit)
  })),
  
  tar_target(renameMonitType, Vectorize(function (name, id) {
    gl_rename <- c("MNPCA-16-0001-00-239", "MNPCA-16-0001-00-258", "MNPCA-16-0001-00-259", "MNPCA-16-0001-00-240", "MNPCA-16-0001-00-241",
                   "MNPCA-S003-989", "MNPCA-S001-268", "MNPCA-S003-978", "MNPCA-S003-986", "WIDNR_WQX-10019105", "WIDNR_WQX-10038138", 
                   "REDCLIFF_WQX-SRM01", "REDCLIFF_WQX-RRM01", "REDCLIFF_WQX-FCM01", "REDCLIFF_WQX-RCCM01", "WIDNR_WQX-043099", 
                   "BADRIVER_WQX-KakLakeSup", "BADRIVER_WQX-BellMouth", "USGS-04027675", "BADRIVER_WQX-BadMouth", "USGS-04027655",
                   "USGS-464643092053701", "USGS-464222092010201", "USGS-464048091563501", "USGS-465135091061401", "USGS-465133091060301",
                   "USGS-465130091060701", "USGS-465142090311901", "USGS-464449090310901", "USGS-464742090480701", "USGS-464413090465901",
                   "USGS-464200090432301", "USGS-464158090472101", "USGS-463905090460701", "USGS-463927090384501", "USGS-463859090385701",
                   "USGS-463844090390401", "USGS-463834090390301", "USGS-463812090375101", "USGS-463800090370701", "WIDNR_WQX-10019598", "USGS-464226092005600")
    es_rename <- c("MNPCA-09-0167-00-100", "MNPCA-16-0001-00-242", "USGS-463914092124701", "MNPCA-69-1291-00-254", "USGS-464107092094201",
                   "USGS-464217092085301", "USGS-464230092083201", "USGS-464444092070401", "MNPCA-69-1291-02-201", "USGS-464433092050601",
                   "USGS-464348092040901", "USGS-464313092031101", "USGS-464131092001201", "NARS_WQX-NGL_MN-10014", "NARS_WQX-NGL_MN-10012",
                   "NARS_WQX-NGL_MN-10011", "NARS_WQX-NGL_MN-10017", "NARS_WQX-NGL_MN-10019", "NARS_WQX-NGL_MN-10015", "NARS_WQX-NCCAGL10-2001",
                   "NARS_WQX-NCCAGL10-GLBA10-134", "NARS_WQX-NCCAGL10-GLBA10-118", "NARS_WQX-NCCAGL10-GLBA10-038", "NARS_WQX-NCCAGL10-GLBA10-054",
                   "MNPCA-16-0001-00-B007", "MNPCA-16-0001-00-B037", "MNPCA-16-0001-00-B004", "21WIBCH-0313B110", "MNPCA-16-0001-00-B002", "MNPCA-16-0001-00-B001",
                   "WIDNR_WQX-10048289", "WIDNR_WQX-10048290", "WIDNR_WQX-10048291", "WIDNR_WQX-10048784", "WIDNR_WQX-10048785", "WIDNR_WQX-10048292",
                   "WIDNR_WQX-10048783", "WIDNR_WQX-10041152", "WIDNR_WQX-10034508", "WIDNR_WQX-10041156", "WIDNR_WQX-10041155", "WIDNR_WQX-10041153",
                   "WIDNR_WQX-10014688", "WIDNR_WQX-10034506", "WIDNR_WQX-10034507", "WIDNR_WQX-10052531", "WIDNR_WQX-10034505", "WIDNR_WQX-10014687",
                   "WIDNR_WQX-10041154", "WIDNR_WQX-10038054", "WIDNR_WQX-10012690", "WIDNR_WQX-10034504", "WIDNR_WQX-10038055", "WIDNR_WQX-10014686",
                   "WIDNR_WQX-10031417", "WIDNR_WQX-10028979", "WIDNR_WQX-10048278", "WIDNR_WQX-10034502", "WIDNR_WQX-10048284", "WIDNR_WQX-10048281",
                   "WIDNR_WQX-10037323", "WIDNR_WQX-10037322", "WIDNR_WQX-10037321", "WIDNR_WQX-10029156", "MNPCA_BIO-S004-971", "MNPCA-S004-971", "21WIBCH-0313B64",
                   "MNPCA-S000-021", "USGS-04024025", "MNPCA-S004-976", "USGS-463913092162401", "USGS-04024026", "WIDNR_WQX-10037338", "WIDNR_WQX-10037337",
                   "WIDNR_WQX-10037336", "WIDNR_WQX-10037334", "WIDNR_WQX-10014692", "WIDNR_WQX-10037330", "WIDNR_WQX-10037331", "WIDNR_WQX-10037332",
                   "WIDNR_WQX-10037333", "USGS-0402402850", "WIDNR_WQX-10034500", "WIDNR_WQX-163001", "MNPCA-S000-262", "USGS-463925092120601", "WIDNR_WQX-10037320",
                   "WIDNR_WQX-10014691", "WIDNR_WQX-10034501", "FONDULAC_WQX-SLRSIU", "FONDULAC_WQX-SLRSID", "USGS-0402403450", "USGS-464145092105401",
                   "WIDNR_WQX-10037329", "WIDNR_WQX-10037328", "WIDNR_WQX-10037326", "WIDNR_WQX-10037325", "WIDNR_WQX-10014690", "WIDNR_WQX-10028981",
                   "WIDNR_WQX-10034503", "MNPCA-S000-639", "WIDNR_WQX-10034499", "USGS-464227092092701", "WIDNR_WQX-10014689", "USGS-464326092084101",
                   "USGS-464348092091401", "USGS-464349092083701", "USGS-464414092090201", "USGS-464419092085301", "USGS-464502092080401", "USGS-464459092072503",
                   "USGS-464459092072501", "USGS-464506092065601", "USGS-464507092065802", "USGS-464508092064402", "USGS-464513092063603", "USGS-464509092070903",
                   "USGS-464517092071802", "USGS-464514092070003", "USGS-464516092065303", "USGS-464516092064503", "USGS-464519092065603", "USGS-464520092065003",
                   "USGS-464521092065403", "USGS-464521092070802", "USGS-464521092070202", "USGS-464523092064703", "USGS-464524092070302", "USGS-464524092070303",
                   "USGS-464525092064703", "USGS-464526092072002", "USGS-464527092070102", "USGS-464527092070103", "USGS-464528092070401", "USGS-464528092065903",
                   "USGS-464528092065002", "USGS-464529092065602", "USGS-464529092065402", "USGS-464529092065403", "USGS-464529092070402", "USGS-464531092070502",
                   "USGS-464531092064901", "USGS-464532092070203", "USGS-464532092070602", "USGS-464532092065402", "USGS-464532092071201", "USGS-464532092071102",
                   "USGS-464533092070203", "USGS-464533092070103", "USGS-464534092070103", "USGS-464533092065702", "USGS-464534092065402", "USGS-464534092065301",
                   "USGS-464535092070702", "USGS-464534092071501", "USGS-464534092071502", "USGS-464538092070603", "USGS-464537092071502", "USGS-464538092070902",
                   "USGS-464538092070903", "USGS-464542092070902", "USGS-464543092070601", "USGS-464543092071401", "USGS-464545092070601", "USGS-464545092070602",
                   "USGS-464546092071301", "USGS-464546092071302", "USGS-464546092071303", "USGS-464548092071001", "USGS-464548092071002", "USGS-464504092063401",
                   "USGS-464503092061803", "MNPCA-S003-975", "MNPCA-S000-277", "USGS-464655092054801", "USGS-464626092062201", "USGS-464633092053801", "USGS-464629092053801",
                   "USGS-464610092053301", "USGS-464611092052601", "USGS-464453092050301", "USGS-464503092042301", "USGS-464434092042201", "USGS-464425092035701",
                   "USGS-464420092034301", "USGS-464303092022301", "USGS-464258092022001", "USGS-463849092131601", "USGS-463959092114401", "USGS-464209092120001",
                   "USGS-464503092061802", "USGS-464459092072502", "USGS-464503092061801", "USGS-464513092063602", "USGS-464508092064401", "USGS-464507092065801",
                   "USGS-464509092070902", "USGS-464513092063602", "USGS-464516092064502", "USGS-464516092065302", "USGS-464514092070002", "USGS-464517092071801",
                   "USGS-464526092072001", "USGS-464521092070801", "USGS-464509092070901", "USGS-464514092070001", "USGS-464516092065301", "USGS-464516092064501",
                   "USGS-464513092063601", "USGS-464519092065602", "USGS-464521092065402", "USGS-464520092065002", "USGS-464523092064702", "USGS-464525092064702",
                   "USGS-464528092065001", "USGS-464529092065401", "USGS-464529092065601", "USGS-464528092065902", "USGS-464527092070101", "USGS-464524092070301",
                   "USGS-464521092070201", "USGS-464532092071101", "USGS-464537092071501", "USGS-464542092070901", "USGS-464538092070901", "USGS-464538092070602",
                   "USGS-464535092070701", "USGS-464519092065601", "USGS-464521092065401", "USGS-464520092065001", "USGS-464523092064701", "USGS-464525092064701",
                   "USGS-464528092065901", "USGS-464538092070601", "USGS-464529092070401", "USGS-464531092070501", "USGS-464532092070601", "USGS-464532092070202",
                   "USGS-464533092070202", "USGS-464533092070102", "USGS-464534092070102", "USGS-464533092065701", "USGS-464534092065401", "USGS-464532092065401",
                   "USGS-464532092070201", "USGS-464532092070201", "USGS-464533092070101", "USGS-464534092070101", "USGS-464533092070201", "USGS-464643092053701")
    
    str_rename <- c("WIDNR_WQX-10048632", "USGS-040260078")
    wet_rename <- c("WIDNR_WQX-10048913", "WIDNR_WQX-10048916", "WIDNR_WQX-10048882", "WIDNR_WQX-10048911", "WIDNR_WQX-10048912")
    lake_rename <- c("WIDNR_WQX-163403", "WIDNR_WQX-163404", "WIDNR_WQX-163405", "WIDNR_WQX-163126", "WIDNR_WQX-163060",
                     "REDCLIFF_WQX-SC01", "REDCLIFF_WQX-SC02")
    
    if (id %in% gl_rename) return("Great Lake")
    else if (id %in% es_rename) return("Estuary")
    else if (id %in% str_rename) return("River/Stream")
    else if (id %in% wet_rename) return("Wetland")
    else if (id %in% lake_rename) return("Lake")
    else if (name == "BEACH Program Site-Great Lake") return("Great Lake")
    else if (name == "Stream") return("River/Stream")
    else if (name == "Lake, Reservoir, Impoundment") return("Lake")
    else return(name)
  })),
  
  
  tar_target(wqp_drop_cols, function(wqp){
    na_cols <- c()
    uno_cols <- c()
    uno_vals <- c()
    duo_cols <- c()
    
    for (n in 1:length(wqp)) {
      cur_name <- colnames(wqp)[n]
      cur_vals <- unique(unlist(wqp[n]))
      if (length(cur_vals) <= 2) {
        if (all(is.na(cur_vals))) na_cols <- append(na_cols, cur_name)
        else if (any(is.na(cur_vals)) | length(cur_vals) == 1) {
          uno_cols <- append(uno_cols, cur_name)
          uno_vals <- append(uno_vals, cur_vals[!is.na(cur_vals)])
        }
        else duo_cols <- append(duo_cols, cur_name)
      }
    }
    
    # currently just returning na cols as needing to drop
    return(na_cols)
  }),
  
  tar_target(wqp_synref_file, "ref/wqp_Synonym_Reference_Table.csv", format = "file"),
  tar_target(wqp_synref, read_csv(wqp_synref_file)),
  
  #pull wqp data
  tar_target(wqp_pull_trib, TADA_BigDataRetrieval(huc = c("04010102", "04010201", "04010202", "04010301", "04010302"), startDate = "2010-01-01", endDate = "2023-12-31")),
  tar_target(wqp_pull_ls, TADA_BigDataRetrieval(huc = "04020300", startDate = "2010-01-01", endDate = "2023-12-31")),
  
  
  # cleaning WQP data
  # first just trim sites outside area, which requires autoclean
  tar_target(wqp_trib, wqp_pull_trib %>%
               TADA_AutoClean() %>%
               filter(TADA.LatitudeMeasure > 46)), #drop sites missing lat/long
  tar_target(wqp_ls, wqp_pull_ls %>%
               TADA_AutoClean() %>%
               filter(TADA.LatitudeMeasure < 47.3 & TADA.LongitudeMeasure < -90.0)), #remove LS data outside of western lobe
  
  #run all other recommended cleaning functions
  tar_target(wqp_s1, bind_rows(wqp_trib, wqp_ls) %>%
               TADA_AnalysisDataFilter(clean = TRUE, surface_water = TRUE) %>%
               filter(!(ActivityMediaSubdivisionName %in% c("Surface Water Sediment", "Interstitial Water", "Drinking Water", "Stormwater", "Ambient Air", "Snowmelt"))) %>% #drops sediment samples from surface waters
               TADA_RunKeyFlagFunctions() %>% #also drops ~160k NAs
               TADA_FindPotentialDuplicatesSingleOrg() %>% 
               TADA_FindPotentialDuplicatesMultipleOrgs(org_hierarchy = c("NARS_WQX", "NALMS")) %>%
               filter(TADA.SingleOrgDup.Flag == 'Unique' & TADA.ResultSelectedMultipleOrgs == 'Y') %>% #drops ~800k duplicated temp records, spot checked and seemed to be accurate
               TADA_FlagMeasureQualifierCode(clean = T) %>% 
               TADA_SimpleCensoredMethods() %>% 
               filter(TADA.CensoredData.Flag != "Detection condition is missing and required for censored data ID.") %>%
               TADA_AutoFilter() %>% 
               TADA_FlagDepthCategory()),
  
  #harmonizing parameters and removing unnecessary parameters
  tar_target(wqp_s2, wqp_s1 %>% 
               filter(TADA.CharacteristicName %in% unique(wqp_synref$TADA.CharacteristicName)) %>%
               TADA_HarmonizeSynonyms(ref = wqp_synref) %>% 
               filter(!(TADA.ComparableDataIdentifier %in% wqp_invalid_units)) %>%
               mutate(TADA.ResultMeasureValue = wqp_fix_values(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode, TADA.ResultMeasureValue),
                      TADA.ResultMeasure.MeasureUnitCode = wqp_fix_units(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode),
                      TADA.CharacteristicName = if_else(TADA.CharacteristicName == "DISSOLVED OXYGEN (DO)" & TADA.ResultMeasure.MeasureUnitCode == "%", "DISSOLVED OXYGEN SATURATION", TADA.CharacteristicName)) %>%
               TADA_CreateComparableID()),
  
  #relabel site types and remove inland lakes/wetlands/etc.
  tar_target(wqp_s3, wqp_s2 %>%
               mutate(MonitoringLocationTypeName = renameMonitType(MonitoringLocationTypeName,MonitoringLocationIdentifier)) %>%
               filter(MonitoringLocationTypeName %in% c("Great Lake", "Estuary", "River/Stream"))),
  # could potentially filter out River/Stream sites in hucs 04010201/04010202, since they drain into the estuary before the lake
  # make sure everything is correctly labeled - remove filter above and then run:
  # filter(wqp_s3, MonitoringLocationTypeName == "Great Lake") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "Estuary") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "River/Stream") %>%
  #   TADA_OverviewMap()
  # filter(wqp_s3, MonitoringLocationTypeName == "Lake") %>%
  #   TADA_OverviewMap()
  
  tar_target(wqp_long, wqp_s3 %>%
               select(-all_of(wqp_drop_cols(wqp_s3))) %>%
               TADA_RetainRequired()),
  
  tar_target(wqp_wide, wqp_long %>% 
               filter(!(TADA.DepthCategory.Flag %in% c("Bottom", "Middle"))) %>% #assume surface if not specified
               select(TADA.ComparableDataIdentifier, ActivityStartDate, TADA.ResultMeasureValue, OrganizationIdentifier, MonitoringLocationTypeName,
                      MonitoringLocationIdentifier, TADA.LatitudeMeasure, TADA.LongitudeMeasure, HUCEightDigitCode) %>% 
               pivot_wider(names_from = TADA.ComparableDataIdentifier, values_from = TADA.ResultMeasureValue, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
               mutate(date = ymd(ActivityStartDate, tz = "America/Chicago"),
                      nh3 = rowMeans(across(c(`AMMONIA_FILTERED_AS N_MG/L`, `AMMONIA_UNFILTERED_AS N_MG/L`)), na.rm = TRUE), #combining filtered and unfiltered parameters where they shouldn't be split (ions, mislabeled chl)
                      cl = rowMeans(across(c(`CHLORIDE_FILTERED_NA_MG/L`, `CHLORIDE_UNFILTERED_NA_MG/L`)), na.rm = TRUE),
                      chl = rowMeans(across(c(`CHLOROPHYLL A_UNFILTERED_NA_UG/L`, `CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN_UNFILTERED_NA_MG/L`, `CHLOROPHYLL A_FILTERED_NA_UG/L`, `CHLOROPHYLL A, UNCORRECTED FOR PHEOPHYTIN_FILTERED_NA_MG/L`))), # all chl sensor measurements (ie buoys) were removed when filtering to surface samples only
                      pheo = rowMeans(across(c(`PHEOPHYTIN A_FILTERED_NA_UG/L`, `PHEOPHYTIN A_UNFILTERED_NA_UG/L`)), na.rm = TRUE),
                      no23 = rowMeans(across(c(`NITRATE + NITRITE_FILTERED_AS N_MG/L`, `NITRATE + NITRITE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      no3 = rowMeans(across(c(`NITRATE_FILTERED_AS N_MG/L`, `NITRATE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      no2 = rowMeans(across(c(`NITRITE_FILTERED_AS N_MG/L`, `NITRITE_UNFILTERED_AS N_MG/L`)), na.rm = TRUE),
                      po4 = rowMeans(across(c(`ORTHOPHOSPHATE_FILTERED_AS P_MG/L`, `ORTHOPHOSPHATE_UNFILTERED_AS P_MG/L`)), na.rm = TRUE),
                      si = rowMeans(across(c(`SILICA_FILTERED_AS SI_MG/L`, `SILICA_UNFILTERED_AS SI_MG/L`)), na.rm = TRUE),
                      across(c(nh3, cl, chl, pheo, no23, no3, no2, po4, si), replace_nan),
                      no23 = if_else(no3 > no23, no3, no23),
                      no23 = if_else(is.na(no23) & !is.na(no3) & !is.na(no2), no3 + no2, no23),
                      no23 = if_else(is.na(no23) & !is.na(no3) & is.na(no2), no3, no23),
                      temp = if_else(`TEMPERATURE_NA_NA_DEG C` > -1 & `TEMPERATURE_NA_NA_DEG C` < 30, `TEMPERATURE_NA_NA_DEG C`, NA), # remove obviously wrong temp data
                      do = if_else(`DISSOLVED OXYGEN (DO)_NA_NA_MG/L` > 0 & `DISSOLVED OXYGEN (DO)_NA_NA_MG/L` < 20, `DISSOLVED OXYGEN (DO)_NA_NA_MG/L`, NA), # remove obviously wrong/outlier do data
                      do_sat = if_else(`DISSOLVED OXYGEN SATURATION_NA_NA_%` > 0 & `DISSOLVED OXYGEN SATURATION_NA_NA_%` < 140, `DISSOLVED OXYGEN SATURATION_NA_NA_%`, NA), # remove obviously wrong/outlier do data
                      ph = if_else(`PH_NA_NA_NA` > 5 & `PH_NA_NA_NA` < 11, `PH_NA_NA_NA`, NA), # remove obviously wrong/outlier ph data
                      cond = if_else(`CONDUCTIVITY_NA_NA_US/CM` > 10000, `CONDUCTIVITY_NA_NA_US/CM` * 0.001, `CONDUCTIVITY_NA_NA_US/CM`), # fixing obviously mislabeled cond units
                      cond = if_else(cond > 2000, NA, cond),
                      turb = if_else(`TURBIDITY_NA_NA_NTU` < 0, NA, `TURBIDITY_NA_NA_NTU`),
                      trans_tube = if_else(`TRANSPARENCY, TUBE WITH DISK_NA_NA_IN` > 120, NA, `TURBIDITY_NA_NA_NTU`),
                      secchi = `SECCHI DEPTH_NA_NA_M`,
                      discharge = if_else(`STREAM FLOW_NA_NA_CFS` < 0, NA, `STREAM FLOW_NA_NA_CFS`),
                      tss = if_else(`TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_MG/L` < 0, NA, `TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_MG/L`),
                      tds = if_else(`TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L` > 0 & `TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L` < 1000, `TOTAL DISSOLVED SOLIDS_FILTERED_NA_MG/L`, NA),
                      toc = `ORGANIC CARBON_UNFILTERED_NA_MG/L`,
                      doc = if_else(`ORGANIC CARBON_UNFILTERED_NA_MG/L` < `ORGANIC CARBON_FILTERED_NA_MG/L`, `ORGANIC CARBON_UNFILTERED_NA_MG/L`, `ORGANIC CARBON_FILTERED_NA_MG/L`), # if DOC is greater than TOC, set equal to TOC 
                      ton = `ORGANIC NITROGEN_UNFILTERED_AS N_MG/L`,
                      don = if_else(`ORGANIC NITROGEN_UNFILTERED_AS N_MG/L` < `ORGANIC NITROGEN_FILTERED_AS N_MG/L`, `ORGANIC NITROGEN_UNFILTERED_AS N_MG/L`, `ORGANIC NITROGEN_FILTERED_AS N_MG/L`), # if DON is greater than TON, set equal to TON 
                      tkn = `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L`,
                      tdkn = if_else(`TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L` < `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_FILTERED_AS N_MG/L`, `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_UNFILTERED_AS N_MG/L`, `TOTAL KJELDAHL NITROGEN (ORGANIC N & NH3)_FILTERED_AS N_MG/L`),
                      tn = `TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L`,
                      tdn = if_else(`TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L` < `TOTAL NITROGEN, MIXED FORMS_FILTERED_AS N_MG/L`, `TOTAL NITROGEN, MIXED FORMS_UNFILTERED_AS N_MG/L`, `TOTAL NITROGEN, MIXED FORMS_FILTERED_AS N_MG/L`),  
                      tp = `TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L`,
                      tdp = if_else(`TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L` < `TOTAL PHOSPHORUS, MIXED FORMS_FILTERED_AS P_MG/L`, `TOTAL PHOSPHORUS, MIXED FORMS_UNFILTERED_AS P_MG/L`, `TOTAL PHOSPHORUS, MIXED FORMS_FILTERED_AS P_MG/L`)  
               ) %>% 
               select(date, source = OrganizationIdentifier, type = MonitoringLocationTypeName, site = MonitoringLocationIdentifier,
                      latitude = TADA.LatitudeMeasure, longitude = TADA.LongitudeMeasure, huc = HUCEightDigitCode, temp, do, do_sat, ph, cond, turb, trans_tube, secchi, discharge,
                      tss, tds, chl, pheo, toc, doc, ton, don, tdkn, tkn, tdn, tn, nh3, no3 = no23, tdp, tp, po4, cl, si) %>% #note that were calling nitrate/nitrite no3
               filter(!if_all(-c(date, source, type, site, latitude, longitude, huc, temp), is.na)) %>% # drop rows with only temp
               arrange(date))
  
)