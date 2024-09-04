# library(remotes)
# install_github("USEPA/EPATADA",
#                dependencies = TRUE
# )
library(EPATADA)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmltools)
library(lubridate)
library(dataRetrieval)


# hucs in area we're interested in
huc_list <- c("04010102", "04010201", "04010202", "04010301", "04010302", "04020300") 

#see what sites have continuous data
nwis_pull <- readNWISdata(huc = huc_list, service = "dv", startDate = "2010-01-01")

site_list <- readNWISdata(huc = huc_list, service = "site", startDate = "2010-01-01") %>%
  filter(site_no %in% nwis_pull$site_no & site_tp_cd == "ST")

leaflet() %>%
  addProviderTiles('Esri.WorldStreetMap') %>%
  addCircleMarkers(
    data=site_list,
    lng=~dec_long_va,
    lat=~dec_lat_va,
    label = ~htmlEscape(str_c(site_no, station_nm, sep = ", "))
  )

#sites that are more upstream than is really useful
upstream_sites <- c("04015438", "465735092504901", "470535092570801", "471149092360901", "04021520", "04026450", "04026561", "04026740", "04001000")

site_list <- filter(site_list, !(site_no %in% upstream_sites))

leaflet() %>%
  addProviderTiles('Esri.WorldStreetMap') %>%
  addCircleMarkers(
    data=site_list,
    lng=~dec_long_va,
    lat=~dec_lat_va,
    label = ~htmlEscape(str_c(site_no, station_nm, sep = ", "))
  )

#site I had found that should have data:
activesites <- c("04015330", "04024000", "464646092052900", "04024430", "04025500", "04026005", "04026157", "04026160", "465130091060701", "040263205", "463741090521301",
                 "04026390", "463836090423701", "04027500", "040265935", "040265981", "04026740", "04026511", "04026561", "04026900", "04027000", "04027595", "04027655", 
                 "463227090365601", "040276505")

missingactive <- activesites[!(activesites %in% site_list$site_no) & !(activesites %in% upstream_sites)]

for (site in missingactive) {
  dv_check <- readNWISdata(sites = site, service = "dv")
  if (nrow(dv_check) > 0) print(site)
}
#duluth returns a dv, but its bad data and won't return dvs if you give it a start date
#data_464646092052900 <- readNWISdata(sites = "464646092052900", service = "dv", startDate = "2010-01-01")

iv_sites <- c()
for (site in missingactive) {
  iv_check <- readNWISdata(sites = site, service = "iv")
  if (nrow(iv_check) > 0) {
    iv_sites <- append(iv_sites, site)
  }
}

#sites with only instantaneous values available:
missing_sitelist <- readNWISdata(site = iv_sites, service = "site")
leaflet() %>%
  addProviderTiles('Esri.WorldStreetMap') %>%
  addCircleMarkers(
    data=missing_sitelist,
    lng=~dec_long_va,
    lat=~dec_lat_va,
    label = ~htmlEscape(str_c(site_no, station_nm, sep = ", "))
  )

# of those 9, 4 seem useful - c("463741090521301", "464646092052900", "463836090423701", "04027655")
# in order of most usefulness Ashland breakwater, Duluth shipping canal, Kakagon river slough, bad river mouth
# the others: 4 are bad river tributaries, 1 is upstream denomie creek
# might want to pull IV data for sites that we have discrete observations for to match up gage data















###########pulling WQP data 
# huc_list <- c("04010102", "04010201", "04010202", "04010301", "04010302")
# wqp_pull_trib <- TADA_BigDataRetrieval(huc = huc_list, startDate = "2010-01-01", endDate = "2023-12-31")
# saveRDS(wqp_pull_trib, "Desktop/wqp_full_trib.rds")
# write_csv(wqp_pull_trib, "Desktop/wqp_full_trib.csv")
# 
# wqp_pull_ls <- TADA_BigDataRetrieval(huc = "04020300", startDate = "2010-01-01", endDate = "2023-12-31") 
# saveRDS(wqp_pull_ls, "Desktop/wqp_full_ls.rds")
# write_csv(wqp_pull_ls, "Desktop/wqp_full_ls.csv")

wqp_trib <- readRDS("Desktop/wqp_full_trib.rds") %>%
  TADA_AutoClean() %>%
  filter(TADA.LatitudeMeasure > 46) #drop sites missing lat/long

wqp_ls <- readRDS("Desktop/wqp_full_ls.rds") %>%
  TADA_AutoClean() %>%
  filter(TADA.LatitudeMeasure < 47.3 & TADA.LongitudeMeasure < -90.0)

wqp <- bind_rows(wqp_trib, wqp_ls) %>%
  TADA_AnalysisDataFilter(clean = TRUE, surface_water = TRUE) %>%
  filter(!(ActivityMediaSubdivisionName %in% c("Surface Water Sediment", "Interstitial Water", "Drinking Water", "Stormwater", "Ambient Air", "Snowmelt"))) %>% #drops sediment samples from surface waters
  TADA_RunKeyFlagFunctions() %>% #also drops ~160k NAs
  TADA_FindPotentialDuplicatesSingleOrg() %>% 
  TADA_FindPotentialDuplicatesMultipleOrgs(org_hierarchy = c("NARS_WQX", "NALMS")) %>%
  filter(TADA.SingleOrgDup.Flag == 'Unique' & TADA.ResultSelectedMultipleOrgs == 'Y') %>% #drops ~800k duplicated temp records, spot checked and seemed to be accurate
  TADA_FlagMeasureQualifierCode(clean = T) %>% 
  TADA_SimpleCensoredMethods() %>% 
  filter(TADA.CensoredData.Flag != "Detection condition is missing and required for censored data ID.") %>%
  TADA_AutoFilter()

rm(wqp_trib, wqp_ls)
gc()

# get synonym reference table to edit
#synref <- TADA_GetSynonymRef(wqp2)
# write_csv(synref, "Desktop/synref.csv", na = "")
# read in once edits are done
synref <- read_csv("Desktop/Synonym Reference Table.csv") 

wqp <- filter(wqp, TADA.CharacteristicName %in% unique(synref$TADA.CharacteristicName)) %>%
  TADA_HarmonizeSynonyms(ref = synref)


invalid_units <- c("CONDUCTIVITY_NA_NA_%", "DISSOLVED OXYGEN (DO)_NA_NA_%", "PH_NA_NA_%", "TEMPERATURE_NA_NA_%", "TURBIDITY_NA_NA_%", "DISSOLVED OXYGEN (DO)_NA_NA_UG/KG",
                   "PH_NA_NA_MMOL/L", "STREAM FLOW_NA_NA_M/SEC", "TURBIDITY_NA_NA_IN", "TURBIDITY_NA_NA_NA", "TOTAL DISSOLVED SOLIDS_FILTERED_NA_LB/DAY",
                   "CHLOROPHYLL A_UNFILTERED_NA_G/M2", "STREAM FLOW_NA_NA_LB/DAY", "TOTAL SUSPENDED SOLIDS_NON-FILTERABLE (PARTICLE)_NA_%")

fix_values <- function(name, unit, val) {
  if (is.na(unit)) return(val)
  else if (unit == "IN") return(val*2.54)
  else if (name %in% c("CHLOROPHYLL A", "PHEOPHYTIN A")) return(val)
  else if (unit == "UG/L" | unit == "UG/KG") return(val*0.001)
  else return(val)
}

fix_units <- function(name, unit) {
  if (is.na(unit)) return(unit)
  else if (name %in% c("CHLOROPHYLL A", "PHEOPHYTIN A")) return(unit)
  else if (unit == "MG N/L******") return ("MG/L")
  else if (unit == "% SATURATN") return ("%")
  else if (unit == "STD UNITS") return ("NA")
  else if (unit == "US/CM @25C") return ("US/CM")
  else if (unit == "UG/L" | unit == "UG/KG") return ("MG/L")
  else return(unit)
}

wqp <- wqp %>% 
  filter(!(TADA.ComparableDataIdentifier %in% invalid_units)) %>%
  mutate(TADA.ResultMeasureValue = fix_values(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode, TADA.ResultMeasureValue),
         TADA.ResultMeasure.MeasureUnitCode = fix_units(TADA.CharacteristicName,TADA.ResultMeasure.MeasureUnitCode),
         TADA.CharacteristicName = if_else(TADA.CharacteristicName == "DISSOLVED OXYGEN (DO)" & TADA.ResultMeasure.MeasureUnitCode == "%", "DISSOLVED OXYGEN SATURATION", TADA.CharacteristicName)
         ) %>%
  TADA_CreateComparableID() 
  ####MAKE SURE ResultTimeBasis IS GETTING APPENDE TO COMAPRABLE dataID???
  #mutate(TADA.ComparableDataIdentifier = if_else(!is.na(ResultTimeBasisText), str_c(TADA.ComparableDataIdentifier, ResultTimeBasisText, StatisticalBaseCode, sep = "_"), TADA.ComparableDataIdentifier))

wqp <- TADA_FlagDepthCategory(wqp) %>%
  TADA_FlagContinuousData()


# split into superior, estuary, tribs datasets
#remove sites we know we don't need
#also fix site types
renameMonitType <- Vectorize(function (name, id) {
  gl_rename <- c("MNPCA-16-0001-00-239", "MNPCA-16-0001-00-258", "MNPCA-16-0001-00-259", "MNPCA-16-0001-00-240", "MNPCA-16-0001-00-241", 
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
  
  if (id %in% gl_rename) return("Great Lake")
  else if (id %in% es_rename) return("Estuary")
  else if (id %in% str_rename) return("River/Stream")
  else if (id %in% wet_rename) return("Wetland")
  else if (name == "BEACH Program Site-Great Lake") return("Great Lake")
  else if (name == "Stream") return("River/Stream")
  else if (name == "Lake, Reservoir, Impoundment") return("Lake")
  else return(name)
})


wqp <- wqp %>% 
  mutate(MonitoringLocationTypeName = renameMonitType(MonitoringLocationTypeName,MonitoringLocationIdentifier))


#make sure everything looks correctly labeled
# filter(wqp, MonitoringLocationTypeName == "Great Lake") %>%
#   TADA_OverviewMap()
# filter(wqp, MonitoringLocationTypeName == "Estuary") %>%
#   TADA_OverviewMap()
# filter(wqp, MonitoringLocationTypeName == "River/Stream") %>%
#   TADA_OverviewMap()
# filter(wqp, MonitoringLocationTypeName == "Lake") %>%
#   TADA_OverviewMap()

wqp <- filter(wqp, MonitoringLocationTypeName %in% c("Great Lake", "Estuary", "River/Stream"))

wqp <- TADA_RetainRequired(wqp)



# drop columns with all NAs
# currently not dropping columns with only 1 value, but note value
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

# wqp_info <- tibble(Descriptor = uno_cols, Info = uno_vals)

wqp <- select(wqp, -all_of(na_cols)) #%>%
  #select(-all_of(uno_cols))

# for (col in duo_cols) {
#   print(col)
#   print(TADA_FieldValuesTable(wqp2, col))
# }


#write out base cleaned dataset
write_rds(wqp, "Desktop/wqp.rds")



# pivot wide - consider aggregating to day

wqp_slim <- wqp %>%
  select(TADA.ComparableDataIdentifier, ActivityStartDateTime, TADA.ResultMeasureValue, TADA.ConsolidatedDepth, TADA.DepthCategory.Flag,
         ActivityIdentifier, OrganizationIdentifier, MonitoringLocationTypeName, TADA.MonitoringLocationIdentifier, TADA.LatitudeMeasure, TADA.LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName)

wqp_wide <- pivot_wider(wqp_slim, names_from = TADA.ComparableDataIdentifier, values_from = TADA.ResultMeasureValue, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
  arrange(ActivityStartDateTime) 

wqp_slimmer <- select(wqp,TADA.ComparableDataIdentifier, ActivityStartDate, TADA.ResultMeasureValue, TADA.DepthCategory.Flag,
                      MonitoringLocationTypeName, TADA.LatitudeMeasure, TADA.LongitudeMeasure)
wqp_wide_dv <- pivot_wider(wqp_slimmer, names_from = TADA.ComparableDataIdentifier, values_from = TADA.ResultMeasureValue, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
  arrange(ActivityStartDate) 

#potentially include: ResultTimeBasisText, StatisticalBaseCode, SampleCollectionMethod.MethodName, HydrologicCondition, HydrologicEvent,
