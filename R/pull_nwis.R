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

