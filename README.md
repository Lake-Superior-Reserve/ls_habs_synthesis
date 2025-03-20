# Lake Superior HABS Data Synthesis

This repository holds code to synthesize and analyze data on the Western arm of Lake Superior and its watershed in order to better understand the increasing frequency of harmful algal blooms. It is setup as an automated pipeline using the [`targets`](https://books.ropensci.org/targets/) R package in order to orchestrate a complex, modular workflow where dependency tracking determines which components need to be built. Currently, this pipeline will need 1-2 hours and an internet connection to build. See the "Running Pipeline" section below for steps.

## Structure

-   `/raw_data` contains data files provided from partner organizations that form the foundation of the data sets assembled in this repo. It's organized into folders according to the providing organization: Lake Superior National Estuarine Research Reserve (LSNERR), Northland College Mary Griggs Burke Center for Freshwater Innovation (NCBC), EPA National Coastal Condition Assessment (NCCA, not a partner), National Park Service (NPS), University of Minnesota Duluth Large Lakes Observatory (UMD), and Wisconsin Department of Natural Resources (WDNR).
-   `/ref` contains reference files with supporting information, such as site coordinates, used to assemble the data sets.
-   `/R` contains the R scripts used to build and analyse the data sets. `read.R` processes the files in `/raw_data` and data pulled from the EPA Water Quality Portal (WQP) and the USGS National Water Information System (NWIS) to create the core data sets. Specific reading and cleaning steps for each raw file can be found in the relevant provider script in `/R/read_targets`. 
-   `/out` contains csv versions of the core data sets.

## Core Data Sets

-   `lake_core.csv` has observations from Lake Superior West of 90° and South of 47.3°.
-   `estuary_core.csv` has observations from the St Louis River Estuary, defined as the St Louis River downstream of Thomson dam and including Superior Bay.
-   `tributary_core.csv` has observations from rivers and streams in the Western Lake Superior watershed (HUC8 codes "04010102", "04010201", "04010202", "04010301", and "04010302").
-   `trib_q_core.csv` has observations of discharge only in the Western Lake Superior watershed (see above) and is split out to keep the main tributary file size down.

Currently, the core data sets have observations from 1/1/2010 - 12/31/2023. All observations are taken in surface waters (\< 2m depth) and may be the average of multiple measurements taken at the same site on the same day. Core data sets may contain the following columns:

-   `date` - date in the America/Chicago timezone
-   `latitude` - latitude, decimal degrees, WGS84
-   `longitude` - longitude, decimal degrees, WGS84
-   `site` - sample site name/id provided by source
-   `source` - providing organization identifier
-   `huc` - for tributary sites, the HUC8 code it falls under
-   `discharge` - daily mean discharge, cubic feet / second
-   `chl` - chlorophyll a, ug/L, analyzed in lab with fluorometry/spectrophotometry
-   `chl_field` - chlorophyll a, ug/L, based on optical sensor deployed in waterbody
-   `tss` - total suspended solids, mg/L
-   `turb` - turbidity, NTU, field observation
-   `cond` - specific conductivity, uS/cm at 25°C, field observation
-   `ph` - pH, SU, field observation
-   `temp` - temperature, °C, field observation
-   `do` - dissolved oxygen, mg/L, field observation
-   `do_sat` - dissolved oxygen, percent saturation, field observation
-   `doc` - dissolved organic carbon, mg/L C
-   `poc` - particulate organic carbon, mg/L C
-   `toc` - total organic carbon, mg/L C
-   `tn` - total nitrogen, mg/L N
-   `tdn` - total dissolved nitrogen, mg/L N
-   `ton` - total organic nitrogen, mg/L N
-   `don` - dissolved organic nitrogen, mg/L N
-   `pon` - particulate organic nitrogen, mg/L N
-   `no3` - nitrate/nitrite, mg/L N
-   `nh3` - ammonia/ammonium, mg/L N
-   `tp` - total phosphorus, mg/L P
-   `tdp` - total dissolved phosphorus, mg/L P
-   `pp` - particulate phosphorus, mg/L P
-   `po4` - soluble reactive phosphorus/orthophosphate, mg/L
-   `npr` - total nitrogen / total phosphorus ratio
-   `cnr` - total organic carbon / total nitrogen ratio
-   `cpr` - total organic carbon / total phosphorus ratio
-   `pnpr` - particulate organic nitrogen / particulate phosphorus ratio
-   `pcnr` - particulate organic carbon / particulate organic nitrogen ratio
-   `pcpr` - particulate organic carbon / particulate phosphorus ratio
-   `si` - silica, mg/L Si
-   `cl` - chloride, mg/L Cl

## Running Pipeline

Just run the following snippet (requires an internet connection and will take 1-2 hours to complete):

``` r
# install required packages
install.packages("targets", "tidyverse", "readxl", "sf", "dataRetrieval", "remotes")
remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

# build datasets
targets::tar_make()
```

## Using the Data

If you are interested in making use of these data, please [get in touch](mailto:srblackburn@wisc.edu)! We would love to chat about potential collaborations and how me can make these data for useful to the community. 
This dataset is made available under the [Creative Commons CCO1.0 License](https://creativecommons.org/publicdomain/zero/1.0/). A generic citation is provided below:

> Blackburn, S.R, Reinl, K.L., Coffman, E.M., Hudson, M.J., Lafrancois, B.M., 2025. Lake Superior Harmful Algal Bloom Synthesized Dataset.

