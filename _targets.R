library(targets)

read_packages <- c("tidyverse", "readxl", "sf", "EPATADA", "dataRetrieval") #EPATADA needs to be installed from github

tar_option_set(packages = c(read_packages))

source("R/read.R")
source("R/analysis.R")

list(
  read_targets,
  analysis_targets
)