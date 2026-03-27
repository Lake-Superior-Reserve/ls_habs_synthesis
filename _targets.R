library(targets)

read_packages <- c("tidyverse", "readxl", "sf", "EPATADA", "dataRetrieval") #EPATADA needs to be installed from github

tar_option_set(packages = c(read_packages))

tar_source()

list(
  read_targets,
  analysis_targets
)
