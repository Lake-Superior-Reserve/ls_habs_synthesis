library(targets)
tar_option_set(packages = c("tidyverse", "lubridate", "readxl"))

source("R/read.R")

list(
  s1_targets
)