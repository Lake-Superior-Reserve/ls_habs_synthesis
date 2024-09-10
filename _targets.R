library(targets)

read_packages <- c("tidyverse", "lubridate", "readxl", "sf")
visual_packages <- c("corrplot", "Hmisc", "gganimate")

tar_option_set(packages = c(read_packages, visual_packages))

source("R/read.R")
source("R/visual.R")

list(
  s1_targets,
  s2_targets
)