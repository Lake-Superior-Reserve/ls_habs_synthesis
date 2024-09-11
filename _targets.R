library(targets)

read_packages <- c("tidyverse", "readxl", "sf")
visual_packages <- c("corrplot", "Hmisc", "gganimate", "randomForest")

tar_option_set(packages = c(read_packages, visual_packages))

source("R/read.R")
source("R/visual.R")

list(
  s1_targets,
  s2_targets
)