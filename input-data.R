source("header.R")

bison <- read.csv("input/Mackenzie model input data.csv")
counts <- read.csv("input/rawcompdata.csv")
weather <- read.csv("input/weather covariates.csv")
pdo <- read.csv("input/PDO_NOAA141128.csv")

remove_dots_colnames_data_frames()

set_folders("input")

save_rdata()
