#if(!grepl("^http://mran.revolutionanalytics.com/snapshot/", getOption("repos"))) {
#  source("install-packages.R")
#}

library(devtools)
library(MASS)
library(RODBC)
library(abind)
library(coda)
library(doParallel)
library(itertools)
library(grid)
library(stringr)
library(lubridate)
library(reshape2)
library(plyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggplot2)
library(scales)
library(knitr)
library(beepr)
library(compare)
library(dplyr)
library(tulip)
library(datalist)
library(juggler)
library(jaggernaut)
library(poiscon)
require(reporting)

if (getDoParWorkers() == 1) {
  registerDoParallel(3)
  opts_jagr(parallel = TRUE)
}
opts_jagr(mode = "report")

rm(list = ls())
graphics.off()

reset_folders()

source("functions.R")
source("plots.R")
