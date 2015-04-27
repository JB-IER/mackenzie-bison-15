source("header.R")

set_folders("clean")

load_rdata()

bison %<>% left_join(weather, by = "Year")
rm(weather)

set_folders("tidy")

save_rdata()
