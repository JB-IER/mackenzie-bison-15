source("header.R")

set_folders("clean")

load_rdata()

bison %<>% full_join(counts, by = "Year")
bison %<>% full_join(weather, by = "Year")

rm(weather, counts)

bison %<>% filter(Year <= 2012)

set_folders("tidy")

save_rdata()
