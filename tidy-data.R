source("header.R")

set_folders("clean")

load_rdata()

bison %<>% full_join(counts, by = "Year")
rm(counts)

bison %<>% filter(Year <= 2012)

set_folders("tidy")

save_rdata()
