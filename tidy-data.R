source("header.R")

set_folders("clean")

load_rdata()

bison %<>% full_join(counts, by = "Year")
rm(counts)

set_folders("tidy")

save_rdata()
