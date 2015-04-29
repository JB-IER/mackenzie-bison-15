source("header.R")

set_folders("clean")

load_rdata()

bison %<>% full_join(counts, by = "Year")
bison %<>% full_join(weather, by = "Year")
bison %<>% left_join(pdo, by = "Year")

rm(weather, counts, pdo)

bison %<>% filter(Year <= 2012)

set_folders("tidy")

save_rdata()
