source("header.R")

set_folders("tidy")

data <- load_rdata("bison")

set_folders("bison")

data$Year %<>% factor
stopifnot(nlevels(data$Year) == length(unique(data$Year)))
print(summary(data))
save_rdata(data)
save_plots(data)
