source("header.R")

set_folders("tidy")

data <- load_rdata("bison")

variables <- c("WSI", "Rainfall", "SummerTemp", "PDO")

data$Year <- factor(data$Year, levels = min(data$Year):max(data$Year))

for(var in variables) {
  set_folders("env", var)
  data$Env <- data[[var]]
  save_rdata(data)
}
