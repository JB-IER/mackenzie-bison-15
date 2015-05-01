source("header.R")
source("models-env.R")

variables <- c("WSI", "Rainfall", "SummerTemp", "PDO")

perform_analyses(models, "env", variables, niters = 10^5)
