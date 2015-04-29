source("header.R")
source("models-bison.R")

perform_analyses(models, "bison", niters = 10^5)
