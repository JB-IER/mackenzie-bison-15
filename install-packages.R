#install.packages("checkpoint")
#library(checkpoint)
#checkpoint("2015-01-19")

# install.packages("RODBC", type = "source")

library(devtools)
library(grid)
install_github(c("poissonconsulting/tulip@v0.0.13",
                 "poissonconsulting/datalist@v0.4",
                 "poissonconsulting/juggler@v0.1.4",
                 "poissonconsulting/jaggernaut@v2.2.10",
                 "poissonconsulting/poiscon@v0.8.20"))

install_github("poissonconsulting/reporting@v0.3.2")
