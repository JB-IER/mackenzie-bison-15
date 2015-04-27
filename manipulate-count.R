source("header.R")
                 
set_folders("tidy")

count <- load_rdata("count")

species <- c("BT", "RB")

for (spp in species) {
    
  print(paste("count", spp))


  # code to manipulate data into a form for analysis
  data <- filter(count, Species == spp)
  data$Species <- droplevels(data$Species)
  
  gp <- ggplot(data = data, aes(x = Count))
  gp <- gp + geom_histogram(binwidth = 1)
  gp <- gp + scale_x_continuous(name = "Count")
  gp <- gp + scale_y_continuous(name = "Frequency")
  
  gwindow(3,3)
  print(gp)

  set_folders("observed")
  
  species <- ifelse(spp == "BT", "Bull Trout", "Rainbow Trout")
  
  save_plot(spp, caption = 
      paste("Histogram of observed", species, "counts"))

  set_folders("count", spp)
  
  print(summary(data))
  save_rdata(data)
  save_plots(data)
}
