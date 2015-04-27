source("header.R")
species <- c("BT", "RB")
for (spp in species) {
  print(paste("count", spp))
  set_folders("count", spp)
  
  data <- load_rdata("data")
  
  analysis <- load_analysis()
  
  stopifnot(identical(data, dataset(analysis)))
  
  print(summary(analysis))
  
  model <- 1
    
  save_tables(analysis, model = model)
  
  count <- predict(analysis, newdata = "", model = model)
  
  gp <- ggplot(data = count, aes(x = Species, y = estimate))
  gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
  gp <- gp + scale_y_continuous(name = "Count")
  gp <- gp + expand_limits(y = 0)
  
  gwindow(50)
  print(gp)
  
  save_plot("count", report = FALSE)
}

count <- data.frame()
for (spp in species) {
  set_folders("count", spp)
  count <- rbind(count, load_plot("count", dataset = TRUE))
}
set_folders("count")

gp <- ggplot(data = count, aes(x = Species, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Count")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

save_plot("count", caption = "Predicted count (with 95% CRIs).") 
