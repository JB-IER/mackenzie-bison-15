source("header.R")

set_folders("bison")

data <- load_rdata("data")

analysis <- load_analysis()

stopifnot(identical(data, dataset(analysis)))

print(summary(analysis))

save_tables(analysis)

bison <- coef(analysis, "bBison")
bison$Year <- as.integer(levels(data$Year))

gp <- ggplot(data = bison, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Bison")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)
