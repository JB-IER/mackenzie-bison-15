source("header.R")

set_folders("bison")

data <- load_rdata("data")

analysis <- load_analysis()

stopifnot(identical(data, dataset(analysis)))

print(summary(analysis))

save_tables(analysis)

bison <- predict(analysis, newdata = "Year")
bison$Year %<>% as.integer

gp <- ggplot(data = bison, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Bison")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

scalf <- predict(analysis, parm = "eSurvivalCalfYear", newdata = "Year")
scalf$Year %<>% as.integer

gp <- ggplot(data = scalf, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Calf Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

calfcow <- predict(analysis, parm = "eCalfCowRatio", newdata = "Year")
calfcow$Year %<>% as.integer

gp <- ggplot(data = calfcow, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)


