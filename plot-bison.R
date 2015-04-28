source("header.R")

set_folders("bison")

data <- load_rdata("data")

ratio <- data
ratio$Year %<>% as.character %>% as.integer

gp <- ggplot(data = filter(ratio), aes(x = Year, y = Calves / Cows, size = Calves + Cows))
gp <- gp + geom_point()
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(100, 50)
print(gp)

gp <- ggplot(data = ratio, aes(x = Year, y = Yearlings / Cows, size = Yearlings + Cows))
gp <- gp + geom_point()
gp <- gp + scale_y_continuous(name = "Yearling:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(100, 50)
print(gp)

analysis <- load_analysis()

stopifnot(identical(data, dataset(analysis)))


cross_corr(analysis, parm = c("bProductivity", "bSurvivalCalf", "bSurvivalAdult"))

print(summary(analysis))

save_tables(analysis)

bison <- predict(analysis, newdata = "Year")
bison$Year %<>% as.character %>% as.integer

gp <- ggplot(data = bison, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Bison")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

scalf <- predict(analysis, parm = "eSurvivalCalfYear", newdata = "Year")
scalf$Year %<>% as.character %>% as.integer

gp <- ggplot(data = scalf, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Calf Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

calfcow <- predict(analysis, parm = "eCalfCowRatio", newdata = "Year")
calfcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = calfcow, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

prod <- predict(analysis, parm = "eProductivityYear", newdata = "Year")
prod$Year %<>% as.character %>% as.integer

gp <- ggplot(data = prod, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Productivity (%)", labels = percent)
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

yearcow <- predict(analysis, parm = "eYearlingCowRatio", newdata = "Year")
yearcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = calfcow, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Yearling:Cow Ratio")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

sadult <- predict(analysis, parm = "eSurvivalAdultYear", newdata = "Year")
sadult$Year %<>% as.character %>% as.integer

gp <- ggplot(data = sadult, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Adult Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

syear <- predict(analysis, parm = "eSurvivalYearlingYear", newdata = "Year")
syear$Year %<>% as.character %>% as.integer

gp <- ggplot(data = syear, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(name = "Yearly Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)
