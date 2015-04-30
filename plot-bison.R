source("header.R")

set_folders("bison")

data <- load_rdata("data")

ratio <- data
ratio$Year %<>% as.character %>% as.integer

gp <- ggplot(data = filter(ratio), aes(x = Year, y = Calves / Cows, size = Calves + Cows))
gp <- gp + geom_point()
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 2))
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(100, 50)
print(gp)

save_plot("calf_cow", caption = "The calf-cow composition data by year.")

gp <- ggplot(data = ratio, aes(x = Year, y = Yearlings / Cows, size = Yearlings + Cows))
gp <- gp + geom_point()
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 2))
gp <- gp + scale_y_continuous(name = "Yearling:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(100, 50)
print(gp)

save_plot("yearling_cow", caption = "The yearling-cow composition data by year.")

analysis <- load_analysis()

stopifnot(identical(data, dataset(analysis)))

calfcow <- predict(analysis, parm = "eCalfCowRatio", newdata = "Year")
calfcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = calfcow, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

save_plot("ccratio", caption = "The predicted calf:cow ratio by year.")

yearcow <- predict(analysis, parm = "eYearlingCowRatio", newdata = "Year")
yearcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = yearcow, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Yearling:Cow Ratio")
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

save_plot("ycratio", caption = "The predicted yearling:cow ratio by year.")

cross_corr(analysis, parm = c("bProductivity", "bSurvivalCalf", "bSurvivalAdult"))

print(summary(analysis))

save_tables(analysis)

bison <- predict(analysis, newdata = "Year")
bison$Year %<>% as.character %>% as.integer

gp <- ggplot(data = bison, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Bison")
gp <- gp + expand_limits(y = 0)

gwindow(50)
print(gp)

save_plot("popn", caption = "The predicted herd size by year.")

newdata <- unique(select(data, Year, PDO))
newdata %<>% arrange(Year)

scalf <- predict(analysis, parm = "eSurvivalCalfYear", newdata = newdata)
scalf$Year %<>% as.character %>% as.integer

gp <- ggplot(data = scalf, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Calf Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

save_plot("scalf", caption = "The predicted calf survival by year.")

newdata$Year <- newdata$Year[nrow(newdata)]
pdo <- predict(analysis, parm = "eSurvivalCalfYear", newdata = newdata)

gp <- ggplot(data = pdo, aes(x = PDO, y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dotted")
gp <- gp + geom_line(aes(y = upper), linetype = "dotted")
gp <- gp + scale_x_continuous(name = "Pacific Decadal Oscillation Index")
gp <- gp + scale_y_continuous(name = "Calf Survival (%)", labels = percent)
gp <- gp + expand_limits(y = c(0,1))

gwindow(50)
print(gp)

save_plot("pdo", caption = "The predicted relationship between calf survival and
          the Pacific Decadal Oscillation Index.")
