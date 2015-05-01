source("header.R")

set_folders("bison")

data <- load_rdata("data")

analysis <- load_analysis()

stopifnot(identical(data, dataset(analysis)))

print(summary(analysis))

save_tables(analysis)

data$Year %<>% as.character %>% as.integer

calfcow <- predict(analysis, parm = "eCalfCowRatio", newdata = "Year")
calfcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = calfcow, aes(x = Year))
gp <- gp + geom_pointrange(aes(y = estimate, ymin = lower, ymax = upper))
gp <- gp + geom_point(data = data, aes(y = Calves / Cows, size = Calves + Cows), alpha = 1/5)
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Calf:Cow Ratio")
gp <- gp + scale_size(name = "Group Size")
gp <- gp + expand_limits(x = c(1999, 2012), y = c(0,1))

gwindow(75, 50)
print(gp)

save_plot("calf_cow", caption = "The predicted calf:cow ratio and composition data by year.")

yearcow <- predict(analysis, parm = "eYearlingCowRatio", newdata = "Year")
yearcow$Year %<>% as.character %>% as.integer

gp <- ggplot(data = yearcow, aes(x = Year))
gp <- gp + geom_pointrange(aes(y = estimate, ymin = lower, ymax = upper))
gp <- gp + geom_point(data = data, aes(y = Yearlings / Cows, size = Yearlings + Cows), alpha = 1/5)
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Yearling:Cow Ratio")
gp <- gp + scale_size(name = "Group Size")
gp <- gp + expand_limits(x = c(1999, 2012), y = c(0,1))

gwindow(75, 50)
print(gp)

save_plot("yearling_cow", caption = "The predicted yearling:cow ratio by year.")

herd <- predict(analysis, newdata = "Year")
herd$Year %<>% as.character %>% as.integer
herd$Year <- herd$Year + 1
herd %<>% filter(Year <= 2012)

gp <- ggplot(data = herd, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = data, aes(y = Bison))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dotted")
gp <- gp + geom_line(aes(y = upper), linetype = "dotted")
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Herd Size", labels = comma)
gp <- gp + expand_limits(x = c(1999, 2012), y = c(0,1))

gwindow(60, 50)
print(gp)

save_plot("herd", caption = "The predicted herd size in March by year.")

scalf <- predict(analysis, parm = "eSurvivalCalfYear", newdata = "Year")
scalf$Year %<>% as.character %>% as.integer

scalf %<>% filter(Year < 2012)

gp <- ggplot(data = scalf, aes(x = Year, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_x_continuous(breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Calf Survival (%)", labels = percent)
gp <- gp + expand_limits(x = c(1999, 2012), y = c(0,1))

gwindow(60, 50)
print(gp)

save_plot("scalf", caption = "The predicted calf survival by year.")
