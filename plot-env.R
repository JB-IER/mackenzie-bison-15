source("header.R")

set_folders("bison")

env <- load_rdata("data")

set_folders("env")

env$Year %<>% as.character %>% as.integer

env %<>% select(Year, WSI, Rainfall, SummerTemp, PDO)
env %<>% gather("Variable", "Value", -Year)

standardise <- function (d) {
  d$Value <- d$Value - mean(d$Value, na.rm = TRUE)
  d$Value <- d$Value / sd(d$Value, na.rm = TRUE)
  d
}

env %<>% ddply(.(Variable), standardise)
levels(env$Variable) <- list("Pacific Decadal Oscillation" = "PDO",
                             "Winter Severity Index" = "WSI",
                             "Rainfall" = "Rainfall",
                             "Summer Air Temperature" = "SummerTemp")

gp <- ggplot(data = env, aes(x = as.integer(as.character(Year)), y = Value))
gp <- gp + facet_wrap(~Variable)
gp <- gp + geom_hline(yintercept = 0)
gp <- gp + geom_line(color = "grey50")
gp <- gp + geom_point()
gp <- gp + expand_limits(y = c(-3, 3))
gp <- gp + scale_x_continuous(name = "Year", breaks = seq(2000, 2012, by = 4))
gp <- gp + scale_y_continuous(name = "Standardised Value")
gp <- gp + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

gwindow()
print(gp)

save_plot("senv", caption = "Standardised environmental variables by year")
