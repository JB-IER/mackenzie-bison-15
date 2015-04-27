source("header.R")

set_folders("input")

load_rdata()

counts$Date %<>% as.Date(format = "%d-%b-%y")
counts$Dayte <- dayte(counts$Date)

bison %<>% select(Year, Bison = HerdN)
bison %<>% na.omit
counts %<>% select(Year = year, Dayte, Calves, Yearlings = yearlings, Cows)

counts %<>% filter(Calves <= Cows)

counts$Calves[is.na(counts$Calves)] <- 0
counts$Yearlings[is.na(counts$Yearlings)] <- 0
counts$Cows[is.na(counts$Cows)] <- 0

weather %<>% rename(Year = bisonyear)
rm(weather)

set_folders("clean")

save_rdata()
