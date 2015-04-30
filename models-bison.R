model1 <- jags_model("model{

  bProductivity ~ dnorm(0, 2^-2)
  bSurvivalCalf ~ dnorm(0, 2^-2)
  bSurvivalAdult ~ dnorm(0, 2^-2)

  sSurvivalCalfYear ~ dunif(0, 2)
  for(i in 1:nYear){
    bSurvivalCalfYear[i] ~ dnorm(0, sSurvivalCalfYear^-2)

    logit(eProductivityYear[i]) <- bProductivity
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfYear[i]
    logit(eSurvivalYearlingYear[i]) <- bSurvivalAdult
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
  }

  bCalves1 ~ dunif(0, 1000)
  bYearlings1 ~ dunif(0, 500)
  bAdults1 ~ dunif(0, 4000)

  bCalves[1] <- bCalves1
  bYearlings[1] <- bYearlings1
  bAdults[1] <- bAdults1

  for(i in 2:nYear){
    bCalves[i] <- bAdults[i-1] / 2 * eSurvivalAdultYear[i-1] * eProductivityYear[i-1]
    bYearlings[i] <- bCalves[i-1] * eSurvivalCalfYear[i-1]
    bAdults[i] <- bYearlings[i-1] * eSurvivalYearlingYear[i-1] + bAdults[i-1] * eSurvivalAdultYear[i-1]
  }

  eCorrection <- 308/365
  for(i in 1:length(YearBison)) {
    eCalves[i] <- bCalves[YearBison[i]] * eSurvivalCalfYear[YearBison[i]]^eCorrection
    eYearlings[i] <- bYearlings[YearBison[i]] * eSurvivalYearlingYear[YearBison[i]]^eCorrection
    eAdults[i] <- bAdults[YearBison[i]] * eSurvivalAdultYear[YearBison[i]]^eCorrection

    eBison[i] <- eCalves[i] + eYearlings[i] + eAdults[i]
    Bison[i] ~ dnorm(eBison[i], 100^-2)
  }

  sDispersionCalves ~ dunif(0, 2)
  for(i in 1:length(Year)) {
    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * eSurvivalYearlingYear[Year[i]]^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * eSurvivalAdultYear[Year[i]]^eCorComp[i]

    eCowsComp[i] <- eAdultsComp[i] / 2

    eDispersionCalves[i] ~ dnorm(0, sDispersionCalves^-2)

    logit(eProportionCalves[i]) <- logit(eCalvesComp[i] / eCowsComp[i]) + eDispersionCalves[i]
    eProportionCowsYearlings[i] <- eCowsComp[i] / (eYearlingsComp[i] + eCowsComp[i])

    Calves[i] ~ dbin(eProportionCalves[i], Cows[i])
    Cows[i] ~ dbin(eProportionCowsYearlings[i], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    prediction[i] <- bCalves[Year[i]] + bYearlings[Year[i]] + bAdults[Year[i]]

    logit(eProductivityYear[i]) <- bProductivity
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfYear[Year[i]]
    logit(eSurvivalYearlingYear[i]) <- bSurvivalAdult
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult

    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * eSurvivalYearlingYear[Year[i]]^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * eSurvivalAdultYear[Year[i]]^eCorComp[i]

    eCalfCowRatio[i] <- eCalvesComp[i] /  (eAdultsComp[i] / 2)
    eYearlingCowRatio[i] <- eYearlingsComp[i] /  (eAdultsComp[i] / 2)
  }
}",
modify_data = function (data) {

  bison <- data.frame(Year = data$Year, Bison = data$Bison)
  bison %<>% na.omit %>% unique

  data$YearBison  <- bison$Year
  data$Bison  <- bison$Bison

  data$Year <- data$Year[!is.na(data$Cows)]
  data$Dayte <- data$Dayte[!is.na(data$Cows)]
  data$Calves <- data$Calves[!is.na(data$Cows)]
  data$Yearlings <- data$Yearlings[!is.na(data$Cows)]
  data$Cows <- data$Cows[!is.na(data$Cows)]

  data$YearlingsCows  <- data$Yearlings + data$Cows
  data$Yearlings <- NULL
  data

},
modify_data_derived = function (data) {
  data
},
random_effects = list(bSurvivalCalfYear = "Year", bCalves = "Year",
                      bYearlings = "Year", bAdults = "Year"),
select_data = c("Year", "Bison", "Dayte", "Calves", "Yearlings", "Cows")
)

model2 <- jags_model("model{

  bProductivity ~ dnorm(0, 2^-2)
  bSurvivalCalf ~ dnorm(0, 2^-2)
  bSurvivalAdult ~ dnorm(0, 2^-2)

  bSurvivalCalfWeather ~ dnorm(0, 2^-2)

  sSurvivalCalfYear ~ dunif(0, 2)
  for(i in 1:nYear){
    bSurvivalCalfYear[i] ~ dnorm(0, sSurvivalCalfYear^-2)

    logit(eProductivityYear[i]) <- bProductivity
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfWeather * Rainfall[i] + bSurvivalCalfYear[i]
    logit(eSurvivalYearlingYear[i]) <- bSurvivalAdult
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
  }

  bAdults[1] ~ dunif(0, 4000)
  bCalves[1] ~ dunif(0, 1000)
  bYearlings[1] ~ dunif(0, 500)

  for(i in 2:nYear){
    bCalves[i] <- bAdults[i-1] / 2 * eSurvivalAdultYear[i-1] * eProductivityYear[i-1]
    bYearlings[i] <- bCalves[i-1] * eSurvivalCalfYear[i-1]
    bAdults[i] <- bYearlings[i-1] * eSurvivalYearlingYear[i-1] + bAdults[i-1] * eSurvivalAdultYear[i-1]
  }

  eCorrection <- 308/365
  sBison ~ dunif(0, 100)
  for(i in 1:length(YearBison)) {
    eCalves[i] <- bCalves[YearBison[i]] * eSurvivalCalfYear[YearBison[i]]^eCorrection
    eYearlings[i] <- bYearlings[YearBison[i]] * eSurvivalYearlingYear[YearBison[i]]^eCorrection
    eAdults[i] <- bAdults[YearBison[i]] * eSurvivalAdultYear[YearBison[i]]^eCorrection

    eBison[i] <- eCalves[i] + eYearlings[i] + eAdults[i]
    Bison[i] ~ dnorm(eBison[i], sBison^-2)
  }

  sDispersionCalves ~ dunif(0, 2)
  for(i in 1:length(Year)) {
    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * eSurvivalYearlingYear[Year[i]]^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * eSurvivalAdultYear[Year[i]]^eCorComp[i]

    eCowsComp[i] <- eAdultsComp[i] / 2

    eDispersionCalves[i] ~ dnorm(0, sDispersionCalves^-2)

    logit(eProportionCalves[i]) <- logit(eCalvesComp[i] / eCowsComp[i]) + eDispersionCalves[i]
    eProportionCowsYearlings[i] <- eCowsComp[i] / (eYearlingsComp[i] + eCowsComp[i])

    Calves[i] ~ dbin(eProportionCalves[i], Cows[i])
    Cows[i] ~ dbin(eProportionCowsYearlings[i], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    prediction[i] <- bCalves[Year[i]] + bYearlings[Year[i]] + bAdults[Year[i]]
    logit(eProductivityYear[i]) <- bProductivity
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfWeather * Rainfall[Year[i]] + bSurvivalCalfYear[i]
    logit(eSurvivalYearlingYear[i]) <- bSurvivalAdult
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
    eCalfCowRatio[i] <- bCalves[Year[i]] /  (bAdults[Year[i]] / 2)
    eYearlingCowRatio[i] <- bYearlings[Year[i]] /  (bAdults[Year[i]] / 2)
  }
}",
modify_data = function (data) {

  weather <- data.frame(Year = data$Year, WSI = data$WSI, SummerTemp = data$SummerTemp,
                        Rainfall = data$Rainfall)
  weather %<>% na.omit %>% unique
  weather %<>% arrange(Year)

  stopifnot(nrow(weather) == data$nYear)

  data$WSI  <- weather$WSI
  data$SummerTemp  <- weather$SummerTemp
  data$Rainfall  <- weather$Rainfall

  bison <- data.frame(Year = data$Year, Bison = data$Bison)
  bison %<>% na.omit %>% unique

  data$YearBison  <- bison$Year
  data$Bison  <- bison$Bison

  data$Year <- data$Year[!is.na(data$Cows)]
  data$Dayte <- data$Dayte[!is.na(data$Cows)]
  data$Calves <- data$Calves[!is.na(data$Cows)]
  data$Yearlings <- data$Yearlings[!is.na(data$Cows)]
  data$Cows <- data$Cows[!is.na(data$Cows)]

  data$YearlingsCows  <- data$Yearlings + data$Cows
  data$Yearlings <- NULL
  data

},
modify_data_derived = function (data) {
  data
},
random_effects = list(bSurvivalCalfYear = "Year"),
select_data = c("Year", "Bison", "Dayte", "Calves", "Yearlings", "Cows",
                "WSI", "Rainfall", "SummerTemp")
)

models <- jaggernaut::combine(model1)
