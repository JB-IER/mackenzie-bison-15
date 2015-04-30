model1 <- jags_model("model{

  bProductivity ~ dunif(0, 1)
  bSurvivalAdult ~ dunif(0, 1)

  bSurvivalCalf ~ dnorm(0, 2^-2)
  bSurvivalCalfPDO ~ dnorm(0, 2^-2)
  sSurvivalCalfYear ~ dunif(0, 2)
  for(i in 1:nYear){
    bSurvivalCalfYear[i] ~ dnorm(0, sSurvivalCalfYear^-2)
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfPDO * PDO[i] + bSurvivalCalfYear[i]
  }

  bYearlings1 ~ dunif(0, 500)
  bAdults1 ~ dunif(0, 4000)

  bCalves[1] <- bAdults1 / 2 * bProductivity
  bYearlings[1] <- bYearlings1
  bAdults[1] <- bAdults1

  for(i in 2:nYear){
    bCalves[i] <- bAdults[i-1] / 2 * bSurvivalAdult * bProductivity
    bYearlings[i] <- bCalves[i-1] * eSurvivalCalfYear[i-1]
    bAdults[i] <- (bYearlings[i-1] + bAdults[i-1]) * bSurvivalAdult
  }

  eCorrection <- 308/365
  for(i in 1:length(YearBison)) {
    eCalves[i] <- bCalves[YearBison[i]] * eSurvivalCalfYear[YearBison[i]]^eCorrection
    eYearlings[i] <- bYearlings[YearBison[i]] * bSurvivalAdult^eCorrection
    eAdults[i] <- bAdults[YearBison[i]] * bSurvivalAdult^eCorrection

    eBison[i] <- eCalves[i] + eYearlings[i] + eAdults[i]
    Bison[i] ~ dnorm(eBison[i], 100^-2)
  }

  sDispersionCalves ~ dunif(0, 2)
  for(i in 1:length(Year)) {
    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * bSurvivalAdult^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * bSurvivalAdult^eCorComp[i]

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

    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfPDO * PDO[i] + bSurvivalCalfYear[Year[i]]

    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[i]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * bSurvivalAdult^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * bSurvivalAdult^eCorComp[i]

    eCalfCowRatio[i] <- eCalvesComp[i] /  (eAdultsComp[i] / 2)
    eYearlingCowRatio[i] <- eYearlingsComp[i] /  (eAdultsComp[i] / 2)
  }
}",
modify_data = function (data) {

  env <- data.frame(Year = data$Year, PDO = data$PDO)
  env %<>% na.omit %>% unique
  env %<>% arrange(Year)

  stopifnot(nrow(env) == data$nYear)

  data$PDO  <- env$PDO

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
select_data = c("Year", "Bison", "Dayte", "Calves", "Yearlings", "Cows", "PDO")
)

models <- jaggernaut::combine(model1)
