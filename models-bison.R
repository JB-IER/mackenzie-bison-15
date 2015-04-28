model1 <- jags_model("model{

  bProductivity ~ dunif(0, 1)
  bSurvivalCalf ~ dunif(0, 1)
  bSurvivalAdult ~ dunif(0, 1)

  for(i in 1:nYear){
    eProductivityYear[i] <- bProductivity
    eSurvivalCalfYear[i] <- bSurvivalCalf
    eSurvivalYearlingYear[i] <- bSurvivalAdult
    eSurvivalAdultYear[i] <- bSurvivalAdult
  }

  bAdults[1] ~ dunif(0, 2500)
  bCalves[1] ~ dunif(0, 800)
  bYearlings[1] ~ dunif(0, 400)

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
  sDispersionYearlings ~ dunif(0, 2)
  for(i in 1:length(Year)) {
    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * eSurvivalYearlingYear[Year[i]]^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * eSurvivalAdultYear[Year[i]]^eCorComp[i]

    eCowsComp[i] <- eAdultsComp[i] / 2

    eDispersionCalves[i] ~ dnorm(0, sDispersionCalves^-2)
    eDispersionYearlings[i] ~ dnorm(0, sDispersionYearlings^-2)

    logit(eProportionCalves[i]) <- logit(eCalvesComp[i] / eCowsComp[i]) + eDispersionCalves[i]
    logit(eProportionCowsYearlings[i]) <- logit(eCowsComp[i] / (eYearlingsComp[i] + eCowsComp[i])) + eDispersionYearlings[i]

    Calves[i] ~ dbin(eProportionCalves[i], Cows[i])
    Cows[i] ~ dbin(eProportionCowsYearlings[i], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    prediction[i] <- bCalves[Year[i]] + bYearlings[Year[i]] + bAdults[Year[i]]
    eProductivityYear[i] <- bProductivity
    eSurvivalCalfYear[i] <- bSurvivalCalf
    eSurvivalYearlingYear[i] <- bSurvivalAdult
    eSurvivalAdultYear[i] <- bSurvivalAdult
    eCalfCowRatio[i] <- bCalves[Year[i]] /  (bAdults[Year[i]] / 2)
    eYearlingCowRatio[i] <- bYearlings[Year[i]] /  (bAdults[Year[i]] / 2)
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
select_data = c("Year", "Bison", "Dayte", "Calves", "Yearlings", "Cows")
)

model2 <- jags_model("model{

  bProportionCalves ~ dnorm(0, 2^-2)
  bProportionYearlings ~ dnorm(0, 2^-2)

  sProportionCalves ~ dunif(0, 2)
  sProportionYearlings ~ dunif(0, 2)
  for(i in 1:nYear){
    bProportionCalvesYear[i] ~ dnorm(0, sProportionCalves^-2)
    bProportionYearlingsYear[i] ~ dnorm(0, sProportionYearlings^-2)

    logit(eProportionCalvesYear[i]) <- bProportionCalves + bProportionCalvesYear[i]
    logit(eProportionYearlingsYear[i]) <- bProportionYearlings + bProportionYearlingsYear[i]
  }

  for(i in 1:length(Year)) {

    Calves[i] ~ dbin(eProportionCalvesYear[Year[i]], Cows[i])
    Cows[i] ~ dbin(eProportionYearlingsYear[Year[i]], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    logit(eProportionCalvesYear[i]) <- bProportionCalves + bProportionCalvesYear[Year[i]]
    logit(eProportionYearlingsYear[i]) <- bProportionYearlings + bProportionYearlingsYear[Year[i]]
  }
  eCalfCowRatio <- eProportionCalvesYear
  eYearlingCowRatio <- eProportionYearlingsYear / (1 - eProportionYearlingsYear)
  prediction <- eProportionCalvesYear
}",
modify_data = function (data) {

  data$Year <- data$Year[!is.na(data$Cows)]
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
select_data = c("Year", "Calves", "Yearlings", "Cows")
)

model3 <- jags_model("model{

  bProportionCalves ~ dnorm(0, 2^-2)
  bProportionYearlings ~ dnorm(0, 2^-2)

  sProportionCalves ~ dunif(0, 2)
  sProportionYearlings ~ dunif(0, 2)
  for(i in 1:nYear){
    bProportionCalvesYear[i] ~ dnorm(0, sProportionCalves^-2)
    bProportionYearlingsYear[i] ~ dnorm(0, sProportionYearlings^-2)

    eProportionCalvesYear[i] <- bProportionCalves + bProportionCalvesYear[i]
    eProportionYearlingsYear[i] <- bProportionYearlings + bProportionYearlingsYear[i]
  }

  sDispersionCalves ~ dunif(0, 2)
  sDispersionYearlings ~ dunif(0, 2)
  for(i in 1:length(Year)) {

    eDispersionCalves[i] ~ dnorm(0, sDispersionCalves^-2)
    eDispersionYearlings[i] ~ dnorm(0, sDispersionYearlings^-2)

    logit(eProportionCalves[i]) <- eProportionCalvesYear[Year[i]] + eDispersionCalves[i]
    logit(eProportionCowsYearlings[i]) <- eProportionYearlingsYear[Year[i]] + eDispersionYearlings[i]

    Calves[i] ~ dbin(eProportionCalves[i], Cows[i])
    Cows[i] ~ dbin(eProportionCowsYearlings[i], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    logit(eProportionCalvesYear[i]) <- bProportionCalves + bProportionCalvesYear[Year[i]]
    logit(eProportionYearlingsYear[i]) <- bProportionYearlings + bProportionYearlingsYear[Year[i]]
  }
  eCalfCowRatio <- eProportionCalvesYear
  eYearlingCowRatio <- (1 - eProportionYearlingsYear) / eProportionYearlingsYear
  prediction <- eProportionCalvesYear
}",
modify_data = function (data) {

  data$Year <- data$Year[!is.na(data$Cows)]
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
select_data = c("Year", "Calves", "Yearlings", "Cows")
)

models <- jaggernaut::combine(model1)
