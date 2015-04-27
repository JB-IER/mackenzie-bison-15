model1 <- jags_model("model{

  bProductivity ~ dunif(0, 1)

  bSurvivalCalf ~ dnorm(0, 2^-2)
  bSurvivalYearling ~ dnorm(0, 2^-2)
  bSurvivalAdult ~ dnorm(0, 2^-2)

  sSurvivalCalfYear ~ dunif(0, 2)
  for(i in 1:nYear){
    bSurvivalCalfYear[i] ~ dnorm(0, sSurvivalCalfYear^-2)
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfYear[i]
    logit(eSurvivalYearlingYear[i]) <- bSurvivalYearling
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
  }

  bCalves[1] ~ dunif(0, 800)
  bYearlings[1] ~ dunif(0, 400)
  bAdults[1] ~ dunif(0, 4000)

  for(i in 2:nYear){
    bCalves[i] <- bAdults[i-1] / 2 * eSurvivalAdultYear[i-1] * bProductivity
    bYearlings[i] <- bCalves[i-1] * eSurvivalCalfYear[i-1]
    bAdults[i] <- bYearlings[i-1] * eSurvivalYearlingYear[i-1] + bAdults[i-1] * eSurvivalAdultYear[i-1]
  }

  eCorrection <- 308/365
  sBison ~ dunif(0, 100)
  for(i in 1:length(YearBison)) {
    eCalves[i] <- bCalves[YearBison[i]] + eSurvivalCalfYear[YearBison[i]]^eCorrection
    eYearlings[i] <- bYearlings[YearBison[i]] + eSurvivalYearlingYear[YearBison[i]]^eCorrection
    eAdults[i] <- bAdults[YearBison[i]] + eSurvivalAdultYear[YearBison[i]]^eCorrection

    eBison[i] <- eCalves[i] + eYearlings[i] + eAdults[i]
    Bison[i] ~ dnorm(eBison[i], sBison^-2)
  }

  sOverDispersion ~ dunif(0, 5)
  for(i in 1:length(Year)) {
    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] + eSurvivalCalfYear[Year[i]]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] + eSurvivalYearlingYear[Year[i]]^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] + eSurvivalAdultYear[Year[i]]^eCorComp[i]

    eCowsComp[i] <- eAdultsComp[i] / 2
    eProportionCalves[i] <- eCalvesComp[i] / eCowsComp[i]
    eOverDispersion[i] ~ dnorm(0, sOverDispersion^-2)
    logit(eProportionCowsYearlings[i]) <- ilogit(eCowsComp[i] / (eYearlingsComp[i] + eCowsComp[i])) + eOverDispersion[i]

    Calves[i] ~ dbin(eProportionCalves[Year[i]], Cows[i])
    Cows[i] ~ dbin(eProportionCowsYearlings[Year[i]], YearlingsCows[i])
  }
}",
derived_code = "data{
  for(i in 1:length(Year)) {
    prediction[i] <- bCalves[Year[i]] + bYearlings[Year[i]] + bAdults[Year[i]]
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf + bSurvivalCalfYear[Year[i]]
    logit(eCalfCowRatio[i]) <- bCalves[Year[i]] /  (bAdults[Year[i]] / 2)
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

models <- jaggernaut::combine(model1)
