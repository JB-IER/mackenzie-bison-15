model1 <- jags_model("model{

  bProductivity ~ dunif(0, 1)

  bSurvivalCalf ~ dnorm(0, 2^-2)
  bSurvivalYearling ~ dnorm(0, 2^-2)
  bSurvivalAdult ~ dnorm(0, 2^-2)

  for(i in 1:nYear){
    logit(eSurvivalCalfYear[i]) <- bSurvivalCalf
    logit(eSurvivalYearlingYear[i]) <- bSurvivalYearling
    logit(eSurvivalAdultYear[i]) <- bSurvivalAdult
  }

  bCalves[1] ~ dunif(0, 800)
  bYearlings[1] ~ dunif(0, 400)
  bAdults[1] ~ dunif(0, 4000)
  bBison[1] <- bCalves[1] + bYearlings[1] + bAdults[1]
  for(i in 2:nYear){
    bCalves[i] <- bAdults[i-1] / 2 * bProductivity
    bYearlings[i] <- bCalves[i-1] * eSurvivalCalfYear[i-1]
    bAdults[i] <- bYearlings[i-1] * eSurvivalYearlingYear[i-1] + bAdults[i-1] * eSurvivalAdultYear[i-1]
    bBison[i] <- bCalves[i] + bYearlings[i] + bAdults[i]
  }

  sBison ~ dunif(0, 100)
  for(i in 1:length(YearBison)) {
    Bison[i] ~ dnorm(bBison[YearBison[i]], sBison^-2)
  }

  for(i in 1:nYear){
    eProportionYearlingsCows[i] <- bYearlings[i] / (bYearlings[i] + bAdults[i] / 2)
  }

  for(i in 1:length(Cows)) {
    Cows[i] ~ dbin(1 - eProportionYearlingsCows[Year[i]], YearlingsCows[i])
  }
}",
modify_data = function (data) {

  bison <- data.frame(Year = data$Year, Bison = data$Bison)
  bison %<>% na.omit %>% unique

  data$YearBison  <- bison$Year
  data$Bison  <- bison$Bison

  data$Year <- data$Year[!is.na(data$Cows)]
  data$Calves <- data$Calves[!is.na(data$Cows)]
  data$Yearlings <- data$Yearlings[!is.na(data$Cows)]
  data$Cows <- data$Cows[!is.na(data$Cows)]

  data$YearlingsCows  <- data$Yearlings + data$Cows
  data$Yearlings <- NULL

  print(data)
  data

},
select_data = c("Year", "Bison", "Dayte", "Calves", "Yearlings", "Cows")
)

models <- jaggernaut::combine(model1)