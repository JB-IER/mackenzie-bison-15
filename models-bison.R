description <- c("`bProductivity`" = "Probability of a female adult calving",
"`bSurvivalAdult`" = "Adult and yearling survival",
"`bSurvivalCalf`" = "Calf survival",
"`sSurvivalCalfYear`" = "SD of the effect of year on `bSurvivalCalf`",
 "`eSurvivalCalfYear[i]`" = "Calf survival from the `i`$^{th}$ to `i+1`$^{th}$ year",
 "`bYearlings1`" = "Number of yearlings at the start of the first year",
 "`bAdults1`" = "Number of adults at the start of the first year",
 "`bCalves[i]`" = "Number of calves at the start of the `i`$^{th}$ year",
 "`bYearlings[i]`" = "Number of yearlings at the start of the `i`$^{th}$ year",
 "`bAdults[i]`" = "Number of adults at the start of the `i`$^{th}$ year",
 "`eCorrection`" = "Survival correction for the timing of the herd size estimates",
 "`YearBison[i]`" = "The year of the `i`$^{th}$ herd size estimate",
 "`Bison[i]`" = "The `i`$^{th}$ herd size estimate",
 "`sDispersionCalves`" = "SD of the extra-binomial variation in cow with calf clustering",
 "`Dayte[i]`" = "Day of the year of the `i`$^{th}$ composition observation",
 "`eProportionCalves[i]`" = "Expected proportion of cows with a calf in the `i`$^{th}$ composition observation",
 "`eProportionCowsYearlings[i]`" = "Expected proportion of cows and yearlings that are cows in the `i`$^{th}$ composition observation",
 "`Calves[i]`" = "Number of calves in the `i`$^{th}$ composition observation",
"`YearlingsCows[i]`" = "Number of yearlings and cows in the `i`$^{th}$ composition observation",
 "`Cows[i]`" = "Number of cows in the `i`$^{th}$ composition observation")

model1 <- jags_model("model{

  bProductivity ~ dunif(0, 1)
  bSurvivalAdult ~ dunif(0, 1)
  bSurvivalCalf ~ dunif(0, 1)

  sSurvivalCalfYear ~ dunif(0, 2)
  for(i in 1:nYear){
    bSurvivalCalfYear[i] ~ dnorm(0, sSurvivalCalfYear^-2)
    logit(eSurvivalCalfYear[i]) <- logit(bSurvivalCalf) + bSurvivalCalfYear[i]
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
    Bison[i] ~ dnorm(eBison[i], 250^-2)
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
    eCorrection <- 308/365

  for(i in 1:length(Year)) {

    logit(eSurvivalCalfYear[i]) <- logit(bSurvivalCalf) + bSurvivalCalfYear[Year[i]]

    prediction[i] <- bCalves[Year[i]] * eSurvivalCalfYear[i]^eCorrection + (bYearlings[Year[i]] + bAdults[Year[i]]) * bSurvivalAdult^eCorrection

    eCorComp[i] <- ((Dayte[i] - 135) / 365)
    eCalvesComp[i] <- bCalves[Year[i]] * eSurvivalCalfYear[i]^eCorComp[i]
    eYearlingsComp[i] <- bYearlings[Year[i]] * bSurvivalAdult^eCorComp[i]
    eAdultsComp[i] <- bAdults[Year[i]] * bSurvivalAdult^eCorComp[i]

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

models <- jaggernaut::combine(model1)
