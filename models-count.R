description <- c(
  "`bCount`" = "Intercept for `log(eCount)`",
  "`sDispersion`" = "SD of `eDispersion`",
  "`eDispersion[i]`" = "Overdispersion of i*th* count",
  "`eCount[i]`" = "Expected value of i*th* count",
  "`Count[i]`" = "The i*th* count")

model1 <- jags_model("model{
  bCount ~ dnorm(0, 5^-2)
  for (i in 1:length(Count)) {
    log(eCount[i]) <- bCount
    Count[i] ~ dpois(eCount[i])
  }
}",
derived_code = "data{
  for (i in 1:length(Count)) {
    log(eCount[i]) <- bCount
    prediction[i] <- eCount[i]
    residual[i] <- log(Count[i] + 0.1) - log(eCount[i] + 0.1) 
  }
}",
select_data = c("Count")
)

model2 <- jags_model("model{
  bCount ~ dnorm(0, 5^-2)
  sDispersion ~ dunif(0, 5)
  for (i in 1:length(Count)) {
    log(eCount[i]) <- bCount
    eDispersion[i] ~ dgamma(1 / sDispersion^2, 1 / sDispersion^2)
    Count[i] ~ dpois(eCount[i] * eDispersion[i])
  }
}",
  derived_code = "data{
  for (i in 1:length(Count)) {
    log(eCount[i]) <- bCount
    prediction[i] <- eCount[i]
    residual[i] <- log(Count[i] + 0.1) - log(eCount[i] + 0.1) 
  }
}",
  select_data = c("Count")
)

models <- jaggernaut::combine(model1, model2)
