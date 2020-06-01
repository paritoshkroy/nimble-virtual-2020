library(nimble)
DeerEcervi <- read.table('DeerEcervi.txt', header = TRUE)

## Create presence/absence data from counts.
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi>0] <- 1
## Center Length for better interpretation
DeerEcervi$cLength <- DeerEcervi$Length - mean(DeerEcervi$Length)
## Make a factor version of Sex for plotting
DeerEcervi$fSex <- factor(DeerEcervi$Sex)
## Make a factor and id version of Farm
DeerEcervi$fFarm <- factor(DeerEcervi$Farm)
DeerEcervi$farm.ids <- as.numeric(DeerEcervi$fFarm)

DEcode1 <- nimbleCode({
  for(i in 1:2) {
    # Priors for ntercepts and length coefficients for sex = 1,2
    sex.effect[i] ~ dnorm(0, sd = 1000)
    length.effect[i] ~ dnorm(0, sd = 1000)
  }
  
  # Priors for farm random effects and their standard deviation.
  farm.sd ~ dunif(0, 20)
  for(i in 1:num.farms) {
    farm.effect[i] ~ dnorm(0, sd = farm.sd)
  }
  
  # logit link and Bernoulli data probabilities
  for(i in 1:num.animals) {
    logit(disease.probability[i]) <- 
      sex.effect[ sex[i] ] +
      length.effect[ sex[i] ]*cLength[i] +
      farm.effect[ farm.ids[i] ]
    Ecervi.01[i] ~ dbern(disease.probability[i])
  }
})

DEconstants <- list(num.farms = 24,
                    num.animals = 826,
                    cLength = DeerEcervi$cLength,
                    sex = DeerEcervi$Sex,
                    farm.ids = DeerEcervi$farm.ids)

DEdata <- list(Ecervi.01 = DeerEcervi$Ecervi.01)

DEinits1 <- function() {
  list(sex.effect = c(0, 0), 
       length.effect = c(0, 0),
       farm.sd = 1,
       farm.effect = rnorm(24, 0, 1) )       
}

set.seed(123)
DEinits_vals <- DEinits1()

## JAGS-compatible version
DEcode2 <- nimbleCode({
  for(i in 1:2) {
    length.effect[i] ~ dnorm(0, 1.0E-6) # precisions
    sex.effect[i] ~ dnorm(0, 1.0E-6)
  }
  farm.sd ~ dunif(0, 20)
  farm.precision <- 1/(farm.sd*farm.sd)
  
  for(i in 1:num.farms) {
    farm.effect[i] ~ dnorm(0, farm.precision) # precision
  }
  
  for(i in 1:num.animals) {
    logit(disease.probability[i]) <- 
      sex.effect[ sex[i] ] +
      length.effect[ sex[i] ]*cLength[i] +
      farm.effect[ farm.ids[i] ]
    Ecervi.01[i] ~ dbern(disease.probability[i])
  }
})

