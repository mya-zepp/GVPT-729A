rm(list=ls())
library(foreign)
library(mvtnorm) 
suppressPackageStartupMessages(library(tidyverse))
library(broom)
library(modelsummary)

setwd("C:\\Users\\mzepp\\Downloads\\School\\R")
d <- read.dta("cps.dta")|>
  drop_na()

d_cleaned <- d |>
  mutate(homeown = case_when(
    homeown %in% c("own home") ~ 1, homeown %in% c("rent/occupy") ~ 0
  )) |>
  drop_na()


d_cleaned <- d_cleaned |>
  mutate(edu7cat = case_when(
    edu7cat %in% c("hsgrad") ~ 1, edu7cat %in% c("5-8 years", "9-11 years") ~ 0, 
    edu7cat %in% c("1-3 years college", "4 college", "5+ college") ~ 2
  )) |>
  drop_na()

probit.model <- glm(vote ~ close + homeown + edu7cat, family=binomial(link="probit"), data=d_cleaned)
summary(probit.model)


preds <- predict(probit.model, type = "response")
summary(preds)


coefs_probit <- coef(probit.model)
coefs_probit



pprobit <- pnorm(coefs_probit[2]*d_cleaned$close + coefs_probit[3]*d_cleaned$homeown + coefs_probit[4]*d_cleaned$edu7cat + coefs_probit[1])
summary(pprobit)


testp <- preds - pprobit
summary(testp)


px1_1 <- pnorm(coefs_probit[2]*1 + coefs_probit[3]*d_cleaned$homeown + coefs_probit[4]*d_cleaned$edu7cat + coefs_probit[1])
summary(px1_1)


px1_0 <- pnorm(coefs_probit[2]*0 + coefs_probit[3]*d_cleaned$homeown + coefs_probit[4]*d_cleaned$edu7cat + coefs_probit[1])
summary(px1_0)


effectx1 <- px1_1 - px1_0


summary(cbind(px1_1, px1_0, effectx1))
#the effect of whatever Im testing is -0.16 percentage points 

#now for the confidence intervals 

m1 <- glm(vote ~ close + homeown + edu7cat, family=binomial(link="probit"), data=d_cleaned)
summary(m1)

coefs <- coef(m1)
coefs


d <- d[complete.cases(d_cleaned),]


n_draws <- 1000


set.seed(27)
sim_coefs <- rmvnorm(n_draws, coefs, vcov(m1)) 

rbind(coefs, apply(sim_coefs, 2, mean)) 

p_mean <- NULL
for (i in 1:1000) {
  p_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]**d_cleaned$close + sim_coefs[i,3]*d_cleaned$homeown + sim_coefs[i,4]*d_cleaned$edu7cat ))
}

summary(p_mean)

px1_1_mean <- NULL
for (i in 1:1000) {
  px1_1_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*20 + sim_coefs[i,3]*d_cleaned$homeown + sim_coefs[i,4]*d_cleaned$edu7cat))
}


px1_0_mean <- NULL
for (i in 1:1000) {
  px1_0_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*0 + sim_coefs[i,3]*d_cleaned$homeown + sim_coefs[i,4]*d_cleaned$edu7cat))
}


effectx1_mean <- px1_1_mean - px1_0_mean
summary(effectx1_mean)

margeffx2_mean <- NULL
for (i in 1:1000) {
  margeffx2_mean[i] <- mean(dnorm(sim_coefs[i,1] + sim_coefs[i,2]*d_cleaned$edu7cat + sim_coefs[i,3]*d_cleaned$homeown + sim_coefs[i,4]*d_cleaned$close)*sim_coefs[i,3])
}

lapply(means <- list(p_mean, px1_1_mean, px1_0_mean, effectx1_mean, margeffx2_mean), summary)


q <- do.call("rbind", (lapply(means, quantile, c(.025,.975)))) 
q                                                             
results.p <- cbind(q[,1], lapply(means, mean),q[,2]) 
colnames(results.p) <- c("2.5", "Mean", "97.5")  
rownames(results.p) <- c("Probability Mean", "Probability of Closing date set to 20 mean", "probability of Closing date set to 0 mean", "Effect of changing closing date mean", "Marginal effect mean") 
results.p

q <- do.call("rbind", lapply(means, function(x) quantile(as.numeric(x), c(0.025, 0.975))))
results.p


results.p <- cbind(
  "2.5%"  = q[, 1],
  "Mean"  = sapply(means, mean),
  "97.5%" = q[, 2]
)


rownames(results.p) <- c(
  "Probability Mean",
  "Probability (Closing date = 20)",
  "Probability (Closing date = 0)",
  "Effect of changing closing date",
  "Marginal effect"
)

results.p <- round(results.p, 3)





