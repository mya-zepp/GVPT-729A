
# GVPT 729a logit and probit examples using cps00for729a.dta.
# This runs logit and probit and calculates predicted probabilities for all 
# of the valid observations and then examines the effect of changing closing date.


rm(list=ls())
library(foreign)

setwd("C:\\Users\\mzepp\\Downloads\\School\\R")
d <- read.dta("cps00for729a.dta") # Import dataset


#install a few packages we will use.

install.packages("arm")

install.packages("plyr")

# Run logit on vote.

logit.model <- glm(vote ~ close + as.numeric(age) + as.numeric(edu7cat), family=binomial(link="logit"), data=d)
summary(logit.model)
sample <- d[complete.cases(d$vote, d$close, d$age, d$edu7cat)==T,]# Regression sample

#education - as education increases, likelihood to vote increases on average holding all else constant. p value is small so its statistically significant
# age - as age increases, likelihood to vote increases on average holding all else constant. p value is small so its statistically significant
# despite the coefficients being different sizes you cannot compare education and age because they are on different scales

# BE SURE TO TAKE NOTES ON THE INTERPRETATION OF THE RESULTS.
# 
# LET'S NOW MOVE TO THE MECHANICS OF OBTAINING THE PREDICTED PROBABILITIES.

# Get the predicted probabilities for the valid observations.

preds <- predict(logit.model, type = "response")
options(digits=4)
summary(preds)

# List the coefficients.
coefs <- coef(logit.model)
coefs

# Calculate the predicted probability by hand, first the way I learned before 
# I was smart enough to look at the Stata manual.
# Note that we are calculating the predictions just for those cases that are in the sample.

plogit <- 1/(1+exp(-1*((-0.00752053*sample$close) + (0.04066068*as.numeric(sample$age))
                       + (0.6480677*as.numeric(sample$edu7cat)) + (-3.930361 ))))
summary(plogit)

# What a pain it is to get all of the coefficients and paste them in & it is harder to check as well.
# So here is the better approach that uses the names of the coefficients as R stores them, through
# the object 'coefs_logit' we created to make the code easier to type and read.

plogit2 <- 1/(1+exp(-1*(coefs['(intercept)'] + coefs['close']*sample$close + coefs['age']*as.numeric(sample$age) + 
                        coefs['edu7cat']*as.numeric(sample$edu7cat)))) 

## Or equivalently:   
plogit2 <- 1/(1+exp(-1*(coefs[1] + coefs[2]*sample$close + coefs[3]*as.numeric(sample$age) + 
                        coefs[4]*as.numeric(sample$edu7cat)))) 
## I will use the numeric one from now on, but the other option is equally fine
summary(plogit2)


# Here is the same thing using the invlogit feature.
library(arm)
plogit3 <- invlogit(coefs_logit[2]*sample$close + coefs_logit[3]*as.numeric(sample$age) + 
                      coefs_logit[4]*as.numeric(sample$edu7cat) + coefs_logit[1])
summary(plogit3)

plogit3 <- invlogit(coefs[2]*sample$close + coefs[3]*as.numeric(sample$age) + 
                      coefs[4]*as.numeric(sample$edu7cat) + coefs[1])
summary(plogit3)

# Create a variable so that the R and hand calculations can be compared.
testl <- preds - plogit
testl2 <- preds - plogit2
testl3 <- preds - plogit3
summary(cbind(testl,testl2,testl3))

# So, the difference is all zeros (even with the less precise method to generate the plogit variable).

# Now probit.

probit.model <- glm(vote ~ close + as.numeric(age) + as.numeric(edu7cat), family=binomial(link="probit"), data=d)
summary(probit.model)

# Get the predicted probabilities for the valid observations.

preds.probit <- predict(probit.model, type = "response")
summary(preds.probit)

# List the coefficients.
coefs_probit <- coef(probit.model)
coefs_probit

# calculate the predicted probability by hand, using the "better approach".

pprobit <- pnorm(coefs_probit[2]*sample$close + coefs_probit[3]*as.numeric(sample$age) + 
                    coefs_probit[4]*as.numeric(sample$edu7cat) + coefs_probit[1])
summary(pprobit)

# create a variable so that the R and hand calculations can be compared.

testp <- preds.probit - pprobit
summary(testp)

# Continuing with probit, what is the effect of changing closing date so that all have close = 0.
# 
# compute predicted prob setting close = 0 for all.

ppclose0 <- pnorm(coefs_probit[2]*0 + coefs_probit[3]*as.numeric(sample$age) + 
                       coefs_probit[4]*as.numeric(sample$edu7cat) + coefs_probit[1])

# Now compute the effect, which we might define as the difference between the predicted turnout rate with close = 0 for all and the status quo.

effect <- ppclose0 - pprobit
summary(effect)

# Now examine the effect by education (R&W (1980) theorize that the effect should decrease as education increases).
library(plyr)
sample$effect <- effect
options(digits = 3)
ddply(sample, ~edu7cat, summarise, N = length(edu7cat), mean = mean(effect), sd = sd(effect),
                                   min = min(effect), max = max(effect))

# Note that you can get the same results (mean, min and max) by running.
summary(sample$effect[sample$edu7cat=="0-4 years"])
summary(sample$effect[sample$edu7cat=="5-8 years"])
summary(sample$effect[sample$edu7cat=="9-11 years"])
summary(sample$effect[sample$edu7cat=="hsgrad"])
summary(sample$effect[sample$edu7cat=="1-3 years college"])
summary(sample$effect[sample$edu7cat=="4 college"])
summary(sample$effect[sample$edu7cat=="5 + college"])

# Another way to think about this is to compare a scenario in which none had close = 0 and all had close = 30.
# We already have a prediction for close = 0, so get a prediction for close = 30.

ppclose30 <- pnorm(coefs_probit[2]*30 + coefs_probit[3]*as.numeric(sample$age) + 
                     coefs_probit[4]*as.numeric(sample$edu7cat) + coefs_probit[1])
summary(ppclose30)

# Now compute the effect, which we might define as the difference between the predicted turnout 
# rate with close = 0 for all and close = 30 for all.

effect2 <- ppclose0 - ppclose30
summary(effect2)


# RECALL THAT THESE ARE ESTIMATES, AND THUS SUBJECT TO UNCERTAINTY. SO, WE SHOULD ALSO 
# THINK ABOUT CONFIDENCE INTERVALS AROUND THESE.
# See Hanmer and Kalkan (2013).

