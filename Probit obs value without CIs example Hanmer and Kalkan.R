# Version as of 2/23/14.
# Probit obs value without CIs example Hanmer and Kalkan AJPS.do . 
# This file uses Hanmer Kalkan AJPS Data for appendix example.dta.
# The goal is to obtain predicted probabilities and estimated effects after running a probit model.
# To get confidence intervals see: "Simulation method for Stata probit example_SI Section C.do".

#x2 = party ID. 0 strong dem. 6 strong rep
#x1 = gender. 1 = female 0= other
# y = who vote for. Bush = 1. Kerry = 0

# 
# 1. Open the data (do this via the menu).
rm(list=ls())
library(foreign)

setwd("C:\\Users\\mzepp\\Downloads\\School\\R")
d <- read.dta("Hanmer Kalkan AJPS Data for appendix example.dta")


# 2. Run the model.

probit.model <- glm(y ~ x1 + x2, family=binomial(link="probit"), data=d)
summary(probit.model)

# 3. Generate the quantities of interest.
# 
# 3a Calculate the baseline predictions and check your work.
# 
# 3a1. Have R estimate the baseline predicted probability for each of
# the valid observations with all values set to their observed values.

preds <- predict(probit.model, type = "response")
summary(preds)

# List the coefficients.
coefs_probit <- coef(probit.model)
coefs_probit

# 3a2. Calculate the baseline predicted probability of success for each of the valid observations
# with all values set to their observed values using the formula for the probit cdf and R's storage code.

pprobit <- pnorm(coefs_probit[2]*d$x1 + coefs_probit[3]*d$x2 + coefs_probit[1])
summary(pprobit)

# 3a3. Check to be sure they are the same (i.e. difference is always 0) - if they are not your code in 2b is likely wrong in some way.

testp <- preds - pprobit
summary(testp)

#3b. Calculate the predicted probability of success when x1=1 and x2 is set to its observed values.

px1_1 <- pnorm(coefs_probit[2]*1 + coefs_probit[3]*d$x2 + coefs_probit[1])
summary(px1_1)

# 3c. Calculate the predicted probability of success when x1=0 and x2 is set to its observed values.
# Corresponds to 3c in "Simulation method for Stata probit example_SI Section C.do".

px1_0 <- pnorm(coefs_probit[2]*0 + coefs_probit[3]*d$x2 + coefs_probit[1])
summary(px1_0)

# 3d) Calculate the effect of x1 when moving from x1 = 0 to x1 = 1 (i.e. the first difference).

effectx1 <- px1_1 - px1_0

# 4. Display the results.

summary(cbind(px1_1, px1_0, effectx1))

# If you are comfortable with this and could get the marginal effect for x2 and compute discrete differences
# go to "Simulation method for R probit example_SI Section C.R".

# the effect of sex is 1.2 percentage points
