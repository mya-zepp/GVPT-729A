
# Sample Stata code to implement the simulation method using the observed value approach for a binary dependent variable. 
# Note that this has been updated(for the better we hope) since publication of the article, so not everything will match SI Section C.
# Prior to using this you should be comfortable with the .R file named "probit obs value without CIs example Hanmer and Kalkan AJPS.R".

## Please cite Hanmer and Kalkan (2013) as recommending use of the observed value approach.  The citation follows: 
## Hanmer, Michael J. and Kerem Ozan Kalkan. 2013. "Behind the Curve: Clarifying the Best Approach to Calculating Predicted Probabilities and Marginal Effects from Limited Dependent Variable Models." American Journal of Political Science 57(1): 263-277.

# 1a) Open the data set (this example uses: Hanmer Kalkan AJPS Data for appendix example.dta).

rm(list=ls())
library(foreign)

install.packages("mvtnorm")

library(mvtnorm)  # for sampling from multivariate normal dist

setwd("C:\\Users\\mzepp\\Downloads\\School\\R")
d <- read.dta("Hanmer Kalkan AJPS Data for appendix example.dta")

## 1b) Run the model.

m1 <- glm(y ~ x1 + x2, family=binomial(link="probit"), data=d)
summary(m1)

#Create coefs object to store the coefficients and streamline the code
coefs <- coef(m1)
coefs

## 1c) Drop observations that are not in the model so that the quantities of 
# interest are not calculated for those observations.
## Note that there aren't any missing values in the example data set 
# but this is something you should do with your data.

d <- d[complete.cases(d),]

## 2a) Create a mean vector for the coefficients and covariance matrix, 
# draw 1000 sets of coefficients from the multivariate normal using this info.
## Note that the random number seed is set to 17 (my last number in youth hockey).
## Using the same seed (whatever you pick) for all of your runs is helpful for replication purposes.

# set number of draws
n_draws <- 1000

# draw coefficients from posterior distribution
set.seed(17)
sim_coefs <- rmvnorm(n_draws, coefs, vcov(m1)) 

# Examine the coefficients and check that the means are close to the original estimates.
# if they are not, something is wrong.  

rbind(coefs, apply(sim_coefs, 2, mean)) 

# 3)  For each simulated set of coefficients (this was set to 1000 in step 2a above) 
# create a variable that contains the quantity of interest for each observation for 
# each set of simulated coefficients (i.e. loop over the 1000 sets of simulated coefficients).
# For each quantity of interest, this creates 1000 new variables with values for 
# each observation in the data set.  They are structured as follows.
# This process continues for each of the 1000 simulated sets of coefficients.

# Then the mean of each of the new variables for a given quantity of interest is stored in a new
# variable with a name ending in _mean, this is what you ultimately want to use (see steps 4 and 5 below).
# Row 1 for this new variable ending in _mean contains the mean across all of the observations 
# using the first set of simulated coefficients (i.e. the mean of the first new variable described above).
# Row 2 for this new variable ending in _mean contains the mean across all of the observations 
# using the second set of simulated coefficients (i.e. the mean of the second new variable described above).
# This continues through row 1000 which contains the mean across all of the observations using 
# the thousandth set of simulated coefficients (i.e. the mean of the thousandth new variable described above).


# 3a) Calculate the predicted probability of success with all variables set to their observed values.
# This is the baseline predicted probability of success from the model without manipulating any of the variables.
# This just serves as a check to make sure things are working well before you run the counterfactual analyses.
# Start by creating a variable named p_mean that is set to a missing value and is then filled in after each 
# of the 1000 variables are created in the loop. 
# Next, in the loop, calculate the quantity of interest for each observation and each set of simulated 
# coefficients, looping over the 1000 sets of simulated coefficients.
# Finally, fill in the p_mean variable with the mean across all observations from each set of simulated coefficients.

p_mean <- NULL
for (i in 1:1000) {
  p_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*d$x1 + sim_coefs[i,3]*d$x2))
}

# Note that, in R, we don't need to keep the intermediate variables. We store the mean directly. 
# check that your results are sensible.
# The result from the command below should be close to what you get when your run the model and 
# use the coefficients to get the baseline prediction.
# That is, compare the mean from below to the result from steps *3a1 (or *3a2) 
# from the "probit obs value without CIs example Hanmer and Kalkan AJPS.R" file.
# If the means are not similar something went wrong with your code above.
# Note that the n for the code below will matches the number of simulations you ran, here that is 1000.

summary(p_mean)

# 3b) Calculate the predicted probability of success when x1=1 and x2 is set to its observed values.
# Start by creating a variable named px1_1_mean that is set to a missing value and is then filled in 
# after each of the 1000 variables are created.
# Next, in the loop, calculate the quantity of interest for each observation and each set of 
# simulated coefficients, looping over the 1000 sets of simulated coefficients.
# Finally, fill in the px1_1_mean variable with the mean across all observations from each set of simulated coefficients.

px1_1_mean <- NULL
for (i in 1:1000) {
  px1_1_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*20 + sim_coefs[i,3]*d$x2))
}
z
px1_0_mean <- NULL
for (i in 1:1000) {
  px1_0_mean[i] <- mean(pnorm(sim_coefs[i,1] + sim_coefs[i,2]*0 + sim_coefs[i,3]*d$x2))
}

effectx1_mean <- px1_1_mean - px1_0_mean
summary(effectx1_mean)

# 3e) Calculate the marginal effect of x2, when x1 is set to its observed values.
# Start by creating a variable named margeffx2_mean that is set to a missing value and is 
# then filled in after each of the 1000 variables are created.
# Next, in the loop, calculate the quantity of interest for each observation and each set 
# of simulated coefficients, looping over the 1000 sets of simulated coefficients.
# Finally, fill in the margeffx2_mean variable with the mean across all observations from each set of simulated coefficients.

margeffx2_mean <- NULL
for (i in 1:1000) {
  margeffx2_mean[i] <- mean(dnorm(sim_coefs[i,1] + sim_coefs[i,2]*d$x1 + sim_coefs[i,3]*d$x2)*sim_coefs[i,3])
}

# 4) For each of the quantities of interest report the mean across the 1000 sets of simulated coefficients.
# The means should be very similar to your calculations from the single run of the data.
# (i.e. those from "Logit obs value without CIs example Hanmer and Kalkan.R").

lapply(means <- list(p_mean, px1_1_mean, px1_0_mean, effectx1_mean, margeffx2_mean), summary)


# A note on the code here: I create a new list 'means' with five elements, and did this inside 
# lapply() to make the code more efficient. Each element in the list holds all of the values
# stored in p_mean, px1_1_mean, px1_0_mean, effectx1_mean and margeffx2_mean. I then summarize
# each element using lapply(data,summary). The reason why I opted for a list instead of a matrix 
# is that the percentile function works more efficiently, in my view, when passed through lapply. 
# A matrix can then be created with the results. There are, as always with R, alternative ways to do this, 
# and you're welcome to experiment around producing the same results with those. 

# Here, e.g. the mean effect of changing x1 from 0 to 1 is to increase the 
# probability of success by 1.2 percentage points, on average, holding all 
# else constant. That is, the mean effect for x1 is 0.012, which when multiplied 
# by 100 can be discussed in terms of percentage points.
# 
# 5) For each of the quantities of interest report the 95% confidence 
# intervals across the 1000 sets of simulated coefficients.

# Get interval for effect for all variables at once:

q <- do.call("rbind", (lapply(means, quantile, c(.025,.975)))) # Get quntiles for each element in the list, 
q                                                              # and then rbind into a matrix in the same line.
results.p <- cbind(q[,1], lapply(means, mean),q[,2]) # Create matrix with lower CI, mean, upper CI to visualize results better
colnames(results.p) <- c("2.5", "Mean", "97.5") # Change the column names  
rownames(results.p) <- c("p_mean", "px1_1_mean", "px1_0_mean", "effectx1_mean", "margeffx2_mean") # Change the row names
results.p # Voilà

# For example, the 95% confidence interval on the effect of changing x1 from 0 to 1 is [-.029, .052].

