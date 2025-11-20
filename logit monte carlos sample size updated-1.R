# GVPT 729a Monte Carlos to explore sample size constraints.

# The code below runs logit simulations with logistically distributed disturbances
# the set up follows Nagler (1994).


#install.packages("arm")
library(arm)

# First, with 1000 observations.
rm(list=ls())
set.seed(894169)
n <- 1000
sims <- 1000

b0 <- NULL
b0.se <- NULL
b1 <- NULL
b1.se <- NULL

for (i in 1:sims) {
      x <- runif(n)*10
      u <- runif(n)
      y_star <- -(10) + 2*x # In true model, b0 = -10 and b1 = 2. We decide that.
      q <- exp(y_star)/(1+exp(y_star))
      y <- ifelse(u <= q, 1, 0)
      m1 <- glm(y ~ x, family=binomial(link='logit'))
      b0 <- c(b0, coef(m1)[[1]])
      b0.se <- c(b0.se ,se.coef(m1)[[1]])
      b1 <- c(b1, coef(m1)[[2]])
      b1.se <- c(b1.se, se.coef(m1)[[2]])
      if (i %%50==0) {
        cat(i, "/ ")
      }
      if (i==1000) {
        cat("Done!")
      }
}
apply(cbind(b0, b1, b0.se, b1.se), 2, mean) # The means of each coefficient and SE.
                                            # How do they compare with the true model?

summary(warnings())

# Second, with 200 observations.
n <- 200
sims <- 1000

b0 <- NULL
b0.se <- NULL
b1 <- NULL
b1.se <- NULL

for (i in 1:sims) {
        x <- runif(n)*10
        u <- runif(n)
        y_star <- -(10) + 2*x 
        q <- exp(y_star)/(1+exp(y_star))
        y <- ifelse(u <= q, 1, 0)
        m1 <- glm(y ~ x, family=binomial(link='logit'))
        b0 <- c(b0, coef(m1)[[1]])
        b0.se <- c(b0.se ,se.coef(m1)[[1]])
        b1 <- c(b1, coef(m1)[[2]])
        b1.se <- c(b1.se, se.coef(m1)[[2]])
        if (i %%50==0) {
          cat(i, "/ ")
        }
        if (i==1000) {
          cat("Done!")
        }
}
apply(cbind(b0, b1, b0.se, b1.se), 2, mean) # The means of each coefficient and SE.
                                            # How do they compare with the true model?

summary(warnings())

# Third, with 100 observations.
n <- 100
sims <- 1000

b0 <- NULL
b0.se <- NULL
b1 <- NULL
b1.se <- NULL

for (i in 1:sims) {
        x <- runif(n)*10
        u <- runif(n)
        y_star <- -(10) + 2*x 
        q <- exp(y_star)/(1+exp(y_star))
        y <- ifelse(u <= q, 1, 0)
        m1 <- glm(y ~ x, family=binomial(link='logit'))
        b0 <- c(b0, coef(m1)[[1]])
        b0.se <- c(b0.se ,se.coef(m1)[[1]])
        b1 <- c(b1, coef(m1)[[2]])
        b1.se <- c(b1.se, se.coef(m1)[[2]])
        if (i %%50==0) {
          cat(i, "/ ")
        }
        if (i==1000) {
          cat("Done!")
        }
}
apply(cbind(b0, b1, b0.se, b1.se), 2, mean) # The means of each coefficient and SE.
                                            # How do they compare with the true model?

summary(warnings())


# Last with 75 observations.
n <- 75
sims <- 1000

b0 <- NULL
b0.se <- NULL
b1 <- NULL
b1.se <- NULL

for (i in 1:sims) {
        x <- runif(n)*10
        u <- runif(n)
        y_star <- -(10) + 2*x 
        q <- exp(y_star)/(1+exp(y_star))
        y <- ifelse(u <= q, 1, 0)
        m1 <- glm(y ~ x, family=binomial(link='logit'))
        b0 <- c(b0, coef(m1)[[1]])
        b0.se <- c(b0.se ,se.coef(m1)[[1]])
        b1 <- c(b1, coef(m1)[[2]])
        b1.se <- c(b1.se, se.coef(m1)[[2]])
        if (i %%50==0) {
          cat(i, "/ ")
        }
        if (i==1000) {
          cat("Done!")
        }
}
apply(cbind(b0, b1, b0.se, b1.se), 2, mean) # The means of each coefficient and SE.
                                            # How do they compare with the true model?

summary(warnings())








