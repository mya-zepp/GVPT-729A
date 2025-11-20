# GVPT 729a, Using the aggcpsgvpt729a data set, investigate identification conditions 
# in logit and probit.
# 
# First, create a variable for southern states.
rm(list=ls())

install.packages("readstata13")
install.packages("gmodels")

library(readstata13)
library(foreign)

library(mvtnorm)

setwd("C:\\Users\\mzepp\\Downloads\\School\\R")
d <- read.dta("aggcpsgvpt729a.dta") # Import dataset

d$stateid <- as.numeric(d$state) # Add an ID for each state. We can coerce it into numeric because 
                                 # it's a factor, and we can leverage the levels. Can't be done with a character variable.
d$south <- ifelse(d$stateid %in% c(1,4,10,11,19,25,34,41,43,44,47),1,0) # Generate "South" dummy

# Run a model predicting whether or not states have election day registration (EDR).

logit.model <- glm(edr ~ south + pcthsg, family=binomial(link="logit"), data=d[d$year==2000,])
summary(logit.model)

probit.model <- glm(edr ~ south + pcthsg, family=binomial(link="probit"), data=d[d$year==2000,])
summary(probit.model)


# What happens?  Are any cases dropped?  If so, which ones? Is the model properly estimated?
# What, if any, bivariate method could have been used to detect this before 
# you ran the logit.
# Explain why no bivariate method would have allowed you to detect this or 
# how you would know from the bivariate result that there was a "problem?".


# Right, run a crosstab. 

table(d$south, d$edr) # Easy crosstab
table(d$south[d$year==2000], d$edr[d$year==2000]) 

library(gmodels) 
CrossTable(d$south[d$year==2000], d$edr[d$year==2000], prop.r = F, prop.c = F, prop.t = F, 
           dnn=c("South", "Edr"), prop.chisq = F) # Fancier


# what if an indep var that is continuous leads to perfect prediction?.

logit.model2 <- glm(edr ~ pctbla + pcthsg, family=binomial(link="logit"), data=d[d$year==2000,])
summary(logit.model2)

probit.model2 <- glm(edr ~ pctbla + pcthsg, family=binomial(link="probit"), data=d[d$year==2000,])
summary(probit.model2)

table(d$pctbla[d$year==2000], d$edr[d$year==2000]) 

CrossTable(d$pctbla[d$year==2000], d$edr[d$year==2000], digits=3, dnn = c("Pctbla","Edr"), #Fancier crosstab
           prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

# What is it about the logit and probit distributions that leads to more perfect 
# predictions of failure in probit than logit?.
# **hint(take a look at the lecture slide that plots both distributions).

