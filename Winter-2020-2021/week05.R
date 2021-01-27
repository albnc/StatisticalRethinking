# WEEK 05
# keys concepts:
#   Casual relations and interactions effects
#   How to build and interpret interaction
#   Purpose and use of Markov Chain Mont Carlo
#   How to check MCMC output

## needs cmdstanr package
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# 1
library(rethinking)
data("Wines2012")
d <- Wines2012

dat_list <- list(
  S = standardize(d$score),
  jid = as.integer(d$judge),
  wid = as.integer(d$wine)
)

m1 <- ulam(
  alist(
    S ~ dnorm( mu, sigma ),
    mu <- a[jid] + w[wid],
    a[jid] ~ dnorm(0, 0.5),
    w[wij] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=dat_list, chains=4, cores=4, cmdstan = TRUE)

precis(m1, 2)

plot(precis(m1,2))

# 2
# indicator (dumy) variables
dat_list2 <- list(
  S = standardize(d$score),
  W = d$wine.amer,
  J = d$judge.amer,
  R = ifelse(d$flight=="red", 1L, 0L)
)

m2a <- ulam(
  alist(
    S ~ dnorm( mu, sigma ),
    mu <- a + bW*W + bJ*J + bR*R,
    a ~ dnorm(0, 0.2),
    c(bW, bJ, bR) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=dat_list2, chains=4, cores=4, cmdstan = TRUE)

plot(precis(m2a,omit="sigma"))
