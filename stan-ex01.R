# remove.packages('rstan')
# install.packages('rstan')
## Tutorial <https://www.youtube.com/watch?v=YZZSYIx1-mw>

## Generate fake data
N <- 100
Y <- rnorm(N, 1.6, 0.2)
hist(Y)

## Load library

library(rstan)

## Compile model
model <- rstan::stan_model('stan-model01.stan')

## Pass the data to stan and run the model
parallel::detectCores()
options(mc.cores=4)
fit <- sampling(model, list(N=N, Y=Y), iter=200, chains=4)

library(shinystan)
launch_shinystan(fit)
