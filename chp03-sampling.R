# Calculate the posterior
p_grid <- seq(from=0, to=1, length.out = 1000)
prob_p <- rep(1,1000)
prob_data <- dbinom(6, size=9, prob=p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

## Sampling
samples <- sample(p_grid, prob=posterior, size=1e4, replace = TRUE)

library(rethinking)

plot(samples)
plot(posterior)

hist(samples, freq = seq(0,1,0.05))

## Posterior Probability
sum( posterior[p_grid < 0.5])
sum( samples < 0.5 ) / 1e4

## Percentile Interval PI = 80%
quantile(samples, c(0.1, 0.9))

## Percentile Interval PI = 50%
quantile(samples, c(0.25, 0.75))

## Highest Posterior Density Interval - HPDI
HPDI(samples, 0.5)

## Loss
sum( posterior * abs(0.5 - p_grid))
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]
median(samples)

## Simulation
dbinom( 0:2, size = 2, prob = 0.7)

rbinom(10, size = 2, prob = 0.7)

dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w) / 1e5

dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab="dummy water count")


w <- rbinom( 1e4, size = 9, prob = samples)
simplehist(w)