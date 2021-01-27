# Statistica Rethinking Winter 2020/2021
# Discussion Seminar 1
# Points
# (1) Procedural questions
# (2) Software setup problems
# (3) Homework review - solutions and broader concepts
# (4) Prepare for the next week - Chapter 4

library(rethinking)


# Problem 1 -------------------------------------------------------------------------
# Book pdf (p.40)
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
p_grid
# define prior
prior <- rep( 1 , 20 )
prior
# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=15 , prob=p_grid )
likelihood
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
unstd.posterior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type="l")


# Problem 2 -------------------------------------------------------------------------
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )
# define prior
prior <- c(rep( 0 , 50 ), rep(1, 50) )
# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=15 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type="l")


# Problem 3 -------------------------------------------------------------------------
samples <- sample( p_grid, prob = posterior, size = 1e4, replace = TRUE )
plot(samples, ylim=c(0,1), xlab="samples", ylab="proportion water")

PI(samples, 0.89)
HPDI( samples, 0.89 )


