library(rethinking)
library(tidyverse)

data(foxes)

d <- foxes
d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)

glimpse(d)

# 1

m1 <- quap(
  alist(
    W ~ dnorm( mu, sigma ),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)

precis(m1)

# 2
m2 <- quap(
  alist(
    W ~ dnorm( mu, sigma ),
    mu <- a + bF * F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m2)

# 3
m3 <- quap(
  alist(
    W ~ dnorm( mu, sigma ),
    mu <- a + bF * F + bG * G,
    a ~ dnorm(0, 0.2),
    c(bF, bG) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m3)

# 4
m4 <- quap(
  alist(
    W ~ dnorm( mu, sigma ),
    mu <- a + bF * F + bG * G + bA * A,
    a ~ dnorm(0, 0.2),
    c(bF, bG, bA) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m4)


## Proving the DAG
# You can prove that the DAG is wrong, never prove that it is correct.
# implied conditional independencies
# Area is independent of Weight conditional on Food: A _||_ W | F
# (1) A _||_ W | F
m5 <- quap(
  alist(
    A ~ dnorm( mu, sigma ),
    mu <- a + bW * W + bF * F,
    a ~ dnorm(0, 0.2),
    c(bF, bW) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m5)

# (2) A _||_ G | F
m6 <- quap(
  alist(
    A ~ dnorm( mu, sigma ),
    mu <- a + bG * G + bF * F,
    a ~ dnorm(0, 0.2),
    c(bG, bF) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d
)
precis(m6)
