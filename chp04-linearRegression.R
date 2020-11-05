
# 4.1.1 Normal by addition ----------------------------------------------------------
## person's position related to the middle of a soccer field
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos, freq = FALSE, breaks = -8:8, xaxt='n')
axis(1, at=seq(-8,8,1))
#plot(density(pos))

plot(1:16, cumsum(c(0,runif(15, -1, 1))), type='l')



# 4.1.2 Normal by multiplication ----------------------------------------------------
prod(1 + runif(12, 0, 0.1))
growth <- replicate(1000, prod(1 + runif(12,0,0.1)))
dens(growth, norm.comp = TRUE)


# 4.1.3 Normal by log-multiplication ------------------------------------------------
log(prod(1 + runif(12, 0, 0.1)))
log.growth <- replicate(1000, log(prod(1 + runif(12,0,0.1))))
dens(log.growth, norm.comp = TRUE)


# 4.3 GAUSSIAN MODEL OF HEIGHT ------------------------------------------------------
library(rethinking)
data("Howell1")
d <- Howell1
str(d)
precis(d, hist = FALSE)

d2 <- d[d$age >= 18,]

## Setting priors
# The ~ symbol means STOCHASTIC -> uncertainty
# hi ~ N(mu, sig)
# mu ~ N(178,20)
# sig~ U(0,50)

curve( dnorm(x, 178, 20), from=100, to=250)
curve( dunif(x, 0, 50), from = -10, to = 60)

sample_mu <- rnorm(1e4, 178, 100)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)


# 4.3.3 Grid approximation of the posterior distribution ----------------------------
mu.list <- seq(from=150, to=160, length.out = 100)
sigma.list <- seq(from=7, to=9, length.out = 100)
post <- expand.grid(mu=mu.list, sigma=sigma.list)
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod- max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)


# 4.3.4 Sampling from the posterior -------------------------------------------------

sample.rows <- sample( 1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot(sample.mu, sample.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))

dens(sample.mu)
dens(sample.sigma)

PI(sample.mu)
PI(sample.sigma)

## Repeat again with a small sample
d3 <- sample(d2$height, size=20)
mu.list <- seq(from=150, to=160, length.out = 200)
sigma.list <- seq(from=4, to=20, length.out = 200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum(dnorm( d3, post2$mu[i], post2$sigma[i], log = TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod- max(post2$prod))

sample2.rows <- sample( 1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]

dens(sample2.mu)
dens(sample2.sigma, norm.comp = TRUE)
plot(sample2.mu, sample2.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))


# 4.3.5 Finding the posterior distribution with quap --------------------------------

library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- quap( flist, data=d2)
precis(m4.1)

start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
m4.1 <- quap(flist, data=d2, start=start)
precis(m4.1)

## Using a narrow prior
m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, .1),
    sigma ~ dunif(0, 50)
  ), data=d2 )
precis(m4.2)



# 4.4 LINEAR PREDICTION -------------------------------------------------------------
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]
plot(d2$height ~ d2$weight)


# 4.4.1 The linear strategy ---------------------------------------------------------
# hi ~ Normal(mui, sigma)
# mui = alpha + beta * (xi - xmean)
# alpha ~ Normal(178, 20)
# beta ~ Normal(0,10)
# sigma ~ Uniform(0,50)

## Priors
set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)
plot(NULL, xlim=range(d2$weight), ylim=c(-100,400), xlab = "weight", ylab="heigth")
abline(h=0,lty=2)
abline(h=272, lty=1, lwd=0.5)
mtext("b ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for(i in 1:N) curve(a[i] + b[i]*(x-xbar),
                    from=min(d2$weight), to=max(d2$weight), add=TRUE,
                    col=col.alpha("black", 0.2))

# Log-norm to positive parameters
b <- rlnorm(1e4, 0, 1)
dens(b, xlim=c(0,5), adj=0.1)

# New Priors
set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)
plot(NULL, xlim=range(d2$weight), ylim=c(-100,400), xlab = "weight", ylab="heigth")
abline(h=0,lty=2)
abline(h=272, lty=1, lwd=0.5)
mtext("b ~ dnorm(0,10)")
xbar <- mean(d2$weight)
for(i in 1:N) curve(a[i] + b[i]*(x-xbar),
                    from=min(d2$weight), to=max(d2$weight), add=TRUE,
                    col=col.alpha("black", 0.2))


## Posterior
# load data again
library(rethinking)
data("Howell1"); d <- Howell1; d2 <- d[d$age>=18, ]

# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data=d2 )

precis(m4.3)
