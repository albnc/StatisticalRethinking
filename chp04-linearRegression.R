
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
# hi ~ N(mu, sig) : LIKELIHOOD
# mu ~ N(178,20) : PRIOR
# sig~ U(0,50) : PRIOR

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
round( vcov(m4.3), 3)


# 4.4.3.2 Plotting posterior inference against data ---------------------------------
plot( height ~ weight, data = d2, col=rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x - xbar), add = TRUE)


# 4.4.3.3 Adding uncertainty around the mean ----------------------------------------
post <- extract.samples(m4.3)
post[1:5,]

N <- 352
dN <- d2[1:N, ]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <-  a + b * (weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0, 50)
  ), data = dN
)

# extract 20 samples from the posterior
post <- extract.samples(mN, n=20)

# display raw data and sample size
plot(dN$weight, dN$height, xlim=range(d2$weight), ylim=range(d2$height),
     col=rangi2, xlab="weight", ylab="heigth")
mtext(concat("N = ",N))

#plot lines with transparency
for(i in 1:20) {
  curve(post$a[i] + post$b[i] * (x - mean(dN$weight)),
        col=col.alpha("black", 0.3), add=TRUE)
}


# 4.4.3.4 Plotting regression intervals and contours --------------------------------
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar)
dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")
PI(mu_at_50, prob=0.89)
mu <- link(m4.3)
str(mu)

## Define sequence of weights to compute predictions for
## these values will be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1)

## use link to compute mu for each sample from posterior
## and for each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)

## use type="n" to hide raw data
plot( height ~ weight, d2, type="n")

## loop over samples and plot each mu valu
for(i in 1:100) {
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))
}

## summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)

## Plot raw data
## fading out points to make line and interval more visible
plot( height ~ weight, data=d2, col=col.alpha(rangi2,0.5))

## Plot the MAP line, aka the mean mu for each weight
lines( weight.seq, mu.mean)

## Plot shaded region for 89% PI
shade(mu.PI, weight.seq)

post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b * (weight - xbar)
weight.seq <- seq(from=25, to = 70, by = 1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu,2, mean)
mu.CI <- apply(mu, 2, PI, prob=0.89)


# 4.4.3.5 Prediction intervals ------------------------------------------------------
sim.height <- sim(m4.3, data=list(weight=weight.seq))
str(sim.height)
height.PI <- apply(sim.height, 2, PI,prob=0.89)

## Plot raw data
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))

## draw MAP line
lines(weight.seq, mu.mean)

## draw HDPI region for lines
shade( mu.CI, weight.seq)

## draw PI region for simulated heights
shade( height.PI, weight.seq)


# 4.5 CURVES FROM LINE --------------------------------------------------------------
# 4.5.1 Polynomial Regression -------------------------------------------------------
library(rethinking)
data("Howell1")
d <- Howell1
plot( height ~ weight, data=d, col=rangi2)


## Quadratic regression
d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~dnorm( mu, sigma),
    mu <- a + b1 * weight_s + b2 *weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,1),
    sigma ~ dunif(0,50)
  ), data = d )

precis(m4.5)

## Simulating values
weight.seq <- seq(from=-2, to=2, length.out = 30)
pred_data <- list(weight_s=weight.seq, weight_s2=weight.seq^2)
mu <- link(m4.5, data=pred_data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data=pred_data)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

## Plot
plot( height ~ weight_s, data=d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

## Cubic
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~dnorm( mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2 + b3 * weight_s3,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0,1),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ), data = d )

pred_datac <- list(weight_s=weight.seq, weight_s2=weight.seq^2, weight_s3=weight.seq^3)
mu <- link(m4.6, data=pred_datac)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.6, data=pred_datac)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

## Plot
plot( height ~ weight_s, data=d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)


# 4.5.2 Splines ---------------------------------------------------------------------
## Data
library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
precis(d)

d2 <- d[complete.cases(d$doy),]
num_knots <- 15
knot_list <- quantile(d2$year, probs=seq(0,1,length.out = num_knots))

# Function
library(splines)
B <- bs(d2$year,
        knots = knot_list[-c(1,num_knots)],
        degree = 3, intercept = TRUE)
plot(NULL, xlim=range(d2$year), ylim=c(0,1), xlab="year", ylab="basis")
for(i in 1:ncol(B)) lines(d2$year, B[,i])

## Model
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=list(D=d2$doy, B=B),
  start=list(w=rep(0, ncol(B)))
)

precis(m4.7, depth=2)

post <- extract.samples( m4.7 )
w <- apply(post$w, 2, mean)
plot(NULL, xlim=range(d2$year), ylim=c(-6,6), xlab="year", ylab="basis * weigth")
for(i in 1:ncol(B)) lines(d2$year, w[i]*B[,i])     

mu <- link(m4.7)
mu_PI <- apply(mu, 2, PI,  0.97)
plot(d2$year, d2$joy, col=col.alpha(rangi2, 0.3), pch=16)
shade(mu_PI, d2$year, col=col.alpha("black", 0.5))
