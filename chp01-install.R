## Step zero
install.packages("pkgbuild")

rt_path = pkgbuild::rtools_path()
rt_bin = paste0(substr(rt_path,1,nchar(rt_path)-4),"/mingw_$(WIN)/bin/")
writeLines(paste0('PATH="',rt_path,';${PATH}"'), con = "~/.Renviron")
writeLines(paste0('Sys.setenv(BINPREF = "',rt_bin,'")'), con = "~/.Rprofile")

install.packages("jsonlite",type="source")

## First STAN
install.packages("rstan")


library(rstan)

## Rethinking package available on Github
install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
library(devtools)
install_github("rmcelreath/rethinking")


