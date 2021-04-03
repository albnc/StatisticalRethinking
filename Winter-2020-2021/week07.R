# week 07

# key concepts:
# GLM strategies extended by:
#   (1) embedding distributions to create mixtures
#   (2) using odd link functions to handle odd variables
# understand beta-binomial and gamma-Poison as *overdispersed* binomial and Poison models - simple "robust" GLMs, like student-t regression instead of Normal

library(rethinking)
data("Trolley")
d <- Trolley

# recode this in order
edu_levels <- c(6,1,8,4,7,2,5,3)
d$edu_new <- edu_levels[d$edu]

