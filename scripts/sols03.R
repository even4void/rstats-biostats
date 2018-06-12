##
## Solutions to practical03.Rmd.
##

library(foreign)
library(Hmisc)
setwd("../data")

################
## Exercice 1 ##
################

d <- read.dta("clslowbwt.dta")

summary(bwt ~ lwt + race + smoke, data = d, fun = smean.sd)

summary(low ~ lwt + race + smoke, data = d, fun = table)
