##
## Solutions to practical02.Rmd.
##

data(birthwt, package = "MASS")

birthwt$lwt <- birthwt$lwt * 0.45
birthwt$race <- factor(birthwt$race, levels = 1:3, labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = 0:1, labels = c("non-smoker", "smoker"))

fm0 <- bwt ~ lwt + race

################
## Exercice 1 ##
################
m0 <- lm(fm0, data = birthwt)
summary(m0)

fm1 <- bwt ~ lwt + race * smoke + ui + ht

m1 <- lm(fm1, data = birthwt)
summary(m1)

birthwt$ftv[birthwt$ftv > 2] <- 3

mstep <- step(m0, scope = list(upper = ~ age + lwt + race * smoke + ui + ht + ftv,
                                lower = ~ 1), trace = TRUE)
summary(mstep)

library(glmnet)
