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
set.seed(101)

desmat <- model.matrix(~ 0 + age + lwt + race + smoke + ui + ht + ftv, data = birthwt)

head(desmat)

enet <- glmnet(desmat, birthwt$bwt, alpha = 1)
plot(enet, xvar = "lambda")

enet.cv <- cv.glmnet(desmat, birthwt$bwt, alpha = 1)
plot(enet.cv)

coef(enet.cv$glmnet.fit, s=enet.cv$lambda.1se) # minus one SE trick


################
## Exercice 2 ##
################
fm <- bwt ~ lwt + race + smoke
m <- lm(fm, data = birthwt)
confint(m)


birthwt$race2cat <- birthwt$race
levels(birthwt$race2cat)[2:3] <- "black+other"

m <- lm(bwt ~ 0 + lwt + race + smoke, data = birthwt)
summary(m)

library(multcomp)

r <- glht(m, linfct = "racewhite - (raceblack + raceother) = 0")
summary(r)

# FIXME TODO  2.3

fm <- bwt ~ age + ht
m <- lm(fm, data = birthwt)
confint(m)

library(boot)

reg.boot <- function(formula, data, k) coef(lm(formula, data[k,]))

r <- boot(data = birthwt, statistic = reg.boot, R = 500, formula = fm)
boot.ci(r, type = "bca", index = 3)
