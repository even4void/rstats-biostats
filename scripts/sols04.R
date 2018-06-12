##
## Solutions to practical04.Rmd.
##

library(geepack)
library(lme4)

data(ohio)

ohio$age <- ohio$age + 9

fm <- resp ~ age*smoke

m1a <- geeglm(fm, id = id, data = ohio, family = binomial, corstr = "exch", scale.fix = TRUE)

summary(m1)

m1b <- geeglm(resp ~ age + smoke, id = id, data = ohio, family = binomial,
              corstr = "exch", scale.fix = TRUE)

exp(coef(m1b)["age"])

rob.se <- sqrt(diag(m1b$geese$vbeta))
exp(coef(m1b)["age"] + c(-1,1)*rob.se[2] * qnorm(0.975))

d <- expand.grid(age = seq(-2, 1, length=50), smoke = c(0,1))
d$yhat <- predict(m1b, d)

plot(exp(yhat) ~ age, data = d, type = "n")
lines(exp(yhat) ~ age, data = subset(d, smoke == 0), col = "red")
lines(exp(yhat) ~ age, data = subset(d, smoke == 1), col = "blue")

m2 <- glmer(resp ~ age + smoke + (1 | id), data=ohio, family = binomial)

summary(m2)

plot(ranef(m2))
