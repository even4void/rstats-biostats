##
## Solutions to practical03.Rmd.
##

library(foreign)
library(rms)
setwd("../data")


################
## Exercice 1 ##
################
d <- read.dta("clslowbwt.dta")

summary(bwt ~ lwt + race + smoke, data = d, fun = smean.sd)

summary(low ~ lwt + race + smoke, data = d, fun = table)

m <- lrm(low ~ lwt + race + smoke, data = d, x = TRUE, y = TRUE)
m

anova(m)
coef(m)

robcov(m, cluster(d$id), "huber")


################
## Exercice 2 ##
################
load("leukemia.rda")

dim(leukemia)

r <- numeric(ncol(leukemia)-1)
for (j in 2:ncol(leukemia)) r[j-1] <- t.test(leukemia[leukemia[,1] == 0,j], leukemia[leukemia[,1] == 1,j])$p.value

hist(-log10(r))

sum(r < 0.05)
sum(r < 0.05/length(r))

names(leukemia)[which(r < 0.05/length(r))+1]

library(glmnet)
set.seed(101)

m <- glmnet(as.matrix(leukemia[,-1]), leukemia[,1], alpha = 1)
plot(m, xvar = "lambda")

m.cv <- cv.glmnet(as.matrix(leukemia[,-1]), leukemia[,1], alpha = 1)
plot(m.cv)

cc <- coef(m.cv$glmnet.fit, s=m.cv$lambda.1se) # minus one SE trick

rc <- cc@x
names(rc) <- dimnames(cc)[[1]][which(cc != 0)]
rc


################
## Exercice 3 ##
################
d <- read.dta("compliance2.dta")

d <- within(d, {
  ranagr <- factor(ranagr)
  t0 <- as.numeric(randate-min(randate))
  t1 <- t0 + as.numeric(enddate - randate)
})

st <- with(d, Surv(t0, t1, as.numeric(died)))

s <- survfit(st ~ particip, data = d)
plot(s)

quantile(s, 0.1)

m <- coxph(st ~ particip + strata(ranagr), data = d)
m


################
## Exercice 4 ##
################
load("pbc.rda")

pbc$rx <- factor(pbc$rx, levels = 1:2, labels = c("Placebo", "DPCA"))
pbc$sex <- factor(pbc$sex, levels = 0:1, labels = c("M","F"))

plot(number ~ years, data = pbc, col = pbc$status+1, cex = .8)

st <- with(pbc, Surv(time = years, event = status))

s <- survfit(st ~ rx, data = pbc)
s

survdiff(st ~ rx, data = pbc)

summary(coxph(st ~ rx, data = pbc))
