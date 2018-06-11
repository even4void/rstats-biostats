## Time-stamp: <2018-06-11 12:52:21 chl>
##
## Solutions to practical01.Rmd.
##

data(ToothGrowth)

################
## Exercice 1 ##
################
ToothGrowth$dosec <- factor(ToothGrowth$dose, levels = unique(ToothGrowth$dose),
                            labels = c("low", "mid", "high"))
fm <- len ~ supp * dosec
m1 <- aov(fm, data <-  ToothGrowth)
summary(m1)

m2a <- lm(len ~ as.numeric(dosec) + I(as.numeric(dosec)^2), data = ToothGrowth)
summary(m2a)

ToothGrowth$doseo <- ordered(ToothGrowth$dose)
round(contr.poly(levels(ToothGrowth$doseo)))
m2b <- aov(len ~ doseo, data = ToothGrowth)
summary(m2b, split = list(doseo = c(1,2)))

################
## Exercice 2 ##
################
