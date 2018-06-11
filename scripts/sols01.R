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
with(ToothGrowth, pairwise.t.test(len, interaction(supp, dose), p.adj = "none"))

with(ToothGrowth, pairwise.t.test(len, interaction(supp, dose), p.adj = "bonferroni"))

with(ToothGrowth, pairwise.t.test(len, interaction(supp, dose), p.adj = "holm"))


################
## Exercice 3 ##
################
ToothGrowth$int <- with(ToothGrowth, interaction(supp, dose, sep = ":"))
levels(ToothGrowth$int)

ctr <- rbind(c(-1,1,-1,1,0,0), c(-1,0,-1,0,2,0), c(-1,0,-1,1,0,1))
ctr

library(multcomp)
r <- glht(aov(len ~ int, data = ToothGrowth), linfct = mcp(int = ctr))
summary(r)

################
## Exercice 4 ##
################
set.seed(101)
ToothGrowth[sample(1:nrow(ToothGrowth), 6),"len"] <- NA

fm <- len ~ supp * dosec
m <- aov(fm, data = ToothGrowth)

library(car)

summary(m)
Anova(m, type = 2)
Anova(m, type = 3)
