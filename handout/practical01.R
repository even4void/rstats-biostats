## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("ggplot2", "hrbrthemes", "directlabels", "Hmisc", "multcomp"))

## ---- message = FALSE----------------------------------------------------
library(ggplot2)
library(hrbrthemes)
library(directlabels)
library(Hmisc)
library(multcomp)
options(digits = 6, show.signif.stars = FALSE)
theme_set(theme_ipsum(base_size = 11))

## ------------------------------------------------------------------------
d <- foreign::read.dta("data/polymorphism.dta")
str(d)

## ------------------------------------------------------------------------
summary(age ~ genotype, data = d, fun = smean.sd)

## ------------------------------------------------------------------------
p <- ggplot(data = d, aes(x = age)) +
       geom_histogram(binwidth = 5, fill = "steelblue", color = "steelblue4") +
       facet_wrap(~ genotype, ncol = 3) +
       labs(x = "Age at diagnosis", y = "Count")
p

## ------------------------------------------------------------------------
m <- aov(age ~ genotype, data = d)
summary(m)

## ------------------------------------------------------------------------
r <- with(d, summarize(age, genotype, smean.cl.normal))
p <- ggplot(data = r, aes(x = genotype, y = age)) +
       geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.1) +
       geom_point() +
       geom_jitter(data = d, width = .05, color = grey(.7), alpha = .5) +
       labs(x = "Genotype", y = "Age at diagnosis")
p

## ------------------------------------------------------------------------
with(d, pairwise.t.test(age, genotype, p.adj = "none"))

## ------------------------------------------------------------------------
with(d, pairwise.t.test(age, genotype, p.adj = "none", pool.sd = FALSE))
t.test(age ~ genotype, data = d, subset = genotype != "0.7/0.7", var.equal = TRUE)

## ------------------------------------------------------------------------
m <- lm(age ~ genotype, data = d)
summary(m)

## ------------------------------------------------------------------------
anova(m)

## ------------------------------------------------------------------------
load("data/weight.rda")
str(weight)

## ------------------------------------------------------------------------
d <- data.frame(weight = as.numeric(unlist(weight)),
                type   = gl(2, 20, labels = c("Beef", "Cereal")),
                level  = gl(2, 10, labels = c("Low", "High")))
head(d)

## ------------------------------------------------------------------------
p <- ggplot(data = d, aes(x = level, y = weight)) +
       geom_boxplot(position = position_dodge()) +
       geom_jitter(size = .8, width = .05) +
       facet_wrap(~ type, nrow = 2) +
       labs(x = NULL, y = "Rat weight (g)")
p

## ------------------------------------------------------------------------
p <- ggplot(data = d, aes(x = level, y = weight, color = type)) +
       stat_summary(fun.y = mean, geom = "line", aes(group = type), size = 1) +
       scale_color_manual("Diet type", values = c("steelblue", "orange")) +
       labs(x = "Diet level", y = "Rat weight (g)")
p

## ------------------------------------------------------------------------
with(d, summarize(weight, llist(type, level), smean.cl.normal))

## ------------------------------------------------------------------------
m1 <- aov(weight ~ type * level, data = d)
summary(m1)

## ------------------------------------------------------------------------
m0 <- aov(weight ~ type + level, data = d)
summary(m0)

## ------------------------------------------------------------------------
anova(m0, m1)

## ------------------------------------------------------------------------
r <- t.test(weight ~ level, data = d, var.equal = TRUE)
r
r$statistic^2

## ------------------------------------------------------------------------
with(d, pairwise.t.test(weight, interaction(type, level), p.adj = "bonf"))

## ------------------------------------------------------------------------
p <- ggplot(data = d, aes(x = interaction(type, level, sep = "/"), y = weight)) +
       geom_boxplot() +
       labs(x = "Treatment (Diet type x Diet level)", y = "Rat weight (g)")
p

## ------------------------------------------------------------------------
d$tx <- with(d, interaction(type, level))
m <- lm(weight ~ tx, data = d)
r <- glht(m, linfct = mcp(tx = "Tukey"))
summary(r)

## ------------------------------------------------------------------------
data(ToothGrowth)
summary(ToothGrowth)

## ------------------------------------------------------------------------
r <- aggregate(len ~ dose + supp, data = ToothGrowth, mean)
p <- ggplot(data = ToothGrowth, aes(x = dose, y = len, color = supp)) +
       geom_point(position = position_jitterdodge(jitter.width = .1, dodge.width = 0.25)) +
       geom_line(data = r, aes(x = dose, y = len, color = supp)) +
       scale_color_manual(values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       geom_dl(aes(label = supp), method = list("smart.grid", cex = .8)) +
       labs(x = "Dose (mg/day)", y = "Length (oc. unit)")
p

## ------------------------------------------------------------------------
set.seed(101)
ToothGrowth[sample(1:nrow(ToothGrowth), 6),"len"] <- NA

