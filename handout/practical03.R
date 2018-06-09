## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("ggplot2", "hrbrthemes", "directlabels", "cowplot", "texreg", "rms")

## ---- message = FALSE----------------------------------------------------
library(hrbrthemes)
library(directlabels)
library(ggfortify)
library(cowplot)
library(texreg)
library(survival)
library(rms)            ## Imports: Hmisc, ggplot2
options(digits = 6, show.signif.stars = FALSE)
theme_set(theme_ipsum(base_size = 11))

## ------------------------------------------------------------------------
data(birthwt, package = "MASS")
str(birthwt)

## ------------------------------------------------------------------------
birthwt$lwt <- birthwt$lwt * 0.45
birthwt$race <- factor(birthwt$race, levels = 1:3, labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = 0:1, labels = c("non-smoker", "smoker"))
birthwt$ftv[birthwt$ftv > 2] <- 2

## ---- echo = FALSE-------------------------------------------------------
p <- ggplot(data = birthwt, aes(x = lwt, y = bwt, color = factor(low))) +
       geom_point() +
       geom_smooth(method = "loess", span = 1.5, se = FALSE, color = grey(.3)) +
       scale_color_manual("", values = c("grey50", "lightcoral")) +
       facet_wrap(~ race, ncol = 3) +
       guides(color = FALSE) +
       labs(x = "Mother weight (kg)", y = "Baby weight (g)")
p

## ------------------------------------------------------------------------
s <- summary(low ~ age + lwt + race + smoke + ht + ui + ftv,
             data = birthwt, method = "reverse")
print(s, exclude1 = FALSE, npct = "both")

## ------------------------------------------------------------------------
d <- aggregate(low ~ race + cut2(lwt, g = 3) + smoke, data = birthwt, sum)
d$pct <- d$low / nrow(birthwt)
names(d)[2] <- "age"
p <- ggplot(data = d, aes(x = pct, y = age)) +
       geom_line(aes(group = age), color = grey(.7)) +
       geom_point(aes(color = smoke)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       facet_wrap(~ race, ncol = 3) +
       labs(x = "Proportion weight < 2.5 kg", y = "Mother age")
p

## ------------------------------------------------------------------------
fm <- low ~ lwt + race + smoke + ui
m <- glm(fm, data = birthwt, family = binomial)
summary(m)

## ------------------------------------------------------------------------
confint(m)
exp(coef(m)["smokesmoker"])

## ------------------------------------------------------------------------
d <- expand.grid(lwt = seq(40, 100), race = factor(levels(birthwt$race)),
                 smoke = "smoker", ui = 1)
d$yhat <- predict(m, d, type = "response")
p <- ggplot(data = d, aes(x = lwt, y = yhat, color = race)) +
       geom_line(aes(group = race), size = 1) +
       scale_color_ipsum() +
       guides(color = FALSE) +
       labs(x = "Mother weight (kg)", y = "Pr(low = 1)", caption = "Predicted response curves")
direct.label(p + aes(label = race), method = "smart.grid")

## ------------------------------------------------------------------------
ddist <- with(birthwt, datadist(lwt, race, smoke, ui))  ## mandatory for Predict with ggplot, see below
options(datadist = "ddist")
m <- lrm(fm, data = birthwt, x = TRUE, y = TRUE)
m

## ------------------------------------------------------------------------
r <- anova(m)

## ------------------------------------------------------------------------
ggplot(Predict(m, lwt, race="white", smoke, ui=1), anova = r, pval = TRUE)

## ------------------------------------------------------------------------
validate(m)

## ------------------------------------------------------------------------
load("data/hdis.rda")
str(hdis)

## ------------------------------------------------------------------------
round(xtabs(hdis/total ~ bpress + chol, data = hdis), 2)

## ------------------------------------------------------------------------
labs <- c(111.5,121.5,131.5,141.5,151.5,161.5,176.5,191.5)
hdis$bpress <- rep(labs, each = 7)
d <- aggregate(cbind(hdis, total) ~ bpress, data = hdis, sum)

## ------------------------------------------------------------------------
d$prop <- d$hdis/d$total

## ------------------------------------------------------------------------
m <- glm(cbind(hdis, total-hdis) ~ bpress, data = d, family = binomial)
d$yhat <- predict(m, type = "response")
d

## ------------------------------------------------------------------------
f <- function(x) 1/(1 + exp(-(coef(m)[1] + coef(m)[2]*x)))
p <- ggplot(data = d, aes(x = bpress, y = hdis/total)) +
       geom_point() +
       stat_function(fun = f, col = "lightcoral", size = 1) +
       labs(x = "Blodd pressure (mm Hg)", y = "Proportion CHD", caption = "Observed vs. fitted proportions")
p

## ------------------------------------------------------------------------
load("data/schoolinf.rda")
str(schoolinf)

## ------------------------------------------------------------------------
p <- ggplot(data = schoolinf, aes(x = days, y = count)) +
       geom_point() +
       geom_smooth(method = "loess", color = "lightcoral") +
       labs(x = "No. days", y = "No. students diagnosed")
p

## ------------------------------------------------------------------------
m1 <- glm(count ~ days, data = schoolinf, family = poisson)
summary(m1)

## ------------------------------------------------------------------------
m2 <- glm(count ~ days, data = schoolinf, family = quasipoisson)
summary(m2)

## ------------------------------------------------------------------------
d <- data.frame(days = seq(0, 150, 2))
d$yhat <- predict(m2, d, type = "response")
p <- ggplot(data = schoolinf, aes(x = days, y = count)) +
       geom_point() +
       geom_line(data = d, aes(x = days, y = yhat), color = "lightcoral", size = 1) +
       labs(x = "No. days", y = "No. students diagnosed")
p

## ------------------------------------------------------------------------
data(lung, package = "survival")
lung$sex <- factor(lung$sex, levels = 1:2, labels = c("Male", "Female"))
st <- with(lung, Surv(time=time, event=status))
head(st)

## ------------------------------------------------------------------------
s <- survfit(st ~ 1, data=lung)
s

## ------------------------------------------------------------------------
summary(s, times=seq(1, 200, by=20))

## ------------------------------------------------------------------------
s <- survfit(st ~ sex, data=lung)
p <- autoplot(s, censor = TRUE, censor.colour = grey(.5)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       scale_fill_manual("", values = c("steelblue", "orange")) +
       guides(fill = FALSE) +
       theme(legend.position = c(.9, .9)) +
       labs(x = "Followup time (hr.)", y = "Probability survival")
p

## ------------------------------------------------------------------------
survdiff(st ~ sex, data=lung)

## ------------------------------------------------------------------------
m <- coxph(st ~ sex, data=lung)
m

## ------------------------------------------------------------------------
coxph(st ~ sex + strata(age), data=lung)

## ------------------------------------------------------------------------
load("data/pbc.rda")
pbc$rx <- factor(pbc$rx, levels = 1:2, labels = c("Placebo", "DPCA"))
pbc$sex <- factor(pbc$sex, levels = 0:1, labels=c("M","F"))

## ------------------------------------------------------------------------
summary(status ~ rx + sex + cut2(age, g = 4), data = pbc, fun = table)

