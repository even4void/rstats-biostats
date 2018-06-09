## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("ggplot2", "ggfortify", "hrbrthemes", "directlabels",
##                      "cowplot", "texreg", "lme4", "geepack")

## ---- message = FALSE----------------------------------------------------
library(ggplot2)
library(ggfortify)
library(hrbrthemes)
library(directlabels)
library(cowplot)
library(texreg)
library(Hmisc)
library(survival)
library(lme4)
library(geepack)
library(reshape2)
options(digits = 6, show.signif.stars = FALSE)
theme_set(theme_ipsum(base_size = 11))

## ------------------------------------------------------------------------
load("data/fat.rda")
str(fat)

## ------------------------------------------------------------------------
summary(fecfat ~ pilltype, data = fat, fun = smean.sd)

## ------------------------------------------------------------------------
d <- aggregate(fecfat ~ pilltype, data = fat, mean)
p <- ggplot(data = fat, aes(x = reorder(pilltype, fecfat), y = fecfat)) +
       geom_line(aes(group = subject), color = grey(.3)) +
       geom_line(data = d, aes(x = pilltype, y = fecfat, group = 1),
                 color = "lightcoral", size = 1.2) +
       labs(x = NULL, y = "Fecal Fat")
p

## ------------------------------------------------------------------------
m1 <- aov(fecfat ~ pilltype, data = fat)
m2 <- aov(fecfat ~ pilltype + subject, data = fat)
m3 <- aov(fecfat ~ pilltype + Error(subject), data = fat)
summary(m3)

## ------------------------------------------------------------------------
m <- lmer(fecfat ~ pilltype + (1 | subject), data = fat)
summary(m)
confint(m)

## ------------------------------------------------------------------------
yhat <- predict(m)
p <- ggplot(data = fat, aes(x = reorder(pilltype, fecfat), y = yhat)) +
       geom_line(aes(group = subject), color = grey(.3)) +
       geom_line(data = d, aes(x = pilltype, y = fecfat, group = 1),
                 color = "lightcoral", size = 1.2) +
       labs(x = NULL, y = "Predicted Fecal Fat")
p

## ------------------------------------------------------------------------
sepsis <- foreign::read.dta("data/sepsis.dta")
str(sepsis)

## ------------------------------------------------------------------------
sepsis$treat <- as.factor(sepsis$treat)
sepsis$id <- factor(sepsis$id)
sepsis.long <- melt(sepsis, measure.vars = 8:14, id.vars = 1:2)
celsius2fahr <- function(x) (x-32) / 1.8              ## °F -> °C
sepsis.long$value <- celsius2fahr(sepsis.long$value)

## ------------------------------------------------------------------------
s <- summary(treat ~ fate + race + apache + temp0 + temp1, data = sepsis,
             method = "reverse", overall = TRUE)
print(s, digits = 3)

## ------------------------------------------------------------------------
p <- ggplot(data = na.omit(sepsis.long), aes(x = variable, y = value)) +
       geom_line(aes(group = id), col = grey(.7), alpha = .2) +
       geom_line(data = subset(na.omit(sepsis.long), variable %in% c("temp0", "temp1")),
                 aes(group = id, color = treat)) +
       scale_x_discrete(labels = seq(0, 6*2, by = 2)) +
       scale_y_continuous(breaks = seq(31.5, 43, by = 1.5)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       labs(x = "Durée de suivi (heure)", y = "Température (°C)") +
       theme(legend.position = c(.5, 1.05), legend.direction = "horizontal")
p

## ------------------------------------------------------------------------
d <- with(sepsis.long, summarize(value, llist(treat, variable), smean.cl.normal))

p <- ggplot(data = d, aes(x = variable, y = value, shape = treat, color = treat)) +
       geom_point(size = 2) +
       geom_line(aes(group = treat)) +
       geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.1) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       scale_x_discrete(labels = seq(0, 6*2, by = 2)) +
       guides(color = FALSE, shape = FALSE) +
       geom_dl(aes(label = treat), method = list("smart.grid", cex = .8)) +
       labs(x = "Durée de suivi (heure)", y = "Température (°C)")
p

## ------------------------------------------------------------------------
## 0 = alive, 1 = dead
st <- with(sepsis, Surv(time = followup, event = as.numeric(fate)-1))
s <- survfit(st ~ treat, data = sepsis)
p <- autoplot(s, censor = FALSE) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       scale_fill_manual("", values = c("steelblue", "orange")) +
       guides(fill = FALSE) +
       theme(legend.position = c(.9, .9)) +
       labs(x = "Followup time (hr.)", y = "Probability survival")
p

## ------------------------------------------------------------------------
cc <- aggregate(value ~ treat + id, sepsis.long, length)
idx <- cc$id[cc$value == 7]  ## complete case IDs
d <- subset(sepsis.long, id %in% idx)
d$id <- droplevels(d$id)     ## n = 328
names(d)[3] <- "time"
levels(d$time) <- 0:6
d$time <- as.numeric(as.character(d$time))

## ------------------------------------------------------------------------
## lme(value ~ time * treat, data = d, random = ~ 1 | id)
m1 <- lmer(value ~ time * treat + (1 | id), data = d)
summary(m1)

## ------------------------------------------------------------------------
## lme(value ~ time * treat, data = d, random = ~ time | id)
m2 <- lmer(value ~ time * treat + (time | id), data = d)

## ------------------------------------------------------------------------
## lme(value ~ time * treat, data = d, random = list(id = pdDiag(~ time)))
m3 <- lmer(value ~ time * treat + (1 | id) + (0 + time | id), data = d)

## lme(value ~ time * treat, data = d, random = ~ 0 + time | id)
m4 <- lmer(value ~ time * treat + (0 + time | id), data = d)

## ------------------------------------------------------------------------
screenreg(list(m1, m2, m3), include.variance = TRUE)

## ------------------------------------------------------------------------
anova(m1, m3, m2)

## ---- echo = FALSE-------------------------------------------------------
d$yhat1 <- predict(m1)
d$yhat2 <- predict(m2)
p1 <- ggplot(data = d, aes(x = time, y = yhat1, color = treat)) +
       geom_line(aes(group = id), alpha = .2) +
       scale_x_continuous(breaks = 0:6, labels = seq(0, 6*2, by = 2)) +
       scale_y_continuous(breaks = seq(31.5, 43, by = 1.5)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       geom_dl(aes(label = treat), method = "smart.grid") +
       labs(x = "Durée de suivi (heure)", y = "Température (°C)", caption = "Model 1")
p2 <- ggplot(data = d, aes(x = time, y = yhat2, color = treat)) +
       geom_line(aes(group = id), alpha = .2) +
       scale_x_continuous(breaks = 0:6, labels = seq(0, 6*2, by = 2)) +
       scale_y_continuous(breaks = seq(31.5, 43, by = 1.5)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       geom_dl(aes(label = treat), method = "smart.grid") +
       labs(x = "Durée de suivi (heure)", y = "Température (°C)", caption = "Model 2")
plot_grid(p1, p2)

## ------------------------------------------------------------------------
m <- geeglm(value ~ time * treat, data = d, id = id, corstr = "exch", scale.fix=TRUE)
summary(m)

## ------------------------------------------------------------------------
m0 <- geese(value ~ time * treat, data = d, id = id, corstr = "exch", scale.fix=TRUE)
m0$vbeta
m0$vbeta.naiv

## ------------------------------------------------------------------------
d$yhat3 <- predict(m)
p <- ggplot(data = d, aes(x = time, y = yhat3, color = treat)) +
       geom_line(aes(group = id), alpha = .2) +
       scale_x_continuous(breaks = 0:6, labels = seq(0, 6*2, by = 2)) +
       scale_y_continuous(breaks = seq(31.5, 43, by = 1.5)) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       geom_dl(aes(label = treat), method = "smart.grid") +
       labs(x = "Durée de suivi (heure)", y = "Température (°C)", caption = "Model GEE")
p

## ------------------------------------------------------------------------
data(ohio, package = "geepack")
str(ohio)
ohio$age <- ohio$age + 9    ## in years

## ------------------------------------------------------------------------
summary(resp ~ age + smoke, data = ohio, method = "cross")

