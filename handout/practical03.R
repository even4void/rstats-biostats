## ---- message = FALSE----------------------------------------------------
library(hrbrthemes)
library(directlabels)
library(cowplot)
library(texreg)
library(rms)            ## Hmisc, ggplot2
options(digits = 6, show.signif.stars = FALSE)
theme_set(theme_ipsum(base_size = 11))

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

