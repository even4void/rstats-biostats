## ---- message = FALSE----------------------------------------------------
library(hrbrthemes)
library(directlabels)
library(cowplot)
library(texreg)
library(rms)            ## Hmisc, ggplot2
library(mfp)
library(multcomp)
library(reshape2)
options(digits = 6, show.signif.stars = FALSE)
theme_set(theme_ipsum(base_size = 11))

## ------------------------------------------------------------------------
load("data/fev.rda")
str(FEV)

## ------------------------------------------------------------------------
summary(FEV)

## ------------------------------------------------------------------------
describe(FEV$age)
FEV$age[FEV$age == 3] <- 4         ## low counts in extreme categories
FEV$age[FEV$age == 19] <- 18
FEV$height <- FEV$height * 2.54  ## inches -> centimeters
FEV$lfev <- log(FEV$fev)

## ---- warning = FALSE----------------------------------------------------
## switch to long format in order to get a facet variable
d <- melt(FEV[,c("id", "age", "fev", "height")], id.vars = "id")
levels(d$variable) <- c("Age (yr.)", "FEV (l/s)", "Height (cm)")
p <- ggplot(data = d, aes(x = value)) +
       geom_line(stat = "density") +
       geom_rug(size = .5, alpha = .3) +
       facet_wrap(~ variable, ncol = 3, scales = "free")  +
       labs(x = "", y = "Density")
p

## ---- warning = FALSE----------------------------------------------------
## switch to long format in order to get a facet variable
d <- melt(FEV[,c("id", "sex", "smoke", "age", "fev")], measure.vars = 4:5)
levels(d$variable) <- c("Age (yr.)", "FEV (l/s)")
p <- ggplot(data = d, aes(x = sex, y = value)) +
       geom_boxplot() +
       facet_grid(variable ~ smoke, scales = "free_y") +
       labs(x = NULL, y = NULL)
p

## ------------------------------------------------------------------------
p <- ggplot(data = FEV, aes(x = age, y = fev, color = sex)) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "loess", se = FALSE) +
       scale_color_manual(values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       labs(x = "Age (yr.)", y = "FEV (l/s)")
p1 <- direct.label(p + aes(label = sex))
p <- ggplot(data = FEV, aes(x = height, y = fev, color = sex)) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "loess", se = FALSE) +
       scale_color_manual(values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       labs(x = "Height (cm)", y = "FEV (l/s)")
p2 <- direct.label(p + aes(label = sex))
plot_grid(p1, p2)

## ------------------------------------------------------------------------
m <- lm(fev ~ height, data = FEV)
summary(m)

## ------------------------------------------------------------------------
yhat <- fitted(m)
rstd <- rstandard(m)
p <- ggplot(data = NULL, aes(x = yhat, y = rstd)) +
       geom_hline(yintercept = 0, linetype = 1, color = grey(.3)) + 
       geom_hline(yintercept = c(-2,2), linetype = 2, color = grey(.3)) + 
       geom_point() +
       geom_smooth(method = "loess", se = FALSE, color = "lightcoral") +
       labs(x = "Fitted values", y = "Residuals")
p

## ------------------------------------------------------------------------
m <- lm(I(fev ^ (1/3)) ~ height, data = FEV)

## ---- echo = FALSE-------------------------------------------------------
yhat <- fitted(m)
names(yhat) <- FEV$id
rstd <- rstandard(m)
p <- ggplot(data = NULL, aes(x = yhat, y = rstd)) +
       geom_hline(yintercept = 0, linetype = 1, color = grey(.3)) + 
       geom_hline(yintercept = c(-2,2), linetype = 2, color = grey(.3)) + 
       geom_point() +
       annotate("text", x = yhat[abs(rstd) > 3], y = rstd[abs(rstd) > 3], 
                label = names(yhat[abs(rstd) > 3]), size = 3, hjust = -0.5) +
       geom_smooth(method = "loess", se = FALSE, color = "lightcoral") +
       labs(x = "Fitted values", y = "Residuals", caption = ~ "Model considering height" ^3)
p

## ------------------------------------------------------------------------
influence.measures(m)

## ------------------------------------------------------------------------
m1 <- lm(fev ~ height + I(height^2), data = FEV)
m2 <- lm(fev ~ pol(height, 3), data = FEV)        ## or poly(height, 3, raw = TRUE)
m3 <- lm(fev ~ rcs(height, 3), data = FEV)

## ------------------------------------------------------------------------
m4 <- mfp(fev ~ fp(height, df = 4, select = .05), data = FEV)
m4

## ---- echo = FALSE-------------------------------------------------------
dd <- data.frame(height = 110:190)
yhat <- cbind.data.frame(dd, y1 = predict(m1, dd), y2 = predict(m2, dd), 
                         y3 = predict(m3, dd), y4 = predict(m4, dd))
p <- ggplot(data = FEV, aes(x = height, y = fev)) +
       geom_point(color = grey(.3), alpha = .5) +
       geom_line(data = melt(yhat, measure.vars = 2:5), aes(x = height, y = value, color = variable), size = 1) +
       scale_color_ipsum(name = "", labels = c("height + height²", "poly(height, 3)", "rcs(height, 3)", "fp(height, 4)")) +
       theme(legend.position = c(0.1, 0.88)) +
       labs(x = "Height (cm)", y = "FEV (l/s)")
p

## ------------------------------------------------------------------------
levels(FEV$smoke)
m <- lm(fev ~ smoke, data = FEV)
summary(m)
confint(m)

## ------------------------------------------------------------------------
t.test(fev ~ smoke, data = FEV, var.equal = TRUE)

## ------------------------------------------------------------------------
FEV$age4 <- cut2(FEV$age, g = 4)
r <- with(FEV, summarize(fev, age4, smean.sd))
r
r$fev[2:4] - r$fev[1]
m1 <- lm(fev ~ age, data = FEV)
m2 <- lm(fev ~ age4, data = FEV)
m3 <- aov(fev ~ age4, data = FEV)

## ------------------------------------------------------------------------
summary(m2)

## ------------------------------------------------------------------------
r <- with(FEV, summarize(fev, age4, smean.sd))
r

## ---- echo = FALSE-------------------------------------------------------
FEV$age4c <- as.numeric(as.character(cut2(FEV$age, g = 4, levels.mean = TRUE)))
p <- ggplot(data = FEV, aes(x = age, y = fev)) +
       geom_boxplot(aes(x = age4c, y = fev, group = age4c), outlier.size = -1, color = grey(.3), fill = "transparent") +
       geom_jitter(color = grey(.7), alpha = .5, width = .05) +
       geom_smooth(method = "lm", color = "steelblue") +
       labs(x = "Age (yr.)", y = "FEV (l/s)")
p
FEV$age4c <- NULL

## ---- echo = FALSE-------------------------------------------------------
p <- ggplot(data = FEV, aes(x = age, y = fev, color = smoke)) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "loess", se = FALSE) +
       annotate("rect", xmin = -Inf, ymin = -Inf, xmax = 9, ymax = Inf, fill = grey(.7), alpha = .5) +
       scale_color_manual(values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       labs(x = "Age (yr.)", y = "FEV (l/s)")
direct.label(p + aes(label = smoke))

## ------------------------------------------------------------------------
m1 <- lm(fev ~ smoke, data = FEV)
m2 <- lm(fev ~ age + smoke, data = FEV)
screenreg(list(m1,m2))

## ------------------------------------------------------------------------
anova(m1, m2)

## ------------------------------------------------------------------------
m3 <- lm(fev ~ age + smoke + age:smoke, data = FEV)
summary(m3)

## ------------------------------------------------------------------------
p <- ggplot(data = FEV, aes(x = age, y = fev, color = smoke)) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "lm", show.legend = FALSE) +
       scale_color_manual("", values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       labs(x = "Age (yr.)", y = "FEV (l/s)")
direct.label(p + aes(label = smoke))

## ------------------------------------------------------------------------
m <- ols(fev ~ age + smoke + age:smoke + pol(height, 2), data = FEV, x = TRUE)
m

## ------------------------------------------------------------------------
d <- expand.grid(age = 9:18, smoke = levels(FEV$smoke), 
                 height = seq(120, 180, by = 10))
yhat <- predict(m, d, se.fit = TRUE)
d <- cbind.data.frame(d, yhat)
head(d)

## ------------------------------------------------------------------------
Predict(m, age = 18, height = seq(160, 180, by = 2), 
        smoke = "current smoker", conf.type = "simult") 

## ------------------------------------------------------------------------
data(anorexia, package = "MASS")
str(anorexia)

## ------------------------------------------------------------------------
anorexia$Prewt <- anorexia$Prewt * 0.45    ## lbs -> kg
anorexia$Postwt <- anorexia$Postwt * 0.45
anorexia$Treat <- relevel(anorexia$Treat, ref = "Cont")
describe(anorexia)

## ------------------------------------------------------------------------
p <- ggplot(data = anorexia, aes(x = Prewt, y = Postwt, color = Treat)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       scale_color_manual("", values = c("grey30", "steelblue", "orange")) +
       guides(color = FALSE) +
       labs(x = "Weight at entry (kg)", y = "Weight at discharge (kg)")
direct.label(p + aes(label = Treat))

## ------------------------------------------------------------------------
m0 <- lm(Postwt ~ Prewt + Treat, data = anorexia, subset = Treat != "Cont")
m1 <- lm(Postwt ~ Prewt + Treat + Prewt:Treat, data = anorexia, subset = Treat != "Cont")
anova(m0, m1)

## ------------------------------------------------------------------------
summary(m0)
confint(m0)

## ------------------------------------------------------------------------
data(birthwt, package = "MASS")
summary(birthwt)

## ------------------------------------------------------------------------
birthwt$lwt <- birthwt$lwt * 0.45
birthwt$race <- factor(birthwt$race, levels = 1:3, labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = 0:1, labels = c("non-smoker", "smoker"))
p <- ggplot(data = birthwt, aes(x = lwt, y = bwt, color = smoke)) +
       geom_point() +
       geom_smooth(method = "lm", se = FALSE) +
       scale_color_manual(values = c("steelblue", "orange")) +
       guides(color = FALSE) +
       facet_wrap(~ race, ncol = 3) +
       geom_text(data = data.frame(lwt = c(45,90), bwt = c(1500,4500), 
                                   race = "white", smoke = c("smoker", "non-smoker")),
                 aes(x = lwt, y = bwt, label = smoke), color = c("orange", "steelblue")) +
       labs(x = "Mother weight (kg)", y = "Baby weight (g)")
p

