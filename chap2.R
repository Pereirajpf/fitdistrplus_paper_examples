#Chap 2: Fitting distributions to continuous non-censored data

library("fitdistrplus")
data("groundbeef")
str(groundbeef)

# Choice of candidate distributions
plotdist(groundbeef$serving, hist = T, demp = T)

descdist(groundbeef$serving, boot = 1000)

#Fit of distributions by maximum likelihood estimation
fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)
plot(fw)

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")

denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)

#Fit to any dist
data("endosulfan")
ATV <-endosulfan$ATV
fendo.ln <- fitdist(ATV, "lnorm")
library("actuar")
fendo.ll <- fitdist(ATV, "llogis", start = list(shape = 1, scale = 500))
fendo.P <- fitdist(ATV, "pareto", start = list(shape = 1, scale = 500))
fendo.B <- fitdist(ATV, "burr", start = list(shape1 = 0.3, shape2 = 1, rate = 1))
cdfcomp(list(fendo.ln, fendo.ll, fendo.P, fendo.B),
           xlogscale = TRUE, ylogscale = TRUE,
           legendtext = c("lognormal", "loglogistic", "Pareto", "Burr"))

quantile(fendo.B, probs = 0.05)
quantile(ATV, probs = 0.05)

gofstat(list(fendo.ln, fendo.ll, fendo.P, fendo.B),
        fitnames = c("lnorm", "llogis", "Pareto", "Burr"))


#Uncertainty in parameter estimates
bendo.B <- bootdist(fendo.B, niter = 1001)
summary(bendo.B)
plot(bendo.B)

quantile(bendo.B, probs = 0.05)
