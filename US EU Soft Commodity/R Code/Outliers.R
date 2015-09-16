install.packages('tsoutliers')
library('tsoutliers')

set.seed(123)
y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 120)
y[15] <- -4
y[45] <- 5
y[80:120] <- y[80:120] + 5
y <- round(y, 2)
plot(y)

fit <- forecast::auto.arima(x = y, allowdrift = FALSE, ic = "bic")
pars <- coefs2poly(fit)
resid <- residuals(fit)

otypes <- c("AO", "LS", "TC")
mo0 <- locate.outliers(resid, pars, types = otypes)
mo0
mo1 <- locate.outliers.iloop(resid, pars, types = otypes)
mo1
mo2 <- locate.outliers.oloop(y, fit, types = otypes)
mo2$iter
mo2$outliers
