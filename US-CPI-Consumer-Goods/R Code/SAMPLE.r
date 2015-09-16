R> library("vars")
R> data("Canada")
R> summary(Canada)
R> plot(Canada, nc = 2, xlab = "")
R> adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
R> adf1
R> adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))
R> adf2



R> VARselect(Canada, lag.max = 8, type = "both")
R> Canada <- Canada[, c("prod", "e", "U", "rw")]
R> p1ct <- VAR(Canada, p = 1, type = "both")
R> p1ct
R> summary(p1ct, equation = "e")
R> plot(p1ct, names = "e")
R> ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
R> ser11$serial
R> norm1 <- normality.test(p1ct)
R> norm1$jb.mul
R> arch1 <- arch.test(p1ct, lags.multi = 5)
R> arch1$arch.mul
R> plot(arch1, names = "e")
R> plot(stability(p1ct), nc = 2)




R> summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3,
+ spec = "transitory"))
R> summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2,
+ spec = "transitory"))
R> vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",
+ ecdet = "trend", K = 3, spec = "transitory")
R> vecm.r1 <- cajorls(vecm, r = 1)




R> vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
+ ecdet = "trend", K = 3, spec = "transitory")
R> SR <- matrix(NA, nrow = 4, ncol = 4)
R> SR[4, 2] <- 0
R> LR <- matrix(NA, nrow = 4, ncol = 4)
R> LR[1, 2:4] <- 0
R> LR[2:4, 4] <- 0
R> svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE,
+ runs = 100)
R> summary(svec)
R> LR[3, 3] <- 0
R> svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
R> svec.oi$LRover
R> svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
R> plot(svec.irf)
R> fevd.U <- fevd(svec, n.ahead = 48)$U