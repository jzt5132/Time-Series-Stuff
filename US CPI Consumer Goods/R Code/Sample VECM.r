data(Canada)
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE,
 runs = 100)
summary(svec)
LR[3, 3] <- 0
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.oi$LRover
svec.oi