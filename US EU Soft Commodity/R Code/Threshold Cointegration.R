

###Threshold Cointegration###


# This code determines threshold cointegration rank and return the result in
# ThresCI matrix.


options(digits = 4)
ThresCI <- matrix(data = NA, nrow = 7, ncol = 7)
for (i in 1:6){
  for (j in (i+1):7){
    b <- TVECM.HStest(a[,c(i,j)], lag = 1, ngridTh = 50, trim = 0.05,
                   nboot = 200, fixed.beta = NULL, intercept = TRUE)
    ThresCI[i,j] <- b$PvalBoot
  }
}
for (j in 1:6){
  for (i in (j+1):7){
    b <- TVECM.HStest(a[,c(i,j)], lag = 1, ngridTh = 50, trim = 0.05,
                   nboot = 200, fixed.beta = NULL, intercept = TRUE)
    ThresCI[i,j] <- b$PvalBoot
  }
}


###TVECM Spec###

TVECM.1.3 <- TVECM(a[,c(1,3)], lag = 1, nthresh = 1, trim = 0.05, ngridBeta = 50,
                ngridTh = 50, plot = TRUE, th1 = list(exact = NULL, int = c("from",
                "to"), around = "val"), th2 = list(exact = NULL, int = c("from", "to"),
              around = "val"), beta = list(exact = NULL, int = c("from", "to"), around =
             c("val", "by")), restr = c("none", "equal", "signOp"), common = c("All",
             "only_ECT"), include = c("const", "trend", "none", "both"),
            dummyToBothRegimes = TRUE, beta0 = 0, methodMapply = FALSE,
            trace = TRUE)

TVECM.1.6 <- TVECM(a[,c(1,6)], lag = 1, nthresh = 1, trim = 0.05, ngridBeta = 50,
                ngridTh = 50, plot = TRUE, th1 = list(exact = NULL, int = c("from",
                "to"), around = "val"), th2 = list(exact = NULL, int = c("from", "to"),
                around = "val"), beta = list(exact = NULL, int = c("from", "to"), around =
                c("val", "by")), restr = c("none", "equal", "signOp"), common = c("All",
                "only_ECT"), include = c("const", "trend", "none", "both"),
                dummyToBothRegimes = TRUE, beta0 = 0, methodMapply = FALSE,
                trace = TRUE)

TVECM.2.3 <- TVECM(a[,c(2,3)], lag = 1, nthresh = 1, trim = 0.05, ngridBeta = 50,
                ngridTh = 50, plot = TRUE, th1 = list(exact = NULL, int = c("from",
                "to"), around = "val"), th2 = list(exact = NULL, int = c("from", "to"),
                around = "val"), beta = list(exact = NULL, int = c("from", "to"), around =
                c("val", "by")), restr = c("none", "equal", "signOp"), common = c("All",
                "only_ECT"), include = c("const", "trend", "none", "both"),
                dummyToBothRegimes = TRUE, beta0 = 0, methodMapply = FALSE,
                trace = TRUE)


TVECM.4.6 <- TVECM(a[,c(4,6)], lag = 1, nthresh = 1, trim = 0.05, ngridBeta = 50,
                ngridTh = 50, plot = TRUE, th1 = list(exact = NULL, int = c("from",
                "to"), around = "val"), th2 = list(exact = NULL, int = c("from", "to"),
                around = "val"), beta = list(exact = NULL, int = c("from", "to"), around =
                c("val", "by")), restr = c("none", "equal", "signOp"), common = c("All",
                "only_ECT"), include = c("const", "trend", "none", "both"),
                dummyToBothRegimes = TRUE, beta0 = 0, methodMapply = FALSE,
                trace = TRUE)

summary(TVECM.1.3); summary(TVECM.1.6); summary(TVECM.2.3); summary(TVECM.4.6);