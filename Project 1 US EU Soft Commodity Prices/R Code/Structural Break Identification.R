###Str.BK Identification###

# This code identifies structural break using bivariate VECM

###Oil==>EUcorn###
lr.reg=lm(a[,1]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,1)],diff(a[,c(7,1)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)

sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)

###Oil==>EUwheat###
lr.reg=lm(a[,2]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,2)],diff(a[,c(7,2)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)
sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)

summary(breakpoints(dy2~error.lagged+dy1,data=a2))
sctest(dy2[1:260]~error.lagged[1:260]+dy1[1:260],type="RE",data=a2)
sctest(dy2[261:345]~error.lagged[261:345]+dy1[261:345],type="RE",data=a2)
sctest(dy2[346:573]~error.lagged[346:573]+dy1[346:573],type="RE",data=a2)

c721=ca.jo(a[(1:260),c(2,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c721)@teststat;summary(c721)@cval
c722=ca.jo(a[(261:345),c(2,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c722)@teststat;summary(c722)@cval
c723=ca.jo(a[(346:573),c(2,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c723)@teststat;summary(c723)@cval

v721=VAR(cbind(diff(a[(1:260),2]),diff(a[(1:260),7])),p=2)
cs721=causality(v721)$Granger$p.value
v722=VAR(cbind(diff(a[(261:345),2]),diff(a[(261:345),7])),p=2)
cs722=causality(v722)$Granger$p.value
v723=VAR(cbind(diff(a[(346:573),2]),diff(a[(346:573),7])),p=2)
cs723=causality(v723)$Granger$p.value


###Oil==>EUsoybean###
lr.reg=lm(a[,3]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,3)],diff(a[,c(7,3)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)

sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)

summary(breakpoints(dy2~error.lagged+dy1,data=a2))
sctest(dy2[1:191]~error.lagged[1:191]+dy1[1:191],type="RE",data=a2)
sctest(dy2[192:573]~error.lagged[192:573]+dy1[192:573],type="RE",data=a2)

c731=ca.jo(a[(1:191),c(3,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c731)@teststat;summary(c731)@cval
c732=ca.jo(a[(192:573),c(3,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c732)@teststat;summary(c732)@cval

v731=VAR(cbind(diff(a[(1:191),3]),diff(a[(1:191),7])),p=2)
cs731=causality(v731)$Granger$p.value
v732=VAR(cbind(diff(a[(192:573),3]),diff(a[(192:573),7])),p=2)
cs732=causality(v732)$Granger$p.value


###Oil==>UScorn###
lr.reg=lm(a[,4]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,4)],diff(a[,c(7,4)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)

sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)

summary(breakpoints(dy2~error.lagged+dy1,data=a2))

sctest(dy2[1:398]~error.lagged[1:398]+dy1[1:398],type="RE",data=a2)
sctest(dy2[399:573]~error.lagged[399:573]+dy1[399:573],type="RE",data=a2)

c741=ca.jo(a[(1:398),c(4,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c741)@teststat;summary(c741)@cval
c742=ca.jo(a[(399:573),c(4,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c742)@teststat;summary(c742)@cval


v741=VAR(cbind(diff(a[(1:398),4]),diff(a[(1:398),7])),p=2)
cs741=causality(v741)$Granger$p.value
v742=VAR(cbind(diff(a[(399:573),4]),diff(a[(399:573),7])),p=2)
cs742=causality(v742)$Granger$p.value



###Oil==>USwheat###
lr.reg=lm(a[,5]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,5)],diff(a[,c(7,5)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)

sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)

summary(breakpoints(dy2~error.lagged+dy1,data=a2))
sctest(dy2[1:424]~error.lagged[1:424]+dy1[1:424],type="RE",data=a2)
sctest(dy2[425:573]~error.lagged[425:573]+dy1[425:573],type="RE",data=a2)

c751=ca.jo(a[(1:424),c(5,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c751)@teststat;summary(c751)@cval
c752=ca.jo(a[(425:573),c(5,7)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=52)
summary(c752)@teststat;summary(c752)@cval


v751=VAR(cbind(diff(a[(1:424),5]),diff(a[(1:424),7])),p=2)
cs751=causality(v751)$Granger$p.value
v752=VAR(cbind(diff(a[(425:573),5]),diff(a[(425:573),7])),p=2)
cs752=causality(v752)$Granger$p.value


###Oil==>USsoybean###
lr.reg=lm(a[,6]~a[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error),k=-1)
a2=cbind(a[,c(7,6)],diff(a[,c(7,6)]),error.lagged)
a2=window(a2,start=2,end=574)
colnames(a2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a2)

sctest(dy2~error.lagged+dy1, type="RE",data=a2)
sctest(dy2~error.lagged+dy1, type="ME",data=a2)
sctest(dy2~error.lagged+dy1,type="supF",data=a2)
sctest(dy2~error.lagged+dy1,type="expF",data=a2)
