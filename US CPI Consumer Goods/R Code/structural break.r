a.fe=a[,c(4,8,10,14,23)]
lr.reg=lm(a.fe[,3]~a.fe[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(1,2)],diff(a.fe[,c(1,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2[1:259]~error.lagged[1:259]+dy1[1:259],type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2[1:259]~error.lagged[1:259]+dy1[1:259],type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,from=1,to=259,type="supF",data=a.fe2)


summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="supF",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="RE",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="supF",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="RE",data=a.fe2)
summary(breakpoints(dy2[67:259]~error.lagged[67:259]+dy1[67:259],data=a.fe2))
sctest(dy2[67:169]~error.lagged[67:169]+dy1[67:169],type="RE",data=a.fe2)
sctest(dy2[170:259]~error.lagged[170:259]+dy1[170:259],type="RE",data=a.fe2)