##############GAS==>Beef########################################

lr.reg=lm(a.fe[,1]~a.fe[,6])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(6,1)],diff(a.fe[,c(6,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

*****Testing*****
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:45]~error.lagged[1:45]+dy1[1:45],type="RE",data=a.fe2)
sctest(dy2[1:45]~error.lagged[1:45]+dy1[1:45],type="ME",data=a.fe2)
sctest(dy2[1:45]~error.lagged[1:45]+dy1[1:45],type="supF",data=a.fe2)
sctest(dy2[1:45]~error.lagged[1:45]+dy1[1:45],type="expF",data=a.fe2)
sctest(dy2[46:259]~error.lagged[46:259]+dy1[46:259],type="RE",data=a.fe2)
sctest(dy2[46:259]~error.lagged[46:259]+dy1[46:259],type="ME",data=a.fe2)
sctest(dy2[46:259]~error.lagged[46:259]+dy1[46:259],type="supF",data=a.fe2)
sctest(dy2[46:259]~error.lagged[46:259]+dy1[46:259],type="expF",data=a.fe2)

##############GASOLINE==>Beef########################################

lr.reg=lm(a.fe[,1]~a.fe[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(7,1)],diff(a.fe[,c(7,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

*****Testing*****
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

##############GAS==>Chicken########################################

lr.reg=lm(a.fe[,2]~a.fe[,6])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(6,2)],diff(a.fe[,c(6,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

##############Gasoline==>Chicken########################################

lr.reg=lm(a.fe[,2]~a.fe[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(7,2)],diff(a.fe[,c(7,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:63]~error.lagged[1:63]+dy1[1:63],type="RE",data=a.fe2)
sctest(dy2[1:63]~error.lagged[1:63]+dy1[1:63],type="ME",data=a.fe2)
sctest(dy2[1:63]~error.lagged[1:63]+dy1[1:63],type="supF",data=a.fe2)
sctest(dy2[1:63]~error.lagged[1:63]+dy1[1:63],type="expF",data=a.fe2)
sctest(dy2[64:259]~error.lagged[64:259]+dy1[64:259],type="RE",data=a.fe2)
sctest(dy2[64:259]~error.lagged[64:259]+dy1[64:259],type="ME",data=a.fe2)
sctest(dy2[64:259]~error.lagged[64:259]+dy1[64:259],type="supF",data=a.fe2)
sctest(dy2[64:259]~error.lagged[64:259]+dy1[64:259],type="expF",data=a.fe2)


##############GAS==>Milk########################################

lr.reg=lm(a.fe[,3]~a.fe[,6])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(6,3)],diff(a.fe[,c(6,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

##############GASOLINE==>Milk########################################

lr.reg=lm(a.fe[,3]~a.fe[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(7,3)],diff(a.fe[,c(7,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="RE",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="ME",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="supF",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="expF",data=a.fe2)
sctest(dy2[110:259]~error.lagged[110:259]+dy1[110:259],type="RE",data=a.fe2)
sctest(dy2[110:259]~error.lagged[110:259]+dy1[110:259],type="ME",data=a.fe2)
sctest(dy2[110:259]~error.lagged[110:259]+dy1[110:259],type="supF",data=a.fe2)
sctest(dy2[110:259]~error.lagged[110:259]+dy1[110:259],type="expF",data=a.fe2)


sctest(dy2[1:75]~error.lagged[1:75]+dy1[1:75],type="RE",data=a.fe2)
sctest(dy2[1:75]~error.lagged[1:75]+dy1[1:75],type="ME",data=a.fe2)
sctest(dy2[1:75]~error.lagged[1:75]+dy1[1:75],type="supF",data=a.fe2)
sctest(dy2[1:75]~error.lagged[1:75]+dy1[1:75],type="expF",data=a.fe2)
sctest(dy2[76:117]~error.lagged[76:117]+dy1[76:117],type="RE",data=a.fe2)
sctest(dy2[76:117]~error.lagged[76:117]+dy1[76:117],type="ME",data=a.fe2)
sctest(dy2[76:117]~error.lagged[76:117]+dy1[76:117],type="supF",data=a.fe2)
sctest(dy2[76:117]~error.lagged[76:117]+dy1[76:117],type="expF",data=a.fe2)




##############GAS==>Apple########################################

lr.reg=lm(a.fe[,4]~a.fe[,6])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(6,4)],diff(a.fe[,c(6,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:212]~error.lagged[1:212]+dy1[1:212],type="RE",data=a.fe2)
sctest(dy2[1:212]~error.lagged[1:212]+dy1[1:212],type="ME",data=a.fe2)
sctest(dy2[1:212]~error.lagged[1:212]+dy1[1:212],type="supF",data=a.fe2)
sctest(dy2[1:212]~error.lagged[1:212]+dy1[1:212],type="expF",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="RE",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="ME",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="supF",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="expF",data=a.fe2)




##############GASOLINE==>Apple########################################

lr.reg=lm(a.fe[,4]~a.fe[,7])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(7,4)],diff(a.fe[,c(7,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2

#####Testing#######
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

