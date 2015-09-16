##############Fuel==>Bread########################################

lr.reg=lm(a.fe[,1]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,1)],diff(a.fe[,c(8,1)]),error.lagged)
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


##############Fuel==>beef########################################
lr.reg=lm(a.fe[,2]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,2)],diff(a.fe[,c(8,2)]),error.lagged)
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

##############Electricity==>Bread########################################
lr.reg=lm(a.fe[,1]~a.fe[,9])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(9,1)],diff(a.fe[,c(9,1)]),error.lagged)
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


##############GAS==>Bread########################################
lr.reg=lm(a.fe[,1]~a.fe[,10])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(10,1)],diff(a.fe[,c(10,1)]),error.lagged)
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


##############GASoline==>Bread########################################
lr.reg=lm(a.fe[,1]~a.fe[,11])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(11,1)],diff(a.fe[,c(11,1)]),error.lagged)
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



##############Fuel==>Chicken########################################
lr.reg=lm(a.fe[,3]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,3)],diff(a.fe[,c(8,3)]),error.lagged)
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



##############Fuel==>Milk########################################
lr.reg=lm(a.fe[,4]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,4)],diff(a.fe[,c(8,4)]),error.lagged)
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
sctest(dy2[1:85]~error.lagged[1:85]+dy1[1:85],type="RE",data=a.fe2)
sctest(dy2[1:85]~error.lagged[1:85]+dy1[1:85],type="ME",data=a.fe2)
sctest(dy2[1:85]~error.lagged[1:85]+dy1[1:85],type="supF",data=a.fe2)
sctest(dy2[1:85]~error.lagged[1:85]+dy1[1:85],type="expF",data=a.fe2)
sctest(dy2[86:220]~error.lagged[86:220]+dy1[86:220],type="RE",data=a.fe2)
sctest(dy2[86:220]~error.lagged[86:220]+dy1[86:220],type="ME",data=a.fe2)
sctest(dy2[86:220]~error.lagged[86:220]+dy1[86:220],type="supF",data=a.fe2)
sctest(dy2[86:220]~error.lagged[86:220]+dy1[86:220],type="expF",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="RE",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="ME",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="supF",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="expF",data=a.fe2)




##############Fuel==>Apple########################################
lr.reg=lm(a.fe[,5]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,5)],diff(a.fe[,c(8,5)]),error.lagged)
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
sctest(dy2[1:194]~error.lagged[1:194]+dy1[1:194],type="RE",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="ME",data=a.fe2)
sctest(dy2[1:194]~error.lagged[1:194]+dy1[1:194],type="supF",data=a.fe2)
sctest(dy2[1:194]~error.lagged[1:194]+dy1[1:194],type="expF",data=a.fe2)
sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="expF",data=a.fe2)
sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="expF",data=a.fe2)
sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="expF",data=a.fe2)
sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="expF",data=a.fe2)





##############Fuel==>OJ########################################
lr.reg=lm(a.fe[,6]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,6)],diff(a.fe[,c(8,6)]),error.lagged)
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


##############Fuel==>PC########################################
lr.reg=lm(a.fe[,7]~a.fe[,8])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(8,7)],diff(a.fe[,c(8,7)]),error.lagged)
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
sctest(dy2[1:47]~error.lagged[1:47]+dy1[1:47],type="RE",data=a.fe2)
sctest(dy2[48:176]~error.lagged[48:176]+dy1[48:176],type="ME",data=a.fe2)
sctest(dy2[1:220]~error.lagged[1:220]+dy1[1:220],type="supF",data=a.fe2)
sctest(dy2[1:220]~error.lagged[1:220]+dy1[1:220],type="expF",data=a.fe2)
sctest(dy2[177:259]~error.lagged[221:259]+dy1[221:259],type="RE",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="ME",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="supF",data=a.fe2)
sctest(dy2[221:259]~error.lagged[221:259]+dy1[221:259],type="expF",data=a.fe2)


summary(breakpoints(dy2[89:259]~error.lagged[89:259]+dy1[89:259],data=a.fe2))




##############ELECTRI==>OJ########################################
lr.reg=lm(a.fe[,6]~a.fe[,9])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(9,6)],diff(a.fe[,c(9,6)]),error.lagged)
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
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="RE",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="ME",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="supF",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="expF",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="RE",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="ME",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="supF",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="expF",data=a.fe2)


##############GAS==>OJ########################################
lr.reg=lm(a.fe[,6]~a.fe[,10])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(10,6)],diff(a.fe[,c(10,6)]),error.lagged)
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



##############GASOLINE==>OJ########################################
lr.reg=lm(a.fe[,6]~a.fe[,11])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(11,6)],diff(a.fe[,c(11,6)]),error.lagged)
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



##############GASOLINE==>PC########################################
lr.reg=lm(a.fe[,7]~a.fe[,11])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(11,7)],diff(a.fe[,c(11,7)]),error.lagged)
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
sctest(dy2[1:215]~error.lagged[1:215]+dy1[1:215],type="RE",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="ME",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="supF",data=a.fe2)
sctest(dy2[1:103]~error.lagged[1:103]+dy1[1:103],type="expF",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="RE",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="ME",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="supF",data=a.fe2)
sctest(dy2[104:259]~error.lagged[104:259]+dy1[104:259],type="expF",data=a.fe2)







##############GAS==>PC########################################
lr.reg=lm(a.fe[,7]~a.fe[,10])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(10,7)],diff(a.fe[,c(10,7)]),error.lagged)
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
sctest(dy2[1:76]~error.lagged[1:76]+dy1[1:76],type="RE",data=a.fe2)

sctest(dy2[135:259]~error.lagged[135:259]+dy1[135:259],type="RE",data=a.fe2)
sctest(dy2[77:97]~error.lagged[77:97]+dy1[77:97],type="RE",data=a.fe2)

sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="RE",data=a.fe2)





##############ELECTR==>PC########################################
lr.reg=lm(a.fe[,7]~a.fe[,9])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(9,7)],diff(a.fe[,c(9,7)]),error.lagged)
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
sctest(dy2[1:76]~error.lagged[1:76]+dy1[1:76],type="RE",data=a.fe2)

sctest(dy2[135:259]~error.lagged[135:259]+dy1[135:259],type="RE",data=a.fe2)
sctest(dy2[77:97]~error.lagged[77:97]+dy1[77:97],type="RE",data=a.fe2)

sctest(dy2[216:259]~error.lagged[216:259]+dy1[216:259],type="RE",data=a.fe2)