library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)
library(urca)
library(strucchange)
library(pastecs)
library(deseasonalize)
###########Import Data##################################################
data=read.csv(file="CPI goods prices 4.csv")
data=ts(data,start=c(1990,1),end=c(2011,8),frequency=12)
name=colnames(data)
length(name)
data=log(data)


###########Change the column name########################################
a=matrix(data=NA,nrow=260,ncol=25)
for (i in 1:25)
{a[,i]=data[,(i+1)]}
colnames(a)=c(1:25)
a=ts(a,start=c(1990,1),end=c(2011,8),frequency=12)

##############Break Point3 (Energy and Food)#############################
a.fe=a[,c(4,8,10,14,23)]

##############Deseasonalized data#######################################
a.fe=removeNA(decompose(a.fe)$random)


##############Energy==>Beef########################################

lr.reg=lm(a.fe[,1]~a.fe[,5])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(5,1)],diff(a.fe[,c(5,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


****PLOT*****
RE=efp(ecm.reg, type="RE", data=a.fe2)
plot(RE)
FS=Fstats(ecm.reg, data=a.fe2)
plot(FS)


*****Testing*****
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


###############Energy==>Chicken######################################
lr.reg=lm(a.fe[,2]~a.fe[,5])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(5,2)],diff(a.fe[,c(5,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


****PLOT*****
RE=efp(ecm.reg, type="RE", data=a.fe2)
plot(RE)
FS=Fstats(ecm.reg, data=a.fe2)
plot(FS)


*****Testing*****
sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


***NOTE**************
***Same as: sctest(dy2[1:259]~error.lagged[1:259]+dy1[1:259],type="supF",data=a.fe2)***
***But Different with: (dy2~error.lagged+dy1,from=1,to=259,type="supF",data=a.fe2)***
***WIERD!!!!MUST BE THE DEGREE OF FREEDOM******


summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="RE",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="ME",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="supF",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="expF",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="RE",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="ME",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="supF",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="expF",data=a.fe2)



###############Energy==>Milk######################################
lr.reg=lm(a.fe[,3]~a.fe[,5])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(5,3)],diff(a.fe[,c(5,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


***NOTE**************
***Same as: sctest(dy2[1:259]~error.lagged[1:259]+dy1[1:259],type="supF",data=a.fe2)***
***But Different with: (dy2~error.lagged+dy1,from=1,to=259,type="supF",data=a.fe2)***
***WIERD!!!!MUST BE THE DEGREE OF FREEDOM******


###############Energy==>Apple######################################
lr.reg=lm(a.fe[,4]~a.fe[,5])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(5,4)],diff(a.fe[,c(5,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:57]~error.lagged[1:57]+dy1[1:57],type="RE",data=a.fe2)
sctest(dy2[1:57]~error.lagged[1:57]+dy1[1:57],type="ME",data=a.fe2)
sctest(dy2[1:57]~error.lagged[1:57]+dy1[1:57],type="supF",data=a.fe2)
sctest(dy2[1:57]~error.lagged[1:57]+dy1[1:57],type="expF",data=a.fe2)
sctest(dy2[58:259]~error.lagged[58:259]+dy1[58:259],type="RE",data=a.fe2)
sctest(dy2[58:259]~error.lagged[58:259]+dy1[58:259],type="ME",data=a.fe2)
sctest(dy2[58:259]~error.lagged[58:259]+dy1[58:259],type="supF",data=a.fe2)
sctest(dy2[58:259]~error.lagged[58:259]+dy1[58:259],type="expF",data=a.fe2)

summary(breakpoints(dy2[1:57]~error.lagged[1:57]+dy1[1:57],data=a.fe2))
sctest(dy2[1:30]~error.lagged[1:30]+dy1[1:30],type="RE",data=a.fe2)
sctest(dy2[1:30]~error.lagged[1:30]+dy1[1:30],type="ME",data=a.fe2)
sctest(dy2[1:30]~error.lagged[1:30]+dy1[1:30],type="supF",data=a.fe2)
sctest(dy2[1:30]~error.lagged[1:30]+dy1[1:30],type="expF",data=a.fe2)
sctest(dy2[31:57]~error.lagged[31:57]+dy1[31:57],type="RE",data=a.fe2)
sctest(dy2[31:57]~error.lagged[31:57]+dy1[31:57],type="ME",data=a.fe2)
sctest(dy2[31:57]~error.lagged[31:57]+dy1[31:57],type="supF",data=a.fe2)
sctest(dy2[31:57]~error.lagged[31:57]+dy1[31:57],type="expF",data=a.fe2)

summary(breakpoints(dy2[1:30]~error.lagged[1:30]+dy1[1:30],data=a.fe2))
sctest(dy2[1:4]~error.lagged[1:4]+dy1[1:4],type="RE",data=a.fe2)
sctest(dy2[1:4]~error.lagged[1:4]+dy1[1:4],type="ME",data=a.fe2)
sctest(dy2[1:4]~error.lagged[1:4]+dy1[1:4],type="supF",data=a.fe2)
sctest(dy2[1:4]~error.lagged[1:4]+dy1[1:4],type="expF",data=a.fe2)
sctest(dy2[5:30]~error.lagged[5:30]+dy1[5:30],type="RE",data=a.fe2)
sctest(dy2[5:30]~error.lagged[5:30]+dy1[5:30],type="ME",data=a.fe2)
sctest(dy2[5:30]~error.lagged[5:30]+dy1[5:30],type="supF",data=a.fe2)
sctest(dy2[5:30]~error.lagged[5:30]+dy1[5:30],type="expF",data=a.fe2)

####################Beef==>Chicken#######################
lr.reg=lm(a.fe[,2]~a.fe[,1])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(1,2)],diff(a.fe[,c(1,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="RE",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="ME",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="supF",data=a.fe2)
sctest(dy2[1:66]~error.lagged[1:66]+dy1[1:66],type="expF",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="RE",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="ME",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="supF",data=a.fe2)
sctest(dy2[67:259]~error.lagged[67:259]+dy1[67:259],type="expF",data=a.fe2)

####################Beef==>Milk#######################
lr.reg=lm(a.fe[,3]~a.fe[,1])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(1,3)],diff(a.fe[,c(1,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))

####################Beef==>Apple#######################
lr.reg=lm(a.fe[,4]~a.fe[,1])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(1,4)],diff(a.fe[,c(1,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Beef==>Energy#######################
lr.reg=lm(a.fe[,5]~a.fe[,1])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(1,5)],diff(a.fe[,c(1,5)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:190]~error.lagged[1:190]+dy1[1:190],type="RE",data=a.fe2)
sctest(dy2[1:190]~error.lagged[1:190]+dy1[1:190],type="ME",data=a.fe2)
sctest(dy2[1:190]~error.lagged[1:190]+dy1[1:190],type="supF",data=a.fe2)
sctest(dy2[1:190]~error.lagged[1:190]+dy1[1:190],type="expF",data=a.fe2)
sctest(dy2[191:259]~error.lagged[191:259]+dy1[191:259],type="RE",data=a.fe2)
sctest(dy2[191:259]~error.lagged[191:259]+dy1[191:259],type="ME",data=a.fe2)
sctest(dy2[191:259]~error.lagged[191:259]+dy1[191:259],type="supF",data=a.fe2)
sctest(dy2[191:259]~error.lagged[191:259]+dy1[191:259],type="expF",data=a.fe2)

####################Chicken==>Beef#######################
lr.reg=lm(a.fe[,1]~a.fe[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(2,1)],diff(a.fe[,c(2,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:154]~error.lagged[1:154]+dy1[1:154],type="RE",data=a.fe2)
sctest(dy2[1:154]~error.lagged[1:154]+dy1[1:154],type="ME",data=a.fe2)
sctest(dy2[1:154]~error.lagged[1:154]+dy1[1:154],type="supF",data=a.fe2)
sctest(dy2[1:154]~error.lagged[1:154]+dy1[1:154],type="expF",data=a.fe2)
sctest(dy2[155:259]~error.lagged[155:259]+dy1[155:259],type="RE",data=a.fe2)
sctest(dy2[155:259]~error.lagged[155:259]+dy1[155:259],type="ME",data=a.fe2)
sctest(dy2[155:259]~error.lagged[155:259]+dy1[155:259],type="supF",data=a.fe2)
sctest(dy2[155:259]~error.lagged[155:259]+dy1[155:259],type="expF",data=a.fe2)


####################Chicken==>Milk#######################
lr.reg=lm(a.fe[,3]~a.fe[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(2,3)],diff(a.fe[,c(2,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))

sctest(dy2[1:170]~error.lagged[1:170]+dy1[1:170],type="RE",data=a.fe2)
sctest(dy2[1:170]~error.lagged[1:170]+dy1[1:170],type="ME",data=a.fe2)
sctest(dy2[1:170]~error.lagged[1:170]+dy1[1:170],type="supF",data=a.fe2)
sctest(dy2[1:170]~error.lagged[1:170]+dy1[1:170],type="expF",data=a.fe2)
sctest(dy2[171:259]~error.lagged[171:259]+dy1[171:259],type="RE",data=a.fe2)
sctest(dy2[171:259]~error.lagged[171:259]+dy1[171:259],type="ME",data=a.fe2)
sctest(dy2[171:259]~error.lagged[171:259]+dy1[171:259],type="supF",data=a.fe2)
sctest(dy2[171:259]~error.lagged[171:259]+dy1[171:259],type="expF",data=a.fe2)

summary(breakpoints(dy2[1:170]~error.lagged[1:170]+dy1[1:170],data=a.fe2))
summary(breakpoints(dy2[171:259]~error.lagged[171:259]+dy1[171:259],data=a.fe2))
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="RE",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="ME",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="supF",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="expF",data=a.fe2)
sctest(dy2[110:170]~error.lagged[110:170]+dy1[110:170],type="RE",data=a.fe2)
sctest(dy2[110:170]~error.lagged[110:170]+dy1[110:170],type="ME",data=a.fe2)
sctest(dy2[110:170]~error.lagged[110:170]+dy1[110:170],type="supF",data=a.fe2)
sctest(dy2[110:170]~error.lagged[110:170]+dy1[110:170],type="expF",data=a.fe2)
sctest(dy2[171:219]~error.lagged[171:219]+dy1[171:219],type="RE",data=a.fe2)
sctest(dy2[171:219]~error.lagged[171:219]+dy1[171:219],type="ME",data=a.fe2)
sctest(dy2[171:219]~error.lagged[171:219]+dy1[171:219],type="supF",data=a.fe2)
sctest(dy2[171:219]~error.lagged[171:219]+dy1[171:219],type="expF",data=a.fe2)
sctest(dy2[220:259]~error.lagged[220:259]+dy1[220:259],type="RE",data=a.fe2)
sctest(dy2[220:259]~error.lagged[220:259]+dy1[220:259],type="ME",data=a.fe2)
sctest(dy2[220:259]~error.lagged[220:259]+dy1[220:259],type="supF",data=a.fe2)
sctest(dy2[220:259]~error.lagged[220:259]+dy1[220:259],type="expF",data=a.fe2)

summary(breakpoints(dy2[171:219]~error.lagged[171:219]+dy1[171:219],data=a.fe2))
summary(breakpoints(dy2[220:259]~error.lagged[220:259]+dy1[220:259],data=a.fe2))

sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="RE",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="ME",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="supF",data=a.fe2)
sctest(dy2[1:109]~error.lagged[1:109]+dy1[1:109],type="expF",data=a.fe2)
sctest(dy2[110:162]~error.lagged[110:162]+dy1[110:162],type="RE",data=a.fe2)
sctest(dy2[110:162]~error.lagged[110:162]+dy1[110:162],type="ME",data=a.fe2)
sctest(dy2[110:162]~error.lagged[110:162]+dy1[110:162],type="supF",data=a.fe2)
sctest(dy2[110:162]~error.lagged[110:162]+dy1[110:162],type="expF",data=a.fe2)
sctest(dy2[163:203]~error.lagged[163:203]+dy1[163:203],type="RE",data=a.fe2)
sctest(dy2[163:203]~error.lagged[163:203]+dy1[163:203],type="ME",data=a.fe2)
sctest(dy2[163:203]~error.lagged[163:203]+dy1[163:203],type="supF",data=a.fe2)
sctest(dy2[163:203]~error.lagged[163:203]+dy1[163:203],type="expF",data=a.fe2)
sctest(dy2[204:259]~error.lagged[204:259]+dy1[204:259],type="RE",data=a.fe2)
sctest(dy2[204:259]~error.lagged[204:259]+dy1[204:259],type="ME",data=a.fe2)
sctest(dy2[204:259]~error.lagged[204:259]+dy1[204:259],type="supF",data=a.fe2)
sctest(dy2[204:259]~error.lagged[204:259]+dy1[204:259],type="expF",data=a.fe2)

####################Chicken==>Apple#######################
lr.reg=lm(a.fe[,4]~a.fe[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(2,4)],diff(a.fe[,c(2,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:182]~error.lagged[1:182]+dy1[1:182],type="RE",data=a.fe2)
sctest(dy2[1:182]~error.lagged[1:182]+dy1[1:182],type="ME",data=a.fe2)
sctest(dy2[1:182]~error.lagged[1:182]+dy1[1:182],type="supF",data=a.fe2)
sctest(dy2[1:182]~error.lagged[1:182]+dy1[1:182],type="expF",data=a.fe2)
sctest(dy2[183:259]~error.lagged[183:259]+dy1[183:259],type="RE",data=a.fe2)
sctest(dy2[183:259]~error.lagged[183:259]+dy1[183:259],type="ME",data=a.fe2)
sctest(dy2[183:259]~error.lagged[183:259]+dy1[183:259],type="supF",data=a.fe2)
sctest(dy2[183:259]~error.lagged[183:259]+dy1[183:259],type="expF",data=a.fe2)

sctest(dy2[1:91]~error.lagged[1:91]+dy1[1:91],type="RE",data=a.fe2)
sctest(dy2[1:91]~error.lagged[1:91]+dy1[1:91],type="ME",data=a.fe2)
sctest(dy2[1:91]~error.lagged[1:91]+dy1[1:91],type="supF",data=a.fe2)
sctest(dy2[1:91]~error.lagged[1:91]+dy1[1:91],type="expF",data=a.fe2)
sctest(dy2[92:182]~error.lagged[92:182]+dy1[92:182],type="RE",data=a.fe2)
sctest(dy2[92:182]~error.lagged[92:182]+dy1[92:182],type="ME",data=a.fe2)
sctest(dy2[92:182]~error.lagged[92:182]+dy1[92:182],type="supF",data=a.fe2)
sctest(dy2[92:182]~error.lagged[92:182]+dy1[92:182],type="expF",data=a.fe2)


####################Chicken==>Energy#######################
lr.reg=lm(a.fe[,5]~a.fe[,2])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(2,5)],diff(a.fe[,c(2,5)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Milk==>Beef#######################
lr.reg=lm(a.fe[,1]~a.fe[,3])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(3,1)],diff(a.fe[,c(3,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Milk==>Chicken#######################
lr.reg=lm(a.fe[,2]~a.fe[,3])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(3,2)],diff(a.fe[,c(3,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Milk==>Apple#######################
lr.reg=lm(a.fe[,4]~a.fe[,3])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(3,4)],diff(a.fe[,c(3,4)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Milk==>Energy#######################
lr.reg=lm(a.fe[,5]~a.fe[,3])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(3,5)],diff(a.fe[,c(3,5)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Apple==>Beef#######################
lr.reg=lm(a.fe[,1]~a.fe[,4])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(4,1)],diff(a.fe[,c(4,1)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:129]~error.lagged[1:129]+dy1[1:129],type="RE",data=a.fe2)
sctest(dy2[1:129]~error.lagged[1:129]+dy1[1:129],type="ME",data=a.fe2)
sctest(dy2[1:129]~error.lagged[1:129]+dy1[1:129],type="supF",data=a.fe2)
sctest(dy2[1:129]~error.lagged[1:129]+dy1[1:129],type="expF",data=a.fe2)
sctest(dy2[130:259]~error.lagged[130:259]+dy1[130:259],type="RE",data=a.fe2)
sctest(dy2[130:259]~error.lagged[130:259]+dy1[130:259],type="ME",data=a.fe2)
sctest(dy2[130:259]~error.lagged[130:259]+dy1[130:259],type="supF",data=a.fe2)
sctest(dy2[130:259]~error.lagged[130:259]+dy1[130:259],type="expF",data=a.fe2)

####################Apple==>Chicken#######################
lr.reg=lm(a.fe[,2]~a.fe[,4])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(4,2)],diff(a.fe[,c(4,2)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

summary(breakpoints(dy2~error.lagged+dy1,data=a.fe2))
sctest(dy2[1:204]~error.lagged[1:204]+dy1[1:204],type="RE",data=a.fe2)
sctest(dy2[1:204]~error.lagged[1:204]+dy1[1:204],type="ME",data=a.fe2)
sctest(dy2[1:204]~error.lagged[1:204]+dy1[1:204],type="supF",data=a.fe2)
sctest(dy2[1:204]~error.lagged[1:204]+dy1[1:204],type="expF",data=a.fe2)
sctest(dy2[205:259]~error.lagged[205:259]+dy1[205:259],type="RE",data=a.fe2)
sctest(dy2[205:259]~error.lagged[205:259]+dy1[205:259],type="ME",data=a.fe2)
sctest(dy2[205:259]~error.lagged[205:259]+dy1[205:259],type="supF",data=a.fe2)
sctest(dy2[205:259]~error.lagged[205:259]+dy1[205:259],type="expF",data=a.fe2)

sctest(dy2[1:146]~error.lagged[1:146]+dy1[1:146],type="RE",data=a.fe2)
sctest(dy2[1:146]~error.lagged[1:146]+dy1[1:146],type="ME",data=a.fe2)
sctest(dy2[1:146]~error.lagged[1:146]+dy1[1:146],type="supF",data=a.fe2)
sctest(dy2[1:146]~error.lagged[1:146]+dy1[1:146],type="expF",data=a.fe2)
sctest(dy2[147:212]~error.lagged[147:212]+dy1[147:212],type="RE",data=a.fe2)
sctest(dy2[147:212]~error.lagged[147:212]+dy1[147:212],type="ME",data=a.fe2)
sctest(dy2[147:212]~error.lagged[147:212]+dy1[147:212],type="supF",data=a.fe2)
sctest(dy2[147:212]~error.lagged[147:212]+dy1[147:212],type="expF",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="RE",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="ME",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="supF",data=a.fe2)
sctest(dy2[213:259]~error.lagged[213:259]+dy1[213:259],type="expF",data=a.fe2)

summary(breakpoints(dy2[213:259]~error.lagged[213:259]+dy1[213:259],data=a.fe2))
sctest(dy2[1:224]~error.lagged[1:224]+dy1[1:224],type="RE",data=a.fe2)
sctest(dy2[1:224]~error.lagged[1:224]+dy1[1:224],type="ME",data=a.fe2)
sctest(dy2[1:224]~error.lagged[1:224]+dy1[1:224],type="supF",data=a.fe2)
sctest(dy2[1:224]~error.lagged[1:224]+dy1[1:224],type="expF",data=a.fe2)
sctest(dy2[225:259]~error.lagged[225:259]+dy1[225:259],type="RE",data=a.fe2)
sctest(dy2[225:259]~error.lagged[225:259]+dy1[225:259],type="ME",data=a.fe2)
sctest(dy2[225:259]~error.lagged[225:259]+dy1[225:259],type="supF",data=a.fe2)
sctest(dy2[225:259]~error.lagged[225:259]+dy1[225:259],type="expF",data=a.fe2)


####################Apple==>Milk#######################
lr.reg=lm(a.fe[,3]~a.fe[,4])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(4,3)],diff(a.fe[,c(4,3)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)

####################Apple==>Milk#######################
lr.reg=lm(a.fe[,5]~a.fe[,4])
error=residuals(lr.reg)
error.lagged=lag(ts(error,start=c(1990,1),freq=12),k=-1)
a.fe2=cbind(a.fe[,c(4,5)],diff(a.fe[,c(4,5)]),error.lagged)
a.fe2=window(a.fe2,start=c(1990,2),end=c(2011,8))
colnames(a.fe2)=c("y1","y2","dy1","dy2","error.lagged")
ecm.reg=lm(dy2~error.lagged+dy1,data=a.fe2)
ecm.reg2=lm(dy2[1:259]~error.lagged[1:259]+dy1[1:259],data=a.fe2)
ecm.reg; ecm.reg2


sctest(dy2~error.lagged+dy1, type="RE",data=a.fe2)
sctest(dy2~error.lagged+dy1, type="ME",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="supF",data=a.fe2)
sctest(dy2~error.lagged+dy1,type="expF",data=a.fe2)


####################irf###############################
####################Energy==>Beef#####################
VAR1=VAR(a.fe[,c(1,5)],p=min(VARselect(a.fe[,c(1,5)])$selection), type="both")
plot(irf(VAR1,boot=TRUE))

####################Energy==>Chicken##################
VAR1=VAR(a.fe[(1:66),c(2,5)],p=min(VARselect(a.fe[(1:66),c(2,5)])$selection), type="both")
VAR2=VAR(a.fe[(67:259),c(2,5)],p=min(VARselect(a.fe[(67:259),c(2,5)])$selection), type="both")
plot(irf(VAR1,boot=TRUE));plot(irf(VAR2,boot=TRUE))

####################Energy==>Milk######################
VAR1=VAR(a.fe[,c(3,5)],p=min(VARselect(a.fe[,c(3,5)])$selection), type="both")
plot(irf(VAR1,boot=TRUE))


####################Energy==>Apple##################
VAR1=VAR(a.fe[(1:30),c(4,5)],p=5, type="both")
VAR2=VAR(a.fe[(31:57),c(4,5)],p=3, type="both")
VAR3=VAR(a.fe[(58:259),c(4,5)],p=13, type="both")
plot(irf(VAR1,boot=TRUE));plot(irf(VAR2,boot=TRUE));plot(irf(VAR3,boot=TRUE))


#####################VECM==>VAR==>IRF######################
c1=ca.jo(a.fe[,c(1,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
summary(c1)@teststat;summary(c1)@cval
v1=vec2var(c1)
plot(irf(v1,boot=TRUE))



c21=ca.jo(a.fe[(1:66),c(2,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
summary(c21)@teststat;summary(c21)@cval
c22=ca.jo(a.fe[(67:259),c(2,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
summary(c22)@teststat;summary(c22)@cval

c3=ca.jo(a.fe[,c(3,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c3@teststat;c3@cval

c41=ca.jo(a.fe[(1:30),c(4,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c41@teststat;c41@cval
c42=ca.jo(a.fe[(31:57),c(4,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c42@teststat;c42@cval
c43=ca.jo(a.fe[(58:259),c(4,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c43@teststat;c43@cval

##############################VEC2VAR################
v1=vec2var(c1)
v21=vec2var(c21)
v22=vec2var(c22)
v3=vec2var(c3)
v41=vec2var(c41)
v42=vec2var(c42)
v43=vec2var(c43)

v1;v21;v22;v3;v41;v42;v43

plot(irf(v1,boot=TRUE))
plot(irf(v21,boot=TRUE))
plot(irf(v22,boot=TRUE))
plot(irf(v3,boot=TRUE))
plot(irf(v41,boot=TRUE))
plot(irf(v42,boot=TRUE))
plot(irf(v43,boot=TRUE))

####################################DESEASONALIZED DATA COINTEGRATION#############

c1=ca.jo(a.fe[,c(1,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c2=ca.jo(a.fe[,c(2,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c3=ca.jo(a.fe[,c(3,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)
c4=ca.jo(a.fe[,c(4,5)], type = "trace", ecdet = "trend", K = 2,spec = "transitory",season=12)

summary(c1)@teststat;summary(c1)@cval
summary(c2)@teststat;summary(c2)@cval
summary(c3)@teststat;summary(c3)@cval
summary(c4)@teststat;summary(c4)@cval

