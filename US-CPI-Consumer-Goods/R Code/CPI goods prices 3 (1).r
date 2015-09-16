#Call libraries
library(tseries)
library(timeSeries)
library(stats)
library(forecast)
library(MSBVAR)
library(vars)

###predict arima
auto.arima(data.ts)
fit=arima(data.ts,order=c(2,0,0),xreg=1:length(data.ts))
fore=predict(fit,12,newxreg=(length(data.ts)+1):(length(data.ts)+12))
ts.plot(data.ts,fore$pred)

# read data
data=read.csv(file="CPI goods prices 2.csv")
data=ts(data, start=1990, frequency=12)
data.t=t(data)
data.r=removeNA(data.t)
data.r.t=t(data.r)
data.r.t=ts(data.r.t,start=1990,frequency=12)
name=colnames(data)
length(name)
name2=colnames(data.r.t)
length(name2)
data.e=data.r.t[,c(34:35,39:42)]
name3=colnames(data.e)
name3


##########################
sink("myfile", append=FALSE, split=FALSE)
{var1=VARselect(data.r.t[,2:39],lag.max = 5, type="const")
v=VAR(data.r.t[,2:39],p=var1$selection[1])
show(v)}
#######################


#stationarity test 1 (level)
a=matrix(data=NA,nrow=41,ncol=1)
a.p=matrix(data=NA,nrow=41,ncol=1)
b=matrix(data=NA,nrow=41,ncol=1)
b.p=matrix(data=NA,nrow=41,ncol=1)
k=matrix(data=NA,nrow=41,ncol=1)
k.p=matrix(data=NA,nrow=41,ncol=1)
p=matrix(data=NA,nrow=41,ncol=1)
p.p=matrix(data=NA,nrow=41,ncol=1)

for (i in  2: 42)
{
a.t=adf.test(data.r.t[,i],alternative="stationary")
b.t=Box.test(data.r.t[,i])
k.t=kpss.test(data.r.t[,i])
p.t=pp.test(data.r.t[,i])
a[i]=a.t$statistic
a.p[i]=a.t$p.value
b[i]=b.t$statistic
b.p[i]=b.t$p.value
k[i]=k.t$statistic
k.p[i]=k.t$p.value
p[i]=p.t$statistic
p.p[i]=p.t$p.value
}
summary.table=cbind(a,a.p,b,b.p,k,k.p,p,p.p)
summary.table

#stationarity test 2 (diff)
a.d=matrix(data=NA,nrow=41,ncol=1)
a.p.d=matrix(data=NA,nrow=41,ncol=1)
k.d=matrix(data=NA,nrow=41,ncol=1)
k.p.d=matrix(data=NA,nrow=41,ncol=1)
p.d=matrix(data=NA,nrow=41,ncol=1)
p.p.d=matrix(data=NA,nrow=41,ncol=1)
for (i in  2: 42)
{
a.t.d=adf.test(diff(data.r.t[,i]),alternative="stationary")
k.t.d=kpss.test(diff(data.r.t[,i]))
p.t.d=pp.test(diff(data.r.t[,i]))
a.d[i]=a.t.d$statistic
a.p.d[i]=a.t.d$p.value
k.d[i]=k.t.d$statistic
k.p.d[i]=k.t.d$p.value
p.d[i]=p.t.d$statistic
p.p.d[i]=p.t.d$p.value
}
summary.table=cbind(a.d,a.p.d,k.d,k.p.d,p.d,p.p.d)
summary.table



#cointegration 1(random component)
p=matrix(data=NA,nrow=33,ncol=1)
r=matrix(data=NA,nrow=33,ncol=1)

for (i in 2:33)
{
lm=lm(decompose(data.r.t[,i])$random~decompose(data.r.t[,31])$random+
decompose(data.r.t[,32])$random+
decompose(data.r.t[,36])$random+
decompose(data.r.t[,37])$random+
decompose(data.r.t[,38])$random+
decompose(data.r.t[,39])$random)
f.stat=summary(lm)$fstatistic
p.value <- 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
p[i]=p.value
r[i]=summary(lm)$adj.r.squared
}
p;r


pm=matrix(data=NA,nrow=33,ncol=6)
rm=matrix(data=NA,nrow=33,ncol=6)

for (i in 2:33)
{ for (j in 1:6)
{lm1=lm(decompose(data.r.t[,i])$random~decompose(data.e[,j])$random)
f.stat=summary(lm1)$fstatistic
p.value <- 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
pm[i,j]=p.value
rm[i,j]=summary(lm1)$adj.r.squared
}
}
pm
rm

#cointegration 2(difference)
pm2=matrix(data=NA,nrow=33,ncol=6)
rm2=matrix(data=NA,nrow=33,ncol=6)


for (i in 2:33)
{ for (j in 1:6)
{lm1=lm(diff(data.r.t[,i])~diff(data.e[,j]))
f.stat=summary(lm1)$fstatistic
p.value <- 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
pm2[i,j]=p.value
rm2[i,j]=summary(lm1)$adj.r.squared
}
}
pm2
rm2


###comovement of energy prices

pm3=matrix(data=NA,nrow=6,ncol=6)

for (i in 1:6)
{ for (j in 1:6)
{lm1=lm(diff(data.e[,i])~diff(data.e[,j]))
f.stat=summary(lm1)$fstatistic
p.value <- 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])
pm3[i,j]=p.value
}
}
pm3
\\\

v2=matrix(data=NA,nrow=6,ncol=6)
for (i in 1:6)
{ for (j in (i+1):6){
var1=VARselect(cbind(diff(data.e[,j]),diff(data.e[,i])),lag.max = 5, type="const")
v=VAR(cbind(diff(data.e[,j]),diff(data.e[,i])),p=var1$selection[1])
c=causality(v)
v2[i,j]=c$Granger$p.value
}}
v2

v3=matrix(data=NA,nrow=6,ncol=6)
for (i in 1:6)
{ for (j in (i+1):6){
var1=VARselect(cbind(diff(data.e[,i]),diff(data.e[,j])),lag.max = 5, type="const")
v=VAR(cbind(diff(data.e[,i]),diff(data.e[,j])),p=var1$selection[1])
c=causality(v)
v3[i,j]=c$Granger$p.value
}}
v3
####causility
c=matrix(data=NA,nrow=33,ncol=6)

for (i in 2:33)
{ for (j in 1:6){
gr=granger.test(cbind(data.r.t[,i],data.e[,j]),p=6)
c[i,j]=gr[,"p-value"][1]
}}
c

####VAR
v1=matrix(data=NA,nrow=33,ncol=6)
for (i in 2:33)
{ for (j in 1:6){
var1=VARselect(cbind(diff(data.e[,j]),diff(data.r.t[,i])),lag.max = 5, type="const")
v=VAR(cbind(diff(data.e[,j]),diff(data.r.t[,i])),p=var1$selection[1])
c=causality(v)
v1[i,j]=c$Granger$p.value
}}
v1


#univariate plot data without missing values
pdf(file="c:\\Univariate Plot without missing Values.pdf")
for (i in 2:42)
{
plot(data.r.t[,i],xlab="Date",ylab=name[i],main=name[i],
font.main=2,xaxs="i",cex.sub=.6,font.sub=4,
sub="Source:BLS All US Cities CPI Average Price http://data.bls.gov/pdq/querytool.jsp?survey=ap Monthly Average")
}
dev.off()

#Bivariate Plot with out missing values
pdf(file="c:\\Bivariate Plot.pdf")
for (i in 2:33)
{ for (j in 1:6)
		{ts.plot(data.r.t[,i],data.e[,j],lty=c(1,2),xlab="Date",ylab=c(name[i], "and", name[j]), 
main=c(name[i], "and", name[j]),
sub="Source:BLS CPI http://data.bls.gov/pdq/querytool.jsp?survey=ap,Solid line is food price")
}}

dev.off()




# plot data (Univariate)
pdf(file="c:\\Univariate Plot.pdf")
for (i in 2:length(name))
{
plot(data[,i],xlab="Date",ylab=name[i],main=name[i],
font.main=2,xaxs="i",cex.sub=.6,font.sub=4,
sub="Source:BLS All US Cities CPI Average Price http://data.bls.gov/pdq/querytool.jsp?survey=ap Monthly Average")
}
dev.off()


#Adjust Data for bivariate plot
data[,2]=data[,2]*8
name[2]=paste(name[2],"multiplied by 8")
data[,3]=data[,3]*5
name[3]=paste(name[3],"multiplied by 5")
data[,4]=data[,4]*3
name[4]=paste(name[4],"multiplied by 3")
data[,5]=data[,5]*3
name[5]=paste(name[5],"multiplied by 3")
data[,6]=data[,6]*2
name[6]=paste(name[6],"multiplied by 2")
data[,7]=data[,7]*2
name[7]=paste(name[7],"multiplied by 2")
data[,9]=data[,9]*2
name[9]=paste(name[9],"multiplied by 2")
data[,13]=data[,13]*2
name[13]=paste(name[13],"multiplied by 2")
data[,19]=data[,19]/2
name[19]=paste(name[19],"devided by 2")
data[,20]=data[,20]/2
name[20]=paste(name[20],"devided by 2")
data[,26]=data[,26]/2
name[26]=paste(name[26],"devided by 2")
data[,27]=data[,27]/2
name[27]=paste(name[27],"devided by 2")
data[,31]=data[,31]*2
name[31]=paste(name[31],"multiplied by 2")
data[,34]=data[,34]*3
name[34]=paste(name[34],"multiplied by 3")
data[,36]=data[,36]*2
name[36]=paste(name[36],"multiplied by 2")
data[,39]=data[,39]*3
name[39]=paste(name[39],"multiplied by 3")
data[,40]=data[,40]*2
name[40]=paste(name[40],"multiplied by 2")
data[,41]=data[,41]*3
name[41]=paste(name[41],"multiplied by 3")
data[,42]=data[,42]*2
name[42]=paste(name[42],"multiplied by 2")
data[,43]=data[,43]*2
name[43]=paste(name[43],"multiplied by 2")
data[,44]=data[,44]*2
name[44]=paste(name[44],"multiplied by 2")
data[,45]=data[,45]*3
name[45]=paste(name[45],"multiplied by 3")
data[,46]=data[,46]*2
name[46]=paste(name[46],"multiplied by 2")
data[,48]=data[,48]*3
name[48]=paste(name[48],"multiplied by 3")
data[,51]=data[,51]*5
name[51]=paste(name[51],"multiplied by 5")
data[,55]=data[,55]*2
name[55]=paste(name[55],"multiplied by 2")
data[,56]=data[,56]*7
name[56]=paste(name[56],"multiplied by 7")
data[,57]=data[,57]*3
name[57]=paste(name[57],"multiplied by 3")
data[,58]=data[,58]*4
name[58]=paste(name[58],"multiplied by 4")
data[,59]=data[,59]*3
name[59]=paste(name[59],"multiplied by 3")
data[,60]=data[,60]*2
name[60]=paste(name[60],"multiplied by 2")
data[,61]=data[,61]*3
name[61]=paste(name[61],"multiplied by 3")
data[,62]=data[,62]*2
name[62]=paste(name[62],"multiplied by 2")
data[,66]=data[,66]*5
name[66]=paste(name[66],"multiplied by 5")
data[,67]=data[,67]*3
name[67]=paste(name[67],"multiplied by 3")
data[,69]=data[,69]*5
name[69]=paste(name[69],"multiplied by 5")
data[,70]=data[,70]*3
name[70]=paste(name[70],"multiplied by 3")
data[,71]=data[,71]*5
name[71]=paste(name[71],"multiplied by 5")
data[,72]=data[,72]*5
name[72]=paste(name[72],"multiplied by 5")
data[,74]=data[,74]*4
name[74]=paste(name[74],"multiplied by 4")
data[,75]=data[,75]*2
name[75]=paste(name[75],"multiplied by 2")
data[,77]=data[,77]*4
name[77]=paste(name[77],"multiplied by 4")
data[,78]=data[,78]*4
name[78]=paste(name[78],"multiplied by 4")
data[,79]=data[,79]*3
name[79]=paste(name[79],"multiplied by 3")
data[,80]=data[,80]*4
name[80]=paste(name[80],"multiplied by 4")
data[,81]=data[,81]*5
name[81]=paste(name[81],"multiplied by 5")
data[,82]=data[,82]*3
name[82]=paste(name[82],"multiplied by 3")
data[,83]=data[,83]*5
name[83]=paste(name[83],"multiplied by 5")
data[,84]=data[,84]*6
name[84]=paste(name[84],"multiplied by 6")
data[,85]=data[,85]*3
name[85]=paste(name[85],"multiplied by 3")
data[,86]=data[,86]*2
name[86]=paste(name[86],"multiplied by 2")
data[,87]=data[,87]*3
name[87]=paste(name[87],"multiplied by 3")
data[,88]=data[,88]*2
name[88]=paste(name[88],"multiplied by 2")
data[,89]=data[,89]*8
name[89]=paste(name[89],"multiplied by 8")
data[,90]=data[,90]*3
name[90]=paste(name[90],"multiplied by 3")
data[,92]=data[,92]/3
name[92]=paste(name[92],"divided by 3")
data[,94]=data[,94]*3
name[94]=paste(name[94],"multiplied by 3")
data[,95]=data[,95]/3
name[95]=paste(name[95],"divided by 3")
data[,96]=data[,96]/3
name[96]=paste(name[96],"divided by 3")
data[,98]=data[,98]/20
name[98]=paste(name[98],"divided by 20")
data[,99]=data[,99]*30
name[99]=paste(name[99],"multiplied by 30")
data[,100]=data[,100]/40
name[100]=paste(name[100],"divided by 40")
data[,101]=data[,101]*2
name[101]=paste(name[101],"multiplied by 2")
data[,102]=data[,102]*0.06
name[102]=paste(name[102],"multiplied by 0.06")
data[,103]=data[,103]*3
name[103]=paste(name[103],"multiplied by 3")
data[,111]=data[,111]/2
name[111]=paste(name[111],"divided by 2")
data[,117]=data[,117]*2
name[117]=paste(name[117],"multiplied by 2")

#check the adjusted data
pdf(file="c:\\Univariate Plot Adjusted.pdf")
for (i in 2:length(name))
{
plot(data[,i],xlab="Date",ylab=name[i],main=name[i],
font.main=2,xaxs="i",cex.sub=.6,font.sub=4,
sub="Source:BLS All US Cities CPI Average Price http://data.bls.gov/pdq/querytool.jsp?survey=ap Monthly Average")
}
dev.off()

# Plot Data (Bivariate)

pdf(file="c:\\Bivariate Plot.pdf")
for (i in 2:96)
{
	for (j in 97:108)
		{ts.plot(data[,i],data[,j],lty=c(1,2),xlab="Date",ylab=c(name[i], "and", name[j]), 
main=c(name[i], "and", name[j]),
sub="Source:BLS CPI http://data.bls.gov/pdq/querytool.jsp?survey=ap,Solid line is food price")
}}
for (i in 109:117)
{
	for (j in 97:108)
		{ts.plot(data[,i],data[,j],lty=c(1,2),xlab="Date",ylab=c(name[i], "and", name[j]), 
main=c(name[i], "and", name[j]),
sub="Source:BLS CPI http://data.bls.gov/pdq/querytool.jsp?survey=ap,Solid line is food price")
}}
dev.off()




#################################ATTEMP
for (i in 2:117)
{
if (range(data[,i])[2]>100)
{ data[,i]=data[,i]/100
name[i]=paste(name[i],"divided by 100")}

else

if (range(data[,i])[2]>10)
{ data[,i]=data[,i]/10
name[i]=paste(name[i],"divided by 10")}

else

data[,i]=data[,i]
name[i]=name[i]
}

####################################

if ((range(data[,108])[2]-range(data[,108])[1])>100)
{data[,108]=data[,108]/10
name[108]=paste(name[108],"divided by 10")}

####################################
 for (i in 2:117)
{r=range(data[,i])[2]
show(r)}