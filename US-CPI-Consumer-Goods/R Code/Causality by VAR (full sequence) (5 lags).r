cs2=matrix(data=NA,ncol=25,nrow=25)

for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X1[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X1[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,1]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X2[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X2[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,2]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X3[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X3[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,3]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X4[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X4[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,4]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X5[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X5[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,5]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X6[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X6[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,6]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X7[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X7[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,7]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X8[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X8[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,8]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X9[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X9[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,9]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X10[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X10[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,10]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X11[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X11[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,11]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X12[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X12[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,12]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X13[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X13[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,13]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X14[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X14[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,14]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X15[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X15[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,15]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X16[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X16[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,16]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X17[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X17[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,17]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X18[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X18[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,18]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X19[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X19[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,19]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X20[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X20[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,20]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X21[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X21[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,21]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X22[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X22[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,22]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X23[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X23[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,23]=p
}
for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X24[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X24[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,24]=p
}

for (j in 1:25) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X25[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X25[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,25]=p
}






for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X1[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X1[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,1]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X2[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X2[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,2]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X3[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X3[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,3]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X4[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X4[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,4]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X5[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X5[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,5]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X6[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X6[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,6]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X7[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X7[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,7]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X8[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X8[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,8]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X9[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X9[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,9]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X10[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X10[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,10]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X11[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X11[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,11]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X12[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X12[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,12]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X13[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X13[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,13]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X14[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X14[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,14]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X15[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X15[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,15]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X16[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X16[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,16]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X17[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X17[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,17]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X18[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X18[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,18]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X19[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X19[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,19]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X20[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X20[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,20]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X21[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X21[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,21]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X22[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X22[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,22]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X23[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X23[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,23]=p
}
for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X24[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X24[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,24]=p
}

for (j in 25:1) {
v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-j])), p = 5)
SSR1=((summary(v1)[2]$varresult$X25[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X25[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[j,25]=p
}



v1=VAR(diff(diff(a)), p = 5)
v2=VAR(diff(diff(a[,-1])), p = 5)
SSR1=((summary(v1)[2]$varresult$X1[6]$sigma)^2)*127
SSR2=((summary(v2)[2]$varresult$X1[6]$sigma)^2)*132
f=(127*(SSR2-SSR1))/(SSR1*5)
p=1-pf(f,5,127)
cs2[1,1]=p
