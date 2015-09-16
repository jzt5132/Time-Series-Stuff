#Causality (Whole Vectors)(Second Diff)

cs2=matrix(data=NA,ncol=21,nrow=4)

for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X1[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X1[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,1]=p
}

for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X2[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X2[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,2]=p
}

for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X3[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X3[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,3]=p
}

for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X4[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X4[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,4]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X5[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X5[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,5]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X6[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X6[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,6]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X7[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X7[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,7]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X8[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X8[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,8]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X9[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X9[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,9]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X10[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X10[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,10]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X11[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X11[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,11]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X12[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X12[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,12]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X13[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X13[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,13]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X14[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X14[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,14]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X15[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X15[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,15]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X16[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X16[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,16]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X17[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X17[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,17]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X18[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X18[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,18]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X19[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X19[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,19]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X20[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X20[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,20]=p
}
for (j in 22:25) {
v1=VAR(diff(diff(a)), p = 1)
v2=VAR(diff(diff(a[,-j])), p = 1)
SSR1=((summary(v1)[2]$varresult$X21[6]$sigma)^2)*231
SSR2=((summary(v2)[2]$varresult$X21[6]$sigma)^2)*232
f=(231*(SSR2-SSR1))/SSR1
p=1-pf(f,1,231)
cs2[j-21,21]=p
}