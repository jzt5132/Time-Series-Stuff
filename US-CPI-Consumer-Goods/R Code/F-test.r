F-test

lm1=lm(diff(diff(a[,1]))~diff(diff(a[,22])))
summary(lm1)

lm2=lm(diff(diff(a[,1]))~1)
summary(lm2)

SSR1=((summary(lm1)[6]$sigma)^2)*256
SSR2=((summary(lm2)[6]$sigma)^2)*257
f=(256*(SSR2-SSR1))/SSR1
p=1-pf(f,1,256)
p