#Plot Data (First diff & Random Component)
pdf(file="V:\\Univariate Plot(First Difference).pdf")
for (i in 2:26)
{
plot(diff(data[,i]),ylab=name[i])
}
dev.off()

pdf(file="V:\\Univariate Plot(Second Difference).pdf")
for (i in 2:26)
{
plot(diff(diff(data[,i])),ylab=name[i])
}
dev.off()

pdf(file="V:\\Univariate Plot(Random Component).pdf")
for (i in 2:26)
{
plot(decompose(data[,i])$random,ylab=name[i])
}
dev.off()

#####Bivariate
pdf(file="c:\\Bivariate Plot.pdf")
for (i in 2:22)
{
	for (j in 23:26)
		{ts.plot(diff(data[,i]),diff(data[,j]),lty=c(1,2),xlab="Date",ylab=c(name[i], "and", name[j]), 
main=c(name[i], "and", name[j]),
sub="Source:BLS CPI http://data.bls.gov/pdq/querytool.jsp?survey=ap,Solid line is food price")
}}
dev.off()