###Plot Selected Pairs of Data###

data1=read.csv(file="data1.csv", header=T)
data1=ts(data1[,(-1)],start=c(2000,1),frequency=52)


#Plot US vs Italy Corn Prices
plot(data1[,c(1,4)],plot.type="single",pty="m",lty=1:2,main = "US vs Italy corn prices",xlab="Date",ylab="Prices (Euro/ton)")
legend(2000,230, colnames(data1[,c(1,4)]), lty=1:2, cex=.65,bty="n")


#Plot US vs Italy Wheat Prices
plot(data1[,c(2,5)],plot.type="single",pty="m",lty=1:2,main = "US vs Italy wheat prices",xlab="Date",ylab="Prices (Euro/ton)")
legend(2000,290, colnames(data1[,c(2,5)]), lty=1:2, cex=.65,bty="n")


#Plot US vs Italy Soybean Prices
plot(data1[,c(3,6)],plot.type="single",pty="m",lty=1:2,main = "US vs Italy soybean prices",xlab="Date",ylab="Prices (Euro/ton)")
legend(2000,480,colnames(data1[,c(3,6)]), lty=1:2, cex=.65,bty="n")


#Plot Brent Blend vs Italy Commodities Prices
plot(data1[,c(1,2,3,7)],plot.type="single",pty="m",lty=c(1,2,3,5),main = "Brent Blend vs Italy commodity prices",xlab="Date",ylab="Prices (Euro/ton)")
legend(2000,500,colnames(data1[,c(1,2,3,7)]), lty=c(1,2,3,5), cex=.65,bty="n")

#Plot Brent Blend vs US Commidties Prices
plot(data1[,c(4,5,6,7)],plot.type="single",pty="m",lty=c(1,2,3,5),main = "Brent Blend vs US commodity prices",xlab="Date",ylab="Prices (Euro/ton)")
legend(2000,390,colnames(data1[,c(4,5,6,7)]), lty=c(1,2,3,5), cex=.65,bty="n")

#Univariate Plot
plot(data1,main = "US and EU commodity prices")
plot(data1, plot.type="single", col = 1:ncol(data1),main = "US and EU commodity prices",xlab="Date",ylab="Prices (Euro/ton)")
legend("bottomleft", colnames(data1), col=1:ncol(data1), lty=1, cex=.65)
