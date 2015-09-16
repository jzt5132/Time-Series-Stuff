install.packages('forecast', dependencies = TRUE)
library(forecast)
data = read.csv(file = 'data.csv', header = F)
data = ts(data)

result_ARIMA = matrix(NA, nrow = 4, ncol = 19)
result_ES = matrix(NA, nrow = 4, ncol = 19)
pdf(file="c:\\Plot of Forecast.pdf")

for (i in 1:19){
  A_F = forecast.Arima(auto.arima(data[,i]), h = 2)
  ES_F = forecast.HoltWinters(HoltWinters(data[,i], beta = F, gamma = F), h = 2)
  result_ARIMA [1:4, i] = c(A_F$mean[2], A_F$model$sigma2, A_F$lower[2, 2], A_F$upper[2, 2])
  result_ES [1:4, i] = c(ES_F$mean[2], ES_F$model$SSE/4, ES_F$lower[2, 2], ES_F$upper[2, 2])
  plot(A_F,xlab="Month",ylab= i,main="Forecast of Variable i using ARIMA",
       font.main=2,xaxs="i",cex.sub=.6,font.sub=4,)
  plot(ES_F,xlab="Month",ylab= i,main="Forecast of Variable i using Expoential Smoothing",
       font.main=2,xaxs="i",cex.sub=.6,font.sub=4,)
}
dev.off()

