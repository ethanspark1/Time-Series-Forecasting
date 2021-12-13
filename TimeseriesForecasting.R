#read in the data
sales_tax <- readxl::read_xlsx("//storage/homes/S-1-5-21-1167378736-2199707310-2242153877-649047/Downloads/Texas County Sales taxes.xlsx", sheet = 1)
lubbock <- ts(sales_tax[,"Lubbock"], frequency = 12, start = c(2012, 12))
training <- window(lubbock, frequency=12, end=c(2020,8))
test <- window(lubbock, frequency=12, end=c(2021,2))
accuracyset <- window(lubbock, frequency=12, start=c(2020,9), end=c(2021,2))
#Forecast Package is used 
install.packages("forecast")
library(forecast)
#ETS Forecast
ets_training <- ets(training)
summary(ets_training)
ets_forecast <- forecast(ets_training, h=6)
ets_accuracy <- accuracy(ets_forecast, accuracyset)
autoplot(ets_forecast, xlab='Date', ylab='Sales Tax Collected')+autolayer(accuracyset)
#Arima Forecast with seasonal component
lubbock_arima <- auto.arima(training, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(lubbock_arima)
arima_forecast <- forecast(lubbock_arima, h=6)
arima_accuracy <- accuracy(arima_forecast, accuracyset)
autoplot(arima_forecast, xlab='Date', ylab='Sales Tax Collected')+autolayer(accuracyset)
#One Step Ahead ETS Forecast
#R chose model "MAA" as the best ets model for this data
one.step.e <- ts(vector(length=6), frequency=12, start=c(2020, 9)) 
jj <- 1
for (i in c(9, 10, 11, 12, 1, 2)) {
  if(i==1) {j=2021} else {j=2020} 
  zz <- forecast(etstraining, h=1) 
  one.step.e[jj] <- zz$mean
  training <- window(lubbock, end=c(j, i)) 
  etstraining <- ets(training, model="MAA") 
  jj <- jj+1
}
ets_onestep_accuracy<- accuracy(one.step.e, accuracyset)
autoplot(lubbock, xlab='Date', ylab='Sales Tax Collected')+autolayer(one.step.e)
#Arima One Step Ahead Forecast
#R again chose the best arima model for this data
one.step.a <- ts(vector(length=6), frequency=12, start=c(2020, 9)) 
jj <- 1
for (i in c(9, 10, 11, 12, 1, 2)) {
  if(i==1) {j=2021} else {j=2020} 
  zz <- forecast(lubbockarima, h=1) 
  one.step.a[jj] <- zz$mean
  training <- window(lubbock, end=c(j, i)) 
  lubbockarima <- Arima(training, order = c(1,0,1), seasonal = list(order=c(0,1,1), period=12), include.drift=TRUE)
  jj<- jj+1
}

arima_onestep_accuracy <- accuracy(one.step.a, accuracyset)
autoplot(lubbock, xlab='Date', ylab='Sales Tax Collected')+autolayer(one.step.a)
#Review Accuracy for each forecast
ets_accuracy
arima_accuracy
ets_onestep_accuracy
arima_onestep_accuracy
#Introducing new variables
install.packages("vars")
library(vars)
alc <- readxl::read_xlsx("//storage/homes/S-1-5-21-1167378736-2199707310-2242153877-649047/Downloads/Texas County Sales taxes.xlsx", sheet=2)
alcohol <- ts(alc[,"Lubbock"], frequency = 12, start = c(2012, 12))
employment <- ts(alc[,"Employment"], frequency=12, start = c(2012, 12))
tax=lubbock
autoplot(log(alcohol)) 
autoplot(log(employment)) 
nsdiffs(log(tax))
nsdiffs(log(alcohol))
ndiffs(log(employment))
sd_accuracyt <- window(diff(log(tax), 12), start=c(2020, 9), end=c(2021,2))
sd_accuracye <- window(diff(log(employment), 12), start=c(2020, 9), end=c(2021, 2))
sd_accuracya <- window(log(alcohol), 12, start=c(2020, 9), end=c(2021,2))
var.accuracy <- matrix(c(sd_accuracye, sd_accuracya, sd_accuracyt), 
                       nrow = length(sd_accuracyt), 
                       dimnames = list(c(1:6), c("Employment","Alcohol", "Sales_Tax")))


sd_salest <- window(diff(log(tax), 12), start=c(2013, 12), end=c(2021, 2))
sd_employ <- window(diff(log(employment), 12), start=c(2013, 12), end=c(2021, 2))
sd_alc <- window(log(alcohol), 12, start=c(2013, 12), end=c(2021,2))

var.hmat <- matrix(c(sd_employ, sd_alc, sd_salest), 
                   nrow = length(sd_salest), 
                   dimnames = list(c(1:87), c("Employment","Alcohol", "Sales_Tax")))
(hvar.mod1 <- VAR(var.hmat, p=3, type = "const", season=NULL))
var.for <- predict(hvar.mod1, n.ahead=12, ci=0.95)
var.for
summary(hvar.mod1)
summary(var.for)
plot(var.for)
causality(hvar.mod1, cause= "Alcohol")
causality(hvar.mod1, cause= "Employment")
causality(hvar.mod1, cause= "Sales_Tax")
(htvar.sel <- VARselect(var.hmat, lag.max = 12, type = "const", season=NULL))
plot(irf(hvar.mod1, n.ahead = 12)
     amat <- diag(3)
     diag(amat) <- NA
     amat[1,2] <- NA
     amat
     svar.a <- SVAR(x = hvar.mod1, Amat = amat, Bmat = NULL)
     svar.a
     a.inv <- solve(svar.a$A)
     a.inv
     sir1 = irf(svar.a, impulse = "Alcohol", response = c("Alcohol"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir1)
     sir2 = irf(svar.a, impulse = "Alcohol", response = c("Employment"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir2)
     sir3 = irf(svar.a, impulse = "Employment", response = c("Alcohol"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir3)
     sir4 = irf(svar.a, impulse = "Sales_Tax", response = c("Employment"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir4)
     sir5 = irf(svar.a, impulse = "Employment", response = c("Sales_Tax"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir5)
     sir6 = irf(svar.a, impulse = "Alcohol", response = c("Sales_Tax"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir6)
     sir7 = irf(svar.a, impulse = "Sales_Tax", response = c("Alcohol"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir7)
     sir8 = irf(svar.a, impulse = "Sales_Tax", response = c("Sales_Tax"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir8)
     sir9 = irf(svar.a, impulse = "Employment", response = c("Employment"),
                n.ahead=12, boot=TRUE, ci = 0.95, runs = 100)
     plot(sir9)
     (h.fevd <- fevd(svar.a, n.ahead=12)) 
     fanchart(var.for)
     fcst <- forecast(hvar.mod1, n.ahead=6)
     inverse.rle(var.for[,1:6])
     accuracy(var.for$varresults)
     res <- residuals(hvar.mod1)
     res
     fits <- fitted(hvar.mod1)
     fits
     accuracy(var.for, accuracyset)
     print(var.for)
     accuracy(var.for$fcst[[1]][,"fcst"], var.accuracy)
     fanchart(var.for)
     
     