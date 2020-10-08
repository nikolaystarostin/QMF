install.packages("forecast")
install.packages("TSA")
install.packages("lmtest")
install.packages("tseries")
install.packages("vars")
install.packages("urca")

library("forecast")
library("lmtest")
library("tseries")
library("vars")
library("urca")
library("TSA")
library(readxl)
source('K:/Study/КМФ/LAB2/interpret.r')


data <- read_excel('K:/Study/КМФ/LAB2/all revenue.xlsx')

# INDUSTRY BENCHMARK
no_diff_ag <- log(data$Total)
plot(data$Total)
lines(data$Total)
plot(no_diff_ag)
lines(no_diff_ag)

ag<-diff(no_diff_ag, differences = 1)
plot(ag)
Pacf(ag)
ur.df(ag[seq(1,length(ag))], type="drift", lags = 2, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(ag[seq(1,length(ag))], type="drift", lags = 2, 
      selectlags = "Fixed") %>% summary()
# tau2 = -2.389 => fail to reject that there is unit root
# phi1 = 6.32 => reject null at 5 percent => there is no unit root or there is drift
# or both
# theory says we should get second diffs but it will be inconvenient
ag_model<-Arima(ag, c(4,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(ag_model)
summary(ag_model)
forecast_s<-forecast(ag_model, h=20)
plot(forecast_s)
#forecast_s$mean
f_industry <- exp(diffinv(x=forecast_s$mean, lag = 1, xi = no_diff_ag[length(no_diff_ag)]))
plot(c(data$Total, f_industry))

# computing share of firm in industry
data_share <- data
for (i in 3:ncol(data)){
  data_share[,i]=data_share[,i]/data_share[,2]
}

# predicting share
# let's take Volkswagen
s<-log(data_share$VOWG_p.DE)
plot(s)
# looks like there is structural razriv after year 7 (according to the data many companies
# enter to the market(5 in to years))
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

s
b<-8
plot(s[b:length(s)])
Pacf(s)
# lags 1 and 8 are significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()

ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()
# there is unit root and there is no drift
# so, it's not stationary

Pacf(d2)
# no significant auto correlations
ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(d2[seq(b+2,length(d))], type="drift", lags = 3, 
      selectlags = "Fixed") %>% summary()
# no unit root and no drift

s_model<-Arima(s[b:length(s)], c(3,2,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_share <- exp(forecast_s$mean)
f_share <- ts(f_share,start=32, end=51)
f_revenue <- f_share * f_industry
plot(c(data$VOWG_p.DE, f_revenue))

# s_model$coef
# a1<-0
# b1<-0
# i1 <- 0.0116
# a0<-0
# d.sim <- data.frame()
# for (i in 1:100){
#   d.sim[i,1:20]<-arima.sim(n = 20, list(ar = c(a1), i=c(i1),ma=c(b1)),
#                      sd = 0.0110)+a0}
# ms <- c()
# sds <- c()
# for (i in 1:20){
#   ms[i] <- mean(d.sim[,i])
#   sds[i] <- sd(d.sim[,i]) 
# }
# upper <- ms+sds
# lower <- ms-sds
# 
# up_share <- ts(exp(upper),start=32, end=52)
# l_share <- ts(exp(lower),start=32, end=52)
# up_revenue <- up_share * f_industry
# l_revenue <- l_share * f_industry
# 
# plot(c(data$VOWG_p.DE, f_revenue))
# lines(c(data$VOWG_p.DE, f_revenue))
# lines(up_revenue)
# lines(l_revenue)

### NET INCOME
ni <- read_excel('K:/Study/КМФ/LAB2/all cap exp.xlsx')
ros <- ni
for (i in 3:ncol(data)){
  ros[,i]=abs(ni[,i]/data[,i])
}
s <- ros$VOWG_p.DE
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)
s
b<-17
plot(s[b:length(s)])
Pacf(s)
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()

ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()
s_model<-Arima(s[b:length(s)], c(1,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_ros <- ts(exp(forecast_s$mean), start = 32, end = 51)
f_ni <- f_ros * f_industry
plot(c(data$VOWG_p.DE, f_revenue))


### INVESTMENTS
investment <- read_excel('K:/Study/КМФ/LAB2/all cap exp.xlsx')
inv_share <- investment
for (i in 3:ncol(data)){
  inv_share[,i]=abs(investment[,i]/data[,i])
}
s<-log(inv_share$VOWG_p.DE)
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

s
b<-17
plot(s[b:length(s)])
Pacf(s)
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

#there is unit root so it's not stationary
Pacf(d)
# no significant auto correlations
ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
# no unit root and no drift

s_model<-Arima(s[b:length(s)], c(1,0,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_inv_share <- ts(-exp(forecast_s$mean), start = 32, end = 51)
f_inv <- f_inv_share * f_revenue
plot(c(investment$VOWG_p.DE, f_inv))



### ASSETS
assets <- read_excel('K:/Study/КМФ/LAB2/all assets.xlsx')
as_turnover <- assets
for (i in 3:ncol(data)){
  as_turnover[,i]=data[,i]/assets[,i]
}
s<-log(as_turnover$VOWG_p.DE)
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

b<-1
plot(s[b:length(s)])
Pacf(s)
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

#unit root
Pacf(d)
# no significant auto correlations
ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

# no unit root and no drift
s_model<-Arima(s[b:length(s)], c(0,1,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_as_turnover <- ts(exp(forecast_s$mean), start = 32, end = 51)
f_assets <- f_revenue / f_as_turnover
plot(c(assets$VOWG_p.DE, f_assets))


### DEBT
debt <- read_excel('K:/Study/КМФ/LAB2/all debt.xlsx')
cap_str <- debt
for (i in 3:ncol(data)){
  cap_str[,i]=debt[,i]/assets[,i]
}
s<-log(cap_str$VOWG_p.DE)
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

b<-16
plot(s[b:length(s)])
Pacf(s[b:length(s)])
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

#unit root
Pacf(d[b+1:length(d)])
# no significant auto correlations
ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()

ur.df(d[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()
# no unit root and no drift
s_model<-Arima(s[b:length(s)], c(0,1,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_cap_str <- ts(exp(forecast_s$mean), start = 32, end = 51)
f_debt <- f_cap_str * f_assets
plot(c(debt$VOWG_p.DE, f_debt))



### CURRENT ASSETS
ca <- read_excel('K:/Study/КМФ/LAB2/all ca.xlsx')
as_str <- ca
for (i in 3:ncol(data)){
  as_str[,i]=ca[,i]/assets[,i]
}
s<-log(as_str$VOWG_p.DE)
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

b<-17
plot(s[b:length(s)])
Pacf(s[b:length(s)])
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()
#unit root and drift
Pacf(d2[b+1:length(d)])
# no significant significant
ur.df(d2[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(d2[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

# unit root
s_model<-Arima(s[b:length(s)], c(0,2,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_as_str <- ts(exp(forecast_s$mean), start=32, end = 51)
f_ca <- f_as_str * f_assets
plot(c(ca$VOWG_p.DE, f_ca))



### DEPRECIATION
depr <- read_excel('K:/Study/КМФ/LAB2/all depr.xlsx')
avg_depr <- depr
for (i in 3:ncol(data)){
  avg_depr[,i]=depr[,i]/((1-cap_str[,i])*assets[,i])
}
s<-log(as_str$VOWG_p.DE)
plot(s)
d<-c(NA, diff(s, differences=1))
d2<-c(NA,NA, diff(s, differences=2))
plot(d)

b<-17
plot(s[b:length(s)])
Pacf(s[b:length(s)])
# lags 1 is significant
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(s[b:length(s)], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()

#unit root and no drift
Pacf(d2[b+1:length(d)])
# no significant significant
ur.df(d2[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% interp_urdf()
ur.df(d2[seq(b+1,length(d))], type="drift", lags = 1, 
      selectlags = "Fixed") %>% summary()
# unit root
s_model<-Arima(s[b:length(s)], c(0,2,0), include.constant=TRUE, method = c("CSS-ML"))
coeftest(s_model)
summary(s_model)
forecast_s<-forecast(s_model, h=20)
plot(forecast_s)
mn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$mean[seq(1,20)]))
up<-c(exp(s[seq(b,length(s))]),exp(forecast_s$upper[seq(1,20)]))
dn<-c(exp(s[seq(b,length(s))]),exp(forecast_s$lower[seq(1,20)]))
plot(mn)
lines(dn)
lines(up)
f_avg_depr <- ts(exp(forecast_s$mean), start = 32, end = 51)
f_depr <- f_avg_depr * (1 - f_cap_str) * f_assets
plot(c(ca$VOWG_p.DE, f_ca))
