#install packages
library( forecast ) # --- BoxCox Arima auto . arima function is in forecast
library( MASS ) # --- boxcox function is in MASS package
library( FitAR ) # --- LjungBoxTest function is in FitAR package
library( tsoutliers ) # --- tso function is in tsoutliers package
library( lmtest ) # --- coeftest function is in lmtest package
library( stargazer ) # --- stargazer function is in stargazer package
library( TSA ) # --- arimax function is in TSA package
library(ggplot2)
library(astsa)
library(fUnitRoots)
library(FitARMA)
library(strucchange)
library(reshape)
library(Rmisc)
library(fBasics)
library(tseries)
library(car)
library(nortest)
library(portes)
library(portes)
# ====== INPUT DATA
data<-read.csv2("D:/SKRIPSI/DATA/DATA PASANGGER AIRPORT/JUMLAH PENUMPANG.csv", header=TRUE)
data_ts<-ts(data, start =c(2010,1),frequency=12)
data<-data[,2:4]
head(data)
summary(data$Total)
stdev(data$Total)

#========MEMBAGI DATA TRAINING DAN TESTING
training_data<-data[1:128,3]
ts_training<-ts(training_data, start =c(2010,1),frequency=12) 
testing_data<-data[129:137,3]
ts_testings<-ts(testing_data, start =c(2020,9), frequency = 12)

plot.ts(ts_training, col='black',
        lwd=1.5,
        type="l",
        xlab="waktu", 
        ylab="Jumlah Penumpang",
        main='plot data training')
points(ts_training, cex=.5, col="red")

#==========MEMBAGI DATA INTERVENSI DAN SEBELUM INTERVENSI
data_sblm_intervensi<-ts(training_data[1:122], start = c(2010,1), frequency = 12)
data_stlh_intervensi<-ts(training_data[123:128], start = c(2020,3), frequency = 12)

#=========IDENTIFIKASI MODEL SARIMA DATA SEBELUM INTERVENSI

#1. UJI STASIONER
par(mfrow=c(1,2))
Acf(data_sblm_intervensi, lag.max = 48) 
Pacf(data_sblm_intervensi, lag.max = 48) 
plot(data_sblm_intervensi)

#dalam rataan
adf.test(data_sblm_intervensi)
diffsblm_intervensi <- diff(data_sblm_intervensi, differences = 1)
adf.test(diffsblm_intervensi)
plot(diffsblm_intervensi)
#dalam ragam
diffsblm_intervensi_positif<-diffsblm_intervensi+500000
boxcox(diffsblm_intervensi_positif~1)

#2.ESTIMASI PARAMETER
par(mfrow=c(1,2))
Acf(diffsblm_intervensi, lag.max = 48) 
Pacf(diffsblm_intervensi, lag.max = 48) 
eacf(diffsblm_intervensi)


y1<-Arima(data_sblm_intervensi, order = c(0,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(y1)
summary(y1)   #signifikan

y2<-Arima(data_sblm_intervensi, order = c(2,1,0), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(y2)
summary(y2)   #signifikan

y3<-Arima(data_sblm_intervensi, order = c(2,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(y3)
summary(y3)   #tidak signifikan


#3. DIAGNOSTIK MODEL
y1<-Arima(data_sblm_intervensi, order = c(2,1,0), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 

#kebebasan sisaan
residual1 <- y1$residuals
ljung_box_test1<-LjungBox(residual1)
ljung_box_test1
Box.test(y1$residuals,type = "Ljung-Box") 
acf(residual1, main="RACF")
pacf(residual1, main="RPACF")

#kenormalan sisaan
ks.test(y1$residuals,"pnorm")
shapiro.test(y1$residuals)
ad.test(y1$residuals)
lillie.test(y1$residuals)
hist(y1$residuals, probability = T)
lines(density(y1$residuals), col=2)

#4. OVERFITTING
over<-Arima(data_sblm_intervensi, order = c(0,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(over)
summary(over)

#uji diagnostik
ljung_box_test4<-LjungBox(over$residuals)
ljung_box_test4
Box.test(over$residuals,type = "Ljung-Box") 
acf(over$residuals, main="RACF")
pacf(over$residuals, main="RPACF")

#kenormalan sisaan
ks.test(over$residuals,"pnorm")
shapiro.test(over$residuals)
ad.test(over$residuals)
lillie.test(over$residuals)
qqnorm(over$residuals, col=3, main="Plot Q-Q")
qqline(over$residuals)
hist(over$residuals, probability = T, main = "Histogram", xlab = "Sisaan")
lines(density(over$residuals), col=3)

#5. AKURASI model terbaik
model_terbaik<-Arima(data_sblm_intervensi, order = c(0,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
ramalan_sarima<- forecast(model_terbaik,6)
ramalan_sarima
ramalan_sarima <- (ramalan_sarima$mean)
ramalan_sarima<-ts(ramalan_sarima, start =c(2020,3), frequency = 12)
ramalan_sarima
akurasi_sarima<-accuracy(ramalan_sarima, data_stlh_intervensi)
akurasi_sarima

#6. Plot data latih dan data hasil ramalan
a<-y1$fitted
mn<-cbind(a,ramalan_sarima)
mn[123:128,1]<-ramalan_sarima
df<-as.data.frame(mn)
ts.plot(training_ts, df$a, gpars= list(xlab='waktu', ylab='Jumlah penumppang',lty=c(1:2),col=c(1,4)))
abline(v=2020.2, col="red", lty=3, lwd=0.5)
legend("bottomleft", legend =c("Data aktual","Data hasil ramalan"), col =c(1,4), lty =c(1,2))
text(2020.3, 200, "Maret 2020",cex =0.8, pos = 2)

#========IDENTIFIKASI ORDE INTERVENSI
deteksi_outlier<-tsoutliers::tso(ts_training, types =c("AO","LS","TC"), maxit.iloop = 10, tsmethod ="arima")
plot(deteksi_outlier)


 ## Analisis Intervensi ##
par(mfrow=c(1,1))
forecast_int<-predict(model_terbaik,n.ahead=6)
forecast_int<-forecast_int$pred
forecast_int<-ts(forecast_int, start = c(2020,3), frequency = 12)
forecast_int
error_idintv<-rep(0,128)
error_idintv[1:122]<-model_terbaik$residuals
resi<-(data_stlh_intervensi)-(forecast_int)
error_idintv[123:128]<-resi
error_idintv<-ts(error_idintv, start =c(2010,1),frequency=12)
ts.plot(error_idintv, type="h", xlab="waktu ", ylab="residual", xaxt="n")
abline(h=c(-3*112951.8, 3*112951.8), col="red", lyt=1)
abline(v=2020.2, col="blue", lty=1, lwd=2)
text(2020.3, 200, "T=123",cex =0.8, pos = 3)

#==============Identifikasi parameter orde intervensi
#====SAS====
#==============Diagnosis Model
#====SAS====
#==============AKURASI PERAMLAN
#Data hasil peramalan
deta5<-read.csv2("D:/SKRIPSI/DATA/INTERVENSI_MARETT.csv", header = T)
ramalan_intervensi5 <-ts(deta5[129:137,2], start =c(2020,9), frequency = 12)
ramalan_intervensi5<-round(ramalan_intervensi5,0)
akurasi_intervensi5<-accuracy(ramalan_intervensi5, ts_testings)
akurasi_intervensi5



#======PLOT-PLOT
testing_data<-data[129:137,3]
ts_testings<-ts(testing_data, start =c(2020,9), frequency = 12)

#plot data latih dan hasil peramlaan SARIMA
ts.plot(training_ts, model_terbaik$fitted, gpars= list(xlab='waktu', ylab='Jumlah Penumpang',lty=c(1:1),col=c('black','red')))
legend("bottomleft", legend =c("Data latih","Fitted model ARIMA"), col =c('black','red'), lty =1)

#intervensi(0,5,1)
hasil_inter5<-ts(deta5[1:128,2], start =c(2020,1))
ts.plot(hasil_inter5,training_data,  gpars= list(xlab='waktu', ylab='kasus',lty=c(1:2),col=c('blue','black')))
legend("bottomleft", legend =c("intervensi","data latih"), col =c('blue','black'), lty =1)

#Plot data uji dan hasil peramalan  SARIMA dan intervensi
ts.plot(ramalan_intervensi5, ts_testings, ramalan_sarima, col=c('green','black','red'), xlab= 'Waktu', ylab='Jumalah penumpang', ylim=c(-600000,1200000))
legend("bottomleft", legend =c("Data uji","Model intervensi",'Model ARIMA'), col =c('black','blue'), lty =1)

