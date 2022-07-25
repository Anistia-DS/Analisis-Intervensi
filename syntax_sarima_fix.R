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
library(knitr)
library(AID)
library(fGarch)
library(rugarch)

#======= INPUT DATA
data<-read.csv2("D:/SKRIPSI/DATA/DATA PASANGGER AIRPORT/JUMLAH PENUMPANG.csv", header=TRUE)
data<-data[,2:4]
head(data)
View(data)
summary(data$Total)
stdev(data$Total)

# ======PLOT DATA DERET WAKTU
dataa<-ts(data$Total, start =c(2010,1),frequency=12) #mengubah data menjadi data time series
plot.ts(dataa, col='navyblue',
        lwd=1.5,
        type="l",
        xlab="Waktu", 
        ylab="Jumlah Penumpang",
        main='plot jumlah penumpang Januari 2010-Mei 2021')
points(dataa, cex=.5, col="red")

#=======MEMBAGI DATA TRAINING DAN TESTING
data_training<-data[1:128,3]
training_ts<-ts(data_training, start =c(2010,1),frequency=12) 
data_testing<-data[129:137,3]
testing_ts<-ts(data_testing, start =c(2020,9), frequency = 12)

plot.ts(training_ts, col='black',
        lwd=1.5,
        type="l",
        xlab="waktu", 
        ylab="jumlah penumpang",
        main='plot data training')
points(training_ts, cex=.5, col="blue")
abline(v=2020.2, col="red", lty=1, lwd=0.5)
text(2020.3, 200, "Maret 2020",cex =0.8, pos = 2)


#=======UJI STASIONER
library('fpp2')
par(mfrow=c(1,2))
Acf(training_ts, lag.max = 48) 
Pacf(training_ts, lag.max = 48) 
summary(training_ts)

#dalam rataan
adf.test(training_ts)
training_diffnonmusiman <- diff(training_ts, differences = 1)
plot.ts(training_diffnonmusiman)
adf.test(training_diffnonmusiman)

#dalam ragam
data_positif<-training_diffnonmusiman + 1020700
lamda<-boxcox(data_positif~1)
powerTransform(data_positif)

#ACF dan PACF data yang sudah stasioner
par(mfrow=c(1,2))
Acf(training_diffnonmusiman, lag.max = 48) 
Pacf(training_diffnonmusiman, lag.max = 48) 
eacf(training_diffnonmusiman)

#=======PEMODELAN
fit1<-Arima(training_ts, order = c(0,1,0), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(fit1)
summary(fit1)
#ml1<-McLeod.Li.test(y=fit1$residuals, main="McLeod-Li test on model Residuals")
#ml1$p.values

#tidak signifikan
fit2<-Arima(training_ts, order = c(0,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(fit2)
summary(fit2)

#tidak signifikan
fit3<-Arima(training_ts, order = c(1,1,0), seasonal=list(order=c(1,0,0), periode=12), method = "ML") #signifikan
coeftest(fit3)
summary(fit3)


#======DIAGNOSTIK MODEL
#kebebasan sisaan
ljung_box_test1<-LjungBox(fit1$residuals)
ljung_box_test1
Box.test(fit1$residuals,type = "Ljung-Box") 
acf(fit1$residuals, main="RACF")
pacf(fit1$residuals, main="RPACF")

#kenormalan sisaan
ks.test(fit1$residuals,"pnorm")
#lillie.test(fit1$residuals)
qqnorm(fit1$residuals, col=6, main="Plot Q-Q")
qqline(fit1$residuals)
hist(fit1$residuals, probability = T, main = "Histogram", xlab = "Sisaan")
lines(density(fit1$residuals), col=6)


#===== OVERFITTING
overfit<-Arima(training_ts, order = c(0,1,1), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
coeftest(overfit)

#uji diagnostik
ljung_box_test3<-LjungBox(overfit$residuals)
ljung_box_test3
Box.test(overfit$residuals,type = "Ljung-Box") 
acf(overfit$residuals, main="RACF")
pacf(overfit$residuals, main="RPACF")

ks.test(overfit$residuals,"pnorm")
#shapiro.test(overfit$residuals)
#ad.test(overfit$residuals)
#lillie.test(overfit$residuals)
par(mfrow=c(1,2))
qqnorm(overfit$residuals, col="blue", main="Plot Q-Q")
qqline(overfit$residuals)
hist(overfit$residuals, probability = T, main = "Histogram", xlab = "Sisaan")
lines(density(overfit$residuals), col=2)


#===== AKURASI model terbaik
model_terbaik<-Arima(training_ts, order = c(0,1,0), seasonal=list(order=c(1,0,0), periode=12), method = "ML") 
ramalan_sarima<- forecast(model_terbaik,9)
ramalan_sarima
ramalan_sarima <- (ramalan_sarima$mean)
ramalan_sarima<-ts(ramalan_sarima, start =c(2020,9), frequency = 12)
ramalan_sarima
akurasi_sarima<-accuracy(ramalan_sarima, testing_ts)
akurasi_sarima

#======Meneyimpan Data
library('writexl')
df<-data.frame(ts_testings, ramalan_sarima, ramalan_intervensi5)
df
write_xlsx(df, "D:\\SKRIPSI\\DATA\\DATA PASANGGER AIRPORT\\dataGabungan.xlsx")
")