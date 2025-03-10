setwd("~/SOFT/FCI/UG-STOP-Valdivia/CODE")
############################################
# LOADING DATA
############################################
preeclampsia <- read.csv("CAMAS DE HOSPITALIZACION Y URGENCIAS-VALDIVIA-SOLO EMERGENCIASv2.csv",sep = ";")
#preeclampsia$year.month<-as.factor(preeclampsia$year.month)
preeclampsia$year<-as.factor(preeclampsia$year)
preeclampsia$month<-as.factor(preeclampsia$month)
preeclampsia$camas<-as.numeric(preeclampsia$camas)
tsPreeclampsia<-ts(preeclampsia$camas, start=c(2019,1), end=c(2023,12), frequency=12)

############################################
#       Descriptive analysis
############################################

# basic statistics
#......................
summary(preeclampsia$camas)
library(pastecs)
stat.desc(preeclampsia$camas,norm=TRUE)

# Box plot
#......................
boxplot(preeclampsia$camas, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=quantile(preeclampsia$camas),labels=quantile(preeclampsia$camas),y=1.40,cex=0.85)

# Line Plot
#......................
library(zoo)
library(lubridate)
par(mar = c(7.5,5, 0.5, 0.5))
plot(tsPreeclampsia,xlab="",ylab="",yaxt="n",xaxt="n")#,ylim=c(0,25)
text(tsPreeclampsia, labels=round(preeclampsia$camas,0), cex= 0.8)
axis(1,as.yearmon(time(tsPreeclampsia)),format(as.yearmon(time(tsPreeclampsia)), "%b %y"), cex.axis = 1,las=2)
axis(2,cex.axis=1.5)
abline(h=mean(preeclampsia$camas),col=c("red"))
abline(h=summary(preeclampsia$camas)[2],col=c("blue"))
abline(h=summary(preeclampsia$camas)[3],col=c("blue"))
abline(h=summary(preeclampsia$camas)[5],col=c("blue"))
mtext("Time", side=1, line=6, cex=1.5)
mtext("Emergency patients", side=2, line=3, cex=1.5)
grid(length(tsPreeclampsia))


############################################
#       Diagnostic analysis
############################################


#---------------------------------------------------
#Decomposed analysis
#---------------------------------------------------

stlPreeclampsia<-stl(tsPreeclampsia,s.window="periodic")
#Figure: diagSTL.pdf
#......................
plot(stlPreeclampsia)


#---------------------------------------------------
# Trend analysis
#---------------------------------------------------
#Figure: diagTrend.pdf
#......................
#Create a time series plot of the annual data per year and add a smooth line to indicate the existing pattern to visualize the trend.
plot(tsPreeclampsia,ylab="Emergency patients",cex.axis = 1.5,cex.lab=1.5)
#Now we can add a smooth line to visualize the trend
lines(stlPreeclampsia$time.series[,2], col='red')


#---------------------------------------------------
# Seasonal analysis
#---------------------------------------------------
library(dplyr)
library(lubridate)

tbPreeclampsia<-preeclampsia |> mutate(datetime = make_datetime(preeclampsia$year,preeclampsia$month))


library(tidyverse)
library(timetk)
# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- TRUE


tbPreeclampsia<-as_tibble(tbPreeclampsia)

#Figure: diagSeasonalDiagnostic.pdf
#......................
#install.packages("htmltools")
tbPreeclampsia%>%
  plot_seasonal_diagnostics(datetime,camas, .interactive = interactive)




#---------------------------------------------------
# Remainder analysis
#---------------------------------------------------
# Figure: diagRemainderValues.pdf
#......................
plot(stlPreeclampsia$time.series[,3],ylab="Remainder values",cex.axis = 1.5,cex.lab=1.5)


library(lmtest)
dwtest(tsPreeclampsia ~ time(tsPreeclampsia))
#The p value is less than 0.05, with which we reject the null hypothesis and conclude that there is a first-order correlation

#----------------------------------------------------------------------------------
#                   Anomaly detection
#----------------------------------------------------------------------------------

library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr
library(timetk)
library(anomalize) #tidy anomaly detectiom


# Figure: diagAnomalies.pdf
#......................
tbPreeclampsia%>%
  time_decompose(camas) %>%  
  anomalize(remainder) %>%
  plot_anomaly_decomposition()


#---------------------------------------------------
# Autocorrelation analysis
#---------------------------------------------------
library(lmtest)
dwtest(tsPreeclampsia ~ time(tsPreeclampsia))
#The p value is less than 0.05, with which we reject the null hypothesis and conclude that there is a first-order correlation


# Figure: diagAcfPacf.pdf
#......................
par(mfrow=c(1,2))
acf(tsPreeclampsia,lag.max = 30,cex.axis = 1.5,cex.lab=1.5)
pacf(tsPreeclampsia,lag.max = 30,cex.axis = 1.5,cex.lab=1.5)

library(tseries)
adf.test(tsPreeclampsia)


#Figure: diagDiffValues.pdf
#......................
diff<-diff(tsPreeclampsia)
library(zoo)
library(lubridate)
par(mar = c(7.5,5, 0.5, 0.5))
plot(diff,xlab="",ylab="",yaxt="n",xaxt="n")#,ylim=c(0,25)
text(diff, labels=diff, cex= 1.3)
axis(1,as.yearmon(time(diff)),format(as.yearmon(time(diff)), "%b %y"), cex.axis = 1,las=2)
axis(2,cex.axis=1.5)
mtext("Time", side=1, line=6, cex=1.5)
mtext("Differentiated values", side=2, line=3, cex=1.5)
#grid(length(diff))


library(tseries)
adf.test(diff(tsPreeclampsia))



####################################################
#               Predictive analysis 
####################################################

#---------------------------------------------------
#                 ARIMA
#----------------------------------------------------
library(forecast)
# TRAIN TEST
#Define 70% Train and 30% Test Data Set
percent = 0.7
training <- subset(tsPreeclampsia, end=floor(x = percent * length(tsPreeclampsia)))
test <- subset(tsPreeclampsia, start=floor(x = percent * length(tsPreeclampsia))+1)
#TRAIN MODEL
fitArima <- auto.arima(training)
print(fitArima)  
# FORECASTING
frcArima <- forecast(fitArima,h=floor(length(test)))
print(frcArima)

#Figure: confidenceARIMA.pdf
#......................
forecastPreec <- forecast(fitArima, level = c(95), h = 19)
plot(forecastPreec,ylim=c(0,25))
lines(test,lty=3,col = "red")

accuracy(forecastPreec,test)[1,1:5]




#-----------------------------------------------
#                 ETS(exponential smoothing)
#-----------------------------------------------
library(forecast)
# TRAIN TEST
#Define 70% Train and 30% Test Data Set
percent = 0.7
training <- subset(tsPreeclampsia, end=floor(x = percent * length(tsPreeclampsia)))
test <- subset(tsPreeclampsia, start=floor(x = percent * length(tsPreeclampsia))+1)
#TRAIN MODEL
fitETS <- ets(training)
print(fitETS)  
# FORECASTING
frcETS <- forecast(fitETS,h=floor(length(test)),level = c(95))
print(frcETS)
accuracy(frcETS,test)[1,1:5]

#Figure: confidenceETS.pdf
#......................
plot(frcETS,ylab="Emergency patients", level = c(95))
lines(test,lty=5,col = "red")




#----------------------------------------------------------------
#                       MLP
#----------------------------------------------------------------
#https://kourentzes.com/forecasting/2019/01/16/tutorial-for-the-nnfor-r-package/
library(nnfor)
# TRAIN TEST
#Define 70% Train and 30% Test Data Set
percent = 0.7
training <- subset(tsPreeclampsia, end=floor(x = percent * length(tsPreeclampsia)))
test <- subset(tsPreeclampsia, start=floor(x = percent * length(tsPreeclampsia))+1)

#TRAIN MODEL
#fitMLP <- mlp(training)
#print(fitMLP)  

#LOAD MODEL MLP17
fitMLP<-readRDS(file = "fitMLP17.rds")
plot(fitMLP)

# FORECASTING
frcMLP <- forecast(fitMLP,h=floor(length(test)),level = c(95))
print(frcMLP)
#print(frcMLP$all.mean)
plot(frcMLP)


#Figure: confidenceMLP.pdf
#......................
plot(frcMLP,col = "blue",ylab="Emergency patients", ylim=c(5,40),level = c(95))
lines(test,lty=5,col = "red")


accuracy(frcMLP$mean,test)[1,1:5]

#----------------------------------------------------------------
#                       ELM
#----------------------------------------------------------------
#https://kourentzes.com/forecasting/2019/01/16/tutorial-for-the-nnfor-r-package/
library(nnfor)

# TRAIN TEST
#Define 70% Train and 30% Test Data Set
percent = 0.7
training <- subset(tsPreeclampsia, end=floor(x = percent * length(tsPreeclampsia)))
test <- subset(tsPreeclampsia, start=floor(x = percent * length(tsPreeclampsia))+1)

#TRAIN MODEL
#fitELM <- elm(training)
#print(fitELM)  

#LOAD MODEL ELM40
fitELM<-readRDS(file = "fitELM40.rds")
plot(fitELM)


# FORECASTING
frcELM <- forecast(fitELM,h=floor(length(test)), level = c(95))
print(frcELM)

#Figure: confidenceELM.pdf
#......................
plot(frcELM,ylab="Emergency patients", level = c(95))
lines(test,lty=5,col = "red")


accuracy(frcELM$mean,test)[1,1:5]



#-----------------------------------------------
#       ALL CONFIDENCE COMPARISON
#-----------------------------------------------
#Figure: AllConfidenceV2.pdf
#......................
par(mfrow=c(2,2))
plot(forecastPreec,ylab="Emergency patients",ylim=c(0,25))
lines(test,lty=3,col = "red")
plot(frcETS,ylab="Emergency patients")
lines(test,lty=5,col = "red")
plot(frcMLP,ylab="Emergency patients",level = c(95))
lines(test,lty=5,col = "red")
plot(frcELM,ylab="Emergency patients",level = c(95))
lines(test,lty=5,col = "red")



#-----------------------------------------------
#       ALL FORECAST COMPARISON
#-----------------------------------------------

#Figure: AllForecastV2.pdf
#......................
library(zoo)
library(lubridate)
#https://bookdown.org/ndphillips/YaRrr/plot-margins.html
par(mar = c(7.5,5, 0.5, 0.5))
plot(test,col = "red",xlab="",ylab="",yaxt="n",xaxt="n",ylim=c(5,25))
lines(frcArima$mean,lty=5,col = "brown")
lines(frcETS$mean,lty=5,col = "orange")
lines(frcMLP$mean,lty=5,col = "blue")
lines(frcELM$mean,lty=5,col = "green")
axis(1,as.yearmon(time(test)),format(as.yearmon(time(test)), "%b %y"), cex.axis = 1.5,las=2)
axis(2,cex.axis=1.5)
mtext("Time", side=1, line=6, cex=1.5)
mtext("Emergency patients", side=2, line=3, cex=1.5)
legend(x = "topright", legend = c("Actual value", "ARIMA[0,0,1]","ETS", "MLP", "ELM"), 
       col = c("red","brown","orange","blue","green"), lwd = 2)
grid(length(test)+4)



#-----------------------------------------------
#       FUTURE FORECASTING WITH MLP
#-----------------------------------------------
frcMLP <- forecast(fitMLP ,h=floor(length(test))+12)
print(frcMLP)
pred2024<-as.data.frame(frcMLP$mean[19:30])

tsFrc2024<-ts(pred2024$`frcMLP$mean[19:30]`, start=c(2024,1), end=c(2024,12), frequency=12)


#Figure: Forecast2024.pdf
#......................
par(mar = c(7.5,5, 0.5, 0.5))
plot(round(tsFrc2024,0),col = "blue",xlab="",ylab="",yaxt="n",xaxt="n",ylim=c(5,15))
axis(1,as.yearmon(time(tsFrc2024)),format(as.yearmon(time(tsFrc2024)), "%b %y"), cex.axis = 1.5,las=2)
text(tsFrc2024, labels=round(pred2024$`frcMLP$mean[19:30]`,0), cex= 1.7)
axis(2,cex.axis=1.5)
mtext("Time", side=1, line=6, cex=1.5)
mtext("Emergency patients", side=2, line=3, cex=1.5)
grid(length(tsFrc2024))











#############################################################################
#############################################################################
#############################################################################
#---------------------------------------------------
#   Intervention analysis
#---------------------------------------------------
setwd("~/SOFT/FCI/UG-Analytics2024")
#DATOS DE PRECLAMPSIA
#preeclampsia <- read.csv("CAMAS DE HOSPITALIZACION Y URGENCIAS-APprocesado.csv",sep = ";")
preeclampsia <- read.csv("CAMAS DE HOSPITALIZACION Y URGENCIAS-2023.csv",sep = ";")

preeclampsia$year.month<-as.factor(preeclampsia$year.month)
preeclampsia$camas<-as.numeric(preeclampsia$camas)
# Building Time Series
tsPreeclampsia<-ts(preeclampsia[,2], start=c(2017,5), end=c(2023,12), frequency=12)


#1) Cambios estructurales de nivel
#.......................................
lm_fit <- lm(tsPreeclampsia ~ 1)
#lm_fit <- lm(preeclampsia$camas ~ 1)
summary(lm_fit)#coeficiente es significativo


plot(tsPreeclampsia)
lines(ts(fitted(lm_fit), start = c(2019,1), frequency = 12), col = 4)

library(strucchange)
preeclampsia_brk <- breakpoints(tsPreeclampsia ~ 1, h = 0.1)
summary(preeclampsia_brk)

plot(preeclampsia_brk)
#se alcanza el menor Bic cuando m=7

plot(tsPreeclampsia)
lines(fitted(preeclampsia_brk, breaks = 7), col = 4)
lines(confint(preeclampsia_brk, breaks = 7))

breakdates(preeclampsia_brk, breaks = 5)

coef(preeclampsia_brk, breaks = 5)


#2) Cambios estructurales de tendencia
#.......................................
l <- length(tsPreeclampsia)
tt <- 1:l
trend_fit <- lm(tsPreeclampsia ~ tt)
summary(trend_fit)#coeficiente es significativo


plot(tsPreeclampsia)
lines(ts(fitted(trend_fit), start = c(2019,1), frequency = 12), col = 4)

library(strucchange)
preeclampsia_brk <- breakpoints(tsPreeclampsia ~ tt, h = 0.1)
summary(preeclampsia_brk)

plot(preeclampsia_brk)
#se alcanza el menor Bic cuando m=1

#https://www.statmethods.net/advgraphs/axes.html
#Specify axis options within plot()
#plot(x, y, main="title", sub="subtitle",
#     xlab="X-axis label", ylab="y-axix label",
#     xlim=c(xmin, xmax), ylim=c(ymin, ymax))
plot(tsPreeclampsia,ylab="Number of inpatient beds")
lines(fitted(preeclampsia_brk, breaks = 1), col = 4)
lines(confint(preeclampsia_brk, breaks = 1))
lines(lowess(time(tsPreeclampsia), tsPreeclampsia), col='red')

breakdates(preeclampsia_brk, breaks = 1)

coef(preeclampsia_brk, breaks = 1)



#--------------IMAGEN USADA EN ARTICULO---------------------------------------------
stlPreeclampsia<-stl(tsPreeclampsia,s.window = "periodic")
plot(tsPreeclampsia,ylab="Number of inpatient beds")
lines(stlPreeclampsia$time.series[,2], col='red')
lines(fitted(preeclampsia_brk, breaks = 1), col = 4)
lines(confint(preeclampsia_brk, breaks = 1))
#-----------------------------------------------------------------------------------




library(tsoutliers)
outliers_tsPreeclampsia <- tso(tsPreeclampsia, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_tsPreeclampsia
#De acuerdo a los resultados, se ha identificado un cambio atípico transitorio en el año 2020:04
#Series: tsPreeclampsia 
#Regression with ARIMA(1,1,0) errors 
#Outliers:
#type ind    time coefhat tstat
#1   LS  36 2020:04  -89.16 -4.94
plot(outliers_tsPreeclampsia)



outliers_tsPreeclampsia$outliers
#índice de tiempo donde se han detectado los valores atípicos
(outliers_idx <- outliers_tsPreeclampsia$outliers$ind)
#Años calendario en los que se han detectado valores atípicos.
outliers_tsPreeclampsia$outliers$time


#Evaluación visual de efectos de la intervención
#longitud de la serie temporal
n <- length(tsPreeclampsia)
#valor atípico de LS en el mismo índice de tiempo que el encontrado para la serie de tiempo analizada
mo_tc <- outliers("LS", outliers_idx)
#Los datos del efecto de cambio transitorio se almacenan en una matriz de una columna , tc.
tc <- outliers.effects(mo_tc, n)




#convertimos a valores numéricos
coefhat <- as.numeric(outliers_tsPreeclampsia$outliers["coefhat"])
#obtenemos los datos de cambio transitorio con la misma magnitud determinada por la función tso()
tc_effect <- coefhat*tc
#definimos una serie de tiempo para los datos de cambios transitorios
tc_effect_ts <- ts(tc_effect, frequency = frequency(tsPreeclampsia), start = start(tsPreeclampsia))
#restando la intervención de cambio transitorio a la serie de tiempo original , obteniendo la serie
#de tiempo previa a la intervención
tsPreeclampsia_wo_ts <- tsPreeclampsia - tc_effect_ts
#Trazamos la serie temporal original , previa a la intervención y de cambio transitorio.
plot(cbind(tsPreeclampsia, tsPreeclampsia_wo_ts, tc_effect_ts))

plot(cbind(tsPreeclampsia, tsPreeclampsia_wo_ts, tc_effect_ts),main="",ylab="")
#cbind("Original","Without intervention","Effects")




#--------------------------------------------------------------
#                       CAUSAL IMPACT
#--------------------------------------------------------------


setwd("~/SOFT/FCI/UG-ECEIB2024")
#PREECLAMPSIA VALDIVIA
#preeclampsia <- read.csv("orig-CAMAS DE HOSPITALIZACION Y URGENCIAS-VALDIVIA-SOLO EMERGENCIAS.csv",sep = ";")
preeclampsiaClosed <- read.csv("CAMAS DE HOSPITALIZACION Y URGENCIAS-VALDIVIA-SOLO EMERGENCIAS-Preescriptivo.csv",sep = ";")
preeclampsiaClosed$year.month<-as.factor(preeclampsiaClosed$year.month)
preeclampsiaClosed$camas<-as.numeric(preeclampsiaClosed$camas)
tsPreeclampsiaClosed<-ts(preeclampsiaClosed[,2], start=c(2019,1), end=c(2024,12), frequency=12)

#https://google.github.io/CausalImpact/CausalImpact.html
library(CausalImpact)

df_Preeclampsia<-as.data.frame(tsPreeclampsiaClosed)
time.points <- seq.Date(as.Date("2019-01-01"), by ="month", length.out = nrow(df_Preeclampsia))
data <- zoo(df_Preeclampsia$x,time.points)
#definición de período de intervención
pre.period <- as.Date(c("2019-01-01", "2023-12-01"))
post.period <- as.Date(c("2024-01-01", "2024-12-01"))
#post.period.response<- as.Date(c("2020-10-01", "2023-12-01"))
impact <- CausalImpact(data, pre.period, post.period)
#impact <- CausalImpact(data, pre.period, post.period,post.period.response)
plot(impact)


summary(impact, "report")




#-------------------------------------------------------------------------
#                       ESTACIONAREIDAD
#-------------------------------------------------------------------------


#estacionario(SACADO DE AQUI PARA ESTE CODIGO Y EL LIBRO)
#https://www.idrisstsafack.com/post/how-to-test-the-stationarity-of-a-time-series-with-r-software


#Example of a Stationary time series

#simulate a random normal distribution and plot it
Y=rnorm(100,0,2)
ts.plot(Y)

#Example of a nonstationary time series

X=1:100
Y=rnorm(100,0,2*X)
Z=2*X+Y

ts.plot(Z)
abline(reg=lm(Z~time(Z)),col="red")


#obtener valores bitcoin
#https://rstudio-pubs-static.s3.amazonaws.com/520437_534bf3a2dd424e489d882ee0e420caf6.html
library(quantmod)
getSymbols("BTC-USD", from="2015-01-01", to = "2020-12-31", 
           src="yahoo", periodicity="daily")
btc<-as.data.frame(`BTC-USD`)
#https://stackoverflow.com/questions/66791600/adding-symbols-and-labels-to-a-chart-in-shiny-created-with-quantmod
#chartSeries(`BTC-USD`, subset='2015-01-01::2020-12-31', theme=chartTheme('white'))
chartSeries(btc, theme=chartTheme('white'))


btc<-as.data.frame(`BTC-USD`)
#btc<-as.ts(`BTC-USD`)
ts.plot(btc$`BTC-USD.Close`)       
#https://www.biostars.org/p/9474594/
abline(reg=lm(btc$`BTC-USD.Close`~time(rownames(btc))),col="red")

#as.Date(rownames(btc))


#......................ANALISIS DE SEGMENTOS

split1=365*3
split2=365*5

segment1=as.data.frame(btc[1:split1,ncol(btc)])
segment2=as.data.frame(btc[split1+1:split2,ncol(btc)])
segment3=as.data.frame(btc[split2+1:nrow(btc),ncol(btc)])

colnames(segment1)=c("close") 
colnames(segment2)=c("close") 
colnames(segment3)=c("close") 

#drop the NA value in the data frame
#eliminar los valores faltantes (NA)
library(dplyr)
library(tidyverse)
segment1=drop_na(segment1)
segment2=drop_na(segment2)
segment3=drop_na(segment3)

#calculate the mean and the standard deviation of the different segments
#in order to see if we obtain the same mean and standard deviation for all the segments


#calcular la media y la desviación estándar de los diferentes segmentos
#para ver si obtenemos la misma media y desviación estándar para todos los segmentos

#segment1
mean_seg1=mean(segment1[,1])
std_seg1=sqrt(var(segment1[,1]))

#segment2
mean_seg2=mean(segment2[,1])
std_seg2=sqrt(var(segment2[,1]))

#segment3
mean_seg3=mean(segment3[,1])
std_seg3=sqrt(var(segment3[,1]))


mean_seg1
std_seg1
mean_seg2
std_seg2
mean_seg3
std_seg3

#-----------------PRUEBA ESTADISTICA ADF
library(tseries)

adf.test(as.ts(btc$`BTC-USD.Close`))










############################################
# PRESCRIPTIVE analysis
############################################
#https://search.r-project.org/CRAN/refmans/adagio/html/mknapsack.html
#https://www.rdocumentation.org/packages/adagio/versions/0.9.2/topics/knapsack
library(adagio)

#w  integer vector of weights.
#p  integer vector of profits.
#cap  maximal capacity of the knapsack, integer too.

#............................................
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
p <- c(15, 100, 90, 60, 40, 15, 10,  1)
cap <- 102
(is <- mknapsack(w, p, cap))
which(is$ksack == 1)
#............................................

#Sigue la metodología CRISP DM
#https://www.makingscience.es/blog/4-tipos-de-analisis-de-datos-que-debes-conocer/

#La analítica prescriptiva tiene en cuenta específicamente la información sobre posibles 
#situaciones o escenarios, los recursos disponibles, el rendimiento pasado y el rendimiento 
#actual, y sugiere una estrategia operativa.

#Escenarios … completar capacidad de camas
#Recursos …. Camas
#Utilidad…. Riesgo de muerte (porcentaje)

#Peso…. Dias promedio que toma la atención por hospitalización en esa enfermedad
#Límite …. Número de camas o pacientes a ser atendidos

#Minimizar el riesgo de muerte pero no exceder el número de camas

#Problema de la mochila
#https://es.m.wikipedia.org/wiki/Problema_de_la_mochila



#-------------------PREECLAMPSIA-------------

#w  integer vector of weights.
#p  integer vector of profits.
#cap  maximal capacity of the knapsack, integer too.

#............................................
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
p <- c(15, 100, 90, 60, 40, 15, 10,  1)
cap <- 102
(is <- mknapsack(w, p, cap))
which(is$ksack == 1)
#............................................

#duracion estancia hospitalaria
#http://www.scielo.org.co/scielo.php?script=sci_arttext&pid=S0124-81462009000200005

#https://www.msdmanuals.com/es-ec/hogar/salud-femenina/complicaciones-del-embarazo/preeclampsia-y-eclampsia
#La mayoría de las mujeres con preeclampsia y todas las que sufren eclampsia son hospitalizadas. En algunos casos, las mujeres con preeclampsia grave o con eclampsia a menudo son ingresadas en una unidad de cuidados especiales o una unidad de cuidados intensivos (UCI).

#http://www.scielo.org.pe/scielo.php?script=sci_arttext&pid=S2304-51322014000400007#:~:text=La%20mortalidad%20materna%20debida%20a,Hellp%20y%20rotura%20del%20hematoma
#riesgo de muerte preeclampsia

#                   W       W_muerte      Riesgo      P 
#Nefrologia         8.5       2.89            6.71    93.29
#Cardiologia        6.78      8.00            2.89    97.11
#Neumologia         6.45      2.40            4.55    95.45
#endocrinología     6.41      3.50            3.06    96.94
#Med Interna        5.96      6.84            5.99    94.01
#Neurología         5.73      2.89            8.64    91.36
#Trauma             3.45      2.50           18.18    81.82
#preeclampsia       3.5       5               2.09    97.1

#w <- c(8.5,6.78,6.45,6.41,5.96,5.73,3.45,3.5)
#w_muerte<-c(0.0289,0.08,0.0240,0.0350,0.0684,0.0289,0.0250,0.05)


w_muerte<-c(13,36,11,16,31,13,11,23)
p_muerte<-c(93,97,95,97,94,91,82,97)
cap <- 450
(is <- mknapsack(w_muerte, p_muerte, cap))
which(is$ksack == 1)




############UNBOUNDED KNAPSACK###################
#https://www.geeksforgeeks.org/unbounded-knapsack-repetition-items-allowed/
#https://search.r-project.org/CRAN/refmans/nilde/html/get.knapsack.html
library(nilde)

w_muerte<-c(13,36,11,16,31,13,11,23)
p_muerte<-c(93,97,95,97,94,91,82,97)



#255 mil en hospitalización … primer semestre 2023 
#https://www.iess.gob.ec/sala-de-prensa/-/asset_publisher/4DHq/content/hospital-los-ceibos-realiza-mas-de-700-mil-atenciones-en-2023/10174?redirect=https%3A%2F%2Fwww.iess.gob.ec%2Fsala-de-prensa%3Fp_p_id%3D101_INSTANCE_4DHq%26p_p_lifecycle%3D0%26p_p_state%3Dnormal%26p_p_mode%3Dview%26p_p_col_id%3Dcolumn-1%26p_p_col_pos%3D1%26p_p_col_count%3D3?mostrarNoticia=1
#cap <- 450
cap <- 500

bag <- get.knapsack(p_muerte,w_muerte,n=cap,problem="uknap")

setwd("~/SOFT/FCI/UG-ECEIB2024")
#http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata#google_vignette
# Save an object to a file
#saveRDS(bag, file = "bagValdivia.rds")
# Restore the object
#bagValdivia<-readRDS(file = "bagValdivia.rds")


#max(bagValdivia$solutions)#45


#t8<-table(bagValdivia$solutions[8,, drop = FALSE])
#saveRDS(t8, file = "t8.rds")
t8<-readRDS(file = "t8.rds")
#t8df<-as.data.frame(t8)
#saveRDS(t8df, file = "t8df.rds")
t8df<-readRDS(file = "t8df.rds")
class(t8df)







#Figura Prescriptive-preeclampsia.pdf
library(dbplyr)
library(lubridate)
library(tidyverse)

#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg <- t8df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
ggplot(fg, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5)+
  xlab("Preeclampsia Patients") + ylab("Frequency (in millions)")+
  theme(axis.text=element_text(size=14) )
#https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2

20.8+15.4+11.2+8+5.6+27.9


#data<-bagValdivia$solutions
#data<-t(data)
#head(data)
#saveRDS(data, file = "data.rds")
#data2<-as.data.frame(data)
#saveRDS(data2, file = "data2.rds")


library(tidyverse)
#data3<-data2 %>% filter(s8 %in% c(21,20,19,18,17,16,15,14,13,12,11,10))#,9,8,7
#saveRDS(data3, file = "data3.rds")
data3<-readRDS(file = "data3.rds")


max(data2$s1)#38
max(data2$s2)#13
max(data2$s3)#45
max(data2$s4)#31
max(data2$s5)#16
max(data2$s6)#38
max(data2$s7)#45
max(data2$s8)#21






#library(dplyr)
#library(tidyverse)
#data2 %>%
#  group_by(s8)


#ggplot(gather(data2), aes(value)) + 
#  geom_histogram(bins = 10) + 
#  facet_wrap(~key, scales = 'free_x')


library(dplyr)
library(tidyverse)
#data3 %>%
#  group_by(s8)


#https://www.geeksforgeeks.org/how-to-change-number-of-bins-in-histogram-in-r/
ggplot(gather(data3), aes(value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~key, scales = 'free_x')



s1_t8in10orMore<-table(data3$s1)
s2_t8in10orMore<-table(data3$s2)
s3_t8in10orMore<-table(data3$s3)
s4_t8in10orMore<-table(data3$s4)
s5_t8in10orMore<-table(data3$s5)
s6_t8in10orMore<-table(data3$s6)
s7_t8in10orMore<-table(data3$s7)
library(dbplyr)
library(lubridate)
library(tidyverse)


par(mfrow=c(4,2))

#Figura t8in10more_s1.pdf
#.......................................
s1df<-as.data.frame(s1_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg1 <- s1df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot1<-ggplot(fg1, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s1: Nephrology")





#Figura t8in10more_s2.pdf
#.......................................
s2df<-as.data.frame(s2_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg2 <- s2df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot2<-ggplot(fg2, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s2: Cardiology")




#Figura t8in10more_s3.pdf
#.......................................
s3df<-as.data.frame(s3_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg3 <- s3df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot3<-ggplot(fg3, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s3: Pneumology")


#Figura t8in10more_s4.pdf
#.......................................
s4df<-as.data.frame(s4_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg4 <- s4df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot4<-ggplot(fg4, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s4: Endocrinology")



#Figura t8in10more_s5.pdf
#.......................................
s5df<-as.data.frame(s5_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg5 <- s5df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot5<-ggplot(fg5, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s5: Internal Medicine")


#Figura t8in10more_s6.pdf
#.......................................
s6df<-as.data.frame(s6_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg6 <- s6df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot6<-ggplot(fg6, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s6: Neurology")



#Figura t8in10more_s7.pdf
#.......................................
s7df<-as.data.frame(s7_t8in10orMore)
#https://stackoverflow.com/questions/70258531/adding-the-text-of-count-and-percentage-by-the-group-in-a-bar-chart
fg7 <- s7df %>%
  mutate(
    frecMil=Freq/1000,
    perc = round(proportions(Freq) * 100, 1),
    res = str_c(perc, "%"),
    #res = str_c(Freq, "(", perc, ")%"),
    Var1 = as.factor(Var1)
  )

#ggplot(fg, aes(Var1, frecMil, fill = Var1)) +
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
plot7<-ggplot(fg7, aes(Var1, frecMil)) +
  geom_col() +
  geom_text(aes(label = res), vjust = -0.5,size = 2)+
  xlab("Patients") + ylab("Frequency (in millions)")+
  ggtitle("s7: Traumatology")






#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
library(gridExtra)
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2)


pdf("foo.pdf")
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2)
dev.off()




