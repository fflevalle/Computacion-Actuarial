##################################################################################################################################
################################################ TRABAJO PRACTICO NUMERO 2 #######################################################
##################################################################################################################################

########################################################### LIBRERIAS ###########################################################

library(tseries)
library(forecast)
library(astsa)
library(quantmod)
library(Quandl)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(moments)
library(ggfortify)
library(reshape2)
library(urca)
library(AER)
library(fUnitRoots)
library(ggrepel)
library(AdequacyModel) 
library(nortest)

theme_set(theme(
  text = element_text(family = "Times New Roman", size = 12),
  plot.title = element_text(face = "italic" , size = 15, hjust = 0, vjust = 2),
  plot.subtitle = element_text(face = "italic"),
  axis.title.y  = element_text(face = "bold"),
  axis.title.x  = element_text(face = "bold") ,
  legend.position = "none",
  strip.text = element_blank(),
  panel.border = element_rect(color = "grey", fill = NA, size = 0.5)))  


##################################################### IMPORTACION Y LIMPIEZA #######################################################
setwd("C:/Users/Facundo/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Universidad de Buenos Aires/2020/Segundo Cuatrimestre/Computacion Cientifica Actuarial/TP 2/DATASETS")

#Importamos conjunto de entrenamiento y conjunto de prueba
delhi <- read.csv("DailyDelhiClimateTrain.csv", header = TRUE, stringsAsFactors = FALSE)
delhi_test <- read.csv("DailyDelhiClimateTest.csv", header = TRUE, stringsAsFactors = FALSE)

#Realizamos unas modificaciones

delhi$date <- as.Date(delhi$date, format="%Y-%m-%d")
numvars <- c("meantemp","humidity","wind_speed","meanpressure") 
delhi[numvars] <- lapply(delhi[numvars], function(x) round(x, digits = 2))
delhi <- delhi[-1462,] #Removemos la ultima fila que contiene informacion erronea

delhi_test$date <- as.Date(delhi_test$date, format="%Y-%m-%d")
delhi_test[numvars] <- lapply(delhi_test[numvars], function(x) round(x, digits = 2))

###############################################         SERIES DE TIEMPO        #####################################################################

#Configuramos como serie de tiempo cada variable de interes a analizar

mean_temp     <- ts(data = delhi$meantemp     , frequency = 365 , start = c(2013,1))
humidity      <- ts(data = delhi$humidity     , frequency = 365 , start = c(2013,1))
wind_speed    <- ts(data = delhi$wind_speed   , frequency = 365 , start = c(2013,1))

#Grafico de cada variable

#Mean Temperature

meantemp_graph <- 
  ggplot(data = delhi, aes(x = date, y = meantemp)) +
  geom_line( color = "red" , size = 0.5 , alpha = 0.9 )+
  theme_light() +
  labs( x = "Time",
        y = "Mean Temperature in Celsius",
        title = "Mean Temperature in Delhi",
        subtitle = "Through 2013-2017")


# Humidity

humidity_graph <- 
  ggplot(data = delhi, aes(x = date, y = humidity)) +
  geom_line( color = "green" , size = .5 , alpha = 0.9 )+
  theme_light() +
  labs( x = "Time",
        y = "Humidity in g. of water per cubic meter of air",
        title = "Humidity in Delhi",
        subtitle = "Through 2013-2017")


#Wind Speed

windspeed_graph <- 
  ggplot(data = delhi, aes(x = date, y = wind_speed)) +
  geom_line( color = "blue" , size = .5 , alpha = 0.9 )+
  theme_light() +
  labs( x = "Time",
        y = "Humidity in KMPH",
        title = "Wind Speed in Delhi",
        subtitle = "Through 2013-2017")


#Facet graph

facetgraph <- 
  ggplot( data = melt(delhi[,c("date","meantemp","humidity","wind_speed")] , id.vars = "date", measure.vars = c("meantemp","humidity","wind_speed") ) , 
                      aes( x = date , y = value , col = variable , group = 1))    +                                                             
  geom_line() +
  facet_grid(variable~. , scale = "free_y") +                             
  theme_light() +
  labs( x = "Time",
        y = "",
        title = "Daily Climate in Delhi",
        subtitle = "Through 2013-2016") +
  scale_color_discrete(breaks=c('red','green', "blue"))

#################################################### ESTADISTICA DESCRIPTIVA ##########################################################

quant1 <- quantile(delhi$meantemp) 
quant2 <- quantile(delhi$humidity)
quant3 <- quantile(delhi$wind_speed)

names <- c("Mean","Median","Variance","Standard Deviation","Minimum","Maximum","1st Quantile","2nd Quantile","3rd Quantile","4th Quantile","Skewness","Kurtosis")
Mean_Temp <- c( mean(delhi$meantemp), median(delhi$meantemp), var(delhi$meantemp), sd(delhi$meantemp),min(delhi$meantemp),max(delhi$meantemp), quant1[2],quant1[3],quant1[4],quant1[5], skewness(delhi$meantemp),kurtosis(delhi$meantemp))
Humidity <- c( mean(delhi$humidity), median(delhi$humidity), var(delhi$humidity), sd(delhi$humidity),min(delhi$humidity),max(delhi$humidity), quant2[2],quant2[3],quant2[4],quant2[5], skewness(delhi$humidity),kurtosis(delhi$humidity))
Wind_Speed <- c( mean(delhi$wind_speed), median(delhi$wind_speed), var(delhi$wind_speed), sd(delhi$wind_speed),min(delhi$wind_speed),max(delhi$wind_speed), quant3[2],quant3[3],quant3[4],quant3[5], skewness(delhi$wind_speed),kurtosis(delhi$wind_speed))

Statsumm <- data.frame( Mean_Temp , Humidity , Wind_Speed, row.names = names )
colnames(Statsumm) <- c("Temperature","Humidity","Wind Speed")
write.csv(Statsumm , file = "Descrpitive Statistical Summary.csv", row.names = TRUE )


#HISTOGRAMS 


temp_hist <-
        qplot(mean_temp,
        geom="histogram",
        binwidth = 1,  
        main = "Histogram for Mean Temperature", 
        xlab = "Degrees Celsius", 
        ylab = "",
        fill = I("red"), 
        col = I("black"), 
        alpha = I(.2),
        xlim = c(0,40))

hum_hist <-
  qplot(humidity,
        geom="histogram",
        binwidth = 1,  
        main = "Histogram for Humidity", 
        xlab = "G of water per cubic meter of air ", 
        ylab = "",
        fill = I("green"), 
        col = I("black"), 
        alpha = I(.2),
        xlim = c(10,100))

ws_hist <-
  qplot(wind_speed,
        geom="histogram",
        binwidth = 1,  
        main = "Histogram for Wind Speed", 
        xlab = "Kmph", 
        ylab = "",
        fill = I("blue"), 
        col = I("black"), 
        alpha = I(.2),
        xlim = c(0,45))




################################################################################################################################

#FUNCIONES DE CORRELACION

cor_temp <- ggAcf(mean_temp, main = "Mean Temperature Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")
pcor_temp <- ggAcf(mean_temp, main = "Mean Temperature Partial Correlation" , lag.max = 80 , plot = TRUE , type = "partial")

cor_humidity <- ggAcf(humidity, main = "Humidity Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")
pcor_humidity <- ggAcf(humidity, main = "Humidity Partial Correlation" , lag.max = 80 , plot = TRUE , type = "partial")

cor_wind <- ggAcf(wind_speed, main = "Wind Speed Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")
pcor_wind <- ggAcf(wind_speed, main = "Wind Speed Partial Correlation" , lag.max = 80 , plot = TRUE , type = "partial")



## BOX-PIERCE TEST & LJUNG TEST, Creamos una funcion para que haga varios test en simultaneo y arroje los resultados en un data frame


Testing <- function(ts , type = c("Ljung-Box","Box-Pierce"), fitdf = 0){   ##Armo funcion para evaluar
  p_values <- c()
  statistics <- c()
  z <- c(seq(1,length(ts)/4,150),length(ts)/4,length(ts))
  for(i in 1:length(z)){
    p_values[i] <- Box.test(ts,lag = z[i],type = type , fitdf = fitdf)$p.value
    statistics[i] <- Box.test(ts,lag = z[i],type = type , fitdf = fitdf)$statistic  
  }
  table <- data.frame(j = z,
                      P_Value = p_values,
                      Statistic = statistics)
  return(table)
}

write.csv(Testing(ts = mean_temp, type = "Box-Pierce" , fitdf = 0),file = "temp-BP.csv")   
write.csv(Testing(ts = humidity, type = "Box-Pierce" , fitdf = 0),file = "humidity-BP.csv")
write.csv(Testing(ts = wind_speed, type = "Box-Pierce" , fitdf = 0),file = "ws-BP.csv")

write.csv(Testing(ts = mean_temp, type = "Ljung-Box" , fitdf = 0),file = "temp-LB.csv")   
write.csv(Testing(ts = humidity, type = "Ljung-Box" , fitdf = 0),file = "humidity-LB.csv")
write.csv(Testing(ts = wind_speed, type = "Ljung-Box" , fitdf = 0),file = "ws-LB.csv")

##################################################################### ANALISIS ESTACIONAREIDAD ########################################################################
##DICKEY FULLER TEST

DF<-function(ts, lag = 0){
  a<-NULL
  b<-NULL
  test1<-ur.df(ts, type="none" , lags = lag)
  test2<-ur.df(ts,type="drift" , lags = lag )
  test3<-ur.df(ts,type="trend" , lags = lag)
  
  sum1<-summary(test1)
  sum2<-summary(test2)
  sum3<-summary(test3)
  
  a<-cbind(sum1@cval, sum1@teststat)
  b<-sum2@cval[1,]
  b<-rbind(b)
  c<-sum2@teststat[1,1]
  d<-cbind(b,c)
  e<-sum3@cval[1,]
  e<-rbind(e)
  f<-sum3@teststat[1,1]
  g<-cbind(e,f)
  
  final<-rbind(a,d,g)
  
  Data.fr<-data.frame(final)
  rownames(Data.fr)=c("None", "Drift", "Trend")
  colnames(Data.fr)=c("1%","5%","10%","T-Statistic")
  
  return(Data.fr)
  
}

#Realizo el test con las series originales 
write.csv(file = "df_temp.csv" , DF(mean_temp , lag = 1)) 
write.csv(file = "df_hum.csv" , DF(humidity , lag = 1)) 
write.csv(file = "df_ws.csv" , DF(wind_speed , lag = 1)) 

### Cantidad necesaria de diferenciaciones


nsdiffs(mean_temp) #1
ndiffs(mean_temp)  #1

nsdiffs(humidity)  #1
ndiffs(humidity)   #0

nsdiffs(wind_speed)#0
ndiffs(wind_speed) #0


### Convertir en estacionaria

smean_temp <- diff(mean_temp, differences= 1)                     
stationary.temp <- autoplot(smean_temp, main="Mean Temperature - Differenced and Stationary", ylab = "",ts.colour = "red",alpha = 0.5)              
write.csv(file = "df_STATtemp.csv" , DF(smean_temp , lag = 1)) 

shumidity <- diff(humidity, differences= 1)                     
stationary.hum <- autoplot(shumidity, main="Humidity - Differenced and Stationary", ylab = "",ts.colour = "dark green",alpha = 0.5)              
write.csv(file = "df_STAThum.csv" , DF(shumidity , lag = 1)) 

########################################################## CREACION DE MODELOS ##################################################
#Funciones utilizadas
p.valuegraph <- function(model1 , model2 , model3, model4, model5,nameofvariable = NULL){
  M.s1 <- Significance_Test(model1, f = "ar", coefi = "AR")
  M.s2 <- Significance_Test(model2, f = "ar", coefi = "AR")
  M.s3 <- Significance_Test(model3, f = "ar", coefi = "AR")
  M.s4 <- Significance_Test(model4, f = "ar", coefi = "AR")
  M.s5 <- Significance_Test(model5, f = "ar", coefi = "AR")
  
  a <- data.frame( name = row.names(M.s1), order = rep(x = length(M.s1[,1]),length(M.s1[,1])) , p.value = M.s1$p.value )
  b <- data.frame( name = row.names(M.s2), order = rep(x = length(M.s2[,1]),length(M.s2[,1])) , p.value = M.s2$p.value )
  c <- data.frame( name = row.names(M.s3), order = rep(x = length(M.s3[,1]),length(M.s3[,1])) , p.value = M.s3$p.value )
  d <- data.frame( name = row.names(M.s4), order = rep(x = length(M.s4[,1]),length(M.s4[,1])) , p.value = M.s4$p.value )
  e <- data.frame( name = row.names(M.s5), order = rep(x = length(M.s5[,1]),length(M.s5[,1])) , p.value = M.s5$p.value )
  
  
  w <- rbind(a,b,c,d,e)
  w <-cbind(w,roundp = round(x = w$p.value,digits = 4))
  w <- unite(data = w , col = "labels" , sep = "=" , remove = FALSE , c("name","roundp"))
  sp <- ggplot( data = w, aes(order,p.value))
  sp +
    geom_point(aes(color= "red",size=10))+
    theme_light()+
    labs(x = "Autoregressive order AR(p)", y = "Phi Significance p values", title = paste("AR Coefficcients Significance Test for",nameofvariable)) +
    geom_abline( slope = 0 , intercept = 0.10 , linetype = 2 , color = "blue",alpha = 3) +
    theme(legend.position = "none") +
    
    geom_label_repel(aes(label = name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50')+
    ylim(0,1)
  
  
}
p.valuegraphMA <- function(model1 , model2 , model3, model4, model5 ,nameofvariable = NULL, f = c("arma","arima")){
  
  if(f == "arma"){ 
    M.s1 <- Significance_Test(model1, f = "arma", coefi = "MA")
    M.s2 <- Significance_Test(model2, f = "arma", coefi = "MA")
    M.s3 <- Significance_Test(model3, f = "arma", coefi = "MA")
    M.s4 <- Significance_Test(model4, f = "arma", coefi = "MA")
    M.s5 <- Significance_Test(model5, f = "arma", coefi = "MA")}
  if(f == "arima"){ 
    M.s1 <- Significance_Test(model1, f = "arima",coefi = "MA")
    M.s2 <- Significance_Test(model2, f = "arima",coefi = "MA")
    M.s3 <- Significance_Test(model3, f = "arima",coefi = "MA")
    M.s4 <- Significance_Test(model4, f = "arima",coefi = "MA")
    M.s5 <- Significance_Test(model5, f = "arima",coefi = "MA")}  
  
  
  a <- data.frame( name = row.names(M.s1), order = rep(x = length(M.s1[,1]),length(M.s1[,1])) , p.value = M.s1$p.value )
  b <- data.frame( name = row.names(M.s2), order = rep(x = length(M.s2[,1]),length(M.s2[,1])) , p.value = M.s2$p.value )
  c <- data.frame( name = row.names(M.s3), order = rep(x = length(M.s3[,1]),length(M.s3[,1])) , p.value = M.s3$p.value )
  d <- data.frame( name = row.names(M.s4), order = rep(x = length(M.s4[,1]),length(M.s4[,1])) , p.value = M.s4$p.value )
  e <- data.frame( name = row.names(M.s5), order = rep(x = length(M.s5[,1]),length(M.s5[,1])) , p.value = M.s5$p.value )
  
  
  w <- rbind(a,b,c,d,e)
  w <-cbind(w,roundp = round(x = w$p.value,digits = 4))
  w <- unite(data = w , col = "labels" , sep = "=" , remove = FALSE , c("name","roundp"))
  sp <- ggplot( data = w, aes(order,p.value))
  sp +
    geom_point(aes(color= "red",size=10))+
    theme_light()+
    labs(x = "Moving Average Order MA(q)", y = "Theta Significance p values", title = paste("MA Coefficcients Significance Test for",nameofvariable)) +
    geom_abline( slope = 0 , intercept = 0.10 , linetype = 2 , color = "blue",alpha = 3) +
    theme(legend.position = "none") +
    
    geom_label_repel(aes(label = name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50')+
    ylim(0,1)
  
  
}
Significance_Test <- function(model, f = c("ar","arma","arima","sarima"), coefi = c("AR","MA","ARMA"), orderAR = NULL , orderMA = NULL){
  Coefficients = NULL
  S.error = NULL
  t.statistic = NULL
  p.value = NULL
  Table = NULL
  row = NULL
  Significance = NULL
  ARMArow = NULL
  if ( coefi == "ARMA"){
    for(i in 1:orderAR){ARMArow[i] <- paste("phi",i)}
    for(i in (orderAR +1):(orderAR + orderMA)){ARMArow[i] <- paste("theta",i-orderAR)}
    ARMArow[orderAR+orderMA+1] <- "Int"
  }
  
  if( f == "ar"){
    Coefficients <- model$ar
    for(i in 1:model$order){
      row[i] <-paste("phi",i)
      if(model$method == "Unconstrained LS"){
        S.error[i] <- model$asy.se.coef$ar[i]
      }else{
        S.error[i] <- sqrt(model$asy.var.coef[i,i])}
      t.statistic[i] <- model$ar[i]/(S.error[i])
      p.value[i] <- (pt(abs(t.statistic[i]),df=model$n.used - model$order , lower.tail = FALSE) * 2)
      if(abs(t.statistic[i]) > 2){Significance[i] <- "Reject H0"}else{Significance[i] <- "Not Reject H0"}
    }
    Table <- as.data.frame(cbind(Coefficients,S.error,t.statistic,p.value))
    Table <- cbind(Table, Significance )
    rownames(Table) <- row
    return(Table)
  }
  
  if( f == "arma"){
    Coefficients <- model$coef
    
    for(i in 1:length(model$coef)){
      if( coefi == "AR") {row[i] <- c(paste("phi",i))}else
        if( coefi == "MA"){row[i] <- c(paste("theta",i))}else
          if(coefi == "ARMA"){
            row[i] <- ARMArow[i]}
      
      S.error[i] <- sqrt(model$vcov[i,i])
      t.statistic[i] <- model$coef[i]/(S.error[i])
      p.value[i] <- (summary(model))$coef[i,4]
      if(abs(t.statistic[i]) > 2){Significance[i] <- "Reject H0"}else{Significance[i] <- "Not Reject H0"}
    }
    Table <- as.data.frame(cbind(Coefficients,S.error,t.statistic,p.value))
    Table <- cbind(Table, Significance )
    rownames(Table) <- row
    Table <- Table[-length(model$coef),]
    return(Table)
  }
  
  if( f == "arima"){
    Coefficients <- model$coef
    for(i in 1:length(model$coef)){
      if( coefi == "AR") {row[i] <- c(paste("phi",i))}else
        if( coefi == "MA"){row[i] <- c(paste("theta",i))}else
          if(coefi == "ARMA"){
            row[i] <- ARMArow[i]}
      S.error[i] <- sqrt(model$var.coef[i,i])
      t.statistic[i] <- model$coef[i]/(S.error[i])
      p.value[i] <- (pt(abs(t.statistic[i]),df=model$nobs - length(model$coef - 1), lower.tail = FALSE) * 2)
      if(abs(t.statistic[i]) > 2){Significance[i] <- "Reject H0"}else{Significance[i] <- "Not Reject H0"}
    }
    Table <- as.data.frame(cbind(Coefficients,S.error,t.statistic,p.value))
    Table <- cbind(Table, Significance )
    rownames(Table) <- row
    Table <- Table[-length(model$coef),]
    return(Table)
  }  
  
  if( f == "sarima"){
    
    for(i in 1:length(model$ttable[,1])){
      row[i] <-paste("phi",i)
      Coefficients[i] <- model$ttable[i,1]
      S.error[i] <- model$ttable[i,2]
      t.statistic[i] <- Coefficients[i]/(S.error[i])
      p.value[i] <- (pt(abs(t.statistic[i]),df=model$degrees_of_freedom, lower.tail = FALSE) * 2)
      if(abs(t.statistic[i]) > 2){Significance[i] <- "Reject H0"}else{Significance[i] <- "Not Reject H0"}
    }
    row[length(Coefficients)] <- "Intercept"
    Table <- as.data.frame(cbind(Coefficients,S.error,t.statistic,p.value))
    Table <- cbind(Table, Significance )
    rownames(Table) <- row
    return(Table)
    
  }    
  
  
}
Criteria_Matrix <- function(model , p.max = NULL, q.max = NULL, type = c("AIC","BIC")){
  k <- matrix( data = numeric((p.max+1)*(q.max+1)) , ncol = p.max + 1 , nrow = q.max + 1, dimnames = list(c(0:p.max),c(0:q.max)))
  
  
  if(type == "AIC"){
    for(p in 0:p.max){
      for(q in 0:q.max){
        k[p+1,q+1] <- AIC(arima(model,order = c(p,0,q),include.mean = T))
        
      }
    }
  }
  
  if(type == "BIC"){
    for(p in 0:p.max){
      for(q in 0:q.max){
        k[p+1,q+1] <- BIC(arima(model,order = c(p,0,q),include.mean = T))
        
      }
    }
  }
  
  return(k)
  
}

#Utilizamos Criteria Matrix para ver que modelos crear

tAIC <- Criteria_Matrix( smean_temp , p.max = 5 , q.max = 5 , type = "AIC")
which(tAIC == min(tAIC), TRUE)
write.csv(tAIC, file = "tAIC.csv")

hAIC <- Criteria_Matrix( shumidity , p.max = 5 , q.max = 5 , type = "AIC")
which(hAIC == min(hAIC), TRUE)
write.csv(hAIC, file = "hAIC.csv")

wAIC <- Criteria_Matrix( wind_speed , p.max = 5 , q.max = 5 , type = "AIC")
which(wAIC == min(wAIC), TRUE)
write.csv(wAIC, file = "wAIC.csv")
#Creamos los modelos con el menor Akaike en cada caso junto con el modelo que indica el auto.arima

temp.md <- arima(mean_temp,order = c(5,1,2),include.mean = T)
temp.auto <- auto.arima(mean_temp)
hum.auto$coef
hum.md <- arima(humidity,order = c(5,1,5),include.mean = T)
hum.auto <- auto.arima(humidity)

ws.md <- arima(wind_speed,order = c(2,0,1),include.mean = T)
ws.auto <- auto.arima(wind_speed)

#Utilizamos la funcion Significance test para ver la significatividad de los modelos

ST1 <- Significance_Test(temp.md, f = "arima", coefi = "ARMA" , orderAR = 5 , orderMA = 2)   # este
ST2 <- Significance_Test(temp.auto, f = "arima", coefi = "ARMA" , orderAR = 3 , orderMA = 1)

ST3 <- Significance_Test(hum.md, f = "arima", coefi = "ARMA" , orderAR = 5 , orderMA = 5)
ST4 <- Significance_Test(hum.auto, f = "arima", coefi = "ARMA" , orderAR = 2 , orderMA = 0)  # este

ST5 <- Significance_Test(ws.md, f = "arima", coefi = "ARMA" , orderAR = 2 , orderMA = 1)     #son lo mismo
ST6 <- Significance_Test(ws.auto, f = "arima", coefi = "ARMA" , orderAR = 2 , orderMA = 1)

table <- rbind(ST1,ST2,ST3,ST4,ST5,ST6)
write.csv(file = "STb.csv", table)
#los modelos elegidos para cada caso son aquellos con todos los coeficientes significativos
####################################################### ANALISIS RESIDUOS ####################################################
#NORMALIDAD
Normality_Test <- function(ts,type = c("JB", "AD", "SW")){
  if(type == "JB"){
    p_val = jarque.bera.test(ts)$p.value
    stat  = jarque.bera.test(ts)$statistic
  } else if(type == "AD"){
    p_val = ad.test(ts)$p.value
    stat  = ad.test(ts)$statistic
  } else {
    p_val = shapiro.test(ts)$p.value
    stat  = shapiro.test(ts)$statistic
  }
  
  table = data.frame(P_Value = p_val,
                     Statistic = stat)
  return(table)
}

write.csv(file = "jb1.csv",Normality_Test(temp.md$residuals, type = "JB"))
write.csv(file = "jb2.csv",Normality_Test(hum.auto$residuals, type = "JB"))
write.csv(file = "jb3.csv",Normality_Test(ws.md$residuals, type = "JB"))


checkresiduals(residuals(temp.md)) 
checkresiduals(residuals(hum.auto)) 
checkresiduals(residuals(ws.auto)) 

ggplot( data = residuals(temp.md), aes(residuals(temp.md)) ) +
  geom_density( colour = "red")+
  theme_light() +
  labs( title = "Residuals for Temperature",
        y="",
        x="")

ggplot( data = residuals(hum.auto), aes(residuals(hum.auto)) ) +
  geom_density( colour = "green")+
  theme_light() +
  labs( title = "Residuals for Humidity",
        y="",
        x="")

ggplot( data = residuals(ws.auto), aes(residuals(ws.auto)) ) +
  geom_density( colour = "blue")+
  theme_light() +
  labs( title = "Residuals for Wind Speed",
        y="",
        x="")

#CORRELACION

write.csv(Testing(temp.md$residuals , type = "Ljung-Box"),file = "LB-res-t.csv",row.names = T)
write.csv(Testing(hum.auto$residuals , type = "Ljung-Box"),file = "LB-res-h.csv",row.names = T)
write.csv(Testing(ws.md$residuals , type = "Ljung-Box"),file = "LB-res-w.csv",row.names = T)

ggAcf(temp.md$residuals, main = "Temperature Residuals Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")
ggAcf(hum.auto$residuals, main = "Humidity Residuals Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")
ggAcf(ws.md$residuals, main = "Wind Speed Residuals Correlation" , lag.max = 80 , plot = TRUE , type = "correlation")


###################################################### PREDICCION #############################################################

#EN TRAIN SET

matplot(cbind(mean_temp,fitted(temp.md)),type='l',main = "Fitted Temperature Arima model ", ylab = "" , xlab = "Observations")

matplot(cbind(humidity,fitted(hum.auto)),type='l',main = "Fitted Humidity Arima model ", ylab = "" , xlab = "Observations")

matplot(cbind(wind_speed,fitted(ws.md)),type='l',main = "Fitted Wind Speed Arima model ", ylab = "" , xlab = "Observations")

#EN TEST

summary(delhi_test)
mean_temp.T     <- ts(data = delhi_test$meantemp     , frequency = 365 , start = c(2017,1))
humidity.T      <- ts(data = delhi_test$humidity     , frequency = 365 , start = c(2017,1))
wind_speed.T    <- ts(data = delhi_test$wind_speed   , frequency = 365 , start = c(2017,1))


temp.auto %>%
  forecast(h=114) %>%
  autoplot() + autolayer(mean_temp.T)+
  xlim(2016.80,2017.3123)+
  ggtitle("Forecast of Temperature on test data")

accuracy(Arima(mean_temp.T, model=temp.auto))

hum.auto %>%
  forecast(h=114) %>%
  autoplot() + autolayer(humidity.T)+
  xlim(2016.80,2017.3123)+
  ggtitle("Forecast of Humidity on test data")

accuracy(Arima(humidity.T, model=hum.auto))

ws.auto %>%
  forecast(h=114) %>%
  autoplot() + autolayer(wind_speed.T)+
  xlim(2016.80,2017.3123)+
  ggtitle("Forecast of Wind Speed on test data")

accuracy(Arima(wind_speed.T, model=ws.auto))

