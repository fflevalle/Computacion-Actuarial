#################################################################################################################################################################################
########################################## TRABAJO PRACTICO 1 COMPUTACION CIENTIFICA ACTUARIAL###################################################################################
#################################################################################################################################################################################

#LIBRERIAS

library(tidyverse)
library(lubridate)
library(moments)
library(RColorBrewer)
library("pscl")
library("pROC")
library("Metrics")
library("WVPlots")
library(broom)
library("rcompanion")
library("yardstick")
library(modeest)
library(corplot)

#Seteamos el theme que nos gusta por default para aplicarlo al resto de los graficos

theme_set(theme(
  text = element_text(family = "Times New Roman", size = 12),
  plot.title = element_text(face = "bold.italic" , size = 15),
  plot.subtitle = element_text(face = "italic"),
  axis.title.y  = element_text(face = "bold"),
  axis.title.x  = element_text(face = "bold") ,
  legend.position = "none",
  strip.text = element_blank(),
  panel.border = element_rect(color = "grey", fill = NA, size = 0.5)))   


#Importamos el dataset y lo limpio

setwd("C:/Users/Facundo/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/Universidad de Buenos Aires/2020/Segundo Cuatrimestre/Computacion Cientifica Actuarial/TP 1")
choco <- read.csv("flavors_of_cacao.csv", encoding = "UTF-8" , stringsAsFactors = FALSE , na.strings = " ")

colnames(choco) <- c("Company","Bean_Specific_Origin", "REF", "Review_Date","Cocoa_Percent","Company_Location","Rating", "Bean_type", "Bean_Broad_Origin")
choco[, c(8,9)] <- sapply(choco[,c(8,9)], str_trim)
choco[choco==""] <- NA


summarise(choco,count = sum(is.na(choco)))
summary(choco)
str(choco)

summary(choco)
choco$Cocoa_Percent <- as.numeric(sub("%", "", choco$Cocoa_Percent))/100 #La variable porcentaje de cacao en numerico

str(choco)
factor_vars <- c("Company","Bean_Specific_Origin","Company_Location","Bean_type","Bean_Broad_Origin","Review_Date") #Convierto en factores las variables
choco[factor_vars] <- lapply(choco[factor_vars], function(x) factor(x, exclude = NULL))



#Analisis de Variables 

##GRAFICOS

##CACAO PERCENT

ggplot( data = choco , aes( x = Cocoa_Percent))+
  geom_density( colour = "navy blue", fill = "light blue")+
  scale_x_continuous()+
  theme_light()+
  labs(title = "Cocoa Percentage in chocolate bars",
       x = "Cocoa (%)",
       y = "")



ggplot( data = choco , aes( y = Cocoa_Percent))+
  geom_boxplot( colour = "black", fill = "red")+
  scale_y_continuous()+
  theme_light()+
  labs(title = "Cocoa Percentage in chocolate bars",
       y = "Cocoa (%)",
       x = "")



## RATING

ggplot( data = choco , aes( x = Rating))+
  geom_density( colour = "navy blue" , fill= "light blue")+
  scale_x_continuous()+
  theme_light() +
  labs(title = "Chocolate bars Rating",
       x = "Rating",
       y = "")

ggplot( data = choco , aes( y = Rating))+
  geom_boxplot( colour = "black", fill = "red")+
  scale_y_continuous()+
  theme_light()+
  labs(title = "Chocolate bars Rating",
       y = "Rating",
       x = "")


## REF/Review date

ggplot( data = choco , aes( x = as.factor(Review_Date)))+
  geom_bar( colour = "navy blue" , fill= "light blue")+
  theme_light() +
  labs(title = "Review date ",
       x = "Year",
       y = "")

ggplot( data = choco , aes( x = Review_Date, y =REF))+
  geom_point(  alpha = 0.3,colour = "navy blue" , fill= "light blue" , position = "jitter")+
  stat_smooth(method = "lm")+
  theme_light() +
  labs(title = "REF ",
       x = "Year",
       y = "")

clasif <- choco%>% #Clasificamos las variables de acuerdo a la calificacion
  mutate(clasif = 
           ifelse(choco$Rating == 5, "Elite",
                  ifelse(choco$Rating == 4 , "Premium",
                         ifelse(choco$Rating >=3 , "Satisfactorio",
                                ifelse(choco$Rating >= 2, "Decepcionante", "Implacentero")))))

clasif$clasif <- factor(clasif$clasif, ordered = TRUE, levels = c("Implacentero","Decepcionante","Satisfactorio","Premium","Elite" ))
str(clasif$clasif)


clasif%>%
  group_by(clasif, Review_Date)%>%
  summarise(count = n())%>%
  
  ggplot(aes(x = Review_Date , y = count,fill = clasif , group = 1))+
  geom_bar(stat = "identity") +
  labs(x="",
       y="Number of Reviews",
       title = "Ratings through Time")+
  theme(legend.position = "right",
        legend.title = element_blank())+
  theme(
    title = element_text(vjust=2)
  )+
  scale_fill_brewer(palette = "Greens")+
  coord_flip()


# Analisis Descriptivo


correlation <- data.frame(choco$REF, as.numeric(choco$Review_Date), choco$Cocoa_Percent, choco$Rating) #Matriz de correlaciones
names(correlation)[1:4] <- c('REF', 'Review Date', 'Cocoa %', 'Rating')
correlation <- round(cor(correlation), 4)

corrplot::corrplot(correlation)

write.csv(correlation , file = "Correlation.csv", row.names = TRUE )

quant1 <- quantile(choco$REF)            #cuantiles
quant2 <- quantile(choco$Cocoa_Percent)
quant3 <- quantile(choco$Rating)

names <- c("Mean","Median","Variance","Standard Deviation","Minimum","Maximum","1st Quantile","2nd Quantile","3rd Quantile","4th Quantile","Skewness","Kurtosis")
REF <- c( mean(choco$REF), median(choco$REF), var(choco$REF), sd(choco$REF),min(choco$REF),max(choco$REF), quant1[2],quant1[3],quant1[4],quant1[5], skewness(choco$REF),kurtosis(choco$REF))
COCOA <- c( mean(choco$Cocoa_Percent), median(choco$Cocoa_Percent), var(choco$Cocoa_Percent), sd(choco$Cocoa_Percent),min(choco$Cocoa_Percent),max(choco$Cocoa_Percent), quant2[2],quant2[3],quant2[4],quant2[5], skewness(choco$Cocoa_Percent),kurtosis(choco$Cocoa_Percent))
RATING <- c( mean(choco$Rating), median(choco$Rating), var(choco$Rating), sd(choco$Rating),min(choco$Rating),max(choco$Rating), quant3[2],quant3[3],quant3[4],quant3[5], skewness(choco$Rating),kurtosis(choco$Rating))

  
Statsumm <- data.frame( REF , COCOA , RATING, row.names = names )
colnames(Statsumm) <- c("REF","COCOA","RATING")
t(Statsumm)
write.csv(Statsumm , file = "Descrpitive Statistical Summary.csv", row.names = TRUE )


#¿Dónde se producen los mejores granos de cacao?
# Para responder esta pregunta hay que observar las variables Bean Broad Origin y Rating.

comvsrating <- choco[,c("Bean_Broad_Origin","Rating")]%>%
  group_by(Bean_Broad_Origin)%>%
  summarize(count = n(), MeanRating = round(mean(Rating),2))%>%
  filter(count >= 10)%>%
  filter(Bean_Broad_Origin != "NA")%>%
  arrange(desc(MeanRating))

azul <- colorRampPalette( c("light blue", "navy blue" ))

ggplot(comvsrating[1:10,], aes(x = Bean_Broad_Origin, y = MeanRating))+
  geom_bar(aes(fill = Bean_Broad_Origin),stat = "identity")+
  scale_fill_manual(values=azul(10))+
  labs(title = "Top 10 Cocoa Beans Origin",
       x = "",
       y = "Average Rating")+
  coord_flip()+
  geom_text(aes(label=MeanRating), hjust = -0.2, color="black", size=3.5)+
  scale_y_continuous(limits = c(0,4))+
  theme(
    title = element_text(vjust=2)
  )
tb <- comvsrating %>% arrange(desc(count))
write.csv(tb , file = "tb.csv", row.names = TRUE )


#¿Qué paises producen las barras de cacao con mejor calificación?
# Para responder esta pregunta hay que observar las variables Company location y Rating.

locvsrating <- choco[,c("Company_Location","Rating")]%>%
  group_by(Company_Location)%>%
  summarize(count = n() , MeanRating = round(mean(Rating),2))%>%
  filter(count >= 10) %>%
  arrange(desc(MeanRating))

orange <- colorRampPalette( c("darkorange", "sienna4" ))

ggplot(locvsrating[1:10,], aes(x = Company_Location, y = MeanRating))+
  geom_bar(aes(fill = Company_Location),stat = "identity")+
  scale_fill_manual(values=orange(10)) +
  labs(title = "Top 10 Chocolate Manufacturers Locations ",
       x = "",
       y = "Average Rating")+
  coord_flip()+
  geom_text(aes(label=MeanRating), hjust = -0.2, color="black", size=3.5)+
  scale_y_continuous(limits = c(0,4))+
  theme(
    title = element_text(vjust=2)
  )

tb2 <- locvsrating %>% arrange(desc(count))
write.csv(tb2 , file = "tb2.csv", row.names = TRUE )

#¿Qué relación hay entre el porcentaje de cacao en una barra y su calificación?

ggplot( data = choco , aes( x = Cocoa_Percent, y = Rating))+
  geom_point(  alpha = 0.4,colour = "black" , fill= "darkblue" )+
  stat_smooth(method = "gam", color = "red")+
  stat_smooth(method = "lm", color = "navy blue")+
  theme_light() +
  labs(title = "Cocoa Percentage ~ Rating ",
       x = "Cocoa %",
       y = "Rating")

summary(lm(Rating~Cocoa_Percent, choco))
 
##############################################################################################################################
#EJERCICIO 2

#PARTE 1
#Creamos dataset con RATING binario
dummy <- choco %>%
  mutate( bin_Rating = ifelse( choco$Rating == 5, 1, 0))

  dummy$Rating <- NULL #Eliminamos variable Rating no binario
dummy$bin_Rating <- factor(dummy$bin_Rating)  #Convertimos en factor
  
#Guardamos ambos dataset en una lista

list1 <- list(choco,dummy)

#PARTE 2
#Analisis de las variables categoricas del dataset para continuo

#Las variables categoricas son COMPANY, BEAN SP ORIGIN, COMP LOCATION, BEAN TYPE, BEAN BR ORIGIN. Para este caso
#NO utilizaremos la fecha dado que existe una correlacion positiva lineal entre la misma y el valor REF, que si utilizaremos

#BEAN TYPE

beantype <- choco %>%
  group_by(Bean_type) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating))

beantype <- mutate(beantype, Bean_typeG = factor(ifelse(beantype$Mean_rating >= 3.50 , 3 , ifelse(beantype$Mean_rating > 3.00, 2, 1))))
choco <- merge( choco, beantype[,-2])


#BEAN SP ORIGIN

beansporigin <- choco %>%
  group_by(Bean_Specific_Origin) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating)) 
  
beansporigin <- mutate(beansporigin, Bean_sp_OriginG = factor(ifelse(beansporigin$Mean_rating >= 3.50 , 4 , ifelse(beansporigin$Mean_rating >= 3.00, 3,
                                                                                  ifelse(beansporigin$Mean_rating >= 2.00, 2 , 1)))))
choco <- merge( choco, beansporigin[,-2])

#BEAN BR ORIGIN

beanbrorigin <- choco %>%
  group_by(Bean_Broad_Origin) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating)) 

beanbrorigin <- mutate(beanbrorigin, Bean_br_OriginG = factor(ifelse(beanbrorigin$Mean_rating >= 3.50 , 3 , ifelse(beanbrorigin$Mean_rating >= 3.00, 2, 1))))

choco <- merge( choco, beanbrorigin[,-2])

#COMPANY

company <- choco %>%
  group_by(Company) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating)) 

company <- mutate(company, CompanyG = factor(ifelse(company$Mean_rating >= 3.25 , 3 , ifelse(company$Mean_rating >= 2.75, 2, 1))))

choco <- merge( choco, company[,-2])

#COMPANY LOCATION

companyloc <- choco %>%
  group_by(Company_Location) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating)) 

companyloc <- mutate(companyloc, Company_locG = factor(ifelse(companyloc$Mean_rating > 3.25 , 3 , ifelse(companyloc$Mean_rating >= 3.00, 2, 1))))

choco <- merge( choco, companyloc[,-2])



#Analisis de las variables en el dataset binario "dummy"

#Las variables a generar como dummies son las categoricas COMPANY, BEAN SP ORIGIN, COMP LOCATION, BEAN TYPE, BEAN BR ORIGIN
#En este caso si usaremos la varable review date como categorica y la transformaremos a binaria de acuerdo al año
str(dummy)


#Las variables numericas a transformar son REF y COCOA PERCENT

#COCOA PERCENT, creo una variable en grupos

cocoa <- choco %>%
  group_by(Cocoa_Percent) %>%
  summarise(Mean_rating = round(mean(Rating),2)) %>%
  arrange(desc(Mean_rating))

cocoa <-  mutate(cocoa, Cocoa_PercentG = factor(ifelse(cocoa$Mean_rating >= 3 , 2 , 1)))
dummy <- merge(dummy, cocoa)

#REF

#No utilizaremos la variable REF dado que utilizamos la variable Review date que contiene una correlacion positiva con REF

dummy$REF<-NULL #Quito variable REF


#Las variables categoricas son las que utilizamos con anterioridad
#Las convertiremos a dummy utilizando model.matrix
#Primero veamos que variables nos conviene utilizar


form <- formula(bin_Rating ~ Company + Bean_Specific_Origin + Review_Date + Company_Location + Bean_type + Bean_Broad_Origin)  #Primer intento de formula
form <- formula(bin_Rating ~ Review_Date + Company + Company_Location + Bean_Broad_Origin)                                    #Formula reducida para converger el modelo logistico

dummycols <- model.matrix( form  , data = dummy)   #Formando columnas dummies


#Limpiamos los dataset antes de guardarlos en una lista
# Solo las variables de interes

dummy2 <- cbind(dummy[,c("bin_Rating","Cocoa_PercentG")],dummycols)
choco2 <- choco[,-c(1,2,3,4,5,7)]


list2 <- list(dummy2, choco2)


##########
#PARTE 3. Separar los modelos en train y test

#Primero el continuo
set.seed(888) #Seteo semilla para que el trabajo sea reproducible (correr todo junto)
sm <- sample(nrow(choco2), 0.70*nrow(choco2))

train_choco <- choco2[sm,]
test_choco <- choco2[-sm,]

list_tt1 <- list(train_choco,test_choco)

#Procedemos con el binario
set.seed(888)
sm2  <- sample(nrow(dummy2), 0.70*nrow(dummy2))

train_dummy <- dummy2[sm2,]
test_dummy <- dummy2[-sm2,]

list_tt2 <- list(train_dummy,test_dummy)


#Guardamos todo en una lista
list_full <- list(list_tt1,list_tt2)

#########
#PARTE 4 CONSTRUCCION DE MODELOS

#MODELO DE REGRESION LINEAL
linearmodel <- lm(data = train_choco, Rating ~.) # Primer modelo lineal

summary(linearmodel)

write.csv(t(glance(linearmodel)), file = "glancelm.csv")
write.csv(broom::tidy(linearmodel), file = "tidy1.csv")

linearmodel2 <- lm(data = train_choco, Rating ~ Cocoa_Percent + Bean_sp_OriginG + CompanyG + Company_locG + Bean_br_OriginG) #Segundo modelo lineal
summary(linearmodel2)
write.csv(glance(linearmodel2), file = "glance2.csv")
broom::tidy(linearmodel2)

#MODELO DE REGRESION LOGISTICA
logisticmodel <- glm( bin_Rating ~. , family = binomial("logit"), data = train_dummy)
summary(logisticmodel)
glance(logisticmodel)

broom::tidy(logisticmodel) %>%
  group_by(p.value)%>%
  summarise(count = n())%>%
  filter( count < 0)

nagelkerke(logisticmodel) #pseudo R2


write.csv(choco[,1:9]%>%
  filter(Rating == 5), file = "all5s.csv")


#####PREDICCION

#####MODELO LINEAL######


#Testeo sobre el conjunto de entrenamiento
train_pred <- train_choco %>%
  mutate( predictions =  round(predict(linearmodel ),2))%>%
  mutate( residuals = Rating - predictions)

Metrics::rmse(train_pred$Rating, train_pred$predictions)
#0.2974977


#Testeo sobre el conjunto de prueba
test_pred <- test_choco %>%
  mutate( predictions =  round(predict(linearmodel , test_choco ),2))%>%
  mutate( residuals = Rating - predictions)

Metrics::rmse(test_pred$Rating, test_pred$predictions)
#0.2969402

ggplot(train_pred, aes(x = predictions, y = residuals))+
  geom_pointrange(aes(ymin = 0 , ymax = residuals), alpha = 0.3, colour = "red")+
  geom_hline(yintercept = 0 , linetype = 3) +
  theme_light()+
  labs(
    x= "Predictions",
    y="",
    title = "Train set Residuals"
  )


ggplot(test_pred, aes(x = predictions, y = residuals))+
  geom_pointrange(aes(ymin = 0 , ymax = residuals), alpha = 0.3, colour = "orange")+
  geom_hline(yintercept = 0 , linetype = 3) +
  theme_light()+
  labs(
    x= "Predictions",
    y="",
    title = "Test set Residuals"
  )


#####MODELO LOGISTICO######


# Predicciones sobre modelo de prueba

pred.prob <- predict(logisticmodel, test_dummy, type="response") 
pred.Rating <- ifelse(pred.prob >= 0.490, 1 , 0)  # Regla para las probabilidades


# Confusion matrix
tab <- table(Predicted = pred.Rating, Reference = test_dummy$bin_Rating)
write.csv(tab,file="tab.csv")

# Juntamos los datos

ac_pred <- data.frame(observed = test_dummy$bin_Rating, predicted = factor(pred.Rating))

# Calculamos Accuracy de la prediccion
accuracy <- accuracy(ac_pred, observed, predicted)
print(accuracy)

#Graficamos probabilidades
Ratingstest <- choco[-sm2,"Rating"]
plotprob <- data.frame( prob = pred.prob, Rating = Ratingstest, bin = test_dummy$bin_Rating)
head(plotprob)
plotprob%>%
  group_by(prob)%>%
  arrange(desc(bin))

ggplot(plotprob,  aes(x=Rating, y=prob)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  theme_light()+
  labs(x = "",
       y = "",
       title = "Predicted Probabilities")
