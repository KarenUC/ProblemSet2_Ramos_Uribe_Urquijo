# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 07-07-2022
###----------------- Project Set 2----------###

##### ---Limpiar Ambiente --- ######

rm(list = ls())

##### ---Cargar paquetes --- ######
require(pacman)
# usar la funci?n p_load de pacman para instalar/llamar las librer?as de la clase
p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(skimr, # summary data
       caret, # Classification And REgression Training
       rvest,
       stringr,
       dplyr,
       robotstxt
       )


#########################################################################################
#Cargar los datos


#### CAMBIAR PATH SEG贜 RUTA PC AL REPOSITORIO ###


#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/ProblemSet2_Ramos_Uribe_Urquijo/stores")
setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet2_Ramos_Uribe_Urquijo/stores")
#setwd("C:/Users/pau_9/Documents/GitHub/ProblemSet2_Ramos_Uribe_Urquijo/stores")


train_hogares<-readRDS("data/train_hogares.Rds")
train_personas<-readRDS("data/train_personas.Rds")
test_hogares<-readRDS("data/test_hogares.Rds")
test_personas<-readRDS("data/test_personas.Rds")

#########################################################################################

train<-left_join(train_hogares,train_personas, by="id")
test<-merge(test_hogares,test_personas, by="id")

# Utilizando el diccionario, identificamos variables categoricas para volverlas a tipo factor
summary(train)

train <- train %>%
mutate_at(.vars = c("Clase.x", "Dominio.x","P5090", "Pobre", "Indigente",
                      "Depto.x", "Clase.y", "Dominio.y", "Estrato1", "P6020",
                      "P6050", "P6090", "P6100", "P6210", "P6240", "Oficio",
                      "P6430", "P6510", "P6510s2", "P6545", "P6545s2", "P6580",
                      "P6580s2", "P6585s1", "P6585s1a2", "P6585s2", "P6585s2a2",
                      "P6585s3", "P6585s3a2", "P6585s4", "P6585s4a2", "P6590",
                      "P6600", "P6610", "P6620", "P6630s1", "P6630s4","P6630s2",
                      "P6630s3","P6870", "P6920", "P7040", "P7050", "P7090", "P7110",
                      "P7120", "P7140s1","P7140s2","P7150","P7160","P7310","P7150",
                      "P7350","P7422","P7472","P7495","P7500s1","P7500s2","P7500s3",
                      "P7505","P7510s1","P7510s2","P7510s3", "P7510s5","P7510s6","P7510s7",
                      "Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)
test <- test %>%
  mutate_at(.vars = c("Clase.x", "Dominio.x","P5090","Depto.x", "Clase.y", 
                      "Dominio.y", "P6020","P6050", "P6090", "P6100",
                      "P6210", "P6240", "Oficio","P6430", "P6510",
                      "P6545", "P6580","P6585s1", "P6585s2", "P6585s3", 
                      "P6585s4", "P6590","P6600", "P6610", "P6620", "P6630s1",
                      "P6630s4","P6630s2","P6630s3","P6870", "P6920", "P7040",
                      "P7050", "P7090", "P7110","P7120", "P7150","P7160",
                      "P7310","P7150","P7350","P7422","P7472","P7495",
                      "P7500s2","P7500s3","P7505","P7510s1","P7510s2",
                      "P7510s3", "P7510s5","P7510s6","P7510s7",
                      "Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)

summary(train_personas$P7510s5)
summary(train$P7510s5)

### --- 2. a) Data Cleaning --- ###
### --- Missing Values

# Sacar cantidad de NAs por variable
cantidad_na <- sapply(train, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
cantidad_na<- data.frame(cantidad_na)%>%
  rownames_to_column("variable")
cantidad_na$porcentaje_na <- cantidad_na$cantidad_na/nrow(train)



# Eliminamos variables que no aportan
filtro <- cantidad_na$porcentaje_na > 0.85
variables_eliminar <- cantidad_na$variable[filtro]
train <- train %>% 
  select(-variables_eliminar)

####################################################################################################
#Qu茅 variables est谩n en la base train y test
x <- ls(train)
y <- ls(test)
Coincidencias <- list()

for(i in 1:length(y)){
  for(j in 1:length(x)){
    if (y[i]==x[j]){
      Coincidencias <- append(Coincidencias, y[i])
    }
  }
}
Coincidencias
####################################################################################################
################################# Limpieza de Test #############################################

### --- 2. a) Data Cleaning --- ###
### --- Missing Values

# Sacar cantidad de NAs por variable
cantidad_na_test <- sapply(test, function(x) sum(is.na(x)))
cantidad_na_test <- data.frame(cantidad_na_test)
cantidad_na_test<- data.frame(cantidad_na_test)%>%
  rownames_to_column("variable")
cantidad_na_test$porcentaje_na <- cantidad_na_test$cantidad_na/nrow(train)

# Eliminamos variables que no aportan
filtro <- cantidad_na_test$porcentaje_na > 0.85
variables_eliminar_test <- cantidad_na_test$variable[filtro]
test <- test %>% 
  select(-variables_eliminar_test)

### ----- Escoger variables que nos sirven para hacer el modelo -----####

#Caracteristicas del jefe del hogar (P6050- Opci贸n 1 es jefe del hogar - parentesco con jefe del hogar)
#genero_jef(P6020), ocupado (Oc), educaci贸n(P6210, P6210s1),desocupado (Des), 
#ingreso (Ingtot)

#Caracter铆sticas de Individuo:
#Edad(P6040), educaci贸n(P6210, P6210s1), salud(P6090), ahorro(P7510s5), genero(P6020)
#desocupado (Des), ingreso (Ingtot)

#Caracteristicas del hogar:
#Vivienda propia (P5090), total personas en el hogar (), choques a salud(P6240, opci贸n 5),
#choques ,ingreso (Ingtotugarr) 

female_test<-ifelse(test$P6020==2,1,0)
jh_test<-ifelse(test$P6050==1,1,0)
id<-test$id
DB_test<-data_frame(id,female_test,jh_test)

DB_test$female_jh<-ifelse(DB_test$jh_test==1,DB_test$female_test,0)
DB_test$ocu<-ifelse(is.na(test$Oc),0,1)
DB_test$edad<-test$P6040
DB_test$edad_jh<-ifelse(DB_test$jh_test==1,test$P6040,0)
DB_test$menores<-ifelse(DB_test$edad<18,1,0)
DB_test$max_educ_jh <-ifelse(DB_test$jh_test==1,test$P6210s1,0)
DB_test$jh_ocup <-ifelse(DB_test$jh_test==1,DB_test$ocu,0)
DB_test$afiliado<-ifelse(test$P6090!=1 | is.na(test$P6090),0,1)
DB_test$prod_finan_jh<-(ifelse(test$P7510s5!=1 | is.na(test$P7510s5),0,1))
DB_test$prod_finan_jh<-(ifelse(DB_test$jh_test==1 & DB_test$prod_finan_jh==1,1,0))


DB_2_test<-DB_test %>% group_by(id) %>% summarise(total_female = sum(female_test),
                                        female_jh = sum(female_jh),
                                        num_ocu = sum(ocu),
                                        edad_jh = sum(edad_jh),
                                        menores= sum(menores),
                                        max_educ_jh= sum(max_educ_jh), 
                                        jh_ocup= sum(jh_ocup),
                                        num_afsalud = sum(afiliado),
                                        prod_finan_jh=sum(prod_finan_jh)
) 

test_hogares <-test_hogares %>% left_join(DB_2_test,by="id")

test_hogares$viviendapropia <-as.factor(ifelse (test_hogares$P5090==1 | test_hogares$P5090==2,1,0))

sum(is.na(test_hogares$max_educ_jh))

#Eliminar NA en max_edu_jh
test_hogares <- test_hogares[!is.na(test_hogares$max_educ_jh),]

#Base Final TEST

########################################################################################################################################
### ----- Escoger variables que nos sirven para hacer el modelo -----####

#Decidimos hacerlo v?a dos partes: Caracteristicas del jefe del hogar y caracteristicas propias del hogar 

#Caracteristicas del jefe del hogar (P6050- Opci贸n 1 es jefe del hogar - parentesco con jefe del hogar)
#genero_jef(P6020), ocupado (Oc), educaci贸n(P6210, P6210s1),desocupado (Des), 
#ingreso (Ingtot)

#Caracter铆sticas de Individuo:
#Edad(P6040), educaci贸n(P6210, P6210s1), salud(P6090), ahorro(P7510s5), genero(P6020)
#desocupado (Des), ingreso (Ingtot)

#Caracteristicas del hogar:
#Vivienda propia (P5090), total personas en el hogar (), choques a salud(P6240, opci贸n 5),
#choques ,ingreso (Ingtotugarr) 

female<-ifelse(train$P6020==2,1,0)
jh<-ifelse(train$P6050==1,1,0)
id<-train$id
DB<-data_frame(id,female,jh)

DB$female_jh<-ifelse(DB$jh==1,DB$female,0)
DB$ocu<-ifelse(is.na(train$Oc),0,1)
DB$edad<-train$P6040
DB$edad_jh<-ifelse(DB$jh==1,train$P6040,0)
DB$menores<-ifelse(DB$edad<18,1,0)
DB$max_educ_jh <-ifelse(DB$jh==1,train$P6210s1,0)
DB$jh_ocup <-ifelse(DB$jh==1,DB$ocu,0)
DB$afiliado<-ifelse(train$P6090!=1 | is.na(train$P6090),0,1)
DB$prod_finan_jh<-(ifelse(train$P7510s5!=1 | is.na(train$P7510s5),0,1))
DB$prod_finan_jh<-(ifelse(DB$jh==1 & DB$prod_finan_jh==1,1,0))
DB$Estrato<-ifelse(DB$jh==1,train$Estrato1,0)
DB$Ingtot<- train$Ingtot
DB$Ingtot_jh<-ifelse(DB$jh==1,DB$Ingtot, 0)



DB_2<-DB %>% group_by(id) %>% summarise(total_female = sum(female),
                                              female_jh = sum(female_jh),
                                              num_ocu = sum(ocu),
                                              edad_jh = sum(edad_jh),
                                              menores= sum(menores),
                                              Ingtot_jh = sum(Ingtot_jh),
                                              max_educ_jh= sum( max_educ_jh), 
                                              jh_ocup= sum(jh_ocup),
                                              num_afsalud = sum(afiliado),
                                              jh_estrato=sum(Estrato),
                                              prod_finan_jh=sum(prod_finan_jh)
                                              ) 


train_hogares <-train_hogares %>% left_join(DB_2,by="id")

#Estadisticas descriptivas

train_hogares <- train_hogares %>%
  mutate_at(.vars = c("Clase", "Dominio","P5090", "Pobre", "Indigente",
                      "Depto", "female_jh", "jh_ocup", "jh_estrato" ),.funs = factor)
library(stargazer)
library(skimr)
skim(train_hogares)
#Verificaci贸n de missing values
sum(is.na(train_hogares$Ingtot_jh))
sum(is.na(train_hogares$max_educ_jh))
#Imputaci贸n de datos
train_hogares = train_hogares %>%
  mutate(Ingtot_jh = ifelse(is.na(Ingtot_jh),
                         yes = Ingtotugarr,
                         no = Ingtot_jh))

#Eliminar NA en max_edu_jh
train_hogares <- train_hogares[!is.na(train_hogares$max_educ_jh),]

##Creacion de variable Vivienda Propia
train_hogares$viviendapropia <-as.factor(ifelse (train_hogares$P5090==1 | train_hogares$P5090==2,1,0))

######################################################################################################################
######################################################################################################################
######################################################################################################################

#Tabla estad?sticas descriptivas - Variables Num?ricas

library(stargazer)

stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu")], type = "text") 
stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu")], type = "latex") 

stargazer(train_hogares[c("menores", "Ingtot_jh", "max_educ_jh", "num_afsalud")], type = "text")
stargazer(train_hogares[c("menores", "Ingtot_jh", "max_educ_jh", "num_afsalud")], type = "latex")


install.packages("gtsummary")
library(gtsummary)
library(tidyverse)


### Gr?ficas estadisticas descriptivas ###

box_plot <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , Ingtotugarr)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y ="Ingresos Totales Hogar") 

train_hogares$log_ingh <- log(train_hogares$Ingtotugarr+1)

box_plot2 <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , log_ingh)) + 
  geom_boxplot()

box_plot2 <- box_plot2 +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y =" Logaritmo Ingresos Totales Hogar")

ing_graph <- ggarrange(box_plot, box_plot2) ## Para incluir en el an?lisis

pobre_bar <- ggplot(data = train_hogares, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (subtitle="Base de Entrenamiento",
        x= "Pobre = 1", y = "N?mero de Pobres")

total_graphs <- ggarrange(pobre_bar, ing_graph)


############### ------Modelos para pedecir pobreza de los hogares---------############################

####----Classification models

##Probabilidad de hogar pobre
train_hogares$Pobre <-as.factor(train_hogares$Pobre)


set.seed(101010)
#Modelo 1

model_log_1 <- glm(Pobre ~ viviendapropia + total_female + female_jh +
                  num_ocu + edad_jh + menores + max_educ_jh + jh_ocup +
                  num_afsalud + prod_finan_jh,
                  data= train_hogares,
                  family=binomial(link="logit"))

summary(model_log_1)


#Modelo 2 - Sin productos financieros

model_log_2 <- glm( Pobre ~ viviendapropia + total_female + female_jh +
                      num_ocu + edad_jh + menores + max_educ_jh + jh_ocup +
                      num_afsalud,
                    family=binomial(link="logit"),
                    data= train_hogares
)
summary(model_log_2)

#Modelo 3 - Caracteristicas del Hogar unicamente

model_log_3 <- glm(Pobre ~ viviendapropia  + total_female  +
                      num_ocu  + menores  +  num_afsalud,
                    family=binomial(link="logit"),
                    data= train_hogares)

summary(model_log_3)

#Modelo 4 - Caracteristicas del JH unicamente

model_log_4 <- glm( Pobre ~  female_jh + edad_jh + max_educ_jh + jh_ocup + prod_finan_jh,
                   family=binomial(link="logit"),
                   data= train_hogares
)

summary(model_log_4)


#Modelo 5 - Transformaciones edad
train_hogares$edad_jh2<-(train_hogares$edad_jh)^2

model5 <- as.formula(Pobre ~ viviendapropia + total_female + female_jh +
                       num_ocu + edad_jh + edad_jh2 + menores + max_educ_jh + jh_ocup +
                       num_afsalud +  prod_finan_jh)

model_log_5 <- glm( model5,
                    family=binomial(link="logit"),
                    data= train_hogares)


summary(model_log_5)


stargazer(model_log_1, model_log_2, model_log_3, model_log_4, model_log_5, type = "text")

stargazer(model_log_1, model_log_2, model_log_3, model_log_4, model_log_5, type = "latex")

###Predicciones Logit

library("dplyr") #for data wrangling
library("gamlr") #ML

train_hogares$y_hat_1 <- predict(model_log_1, newdata=train_hogares , type="response")
train_hogares$y_hat_2 <- predict(model_log_2 , newdata=train_hogares , type="response")
train_hogares$y_hat_3 <- predict(model_log_3 , newdata=train_hogares , type="response")
train_hogares$y_hat_4 <- predict(model_log_4 , newdata=train_hogares , type="response")
train_hogares$y_hat_5 <- predict(model_log_5 , newdata=train_hogares , type="response")

summary(train_hogares$y_hat_1)
summary(train_hogares$y_hat_2)
summary(train_hogares$y_hat_3)
summary(train_hogares$y_hat_4)
summary(train_hogares$y_hat_5)

## Metricas ##

# Definir regla

rule=0.5
train_hogares$pobre_prob1 = ifelse(train_hogares$y_hat_1>rule,1,0)
train_hogares$pobre_prob2 = ifelse(train_hogares$y_hat_2>rule,1,0)
train_hogares$pobre_prob3 = ifelse(train_hogares$y_hat_3>rule,1,0)
train_hogares$pobre_prob4 = ifelse(train_hogares$y_hat_4>rule,1,0)
train_hogares$pobre_prob5 = ifelse(train_hogares$y_hat_5>rule,1,0)

# Matriz clasificaci?n

## logit
cm_logit1 = confusionMatrix(data=factor(train_hogares$pobre_prob1) , 
                          reference=factor(train_hogares$Pobre) , 
                          mode="sens_spec" , positive="1")

cm_logit2 = confusionMatrix(data=factor(train_hogares$pobre_prob2) , 
                            reference=factor(train_hogares$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit3 = confusionMatrix(data=factor(train_hogares$pobre_prob3) , 
                            reference=factor(train_hogares$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit4 = confusionMatrix(data=factor(train_hogares$pobre_prob4) , 
                            reference=factor(train_hogares$Pobre) , 
                            mode="sens_spec" , positive="1")

cm_logit5 = confusionMatrix(data=factor(train_hogares$pobre_prob5) , 
                            reference=factor(train_hogares$Pobre) , 
                            mode="sens_spec" , positive="1")
##Matriz de confusi贸n para cada modelo
cm1 <- cm_logit1$table
cm2 <- cm_logit2$table
cm3 <- cm_logit3$table
cm4 <- cm_logit4$table
cm5 <- cm_logit5$table
##Metricas para cada modelo
metricas_cm1 <- cm_logit1$byClass
metricas_cm2 <- cm_logit2$byClass
metricas_cm3 <- cm_logit3$byClass
metricas_cm4 <- cm_logit4$byClass
metricas_cm5 <- cm_logit5$byClass
Metricas_modelos <-  rbind(metricas_cm1, metricas_cm2, metricas_cm3, metricas_cm4, metricas_cm5)
Metricas_modelos <- Metricas_modelos[,-8]

require(xtable)

xtable(Metricas_modelos)

##ROC curve

library(ggplot2)
library(pROC)

### Curvas ROC y AUC otro intento ###

install.packages("plotROC")
library(plotROC)

roc1 <- roc(train_hogares$Pobre, train_hogares$y_hat_1)
auc1 <- round(auc(train_hogares$Pobre, train_hogares$y_hat_1),4)
model1 <- paste('Model 1 (AUC=',toString(round(auc1,2)),')',sep = '')

roc2 <- roc(train_hogares$Pobre, train_hogares$y_hat_2)
auc2 <- round(auc(train_hogares$Pobre, train_hogares$y_hat_2),4)
model2 <- paste('Model 2 (AUC=',toString(round(auc2,2)),')',sep = '')


roc3 <- roc(train_hogares$Pobre, train_hogares$y_hat_3)
auc3 <- round(auc(train_hogares$Pobre, train_hogares$y_hat_3),4)
model3 <- paste('Model 3 (AUC=',toString(round(auc3,2)),')',sep = '')

roc4 <- roc(train_hogares$Pobre, train_hogares$y_hat_4)
auc4 <- round(auc(train_hogares$Pobre, train_hogares$y_hat_4),4)
model4 <- paste('Model 4 (AUC=',toString(round(auc4,2)),')',sep = '')

roc5 <- roc(train_hogares$Pobre, train_hogares$y_hat_5)
auc5 <- round(auc(train_hogares$Pobre, train_hogares$y_hat_5),4)
model5 <- paste('Model 5 (AUC=',toString(round(auc5,2)),')',sep = '')

# Create an empty figure, and iteratively add a line for each class
#install.packages("plotly")
#install.packages("tidymodels")
#install.packages("fastDummies")

library(plotly)
library(tidymodels)
library(fastDummies)

fig <- plot_ly()%>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'), showlegend = FALSE) %>%
  add_trace(data = roc1, x = (1-roc1$specificities), y = roc1$sensitivities, mode = 'lines', name = model1, type = 'scatter')%>%
  add_trace(data = roc2 ,x = (1-roc2$specificities), y = roc2$sensitivities, mode = 'lines', name = model2, type = 'scatter')%>%
  add_trace(data = roc3 ,x = (1-roc3$specificities), y = roc3$sensitivities, mode = 'lines', name = model3, type = 'scatter')%>%
  add_trace(data = roc4 ,x = (1-roc4$specificities), y = roc4$sensitivities, mode = 'lines', name = model4, type = 'scatter')%>%
  add_trace(data = roc5 ,x = (1-roc5$specificities), y = roc5$sensitivities, mode = 'lines', name = model5, type = 'scatter')%>%
    layout(xaxis = list(
    title = "False Positive Rate"
  ), yaxis = list(
    title = "True Positive Rate"
  ),legend = list(x = 100, y = 0.5))
fig

require(caret)

train_hogares$Pobre<- factor((train_hogares$Pobre), 
                             levels = c(0, 1), 
                             labels = c("No", "Si"))

## Divisi贸n del balance de clases para evitar overfitting
## Entrenamiento
set.seed(123)
split1 <- createDataPartition(train_hogares$Pobre, p = .7)[[1]]
length(split1)
## Prueba y evaluaci贸n
other <- train_hogares[-split1,]
training <- train_hogares[ split1,]
set.seed(123)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[ split2,]
testing <- other[-split2,]

dim(training)
dim(evaluation)
dim(testing)

summary(training$Pobre)

##Modelo seleccionado: Modelo 1
#Modelo logit Cross validation
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl_pobre <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = fiveStats,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)

logit_caret_pob <- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                           num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                             num_afsalud + prod_finan_jh,
                           data = training,
                           method = "glm",
                           trControl = ctrl_pobre,
                           family = "binomial",
                           preProcess = c("center", "scale")
)

logit_caret_pob

#Modelo logit lasso Cross validation

lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid
set.seed(123)
logit_lasso<- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                             num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                             num_afsalud + prod_finan_jh,
                    data = training,
                    method = "glmnet",
                    trControl = ctrl_pobre,
                    family = "binomial",
                    metric = "Accuracy",
                    tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                    preProcess = c("center", "scale")
)
logit_lasso

logit_lasso[["bestTune"]]
### Posici貌n 55 - Mejor lambda = 0.01539121

### Modelo lasso Roc
logit_lasso_ROC<- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                      num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                      num_afsalud + prod_finan_jh,
                    data = training,
                    method = "glmnet",
                    trControl = ctrl_pobre,
                    family = "binomial",
                    metric = "ROC",
                    tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                    preProcess = c("center", "scale")
)
logit_lasso_ROC
logit_lasso_ROC[["bestTune"]]

### Modelo lasso Sens
logit_lasso_sens<- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                          num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                          num_afsalud + prod_finan_jh,
                        data = training,
                        method = "glmnet",
                        trControl = ctrl_pobre,
                        family = "binomial",
                        metric = "Sens",
                        tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                        preProcess = c("center", "scale")
)
logit_lasso_sens
logit_lasso_sens[["bestTune"]]

#Upsampling
set.seed(123)
upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           yname = "Pobre")

dim(training)
dim(upSampledTrain)
table(upSampledTrain$Pobre)

set.seed(123)
logit_lasso_upsample <- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                                num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                                num_afsalud + prod_finan_jh,
                              data = upSampledTrain,
                              method = "glmnet",
                              trControl = ctrl_pobre,
                              family = "binomial",
                              metric = "ROC",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                              preProcess = c("center", "scale")
)
logit_lasso_upsample
logit_lasso_upsample[["bestTune"]]


#Downsampling
set.seed(1103)
downSampledTrain <- downSample(x = training,
                               y = training$Pobre,
                               yname = "Pobre")
dim(training)
dim(downSampledTrain)
table(downSampledTrain$Pobre)

set.seed(123)
logit_lasso_downsample <- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                                num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
                                num_afsalud + prod_finan_jh,
                              data = downSampledTrain,
                              method = "glmnet",
                              trControl = ctrl_pobre,
                              family = "binomial",
                              metric = "ROC",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                              preProcess = c("center", "scale")
)
logit_lasso_downsample
logit_lasso_downsample[["bestTune"]]


#Smote
# training<-training %>% mutate(viviendapropia=ifelse(viviendapropia=="1",1,0))
# training<-training %>% mutate(female_jh=ifelse(female_jh=="1",1,0))
# training<-training %>% mutate(jh_ocup=ifelse(jh_ocup=="1",1,0))
# training<-training %>% mutate(prod_finan_jh=ifelse(prod_finan_jh=="1",1,0))
# 
# p_load(smotefamily)
# require(smotefamily)
# 
# predictors<-c("viviendapropia", "total_female", "female_jh", "num_ocu",
#               "edad_jh", "menores", "max_educ_jh", "jh_ocup", "num_afsalud", "prod_finan_jh")
# head( training[predictors])
# 
# smote_output = SMOTE(X = training[predictors],
#                     target = training$Pobre)
# oversampled_data = smote_output$data
# table(training$Pobre)
# table(oversampled_data$class)
# 
# logit_lasso_smote <- train(class ~ factor(viviendapropia) + total_female + factor(female_jh) +
#                              num_ocu + edad_jh + menores + max_educ_jh + factor(jh_ocup) +
#                              num_afsalud + prod_finan_jh,
#                                data = oversampled_data,
#                                method = "glmnet",
#                                trControl = ctrl_pobre,
#                                family = "binomial",
#                                metric = "ROC",
#                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
#                                preProcess = c("center", "scale")
# )
# logit_lasso_smote
# logit_lasso_smote[["bestTune"]]
# 


###Resultados de las m茅tricas 
result_logitcv <- logit_caret_pob[["results"]]
colnames(result_logitcv)[1]<-"lambda"
result_lassoacc <- logit_lasso[["results"]][55,-1]
result_lassoroc<- logit_lasso_ROC[["results"]][54,-1]
result_lassosens<- logit_lasso_sens[["results"]][100,-1]
result_lassoupsample <- logit_lasso_upsample[["results"]][57,-1]
result_lassodownsample <- logit_lasso_downsample[["results"]][57,-1]

results<-rbind(result_logitcv,result_lassoacc,result_lassoroc, result_lassosens, result_lassoupsample,result_lassodownsample  )
###El mejor modelo es el Modelo lasso downsample de acuerdo con el ROC
xtable(results)

###Determinar el Cutoff
evaluation
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc <- predict(logit_lasso_downsample,
                           newdata = evaluation,
                           type = "prob")[,1]
library(pROC)
rfROC <- roc(evalResults$Pobre, evalResults$Roc, levels = rev(levels(evalResults$Pobre)))
rfROC
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh

evalResults <- evalResults %>% mutate(hat_pobre_05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    hat_pobre_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Si","No"))

with(evalResults,table(Pobre,hat_pobre_05))
with(evalResults,table(Pobre,hat_pobre_rfThresh))

#El threshold es de 0.53. Sin embargo, se prefiere hat_pobre_05 porque le atina a m谩s hogares que son verdaderamente pobres



############## ----- Evaluaci贸n en la muestra de prueba ------ #####################

testResults <- data.frame(Pobre = testing$Pobre)

summary(testing$Pobre)

testResults$logit<- predict(logit_caret_pob,
                            newdata = testing,
                            type = "prob")[,1]

hist(testResults$logit)

testResults$lasso<- predict(logit_lasso,
                            newdata = testing,
                            type = "prob")[,1]

testResults$lasso_roc<- predict(logit_lasso_ROC,
                                   newdata = testing,
                                   type = "prob")[,1]

testResults$lasso_sens<- predict(logit_lasso_sens,
                                     newdata = testing,
                                     type = "prob")[,1]

testResults$lasso_upsample<- predict(logit_lasso_upsample,
                                 newdata = testing,
                                 type = "prob")[,1]

testResults$lasso_downsample<- predict(logit_lasso_downsample,
                                 newdata = testing,
                                 type = "prob")[,1]

testResults<-testResults %>%
  mutate(logit=ifelse(logit>rfThresh$threshold,"Si","No"),
         lasso=ifelse(lasso>rfThresh$threshold,"Si","No"),
         lasso_roc =ifelse(lasso_roc>rfThresh$threshold,"Si","No"),
         lasso_sens=ifelse(lasso_sens>rfThresh$threshold,"Si","No"),
         lasso_upsample=ifelse(lasso_upsample>rfThresh$threshold,"Si","No"),
         lasso_downsample=ifelse(lasso_downsample>rfThresh$threshold,"Si","No")
          )

with(testResults,table(Pobre,logit))
with(testResults,table(Pobre,lasso))
with(testResults,table(Pobre,lasso_roc))
with(testResults,table(Pobre,lasso_sens))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,lasso_downsample))

##  Una vez escogemos el modelo, basados en que es preferible identificar en las predicciones la mayor cantidad de Pobres (S?). 
##  Procedemos a realizar la predicci?n final de la variable categ?rica en la base de Test. 

test_hogares$Pobre_predicho_final<-predict(logit_lasso_downsample,newdata=test_hogares)

#Prediccion final Modelo Clasificaci?n

summary(test_hogares$Pobre_predicho_final)


######################################################################################################################
######################################################################################################################
######################################################################################################################

####----Regression models ---####

p_load(glmnet)
p_load(corrr)
p_load(pls)
p_load(EnvStats)
###############################################################################
####################### Modelos de regresion lineal ####################################

###############################################################################
### ----Ingreso de los hogares---- #####

# #Transformaci贸n Box Cox Ingreso de los hogares
# sum(is.na(train_hogares$Ingtotugarr))
# x<-as.numeric(train_hogares$Ingtotugarr)
# lambda<-boxcox(x+1, objective.name = "Log-Likelihood", optimize = T)$lambda
# #Transformamos la variable
# train_hogares$Ingtotugarr_boxcox<-boxcoxTransform(x+1, lambda)
#Transformaci贸n con log
#train_hogares$log_Ingtotugarr<-log(train_hogares$Ingtotugarr+1)
# 
# summary(train_hogares$Ingtotugarr)
# summary(train_hogares$Ingtotugarr_boxcox)
#hist(train_hogares$Ingtotugarr)
# hist(train_hogares$Ingtotugarr_boxcox)
#hist(train_hogares$log_Ingtotugarr)
# summary(train_hogares)

# Modelo 1
model1_ols<- lm(Ingtotugarr ~ Dominio + viviendapropia
                + Nper + total_female + female_jh + num_ocu + 
                  edad_jh+ edad_jh2 + menores + max_educ_jh 
                + jh_ocup + prod_finan_jh+num_afsalud, data= train_hogares
                )
summary(model1_ols)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_ols <- predict(model1_ols, newdata = train_hogares)
summary(predicciones_ols)

# MAE de entrenamiento
# ==============================================================================
mae_ols <- mean(abs((predicciones_ols - train_hogares$Ingtotugarr)))
paste("Error (mae) de ols:", mae_ols)
##########################################################################################################################################################################


p_load(faraway)

# Gr谩ficos y tratamiento de datos
# ==============================================================================
p_load(tidyverse)
p_load(skimr)
p_load(DataExplorer)
p_load(scales)
p_load(corrr)

# Modelado
# ==============================================================================
p_load(glmnet)
p_load(pls)

# Matrices de entrenamiento y test
# ==============================================================================
x_train <- model.matrix(Ingtotugarr~Nper+Npersug+P5000+P5010
+viviendapropia+total_female+female_jh+num_ocu+edad_jh+menores
+max_educ_jh+jh_ocup+num_afsalud+prod_finan_jh,data = train_hogares)[, -1]
y_train <- train_hogares$Ingtotugarr

#### ---- Ridge -----######

# Evoluci贸n del error en funci贸n de lambda
# ==============================================================================
set.seed(123)
cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_ridge
plot(cv_error_ridge)

# Mayor valor de lambda con el que el test-error no se aleja m谩s de 1sd del m铆nimo.
paste("Mejor valor de lambda encontrado + 1 desviaci贸n est谩ndar:", cv_error_ridge$lambda.1se)

# Mejor modelo lambda 贸ptimo + 1sd
modelo_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.1se,
  standardize = TRUE
)

modelo_ridge

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_ridge <- coef(modelo_ridge) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_ridge <- predict(modelo_ridge, newx = x_train)

# MAE de entrenamiento
# ==============================================================================
mae_ridge <- mean(abs(predicciones_train_ridge - y_train))
paste("Error (mae) de ridge", mae_ridge)


#### ---- Lasso -----######

# Creaci贸n y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularizaci贸n Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango autom谩tico.
modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

# Evoluci贸n de los coeficientes en funci贸n de lambda
# ==============================================================================
regularizacion_lasso <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion_lasso <- regularizacion_lasso %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion_lasso %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci贸n de la regularizaci贸n") +
  theme_bw() +
  theme(legend.position = "none")

# Evoluci贸n del error en funci贸n de lambda
# ==============================================================================
set.seed(123)
cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso)
# Mayor valor de lambda con el que el test-error no se aleja m谩s de 1sd del m铆nimo.
paste("Mejor valor de lambda encontrado + 1 desviaci贸n est谩ndar:", cv_error_lasso$lambda.1se)

# Mejor modelo lambda 贸ptimo + 1sd
# ==============================================================================
modelo_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes_lasso <- coef(modelo_lasso) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes_lasso %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_lasso <- predict(modelo_lasso, newx = x_train)

# MAE de entrenamiento
# ==============================================================================
mae_lasso <- mean(abs(predicciones_train_lasso - y_train))
print(paste("Error (mae) de lasso", mae_lasso))



### ----- K-fold Validaci贸n cruzada ------#####
set.seed(123)

model1_kfold <- train(Ingtotugarr~ Dominio+viviendapropia + Nper + total_female + female_jh + num_ocu + 
                  edad_jh + menores + max_educ_jh + jh_ocup + num_afsalud + prod_finan_jh, 
                data = train_hogares, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")

mae_kfold1<-model1_kfold$results[,4]
mae_kfold1

### modelo 2 tiene edad al cuadrado

model2_kfold <- train(Ingtotugarr~ Dominio+viviendapropia + Nper + total_female + female_jh + num_ocu + 
                        edad_jh + edad_jh2 + menores + max_educ_jh + jh_ocup + num_afsalud + prod_finan_jh, 
                      data = train_hogares, trControl = trainControl(method = "cv", number = 5), 
                      method = "lm")

mae_kfold2<-model2_kfold$results[,4]
mae_kfold2

mae_modelos<-rbind(mae_kfold1,mae_kfold2,mae_lasso, mae_ridge, mae_ols)

#El mejor modelo es lasso dado que es el que tiene menor MSE

# Predicci贸n modelo lasso con base test
X<-model.matrix(~Nper+Npersug+P5000+P5010
                +viviendapropia+total_female+female_jh+num_ocu+edad_jh+menores
                +max_educ_jh+jh_ocup+num_afsalud+prod_finan_jh, test_hogares)
X<-X[,-1]
test_hogares$Ingreso_predicho_final<-predict(modelo_lasso,newx = X)

#Prediccion final Modelo Clasificaci?n

summary(test_hogares$Ingreso_predicho_final)
hist(test_hogares$Ingreso_predicho_final)

test_hogares$Ing_Pred_test_hogares<-factor(ifelse(test_hogares$Ingreso_predicho_final<=test_hogares$Lp,1,0))
test_hogares$Ing_Pred_test_hogares<- factor((test_hogares$Ing_Pred_test_hogares), 
                             levels = c(0, 1), 
                             labels = c("No", "Si"))
# ########## Box cox para Linea de pobreza
# z<-as.numeric(train_hogares$Lp)
# lambda_lp<-boxcox(z, objective.name = "Log-Likelihood", optimize = T)$lambda
# #Transformamos la variable
# train_hogares$lp_boxcox<-boxcoxTransform(z, lambda_lp)
# 
# summary(train_hogares$Lp)
# summary(train_hogares$lp_boxcox)
# a<-cbind(train_hogares$Lp,train_hogares$lp_boxcox)

##########Archivo de predicciones
id_test_hogares<-test_hogares$id
Pobre_Pred_test_hogares<-test_hogares$Pobre_predicho_final
Ing_Pred_test_hogares<-test_hogares$Ing_Pred_test_hogares
DB_test_hog<-data_frame(id_test_hogares,Pobre_Pred_test_hogares,Ing_Pred_test_hogares)
summary(DB_test_hog)

setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet2_Ramos_Uribe_Urquijo/document")
write.csv(DB_test_hog, file = "predictions_ramos_uribe_urquijo_c10_r7.csv")

########



