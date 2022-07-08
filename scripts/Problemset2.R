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

#test_hogares<-import("https://github.com/KarenUC/ProblemSet2_Ramos_Uribe_Urquijo/tree/main/data/test_hogares.Rds")

#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/ProblemSet2_Ramos_Uribe_Urquijo/")
#setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet2_Ramos_Uribe_Urquijo/")
setwd("C:/Users/pau_9/Documents/GitHub/ProblemSet2_Ramos_Uribe_Urquijo/")

unzip("dataPS2RDS.zip",  list =  T)
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


### ----- Escoger variables que nos sirven para hacer el modelo -----####

#Caracteristicas del jefe del hogar (P6050- Opción 1 es jefe del hogar - parentesco con jefe del hogar)
#genero_jef(P6020), ocupado (Oc), educación(P6210, P6210s1),desocupado (Des), 
#ingreso (Ingtot)

#Características de Individuo:
#Edad(P6040), educación(P6210, P6210s1), salud(P6090), ahorro(P7510s5), genero(P6020)
#desocupado (Des), ingreso (Ingtot)

#Caracteristicas del hogar:
#Vivienda propia (P5090), total personas en el hogar (), choques a salud(P6240, opción 5),
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
#Verificación de missing values
sum(is.na(train_hogares$Ingtot_jh))
sum(is.na(train_hogares$max_educ_jh))
#Imputación de datos
train_hogares = train_hogares %>%
  mutate(Ingtot_jh = ifelse(is.na(Ingtot_jh),
                         yes = Ingtotugarr,
                         no = Ingtot_jh))

#Eliminar NA en max_edu_jh
train_hogares <- train_hogares[!is.na(train_hogares$max_educ_jh),]

##Creacion de variable Vivienda Propia
train_hogares$viviendapropia <-as.factor(ifelse (train_hogares$P5090==1 | train_hogares$P5090==2,1,0))

#Tabla estad?sticas descriptivas - Variables Num?ricas

library(stargazer)

stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu")], type = "text") 
stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu")], type = "latex") 

stargazer(train_hogares[c("menores", "Ingtot_jh", "max_educ_jh", "num_afsalud")], type = "text")
stargazer(train_hogares[c("menores", "Ingtot_jh", "max_educ_jh", "num_afsalud")], type = "latex")


install.packages("gtsummary")
library(gtsummary)
library(tidyverse)
train_hogares %>%
  select(female_jh, jh_ocup, viviendapropia) %>%
  tbl_summary() %>%
  as_gt() %>%
  gt::as_latex()

### Gr?ficas estadisticas descriptivas ###

box_plot <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , Ingtotugarr)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y ="Ingresos Totales Hogar") 

pobre_bar <- ggplot(data = train_hogares, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (x= "Pobre =1", y = "N?mero de Pobres")


############### ------Modelos para pedecir pobreza de los hogares---------############################

####----Classification models

##Probabilidad de hogar pobre
train_hogares$Pobre <-as.factor(train_hogares$Pobre)


set.seed(101010)
#Modelo 1

model_log_1 <- glm(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                  num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + factor(jh_ocup) +
                  num_afsalud + prod_finan_jh,
                  data= train_hogares,
                  family=binomial(link="logit"))

summary(model_log_1)


#Modelo 2 - Sin productos financieros

model_log_2 <- glm( as.factor(Pobre) ~ viviendapropia + total_female + female_jh +
                      num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
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

model_log_4 <- glm( Pobre ~  female_jh + edad_jh + Ingtot_jh + max_educ_jh + jh_ocup + prod_finan_jh,
                   family=binomial(link="logit"),
                   data= train_hogares
)

summary(model_log_4)


#Modelo 5 - Transformaciones edad e ingreso

model5 <- as.formula(Pobre ~ viviendapropia + total_female + female_jh +
                       num_ocu + edad_jh + (edad_jh)^2 + menores + log(Ingtot_jh+1) + max_educ_jh + jh_ocup +
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
##Matriz de confusión para cada modelo
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

##ROC curve

library(ggplot2)
library(pROC)

pred1 <- prediction(train_hogares$y_hat_1, train_hogares$Pobre)
roc_ROCR2 <- performance(pred1,"tpr","fpr")
ROC_1 <- plot(roc_ROCR1, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)

pred2 <- prediction(train_hogares$y_hat_2, train_hogares$Pobre)
roc_ROCR2 <- performance(pred2,"tpr","fpr")
ROC_2 <- plot(roc_ROCR2, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)

pred3 <- prediction(train_hogares$y_hat_3, train_hogares$Pobre)
roc_ROCR3 <- performance(pred3,"tpr","fpr")
ROC_3 <- plot(roc_ROCR3, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)

pred4 <- prediction(train_hogares$y_hat_4, train_hogares$Pobre)
roc_ROCR4 <- performance(pred4,"tpr","fpr")
ROC_4 <- plot(roc_ROCR4, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)

pred5 <- prediction(train_hogares$y_hat_5, train_hogares$Pobre)
roc_ROCR5 <- performance(pred5,"tpr","fpr")
ROC_5 <- plot(roc_ROCR5, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)


par(mfrow=c(3,2))
ROC_1 <- plot(roc_ROCR1, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)
ROC_2 <- plot(roc_ROCR2, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)
ROC_3 <- plot(roc_ROCR3, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)
ROC_4 <- plot(roc_ROCR4, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)
ROC_5 <- plot(roc_ROCR5, main = "ROC curve", colorize = F)
abline(a = 0, b = 1)


##Area under the curve (AUC)

auc_ROCR1 <- performance(pred1, measure = "auc")
auc_ROCR1@y.values[[1]]

auc_ROCR2 <- performance(pred2, measure = "auc")
auc_ROCR2@y.values[[1]]

auc_ROCR3 <- performance(pred3, measure = "auc")
auc_ROCR3@y.values[[1]]

auc_ROCR4 <- performance(pred4, measure = "auc")
auc_ROCR4@y.values[[1]]

auc_ROCR5 <- performance(pred5, measure = "auc")
auc_ROCR5@y.values[[1]]



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
install.packages("plotly")
install.packages("tidymodels")
install.packages("fastDummies")

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
                             labels = c("No", "si"))

matriz_trainhog <- model.matrix(Pobre ~ .^2, data=train_hogares)[,-1]
## División del balance de clases para evitar overfitting
## Entrenamiento
set.seed(123)
split1 <- createDataPartition(train_hogares$Pobre, p = .7)[[1]]
length(split1)
## Prueba y evaluación
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
                           num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + factor(jh_ocup) +
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
                             num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + factor(jh_ocup) +
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
### Posiciòn 54 - Mejor lambda = 0.01402063

### Modelo lasso Roc
logit_lasso_ROC<- train(Pobre ~ factor(viviendapropia) + total_female + factor(female_jh) +
                      num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + factor(jh_ocup) +
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
                          num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + factor(jh_ocup) +
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


###Resultados de las métricas 
result_logitcv <- logit_caret_pob[["results"]]
colnames(result_logitcv)[1]<-"lambda"
result_lassoacc <- logit_lasso[["results"]][54,-1]
result_lassoroc<- logit_lasso_ROC[["results"]][54,-1]
result_lassosens<- logit_lasso_sens[["results"]][100,-1]
results<-rbind(result_logitcv,result_lassoacc,result_lassoroc, result_lassosens)
###El mejor modelo es el Modelo logit de acuerdo con el ROC

###Determinar el Cutoff
evaluation
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc <- predict(logit_caret_pob,
                           newdata = evaluation,
                           type = "prob")[,1]
library(pROC)
rfROC <- roc(evalResults$Pobre, evalResults$Roc, levels = rev(levels(evalResults$Pobre)))
rfROC
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh




###############################################################################
###############################################################################

####----Regression models ---####

install.packages("glmnet")
install.packages("corrr")
library(glmnet)
library(corrr)
library(pls)

##### Test data ##############


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

#Caracteristicas del jefe del hogar (P6050- Opción 1 es jefe del hogar - parentesco con jefe del hogar)
#genero_jef(P6020), ocupado (Oc), educación(P6210, P6210s1),desocupado (Des), 
#ingreso (Ingtot)

#Características de Individuo:
#Edad(P6040), educación(P6210, P6210s1), salud(P6090), ahorro(P7510s5), genero(P6020)
#desocupado (Des), ingreso (Ingtot)

#Caracteristicas del hogar:
#Vivienda propia (P5090), total personas en el hogar (), choques a salud(P6240, opción 5),
#choques ,ingreso (Ingtotugarr) 

female<-ifelse(test$P6020==2,1,0)
jh<-ifelse(test$P6050==1,1,0)
id<-test$id
DB_test<-data_frame(id,female,jh)

DB_test$female_jh<-ifelse(DB_test$jh==1,DB$female,0)
DB_test$ocu<-ifelse(is.na(test$Oc),0,1)
DB_test$edad<-test$P6040
DB_test$edad_jh<-ifelse(DB_test$jh==1,test$P6040,0)
DB_test$menores<-ifelse(DB_test$edad<18,1,0)
DB_test$max_educ_jh <-ifelse(DB_test$jh==1,test$P6210s1,0)
DB_test$jh_ocup <-ifelse(DB_test$jh==1,DB_test$ocu,0)
DB_test$afiliado<-ifelse(test$P6090!=1 | is.na(test$P6090),0,1)
DB_test$prod_finan_jh<-(ifelse(test$P7510s5!=1 | is.na(test$P7510s5),0,1))

DB_test$Estrato<-ifelse(DB_test$jh==1,test$Estrato1,0)


DB_test$Ingtot<- test$Ingtot
DB_test$Ingtot_jh<-ifelse(DB_test$jh==1,DB_test$Ingtot, 0)



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



test_hogares$viviendapropia <-as.factor(ifelse (test_hogares$P5090==1 | test_hogares$P5090==2,1,0))

###############################################################################
### ----Ingreso de los hogares---- #####

hist(train_hogares$Ingtotugarr)

train_hogares$Ingtotugarr<-scale(train_hogares$Ingtotugarr)

# Modelo 1
model1_ols<- lm(Ingtotugarr ~ viviendapropia + Nper + total_female + female_jh + num_ocu + 
                edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                num_afsalud + prod_finan_jh, data= train_hogares
                )
summary(model1_ols)

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train_modelo1_ols <- predict(model1_ols, newdata = train_hogares)

# MSE de entrenamiento
# ==============================================================================
training_mse_modelo1_ols <- mean((predicciones_train_modelo1_ols - train_hogares$Ingtotugarr)^2)
paste("Error (mse) de entrenamiento:", training_mse_modelo1_ols)


####
#TOCA HACER LAS PREDICCIONES CON TEST


###### -------  Ridge -----------############

# Matrices de entrenamiento y test
# ==============================================================================
x_train <- model.matrix(fat~., data = datos_train)[, -1]
y_train <- datos_train$fat

x_test <- model.matrix(fat~., data = datos_test)[, -1]
y_test <- datos_test$fat


