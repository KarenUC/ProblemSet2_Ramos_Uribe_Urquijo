# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 04-07-2022
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
setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet2_Ramos_Uribe_Urquijo/")
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


stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu", "menores", "Ingtot_jh", "max_educ_jh", "num_afsalud" )], type = "text")

### Gr醘icas estadisticas descriptivas ###

box_plot <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , Ingtotugarr)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y ="Ingresos Totales Hogar") 

pobre_bar <- ggplot(data = train_hogares, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (x= "Pobre =1", y = "N鷐ero de Pobres")


############### ------Modelos para pedecir pobreza de los hogares---------############################

####----Classification models

##Probabilidad de hogar pobre
train_hogares$Pobre <-as.factor(train_hogares$Pobre)


##Creacion de variable Vivienda Propia
train_hogares$viviendapropia <-as.factor(ifelse (train_hogares$P5090==1 | train_hogares$P5090==2,1,0))

class(train_hogares$Pobre)



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



#Profe
model_log_profe <- glm(Pobre ~  P5130,
                       family=binomial(link="logit"),
                       data= train_hogares)
###Prediccion
train_hogares$y_hat_profe <- predict(model_log_profe , newdata=train_hogares , type="response")
summary(train_hogares$y_hat_profe )



####----Regression models ---####

install.packages("glmnet")
install.packages("corrr")
library(glmnet)
library(corrr)
library(pls)


### ----Ingreso de los hogares

hist(train_hogares$Ingtotugarr)

df_correlaciones <- train_hogares %>%
  correlate(method = "pearson") %>%
  stretch(remove.dups = TRUE)



# Modelo 1
model1<- lm(Ingtotugarr ~ viviendapropia + Nper + total_female + female_jh + num_ocu + 
                edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                num_afsalud + prod_finan_jh, data= train_hogares
                )
summary(model1)


# Regularizacion



forward <- train(Ingtotugarr ~ P5090 + Nper + total_female + female_jh + num_ocu + 
                   edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                   num_afsalud + jh_prod_finan, data = train_hogares,
                 method = "leapForward",
                 trControl = trainControl(method = "cv", number = 10))


class(train_hogares$Pobre)
levels(train_hogares$Pobre)
train_hogares$Pobre <- as.factor(train_hogares$Pobre)
