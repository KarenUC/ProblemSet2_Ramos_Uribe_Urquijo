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
p_load(EnvStats) # Transformaci?n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(skimr, # summary data
       caret, # Classification And REgression Training
       rvest,
       stringr,
       dplyr,
       RSelenium,
       Rcpp,
       robotstxt,
       sjPlot)


#########################################################################################
#Cargar los datos

#test_hogares<-import("https://github.com/KarenUC/ProblemSet2_Ramos_Uribe_Urquijo/tree/main/data/test_hogares.Rds")

setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/ProblemSet2_Ramos_Uribe_Urquijo/")
unzip("dataPS2RDS.zip",  list =  T)
train_hogares<-readRDS("data/train_hogares.Rds")
train_personas<-readRDS("data/train_personas.Rds")
test_hogares<-readRDS("data/test_hogares.Rds")
test_personas<-readRDS("data/test_personas.Rds")

#########################################################################################

train<-merge(train_hogares,train_personas, by="id")
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
DB$prod_finan_jh<-as.factor(ifelse(train$P7510s5!=1 | is.na(train$P7510s5),0,1))
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
                                              prod_finan_jh = prod_finan_jh,
                                              jh_estrato=sum(Estrato)
                                              ) 


train_hogares <-merge(train_hogares,DB_2, by="id")


#Estadísticas descriptivas

train_hogares <- train_hogares %>%
  mutate_at(.vars = c("Clase", "Dominio","P5090", "Pobre", "Indigente",
                      "Depto", "female_jh", "jh_ocup", "prod_finan_jh", "jh_estrato" ),.funs = factor)
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


stargazer(train_hogares[c("Nper", "Ingtotugarr", "total_female", "num_ocu", "menores", "Ingtot_jh", "max_educ_jh", "num_afsalud" )], type = "text")

############### ------Modelos para pedecir pobreza de los hogares---------############################

##Classification models
##Probabilidad de hogar pobre
train_hogares$Pobre <-as.factor(train_hogares$Pobre)
##Creación de variable Vivienda Propia
train_hogares$viviendapropia <-as.factor(ifelse (train_hogares$P5090==1 | train_hogares$P5090==2,1,0))

model_log_1 <- glm( Pobre ~ viviendapropia + Nper + Ingtotugarr + total_female + female_jh +
                  num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                  num_afsalud + prod_finan_jh,
               family=binomial(link="logit"),
               data= train_hogares
               )
summary(model_log_1)






##Ingreso de los hogares
model1<- lm(Ingtotugarr ~ P5090 + Nper + total_female + female_jh + num_ocu + 
                edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                num_afsalud + jh_prod_finan, data= train_hogares
                )
summary(model1)


forward <- train(Ingtotugarr ~ P5090 + Nper + total_female + female_jh + num_ocu + 
                   edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                   num_afsalud + jh_prod_finan, data = train_hogares,
                 method = "leapForward",
                 trControl = trainControl(method = "cv", number = 10))



