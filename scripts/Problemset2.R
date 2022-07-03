# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 02-07-2022
###----------------- Project Set 2----------###

##### ---Limpiar Ambiente --- ######

rm(list = ls())

setwd("C:/Users/pau_9/Documents/Big Data/dataPS2")
##setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/ProblemSet2_Ramos_Uribe_Urquijo/stores")
getwd()

## Data Cleaning
## unzip("dataPS2.zip",  list =  T)

## train_hogares <- read.csv("data/train_hogares.csv", header=TRUE, sep="," )
## train_personas <- read.csv("data/train_personas.csv",header=TRUE, sep="," )
## test_hogares <- read.csv("data/test_hogares.csv",header=TRUE, sep="," )
## test_personas <- read.csv("data/test_personas.csv",header=TRUE, sep="," )

## colnames(train_hogares)
## colnames(train_personas)


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





train_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/submission_template.csv",header=TRUE, sep="," )
train_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/train_hogares.csv",header=TRUE, sep="," )
test_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/test_hogares.csv",header=TRUE, sep="," )
train_personas <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/train_personas.csv",header=TRUE, sep="," )
test_personas <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/test_personas.csv",header=TRUE, sep="," )


submission <- read.csv("C:/Users/pau_9/Documents/Big Data/dataPS2/data/submission_template.csv",header=TRUE, sep="," )
train_hogares <- read.csv("C:/Users/pau_9/Documents/Big Data/dataPS2/data/train_hogares.csv",header=TRUE, sep="," )
test_hogares <- read.csv("C:/Users/pau_9/Documents/Big Data/dataPS2/data/test_hogares.csv",header=TRUE, sep="," )
train_personas <- read.csv("C:/Users/pau_9/Documents/Big Data/dataPS2/data/train_personas.csv",header=TRUE, sep="," )
test_personas <- read.csv("C:/Users/pau_9/Documents/Big Data/dataPS2/data/test_personas.csv",header=TRUE, sep="," )

train<-merge(train_hogares,train_personas, by="id")
test<-merge(test_hogares,test_personas, by="id")

summary(train) 

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


### --- 2. a) Data Cleaning --- ###
### --- Missing Values

# Sacar cantidad de NAs por variable
cantidad_na <- sapply(train, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
cantidad_na<- data.frame(cantidad_na)%>%
  rownames_to_column("variable")
cantidad_na$porcentaje_na <- cantidad_na$cantidad_na/nrow(train)

# Eliminamos variables que no aportan
filtro <- cantidad_na$porcentaje_na > 0.8
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
female_jh<-ifelse(DB$jh==1,DB$female,0)
DB<-data_frame(id,female,jh,female_jh)

ocu<-ifelse(is.na(train$Oc),0,1)
edad<-train$P6040
DB<-data_frame(id,female,jh,female_jh,ocu,edad)
edad_jh<-ifelse(DB$jh==1,train$P6040,0)
DB<-data_frame(id,female,jh,female_jh,ocu,edad, edad_jh)
DB$menores<-ifelse(DB$edad<18,1,0)
DB$max_educ_jh <-ifelse(DB$jh==1,train$P6210s1,0)
DB$jh_ocup <-ifelse(DB$jh==1,ocu,0)
DB$afiliado<-ifelse(train$P6090!=1 | is.na(train$P6090),0,1)
DB$prod_finan_jh<-ifelse(train$P7510s5!=1 | is.na(train$P6090),0,1)
DB$Estrato<-ifelse(DB$jh==1,train$Estrato1,0)
levels(train$Oc)
levels(train$P6090)


#Mirar si na eran ceros
convert_number <- function(x){
  x <- as.character(x)
  x <- gsub(pattern = ",", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

DB$Ingtot<- convert_number(train$Ingtot)


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
                                              jh_prod_finan =sum(prod_finan_jh),
                                              jh_estrato=sum(Estrato)
                                              ) 


train_hogares <-merge(train_hogares,DB_2, by="id")

#Estadísticas descriptivas

train_hogares <- train_hogares %>%
  mutate_at(.vars = c("Clase", "Dominio","P5090", "Pobre", "Indigente",
                      "Depto", "female_jh", "jh_ocup", "jh_prod_finan", "jh_estrato" ),.funs = factor)
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


#Modelos para pedecir pobreza de los hogares

##Classification models
##Probabilidad de hogar pobre
train_hogares$Pobre <-as.factor(train_hogares$Pobre)
modelo1 <- glm( Pobre ~ P5090  + Nper + Ingtotugarr + total_female + female_jh +
                  num_ocu ,
               family=binomial(link="logit"),
               data= train_hogares
               )
summary(modelo1)

modelo1 <- glm( Pobre ~ P5090 + Nper + Ingtotugarr +  total_female + female_jh + 
                  num_ocu + edad_jh + menores + Ingtot_jh + max_educ_jh + 
                  jh_ocup + num_afsalud + jh_prod_finan,
                family=binomial(link="logit"),
                data= train_hogares
)
summary(modelo1)

##Ingreso de los hogares
modelo2<- lm(Ingtotugarr ~ P5090 + Nper + total_female + female_jh + num_ocu + 
                edad_jh + menores + Ingtot_jh + max_educ_jh + jh_ocup +
                num_afsalud + jh_prod_finan, data= train_hogares
                )
summary(modelo2)

stargazer(modelo1, modelo2)


