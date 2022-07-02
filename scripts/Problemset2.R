# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 02-07-2022
###----------------- Project Set 1----------###

##### ---Limpiar Ambiente --- ######

rm(list = ls())

train_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/submission_template.csv",header=TRUE, sep="," )
train_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/train_hogares.csv",header=TRUE, sep="," )
test_hogares <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/test_hogares.csv",header=TRUE, sep="," )
train_personas <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/train_personas.csv",header=TRUE, sep="," )
test_personas <- read.csv("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Problem set 2/data/test_personas.csv",header=TRUE, sep="," )

train<-merge(train_hogares,train_personas, by="id")
test<-merge(test_hogares,test_personas, by="id")
 