library(adabag);
library("ff")
library("readr")
library("dplyr")
library("ggplot2")
library("scales")
library("treemap")
library("data.table")
library("tcltk2")
library("randomForest")
library("xgboost")
library("Matrix")
library("devtools")
rm(list=ls())
setwd("/home/gse/bimbo")

client <- read.csv("cliente_tabla.csv")
product <- read.csv("product.csv")
town <- read.csv("town_state.csv")
test <- read.csv("test.csv")

product = as.data.table(product)


train=fread('train.csv',select = c("Semana",'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'))
test=fread('test.csv',select = c("Semana",'id','Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))



train$id=0
train[,target:=Demanda_uni_equil]
train[,Demanda_uni_equil:=NULL]
train[,tst:=0]
test$target=0
test[,tst:=1]
data=rbind(train,test)
rm(test)  
rm(train)

data1<-data[,.(Semana=Semana+1,Cliente_ID,Producto_ID,target)]
data=merge(data,data1[Semana>8,.(targetl1=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))

 data1<-data[,.(Semana=Semana+2,Cliente_ID,Producto_ID,target)]
  data=merge(data,data1[Semana>8,.(targetl2=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
  data1<-data[,.(Semana=Semana+3,Cliente_ID,Producto_ID,target)]
  data=merge(data,data1[Semana>8,.(targetl3=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
  data1<-data[,.(Semana=Semana+4,Cliente_ID,Producto_ID,target)]
  data=merge(data,data1[Semana>8,.(targetl4=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
  data1<-data[,.(Semana=Semana+5,Cliente_ID,Producto_ID,target)]
  data=merge(data,data1[Semana>8,.(targetl5=mean(target)), by=.(Semana,Cliente_ID,Producto_ID)],all.x=T, by=c("Semana","Cliente_ID","Producto_ID"))
  
  
  
  
  
  
rm(data1)
data=data[Semana>8,]

# Creating frequency features for some factor variables
nAgencia_ID=data[,.(nAgencia_ID=.N),by=.(Agencia_ID,Semana)]
nAgencia_ID=nAgencia_ID[,.(nAgencia_ID=mean(nAgencia_ID,na.rm=T)),by=Agencia_ID]
data=merge(data,nAgencia_ID,by='Agencia_ID',all.x=T)
nRuta_SAK=data[,.(nRuta_SAK=.N),by=.(Ruta_SAK,Semana)]
nRuta_SAK=nRuta_SAK[,.(nRuta_SAK=mean(nRuta_SAK,na.rm=T)),by=Ruta_SAK]
data=merge(data,nRuta_SAK,by='Ruta_SAK',all.x=T)
nCliente_ID=data[,.(nCliente_ID=.N),by=.(Cliente_ID,Semana)]
nCliente_ID=nCliente_ID[,.(nCliente_ID=mean(nCliente_ID,na.rm=T)),by=Cliente_ID]
data=merge(data,nCliente_ID,by='Cliente_ID',all.x=T)
nProducto_ID=data[,.(nProducto_ID=.N),by=.(Producto_ID,Semana)]
nProducto_ID=nProducto_ID[,.(nProducto_ID=mean(nProducto_ID,na.rm=T)),by=Producto_ID]
data=merge(data,nProducto_ID,by='Producto_ID',all.x=T)
data$target=log(data$target+1)
data_train=data[tst==0,]
data_test=data[tst==1,]

features=names(data_train)[!(names(data_train) %in% c('id',"target",'tst'))] 

rm(data)



adaboost<-boosting(y~features, data=data_train, boos=TRUE, mfinal=20,coeflearn='Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,adadata)
predict(adaboost,adadata)
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)


results=data.frame(id=data_test1$id,Demanda_uni_equil=res)
