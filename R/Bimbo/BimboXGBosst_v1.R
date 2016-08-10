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
product <- read_csv("product.csv")
town <- read.csv("town_state.csv")
test <- read.csv("test.csv")

product = as.data.table(product)


train=fread('train.csv',select = c("Semana",'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'))
test=fread('test.csv',select = c("Semana",'id','Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))

==============================================================================

train = as.data.table(train)

setkey(train, Producto_ID)
setkey(product, Producto_ID)

train_product <- sqldf("select * from train t INNER JOIN product p on t.Producto_ID =  p.Producto_ID")

train<- read.csv.ffdf(file="train.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000000, next.rows=1000000, colClasses=NA)

==============================================================================

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

wltst=sample(nrow(data_train),30000)  

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                  label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist<-list(dval=dval)

clf <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.85,
                               colsample_bytree=0.7) ,
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                    label=data.matrix(data_train[-wltst,target]),missing=NA), 
                 nrounds = 75, 
                 verbose = 1,
                 print_every_n=5,
                 early_stopping_rounds    = 10,
                 watchlist           = watchlist,
                 maximize            = FALSE,
                 eval_metric='rmse'
)

# Make prediction for the 10th week
data_test1=data_test[Semana==10,]
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1

# Create lagged values of target variable which will be used as a feature for the 11th week prediction 
data_test_lag1=data_test1[,.(Cliente_ID,Producto_ID)]
data_test_lag1$targetl1=res
data_test_lag1=data_test_lag1[,.(targetl1=mean(targetl1)), by=.(Cliente_ID,Producto_ID)]

results=data.frame(id=data_test1$id,Demanda_uni_equil=res)

data_test2=data_test[Semana==11,]
data_test2[,targetl1:=NULL]

# Merge lagged values of target variable to test the set for the 11th week
data_test2=merge(data_test2,data_test_lag1,all.x=T,by=c('Cliente_ID','Producto_ID'))
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test2[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$id,Demanda_uni_equil=res)
results=rbind(results, res.df)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='results1.csv',row.names=F)