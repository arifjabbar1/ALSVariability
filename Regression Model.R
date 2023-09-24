#loading files
'%!in%' <- function(x,y)!('%in%'(x,y))
library(xgboost)
library(dplyr)
library(ROSE)
library(ggplot2)
library(seewave)
cv0<-read.csv("Var_12mo_12mo.csv")[,-1] #change to 12mos for 12-month case
databasecv0=read.csv("ALSdataimputedknn_GL_cv0.csv")[,-1]

#filtering data, splitting test and train
ALSdata=cv0%>%filter(is.na(LinCat)==FALSE)%>%filter(n_visits>2)%>%filter(n_pred_visits>2) #filter
for (i in 1:ncol(ALSdata)){ALSdata[,i]=as.numeric(ALSdata[,i])}

#train test split
subjectTrain=(unique(databasecv0%>%filter(train==1)%>%select(subject_id)))[,1]
subjectTest=(unique(databasecv0%>%filter(train==0)%>%select(subject_id)))[,1]
ALSTrain0=ALSdata%>%filter(subject_id%in%subjectTrain)
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

#setting up xgb.DMatrix, parameters
train_matrix_A<- xgb.DMatrix(data = as.matrix(ALSTrain0[,c(3:9,15:144,151:154)]), label = ALSTrain0$y.linearity)
test_matrix_A<- xgb.DMatrix(data = as.matrix(ALSTest0[,c(3:9,15:144,151:154)]), label = ALSTest0$y.linearity)
watchlist_A <- list(train=train_matrix_A,test=test_matrix_A)
xgb_params <- list("objective" = "reg:squarederror")

#model training, hyperparameters
x=0.3#eta
y=2#maxtreedept
z=40#nrounds
min=100
number=0
for (x in seq(0.1,1,0.1)){
  print(x)
  dense<-xgb.train(params=xgb_params,data=train_matrix_A,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_A)
  if(min(dense$evaluation_log[,3])<min){
    min=min(dense$evaluation_log[,3])
    number=x
  }
}
print(min)
print(number)
x=number
min=100
number=0
for (y in seq(1,10,1)){
  print(y)
  dense<-xgb.train(params=xgb_params,data=train_matrix_A,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_A)
  if(min(dense$evaluation_log[,3])<min){
    min=min(dense$evaluation_log[,3])
    number=y
  }
}
print(min)
print(number)
y=number
dense<-xgb.train(params=xgb_params,data=train_matrix_A,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_A)
min=100
for(i in 1:40){if(dense$evaluation_log[i,3]<min){
  z=i
  min=dense$evaluation_log[i,3]}}
dense<-xgb.train(params=xgb_params,data=train_matrix_A,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_A)

attr(test_matrix_A, "objective") <- "reg:squarederror"

#predicting on test set
prediction <- predict(dense,newdata=test_matrix_A)
comparison<-data.frame(ALSTest0$y.linearity,prediction) #dataframe consisting of originals and predictions

#plotting results
colnames(comparison)=c("True","Prediction")
ggplot(comparison)+
  geom_point(aes(x=True,y=Prediction))+xlim(0,4)+ylim(0,4)+geom_abline()
sqrt(mean((comparison$True-comparison$Prediction)^2))
#saving model
xgb.save(dense,"Regression12mo.model")

