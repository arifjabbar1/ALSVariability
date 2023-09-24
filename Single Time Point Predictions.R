#loading files
library(dplyr)
library(xgboost)
library(ROSE)
'%!in%' <- function(x,y)!('%in%'(x,y))
cv0=read.csv("SingleTimePoints.csv")[,-1]

#Find and Replace 6mo by 3,6,9,12 mo respectively,
#filtering sufficient visits, test & train split
ALSdata=cv0%>%filter(is.na(var_6mo)==FALSE)%>%filter(n_6mo>2)
for (i in 1:ncol(ALSdata)){ALSdata[,i]=as.numeric(ALSdata[,i])}
subjectTrain=(unique(ALSdata%>%filter(train==1)%>%select(subject_id)))[,1]
subjectTest=(unique(ALSdata%>%filter(train==0)%>%select(subject_id)))[,1]
ALSTrain0=ALSdata%>%filter(subject_id%in%subjectTrain)
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

#rebalancing classes
n=max(sum(ALSTrain0$`MoreVar_6mo`==1,na.rm=TRUE),sum(ALSTrain0$`MoreVar_6mo`==0,na.rm=TRUE))
ALSTrain=ALSTrain0
ALSTrain<-ovun.sample(`MoreVar_6mo` ~ ., data=ALSTrain0, method="over",N=2*n)$data
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)
n1=max(sum(ALSTest0$`MoreVar_6mo`==1,na.rm=TRUE),sum(ALSTest0$`MoreVar_6mo`==0,na.rm=TRUE))
ALSTest=ALSTest0
ALSTest<-ovun.sample(`MoreVar_6mo`~ ., data=ALSTest0, method="over",N=2*n1)$data

#setting up xgb.DMatrix for inputs
train_label_B <- ALSTrain$`MoreVar_6mo`
test_label_B <- ALSTest$`MoreVar_6mo`
test0_label_B <- ALSTest0$`MoreVar_6mo`
train_matrix_B<- xgb.DMatrix(data = as.matrix(ALSTrain[,c(3:9,12:53)]), label = train_label_B)
test_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest[,c(3:9,12:53)]), label = test_label_B)
test0_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest0[,c(3:9,12:53)]), label = test0_label_B)

#training models, tuning hyperparameters
watchlist_B <- list(train=train_matrix_B,test=test_matrix_B)
xgb_params <- list("objective" = "multi:softprob", eval.metric = "merror",num_class=2)
x=0.3#eta
y=2#maxtreedept
z=40#nrounds
min=100
number=0
for (x in seq(0.1,1,0.1)){
  print(x)
  dense<-xgb.train(params=xgb_params,data=train_matrix_B,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_B)
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
  dense<-xgb.train(params=xgb_params,data=train_matrix_B,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_B)
  if(min(dense$evaluation_log[,3])<min){
    min=min(dense$evaluation_log[,3])
    number=y
  }
}
print(min)
print(number)
y=number
dense<-xgb.train(params=xgb_params,data=train_matrix_B,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_B)
min=100
for(i in 1:40){if(dense$evaluation_log[i,3]<min){
  z=i
  min=dense$evaluation_log[i,3]}}
dense<-xgb.train(params=xgb_params,data=train_matrix_B,max.depth=y,nrounds=z,nthread=8,eta=x,watchlist=watchlist_B)

#adjusting threshold 
prediction <- predict(dense,newdata=test_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction), by = 2)
even_predictions <- prediction[even_indices]
predictions<-data.frame(even_predictions,matrix(NA,length(even_predictions),100))
for(j in 2:101){
  for(i in 1:nrow(predictions)){
    if(predictions[i,1]>(j-1)*0.01){predictions[i,j]=1}
    if(predictions[i,1]<=(j-1)*0.01){predictions[i,j]=0}
  }
}
x=0
thresh=0
for(i in 2:101){
  if(sum(predictions[,i]==test_label_B)>x){x=sum(predictions[,i]==test_label_B);thresh=(i-1)*0.01}
}
colname=thresh*100+1

df<-table(predictions[,colname],test_label_B)

#predicting on test set
prediction0 <- predict(dense,newdata=test0_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction0), by = 2)
predictions0 <- prediction0[even_indices]>thresh
for(i in 1:length(predictions0)){
  if(predictions0[i]==TRUE){predictions0[i]=1}
  if(predictions0[i]==FALSE){predictions0[i]=0}
}

df<-table(test0_label_B,predictions0)#confusion matrix

(df[1,1]+df[2,2])/sum(df)#accuracy
df[2,2]/sum(df[,2])#sensitivity
df[1,1]/sum(df[,1])#specificity

print(df)
xgb.save(dense,"STP6mo.model")

