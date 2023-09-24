#importingdata
library(xgboost)
library(dplyr)
library(ROSE)
'%!in%' <- function(x,y)!('%in%'(x,y))
cv0=read.csv("Var_Tot.csv")[,-1] 
databasecv0=read.csv("ALSdataimputedknn_GL_cv0.csv")[,-1]

ALSdata=cv0%>%filter(is.na(LinCat)==FALSE)%>%filter(n_visits>2) #filter
for (i in 1:ncol(ALSdata)){ALSdata[,i]=as.numeric(ALSdata[,i])} #convert to numeric

#train test split
subjectTrain=(unique(databasecv0%>%filter(train==1)%>%select(subject_id)))[,1]
subjectTest=(unique(databasecv0%>%filter(train==0)%>%select(subject_id)))[,1]
ALSTrain=ALSdata%>%filter(subject_id%in%subjectTrain)
ALSTest=ALSdata%>%filter(subject_id%in%subjectTest)

train_label_B <- ALSTrain$`LinCat`
test_label_B <- ALSTest$`LinCat`

#generating datasets (required for xgboost package in R)
train_matrix_B<- xgb.DMatrix(data = as.matrix(ALSTrain[,c(3:9,15:142,151:154)]), label = train_label_B)#edit the numbers based on which columns are used for input
test_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest[,c(3:9,15:142,151:154)]), label = test_label_B)

watchlist_B <- list(train=train_matrix_B,test=test_matrix_B)

xgb_params <- list("objective" = "multi:softprob", eval.metric = "merror",num_class=2)

#optimising hyperparameters
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

#optimising threshold for deciding more/less variable
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

#calculating data on test sample
prediction <- predict(dense,newdata=test_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction), by = 2)
predictions <- prediction[even_indices]>thresh
for(i in 1:length(predictions)){
  if(predictions[i]==TRUE){predictions[i]=1}
  if(predictions[i]==FALSE){predictions[i]=0}
}

#results
df<-table(predictions,test_label_B)#confusion matrix
(df[1,1]+df[2,2])/sum(df)#accuracy
df[2,2]/sum(df[,2])#sensitivity
df[1,1]/sum(df[,1])#specificity
print(df)

#saving model
xgb.save(dense,"OWTot.model")

#importance matrix
importance=importance_matrix <- xgb.importance(model = dense)[,c(1,2)]#importance matrix

