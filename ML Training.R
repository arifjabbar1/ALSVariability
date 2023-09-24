#importingdata
library(xgboost)
'%!in%' <- function(x,y)!('%in%'(x,y))
cv0=read.csv("Var_6mo_6mo.csv")[,-1] #read processed data files - pick 6mo, 9mo or 12mo
databasecv0=read.csv("ALSdataimputedknn_GL_cv0.csv")[,-1]

ALSdata=cv0%>%filter(is.na(LinCat)==FALSE)%>%filter(n_visits>2)%>%filter(n_pred_visits>2) #filter
for (i in 1:ncol(ALSdata)){ALSdata[,i]=as.numeric(ALSdata[,i])} #convert to numeric

#train test split
subjectTrain=(unique(databasecv0%>%filter(train==1)%>%select(subject_id)))[,1]
subjectTest=(unique(databasecv0%>%filter(train==0)%>%select(subject_id)))[,1]
ALSTrain0=ALSdata%>%filter(subject_id%in%subjectTrain)
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

#rebalancing
n=max(sum(ALSTrain0$`LinCat`==1,na.rm=TRUE),sum(ALSTrain0$`LinCat`==0,na.rm=TRUE))
ALSTrain=ALSTrain0

ALSTrain<-ovun.sample(`LinCat` ~ ., data=ALSTrain0, method="over",N=2*n)$data
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

n1=max(sum(ALSTest0$`LinCat`==1,na.rm=TRUE),sum(ALSTest0$`LinCat`==0,na.rm=TRUE))
ALSTest=ALSTest0

ALSTest<-ovun.sample(`LinCat`~ ., data=ALSTest0, method="over",N=2*n1)$data

train_label_B <- ALSTrain$`LinCat`
test_label_B <- ALSTest$`LinCat`
test0_label_B <- ALSTest0$`LinCat`

#generating datasets (required for xgboost package in R)
train_matrix_B<- xgb.DMatrix(data = as.matrix(ALSTrain[,c(3:9,15:144,151:154)]), label = train_label_B)#edit the numbers based on which columns are used for input
test_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest[,c(3:9,15:144,151:154)]), label = test_label_B)
test0_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest0[,c(3:9,15:144,151:154)]), label = test0_label_B)

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
prediction0 <- predict(dense,newdata=test0_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction0), by = 2)
predictions0 <- prediction0[even_indices]>thresh
for(i in 1:length(predictions0)){
  if(predictions0[i]==TRUE){predictions0[i]=1}
  if(predictions0[i]==FALSE){predictions0[i]=0}
}

#results
df<-table(predictions0,test0_label_B)#confusion matrix
(df[1,1]+df[2,2])/sum(df)#accuracy
df[2,2]/sum(df[,2])#sensitivity
df[1,1]/sum(df[,1])#specificity
print(df)

#saving model
xgb.save(dense,"OW6mo.model")

#importance matrix
importance=importance_matrix <- xgb.importance(model = dense)[,c(1,2)]#importance matrix

