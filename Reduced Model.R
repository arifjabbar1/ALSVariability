cv0=read.csv("SingleTimePoints.csv")[,-1]
Li_Var_0mo_3mo_1=read.csv("Li_Var_0mo_3mo.csv")[,-1 ]
cv0reduced=cv0%>%select(colnames(Li_Var_0mo_3mo_1[,c(1:16,21:23)]),train)

#loading files, splitting train and test
ALSdata=cv0reduced%>%filter(is.na(var_3mo)==FALSE)%>%filter(n_3mo>2)
for (i in 1:ncol(ALSdata)){ALSdata[,i]=as.numeric(ALSdata[,i])}
subjectTrain=(unique(ALSdata%>%filter(train==1)%>%select(subject_id)))[,1]
subjectTest=(unique(ALSdata%>%filter(train==0)%>%select(subject_id)))[,1]
'%!in%' <- function(x,y)!('%in%'(x,y))
ALSTrain0=ALSdata%>%filter(subject_id%in%subjectTrain)
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

n=max(sum(ALSTrain0$`MoreVar_3mo`==1,na.rm=TRUE),sum(ALSTrain0$`MoreVar_3mo`==0,na.rm=TRUE))
ALSTrain=ALSTrain0

ALSTrain<-ovun.sample(`MoreVar_3mo` ~ ., data=ALSTrain0, method="over",N=2*n)$data
ALSTest0=ALSdata%>%filter(subject_id%in%subjectTest)

n1=max(sum(ALSTest0$`MoreVar_3mo`==1,na.rm=TRUE),sum(ALSTest0$`MoreVar_3mo`==0,na.rm=TRUE))
ALSTest=ALSTest0

ALSTest<-ovun.sample(`MoreVar_3mo`~ ., data=ALSTest0, method="over",N=2*n1)$data

train_label_B <- ALSTrain$`MoreVar_3mo`
test_label_B <- ALSTest$`MoreVar_3mo`
test0_label_B <- ALSTest0$`MoreVar_3mo`


#reduced set
train_matrix_B<- xgb.DMatrix(data = as.matrix(ALSTrain[,c(3:16)]), label = train_label_B)
test_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest[,c(3:16)]), label = test_label_B)
test0_matrix_B <- xgb.DMatrix(data = as.matrix(ALSTest0[,c(3:16)]), label = test0_label_B)
li_matrix_B<-xgb.DMatrix(data = as.matrix(Li_Var_0mo_3mo_1[,c(3:16)]), label = Li_Var_0mo_3mo_1$MoreVar_3mo)

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

prediction0 <- predict(dense,newdata=test0_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction0), by = 2)
predictions0 <- prediction0[even_indices]>thresh
for(i in 1:length(predictions0)){
  if(predictions0[i]==TRUE){predictions0[i]=1}
  if(predictions0[i]==FALSE){predictions0[i]=0}
}
df<-table(predictions0,test0_label_B)
sum(predictions0==test0_label_B)/length(test0_label_B)
df[2,2]/(df[2,2]+df[2,1])
df[1,1]/(df[1,1]+df[1,2])
xgb.save(dense,"STP3moReduced.model")
#lithium
prediction0 <- predict(dense,newdata=li_matrix_B,type="prob")
even_indices <- seq(from = 2, to = length(prediction0), by = 2)
predictions0 <- prediction0[even_indices]>thresh
for(i in 1:length(predictions0)){
  if(predictions0[i]==TRUE){predictions0[i]=1}
  if(predictions0[i]==FALSE){predictions0[i]=0}
}

df<-table(predictions0,Li_Var_0mo_3mo_1$MoreVar_3mo)
sum(predictions0==Li_Var_0mo_3mo_1$MoreVar_3mo)/length(Li_Var_0mo_3mo_1$MoreVar_3mo)
df[2,2]/(df[2,2]+df[2,1])
df[1,1]/(df[1,1]+df[1,2])
