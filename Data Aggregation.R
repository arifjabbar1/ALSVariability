#This file is used to aggregate data for observation and prediction windows
library(dplyr)
library(seewave)
ALSdataImputed=read.csv("ALSdataimputedknn_GL_cv0.csv")[,-1]
AE=read.csv("AdverseEvents.csv")#Adverse Event List

#adjust these for different values
pre=365#observation period (6mo=183, 9mo=273, 12mo=365, Tot=10000)
post=365#prediction period (6mo=183, 9mo=273, 12mo=365, Tot=0)
threshold=1.27#threshold of ALSFRS-R Linearity (3mo=0.554919, 6mo=0.831, 9mo=1.09, 12mo=1.27, Tot=1.49)

dfy=data.frame(matrix(NA,0,150))
patientlist=unique(ALSdataImputed$subject_id)

for (i in 1:length(patientlist)){
  if(i%%1000==1){print(i)}
  dfx1=ALSdataImputed%>%filter(subject_id==patientlist[i])%>%filter(is.na(alsfrsr)==FALSE)
  for (m in 1:ncol(dfx1)){dfx1[,m]=as.numeric(dfx1[,m])}
  maxdelta=max(dfx1$delta)-pre-post
  dfx2=dfx1%>%filter(delta<=maxdelta)
  jmax=nrow(dfx2)+1
  for(j in 1:jmax){
    n=nrow(dfy)+1
    dfx=dfx1%>%filter(delta<dfx1$delta[j]+pre)%>%filter(dfx1$delta[j]<=delta)
    if(nrow(dfx)>2){
      for(k in 1:10){
        dfy[n,k]=mean(dfx[,k], na.rm=TRUE)
      }
      dfy[n,11]=j#number for that patient
      dfy[n,12]=nrow(dfx)#pre_number
      dfy[n,13]=dfx$delta[1]#delta start
      dfy[n,14]=dfx$delta[nrow(dfx)]#delta end
      for(k in 11:52){
        x=lm(dfx[,k]~dfx$delta)
        dfy[n,k*3-18]=mean(dfx[,k], na.rm=TRUE)
        dfy[n,k*3-17]=as.numeric(x$coefficients[2])
        dfy[n,k*3-16]=sd(dfx[,k], na.rm=TRUE)
      }
      for(k in 53:53){
        x=lm(dfx[,k]~dfx$delta)
        dfy[n,k*3-18]=mean(dfx[,k], na.rm=TRUE)
        dfy[n,k*3-17]=as.numeric(x$coefficients[2])*30
        dfy[n,k*3-16]=sd(dfx[,k], na.rm=TRUE)
        dfy[n,k*3-15]=rms(x$residuals)
      }
      if(post==0){if(dfy[n,144]<threshold){dfy[n,150]=0}else{dfy[n,150]=1}}
      dfx=dfx1%>%filter(delta<dfx1$delta[j]+pre+post)%>%filter(dfx1$delta[j]+pre<=delta)
      if(nrow(dfx)>2){
        for(k in 53:53){
          x=lm(dfx[,k]~dfx$delta)
          dfy[n,k*3-14]=mean(dfx[,k], na.rm=TRUE)
          dfy[n,k*3-13]=as.numeric(x$coefficients[2])*30
          dfy[n,k*3-12]=sd(dfx[,k], na.rm=TRUE)
          dfy[n,k*3-11]=rms(x$residuals)
          dfy[n,k*3-10]=nrow(dfx)#
          m1=dfy[n,k*3-11]
          if(post>0){if(m1<threshold){m=0}else{m=1}}
          dfy[n,k*3-9]=m
        }
        
      }
    }
    
  }
}
colnames(dfy)[1:10]=colnames(ALSdataImputed)[1:10]
colnames(dfy)[11:14]=c("n","n_visits","firstvisit","lastvisit")
colnames(dfy)[144:150]=c("alsfrsr.rmse","y.mean","y.change","y.SD","y.linearity","n_pred_visits","LinCat")
for (i in 11:53){
  colnames(dfy)[i*3-18]=paste(colnames(ALSdataImputed)[i],"mean",sep=".")
  colnames(dfy)[i*3-17]=paste(colnames(ALSdataImputed)[i],"change",sep=".")
  colnames(dfy)[i*3-16]=paste(colnames(ALSdataImputed)[i],"SD",sep=".")
}

for (i in 1:nrow(dfy)){
  row.names(dfy)[i]=paste(as.character(dfy$subject_id[i]),"/",as.character(dfy$n[i]))
}

#inputing adverse events
dfy$`Adv_Total`=0
dfy$`Adv_Nerv`=0
dfy$`Adv_Resp`=0
dfy$`Adv_Met`=0

for(i in 1:nrow(dfy)){
  if(i%%1000==1){print(i)}
  subj=dfy$subject_id[i]
  startdate=dfy$firstvisit[i]
  enddate=dfy$lastvisit[i]
  dfAE=AE%>%filter(subject_id==subj)%>%filter(Start_Date_Delta>=startdate)%>%filter(Start_Date_Delta<=enddate)
  dfy$`Adv_Total`[i]=nrow(dfAE)
  dfy$Adv_Met[i]=nrow(dfAE%>%filter(SOC_Abbreviation=="Metab"))
  dfy$Adv_Nerv[i]=nrow(dfAE%>%filter(SOC_Abbreviation=="Nerv"))
  dfy$Adv_Resp[i]=nrow(dfAE%>%filter(SOC_Abbreviation=="Resp"))
}

#saving file
write.csv(dfy,"Var_12mo_12mo.csv") #renaming data frame
