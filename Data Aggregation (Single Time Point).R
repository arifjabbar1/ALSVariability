#generating 'Single Time Point Datafiles'
library(dplyr)

# Generating Files for Single Time Point ----------------------------------
#single file contains information for 3mo, 6mo, 9mo, 12mo
#loading files, input is 'ALSdataimputedknn_GL_cv0.csv'
cv0<-read.csv("ALSdataimputedknn_GL_cv0.csv")[,-1]

#generating new columns
cv0$`n_3mo`=NA
cv0$`var_3mo`=NA
cv0$`MoreVar_3mo`=NA
cv0$`n_6mo`=NA
cv0$`var_6mo`=NA
cv0$`MoreVar_6mo`=NA
cv0$`n_9mo`=NA
cv0$`var_9mo`=NA
cv0$`MoreVar_9mo`=NA
cv0$`n_12mo`=NA
cv0$`var_12mo`=NA
cv0$`MoreVar_12mo`=NA

#filling in columns
for(i in 1:nrow(cv0)){
  if(i%%1000==1){print(i)}#progress tracker
  subject=cv0$subject_id[i]
  date=cv0$delta[i]
  
  #filtering scores for a particular subject
  scoresfull=cv0%>%
    filter(subject_id==subject)%>%
    select(delta,alsfrsr)%>%
    filter(is.na(alsfrsr)==FALSE)
  
  #calculating number of visits, variability for each window
  lin_3mo=scoresfull%>%filter(delta>=date)%>%filter(delta<date+91)
  if(nrow(lin_3mo>2)){model_3mo=lm(lin_3mo$alsfrsr~lin_3mo$delta)
  cv0$`n_3mo`[i]=nrow(lin_3mo)
  cv0$`var_3mo`[i]=sqrt(mean(as.vector(model_3mo$residuals)^2))}
  
  #calculating number of visits, variability for each window
  lin_6mo=scoresfull%>%filter(delta>=date)%>%filter(delta<date+183)
  if(nrow(lin_6mo>2)){model_6mo=lm(lin_6mo$alsfrsr~lin_6mo$delta)
  cv0$`n_6mo`[i]=nrow(lin_6mo)
  cv0$`var_6mo`[i]=sqrt(mean(as.vector(model_6mo$residuals)^2))}
  
  #calculating number of visits, variability for each window
  lin_9mo=scoresfull%>%filter(delta>=date)%>%filter(delta<date+273)
  if(nrow(lin_9mo>2)){model_9mo=lm(lin_9mo$alsfrsr~lin_9mo$delta)
  cv0$`n_9mo`[i]=nrow(lin_9mo)
  cv0$`var_9mo`[i]=sqrt(mean(as.vector(model_9mo$residuals)^2))}
  
  #calculating number of visits, variability for each window
  lin_12mo=scoresfull%>%filter(delta>=date)%>%filter(delta<date+365)
  if(nrow(lin_12mo>2)){model_12mo=lm(lin_12mo$alsfrsr~lin_12mo$delta)
  cv0$`n_12mo`[i]=nrow(lin_12mo)
  cv0$`var_12mo`[i]=sqrt(mean(as.vector(model_12mo$residuals)^2))}
  
}

for(i in 1:nrow(cv0)){
  if(i%%1000==1){print(i)}#progress tracker
  
  #assigning classes
  if(is.na(cv0$`var_3mo`[i])==FALSE){
    if(cv0$`var_3mo`[i]<0.554919){cv0$`MoreVar_3mo`[i]=0}
    if(cv0$`var_3mo`[i]>=0.554919){cv0$`MoreVar_3mo`[i]=1}
  }
  if(is.na(cv0$`var_6mo`[i])==FALSE){
    if(cv0$`var_6mo`[i]<0.831){cv0$`MoreVar_6mo`[i]=0}
    if(cv0$`var_6mo`[i]>=0.831){cv0$`MoreVar_6mo`[i]=1} 
  }
  if(is.na(cv0$`var_9mo`[i])==FALSE){
    if(cv0$`var_9mo`[i]<1.09){cv0$`MoreVar_9mo`[i]=0}
    if(cv0$`var_9mo`[i]>=1.09){cv0$`MoreVar_9mo`[i]=1}   
  }
  if(is.na(cv0$`var_12mo`[i])==FALSE){
    if(cv0$`var_12mo`[i]<1.27){cv0$`MoreVar_12mo`[i]=0}
    if(cv0$`var_12mo`[i]>=1.27){cv0$`MoreVar_12mo`[i]=1}
  }
}

#saving datafile
write.csv(cv0,"SingleTimePoints.csv")

