library(survRM2)
data_recover<-read.csv("recover_final2.csv",header=T)
data_recover<-data_recover[complete.cases(data_recover),]
time_recover<-data_recover$month
status_recover<-data_recover$death
arm_recover<-data_recover$Recover_or_not
#X<-data_recover[,c("Sex","Race","Admit_Age","LOS")]
#X$Sex<-factor(X$Sex)
#X$Race<-factor(X$Race)
#unadjusted not required
#mod<-rmst2(time_recover,status_recover,arm_recover)
#Adjusted analysis
#rmst2(time_recover,status_recover,arm_recover,covariates= X$LOS)

#sub_decline<-subset(data_decline,subset=as.logical(Decline_or_not))
#sub_not_decline<- subset(data_decline,select=Decline_or_not==0)

race_num1<-numeric(nrow(data_recover))
race_num2<-numeric(nrow(data_recover))
race_num3<-numeric(nrow(data_recover))
race_num4<-numeric(nrow(data_recover))
race_num5<-numeric(nrow(data_recover))
race_num6<-numeric(nrow(data_recover))
race_num7<-numeric(nrow(data_recover))
race_num8<-numeric(nrow(data_recover))
race_num9<-numeric(nrow(data_recover))
race_num10<-numeric(nrow(data_recover))



#race_num<-numeric(length(X$Race))                         
numeric_race<-function(racevec){
  for(i in 1:nrow(data_recover)){
    if(racevec[i]=="AMERICA"){
      race_num1[i]<-1
    } else if(racevec[i]=="ASIAN"){
      race_num2[i]<-1
    } else if (racevec[i]=="BLACK"){
      race_num3[i]<-1
    } else if (racevec[i]=="HISPANI"){
      race_num4[i]<-1
    } else if (racevec[i]=="MULTIRA"){
      race_num5[i]<-1
    } else if (racevec[i]=="OTHER"){
      race_num6[i]<-1
    } else if(racevec[i]=="PACIFIC"){
      race_num7[i]<-1
    } else if (racevec[i]=="PATIENT"){
      race_num8[i]<-1
    } else if (racevec[i]=="UNKNOWN"){
      race_num9[i]<-1
    } else {
      race_num10[i]<-1
    }
  }
  print(data.frame(race_num1,race_num2,race_num3,race_num4,race_num5,race_num6,race_num7,race_num8,race_num9,race_num10))
}
#race_numeric<-numeric_race(racevec=X$Race)





race_num1<-numeric(nrow(data_recover))
race_num2<-numeric(nrow(data_recover))
race_num3<-numeric(nrow(data_recover))
race_num4<-numeric(nrow(data_recover))
race_num5<-numeric(nrow(data_recover))
race_num6<-numeric(nrow(data_recover))
race_num7<-numeric(nrow(data_recover))
race_num8<-numeric(nrow(data_recover))






vec<-c("MULTIRA|OTHER","PATIENT|UNKNOWN","AMERICA","ASIAN","HISPANI","BLACK","WHITE","PACIFIC")
library(purrr)
index<-map(vec,function(x)grep(x,data_recover$Race))

#index_1<-which(data_recover$Race=="?")
#index[[2]]<-c(index[[2]],index_1)

race_num1[index[[1]]]<-1

race_num2[index[[2]]]<-1

race_num3[index[[3]]]<-1

race_num4[index[[4]]]<-1

race_num5[index[[5]]]<-1

race_num6[index[[6]]]<-1

race_num7[index[[7]]]<-1

race_num8[index[[8]]]<-1

race<-data.frame(race_num1,race_num2,race_num3,race_num4,race_num5,race_num6,race_num7,race_num8)






gender_numeric<-ifelse(data_recover$Sex=="MALE",1,0)
baseline<-data.frame(data_recover$LOS,data_recover$Admit_Age,race,gender_numeric)
model_1<-rmst2(time_recover,status_recover,arm_recover,covariates= baseline)
saveRDS(model_1,file="result_1.rds")
