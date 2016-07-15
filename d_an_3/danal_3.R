#typeofweek timeshifts speedfactor averagetravellingtime Nooforders delays
library(gtools)
library(data.table)
library(randomForest)
#library(caret)

setwd("C:/Users/Madhu/Documents/delay analysis")
spd_f<-c(2:6)


#DOW<-c("weekday","weekend")
#timeslot<-c("06:00","10:00","14:00","17:00")
#ip_data<-as.matrix(merge(DOW,timeslot,all=TRUE))
#ip_data<-rbind(ip_data, matrix(c("sunday","17:00"),nrow=1))

load("delay_aggregates_timeshifts.dat")
ip_data<-data.frame(op_table_final[,.(timeslot,`Total Orders`,delays1=as.numeric(`real No.Delays 1/2hr`),delays2=as.numeric(`real No.Delays 2hr`))])

ip_data<-transform(ip_data,delays=delays1+delays2)
ip_data<-ip_data[,-c(3:4)]
ip_data<-cbind(ip_data,op_table_final$spd_time)
colnames(ip_data)<-c("timeslot","orders","delays","spd_time")

# ##########approach1 NW###########
# 
# pred_del<-vector(mode="integer",nrow(ip_data))
# ip_data<-cbind(ip_data,pred_del)
# 
#   
# all_p<-permutations(n=length(spd_f),r=4,spd_f,repeats.allowed=T)
# i1<-50
# #model_acc<-
# #for(i1 in 1:nrow(all_p)){
# spd_f_slot<-rep(all_p[i1,],30)
#   ip_data2<-data.frame(cbind(ip_data,spd_f_slot))
#   ip_data3<-ip_data2[which(ip_data2$orders!=0),]
#   ip_data3$pred_del<-NULL
#   ip_data3$timeslot<-as.factor(ip_data3$timeslot)
#   ####execute model on ipdata2 and find accuracy for each iteration
#   rf_model<-randomForest(delays~.,data=ip_data3,ntree=500, do.trace=T)
#   rf_model<-train(pred_del~.,data=ip_data3,method="rf", trControl=trainControl(method="cv",number=5),prox=TRUE,allowParallel=TRUE)
#   #####
#   
# #}
# 
#   
#   ######################################
ip_data<-ip_data[which(ip_data$orders!=0),]
ip_data$timeslot<-as.factor(ip_data$timeslot)
train_ind <-sample(seq_len(nrow(ip_data)), size = floor(0.75 * nrow(ip_data)))

ip_data_train<-ip_data[ train_ind,]
ip_data_test<-ip_data[-train_ind,]
rf_model<-randomForest(spd_time~.,data=ip_data,ntree=15000,importance = TRUE)


##### evaluate###

library(caret)
print(rf_model)
importance(rf_model)
errr<-ip_data_test$spd_time-predict(rf_model,ip_data_test)
cbind(ip_data_test[,1],abs(errr/ip_data_test$spd_time)*100)
varImp(rf_model)