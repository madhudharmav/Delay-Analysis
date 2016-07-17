options(shiny.maxRequestSize=50*1024^2) # would increase the limit to 30MB.
library(shiny)
library(DT)
shinyServer(function(input, output,session) {
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })

 
 
  pre_pro<-reactive({
   
   library(dplyr)
   library(reshape2)
   library(data.table)
   library(gtools)
   library(randomForest)
   library(knitr)
    
   validate(
     need(input$file1 != "", "Please select data sets"),
     need(input$file3 != "", "Please select data sets")
   )
   
   infile1  <- input$file1
   infile3  <- input$file3
   
   df_pivot_raw <- read.csv(infile1$datapath ,sep=",", stringsAsFactors = FALSE)
   df_real_raw<-  read.csv(infile3$datapath ,sep=",", stringsAsFactors = FALSE)
   
     df_pivot <- transmute(df_pivot_raw,datei=as.Date(Days.in.forecast__completedAt__date,format="%m/%d/%y"),
                         cluster=fleet__name,
                         schld_arrive=paste(Days.in.forecast__completedAt__date,destination_to_hours),
                         schld_dep=paste(Days.in.forecast__completedAt__date,destination_from_hours),
                         Reference = as.character(destination__externalId),time=forecast__travelingTime)
   df_pivot$schld_arrive<-as.POSIXct(df_pivot$schld_arrive,format="%m/%d/%y %H:%M")
   df_pivot$schld_dep<-as.POSIXct(df_pivot$schld_dep,format="%m/%d/%y %H:%M")
  # pred_date<-as.Date(input$enddate, "%y/%m/%d")
   dates <- rep(as.Date("07/10/16", "%m/%d/%y"), 3)
   dates[3]<-input$enddate
   pred_date<-dates[3]
 # pred_date<-as.Date(input$enddate, format="%m/%d/%y")
 #  print(pred_date)
  
   df_day<-df_pivot[df_pivot$datei==pred_date,]
   df_day<-data.table(subset(df_day,select=-c(Reference,schld_arrive)))
   names(df_day)<-c("datei","cluster","timeslot","time")
   df_day$timeslot<-strftime(as.POSIXct(df_day$timeslot,format="%m/%d/%y %H:%M"), format="%H:%M")
   
   ## pivot has datei,cluster,schdldarrive,timeslot,refid,spd_time
   df_real<-transmute(df_real_raw,Reference=Reference,real_arrived=as.POSIXct(Created.time)+3600,Event=Event,city=CIty)
   
   events<-c("PickUpArrived","DropOffArrived")
   df_real <- df_real[df_real$Event %in% events, ]
   
   df_all<-inner_join( x=df_pivot , y = df_real,  by = c("Reference"))
   
   #df_all has datei,cluster,schdldarrive,timeslot,refid,arrivaltime,event,city,spd_time
   df_all<-cbind(df_all,timeslot=strftime(df_all$schld_dep, format="%H:%M"))
   
   df_all[which(df_all$city=="Berlin"),"real_arrived"]<-df_all[which(df_all$city=="Berlin"),"real_arrived"]+3600
   df_all<-cbind(df_all,real_arr_delay=difftime(df_all$real_arrived,df_all$schld_arrive,units="mins"))
   
   df_all<-subset( df_all, select = -c(Event,Reference,schld_arrive,real_arrived,schld_dep))
   
   #df_all has datei,cluster,timeslot,city,delay,spd_time
   
   df_all$real_arr_delay[df_all$real_arr_delay < 0] <- 0
   
   
   df_all<-data.table(df_all)
   #dates <- rep(as.Date("07/10/16", format="%m/%d/%y"), 2)
 
   dates[2]<-dates[3]-6
 
   dates[1]<-dates[2]-30

   selectweek<-(df_all$datei<=dates[2])&(df_all$datei>dates[1])
   
   df_week<-df_all[selectweek,]

   df_week<-subset(df_week,df_week$real_arr_delay<500)
   df_week2<-df_week[df_week$real_arr_delay>0,]
   
   eachday<-sort(unique(df_week$datei))
   op_table_final<-NULL 
   for(i5 in eachday){
     df_day1<-df_week[datei==i5]
     df_day2<-df_week2[datei==i5]
     i2<-0
     timeslots<-sort(unique(df_all$timeslot))
     no_delay3<-vector(mode="integer",length=length(timeslots))
     no_delay4c<-vector(mode="integer",length=length(timeslots))
     spd_time<-vector(mode="integer",length=length(timeslots))
     
     for(i3 in timeslots){
       i2<-i2+1
       spd_time[i2]<-df_day1[cluster == input$e2  & timeslot == i3, sum(time)]
       
       no_delay3[i2]<-as.integer(df_day1[cluster == input$e2   & timeslot == i3, .N])
       no_delay4c[i2]<-as.integer(df_day2[cluster == input$e2   & timeslot == i3 , .N])
       
       
       
     }
     op_table2<-cbind(timeslots,no_delay3,no_delay4c ,spd_time)
     op_table2<-data.table(op_table2)

     shift_start<-c("06:00","10:00","14:00","17:00")
     shift_end<-c("10:00","13:00","17:00","22:30")
     ag_op_table2<-NULL
     for(i2 in 1:length(shift_start)){
       
       ag_op_table2<-rbind(ag_op_table2,op_table2[timeslots>shift_start[i2]& timeslots<shift_end[i2],.(shift_start[i2],sum(as.numeric(no_delay3)),sum(as.numeric(no_delay4c)),sum(as.numeric(spd_time)))])
     }
     op_table_final<-rbind(ag_op_table2,op_table_final)
     
   }

   colnames(op_table_final)<-c("timeslot","Total Orders","real No.Delays","spd_time")
   
   ip_data<-data.frame(op_table_final)
   colnames(ip_data)<-c("timeslot","orders","delays","spd_time")
   
   ip_data<-ip_data[which(ip_data$orders!=0),]
   ip_data$timeslot<-as.factor(ip_data$timeslot)
   train_ind <-sample(seq_len(nrow(ip_data)), size = floor(0.75 * nrow(ip_data)))
   ip_data_train<-ip_data[ train_ind,]
   ip_data_test<-ip_data[-train_ind,]
   rf_model<-randomForest(spd_time~.,data=ip_data,ntree=15000,importance = TRUE)
   #   save(ip_data_train,rf_model,file="predict_sf.model")
   
   dat2plot<-list(df_day,timeslots,rf_model,ip_data_train)
#    
 })
   output$table2 <- renderTable({
     dat2plot<-pre_pro()
     df_day<-dat2plot[[1]]
     timeslots<-dat2plot[[2]]
     rf_model<-dat2plot[[3]]
   i2<-0
   no_delay3_day<-vector(mode="integer",length=length(timeslots))
   spd_time_day<-vector(mode="integer",length=length(timeslots))
   for(i3 in timeslots){
     i2<-i2+1
     no_delay3_day[i2]<-as.integer(df_day[cluster == input$e2  & timeslot == i3, .N])
     spd_time_day[i2]<-df_day[cluster == input$e2  & timeslot == i3, sum(time)]
   }
   
   op_table2_day<-cbind(timeslots,no_delay3_day,spd_time_day)
   op_table2_day<-data.table(op_table2_day)
   op_table_final_day<-data_frame()
   shift_start<-c("06:00","10:00","14:00","17:00")
   shift_end<-c("10:00","13:00","17:00","22:30")
   
   for(i2 in 1:length(shift_start)){
     
     op_table_final_day<-rbind(op_table_final_day,op_table2_day[timeslots>shift_start[i2]& timeslots<shift_end[i2],.(shift_start[i2],sum(as.numeric(no_delay3_day)),sum(as.numeric(spd_time_day)))])
   }
   colnames(op_table_final_day)<-c("timeslot","Total Orders","spd_time")
   
   op_table_final_day$delays<-c(0,0,0,0)
   op_table_final_day<-data.frame(op_table_final_day)
   
   ip<-op_table_final_day[,c(1,2,4,3)]
   colnames(ip)<-c("timeslot","orders","delays","spd_time")
   ip$timeslot<-as.factor(ip$timeslot)
  
   ip_data_train<-dat2plot[[4]]
   levels(ip$timeslot) <- levels(ip_data_train$timeslot)
   pred_spd<-predict(rf_model,ip)
   spd_f<-(pred_spd-ip$spd_time)/ip$spd_time
   spd_f[which(spd_f==NaN)]<-0
   spd_f[which(spd_f==Inf)]<-0
   op_table3<-cbind(ip,pred_spd,spd_f+3)
   op_table3<-subset(op_table3,select=-c(spd_time))
 
   op_table3[ip$orders==0,5]<-0
   op_table3[ip$orders==0,4]<-0
   print(op_table3)
    colnames(op_table3)<-c("timeslot","Total Orders","Required delays","Predicted time","SpeedFactor")
    print(op_table3)
    print(colnames(op_table3))
    return(op_table3)
   })
 

})
