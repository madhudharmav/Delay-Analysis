library(dplyr)
library(reshape2)
library(data.table)
library(knitr)
setwd("C:/Users/Madhu/Documents/delay analysis")
path <- "C:/Users/Madhu/Documents/delay analysis"
df_pivot_raw <- read.csv(paste(path,"Idletimepivot_CW2627.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
df_class_raw<-read.csv(paste(path,"Geolocation_CW2627.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
df_real_raw<-read.csv(paste(path,"widget_CW2627.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)

dates <- rep(as.Date("07/10/16", "%m/%d/%y"), 2)

dates[1] <- dates[2] - 7
df_pivot <- transmute(df_pivot_raw,datei=as.Date(Days.in.forecast__completedAt__date,format="%m/%d/%y"),
                      arrived=paste(Days.in.forecast__completedAt__date,completedat_hours),
                      cluster=fleet__name,
                      schld_arrive=paste(Days.in.forecast__completedAt__date,destination_to_hours),
                      schld_dep=paste(Days.in.forecast__completedAt__date,destination_from_hours),
                      Reference = as.character(destination__externalId))
df_pivot$arrived<-as.POSIXct(df_pivot$arrived,format="%m/%d/%y %H:%M")
df_pivot$schld_arrive<-as.POSIXct(df_pivot$schld_arrive,format="%m/%d/%y %H:%M")
df_pivot$schld_dep<-as.POSIXct(df_pivot$schld_dep,format="%m/%d/%y %H:%M")

df_real<-transmute(df_real_raw,Reference=Reference,real_arrived=as.POSIXct(Created.time)+3600,Event=Event)

events<-c("PickUpArrived","DropOffArrived")
df_real <- df_real[df_real$Event %in% events, ]

df_all<-inner_join( x=df_pivot , y = df_real,  by = c("Reference"))

df_all$Reference<-substr(df_all$Reference,1,10)

df_class<-transmute(df_class_raw,Reference=reference,class_name=chosenServiceClass__reference)

df_all<-inner_join( x=df_all , y = df_class,  by = c("Reference"))
df_all<-cbind(df_all,timeslot=strftime(df_all$schld_dep, format="%H:%M"),
              arr_delay=difftime(df_all$arrived,df_all$schld_arrive,units="mins"),
              timewin=difftime(df_all$schld_arrive,df_all$schld_dep,units="mins"),
              colsplit(string=df_all$class_name, pattern="-", names=c("n", "class","city") ))

df_all[which(df_all$city=="BERLIN"),"real_arrived"]<-df_all[which(df_all$city=="BERLIN"),"real_arrived"]+3600
df_all<-cbind(df_all,real_arr_delay=difftime(df_all$real_arrived,df_all$schld_arrive,units="mins"))

df_all<-subset( df_all, select = -c(class_name,arrived,n))
df_all$arr_delay[df_all$arr_delay < 6] <- 0
df_all$real_arr_delay[df_all$real_arr_delay < 0] <- 0


df_all<-data.table(df_all)

selectweek<-(df_all$datei<=dates[2])&(df_all$datei>dates[1])
df_week<-df_all[selectweek,]
df_week<-subset(df_week,df_week$arr_delay<500)
df_week<-subset(df_week,df_week$real_arr_delay<500)

df_week1<-df_week[df_week$arr_delay>0,]
df_week2<-df_week[df_week$real_arr_delay>0,]
i2<-0
no_delay1<-vector(mode="integer",length=4)
no_delay2a<-vector(mode="integer",length=4)
no_delay2b<-vector(mode="integer",length=4)
no_delay2c<-vector(mode="integer",length=4)
no_delay2d<-vector(mode="integer",length=4)
classes<-c("PLUS" ,   "LITE"  ,  "CLASSIC" , "EXPRESS")
for(i1 in classes){
  i2<-i2+1
  
  no_delay1[i2]<-as.integer(df_week[cluster == "Soho" & class == i1 , .N])
  no_delay2a[i2]<-as.integer(df_week1[cluster =="Soho" & class == i1 & timewin == 30, .N])
  no_delay2b[i2]<-as.integer(df_week1[cluster == "Soho" & class == i1 & timewin ==120, .N])
  no_delay2c[i2]<-as.integer(df_week2[cluster =="Soho" & class == i1 & timewin == 30, .N])
  no_delay2d[i2]<-as.integer(df_week2[cluster == "Soho" & class == i1 & timewin ==120, .N])
  
 
}
op_table1<-cbind(classes,no_delay1,no_delay2a,no_delay2b,round(no_delay2a/no_delay1*100,digits=2),round(no_delay2b/no_delay1*100,digits=2),no_delay2c,no_delay2d,round(no_delay2c/no_delay1*100,digits=2),round(no_delay2d/no_delay1*100,digits=2))
colnames(op_table1)<-c("class","Total Orders","No.of Delays for 1/2hr","No.of Delays for 2hr","% for 1/2hr","% for 2hr","real No.of Delays for 1/2hr","real No.of Delays for 2hr","real % for 1/2hr","real % for 2hr")
kable(op_table1)

i2<-0
timeslots<-levels(df_all$timeslot)
no_delay3<-vector(mode="integer",length=length(timeslots))
no_delay4a<-vector(mode="integer",length=length(timeslots))
no_delay4b<-vector(mode="integer",length=length(timeslots))
no_delay4c<-vector(mode="integer",length=length(timeslots))
no_delay4d<-vector(mode="integer",length=length(timeslots))
for(i3 in timeslots){
  i2<-i2+1
  no_delay3[i2]<-as.integer(df_week[cluster == "Soho" & timeslot == i3, .N])
  no_delay4a[i2]<-as.integer(df_week1[cluster == "Soho" & timeslot == i3 & timewin ==30, .N])
  no_delay4b[i2]<-as.integer(df_week1[cluster == "Soho" & timeslot == i3 & timewin ==120, .N])
  no_delay4c[i2]<-as.integer(df_week2[cluster == "Soho" & timeslot == i3 & timewin ==30, .N])
  no_delay4d[i2]<-as.integer(df_week2[cluster == "Soho" & timeslot == i3 & timewin ==120, .N])  
}

op_table2<-cbind(timeslots,no_delay3,no_delay4a,no_delay4b,no_delay4c ,no_delay4d)
op_table2<-data.table(op_table2)
shift_start<-c("06:00","10:00","14:00","17:00")
shift_end<-c("10:00","13:00","17:00","22:30")
ag_op_table2<-NULL
for(i2 in 1:length(shift_start)){
  
  ag_op_table2<-rbind(ag_op_table2,op_table2[timeslots>shift_start[i2]& timeslots<shift_end[i2],.(shift_start[i2],sum(as.numeric(no_delay3)),sum(as.numeric(no_delay4a)),sum(as.numeric(no_delay4b)),sum(as.numeric(no_delay4c)),sum(as.numeric(no_delay4d)))])
  }
op_table2<-rbind(setnames(ag_op_table2,names(op_table2)),op_table2)

op_table2<-cbind(op_table2,round(as.numeric(op_table2$no_delay4a)/as.numeric(op_table2$no_delay3)*100,digits=2),round(as.numeric(op_table2$no_delay4b)/as.numeric(op_table2$no_delay3)*100,digits=2),round(as.numeric(op_table2$no_delay4c)/as.numeric(op_table2$no_delay3)*100,digits=2),round(as.numeric(op_table2$no_delay4d)/as.numeric(op_table2$no_delay3)*100,digits=2))
colnames(op_table2)<-c("timeslot","Total Orders","No.Delays 1/2hr","No.Delays 2hr","real No.Delays 1/2hr","real No.Delays 2hr","% 1/2hr","% 2hr","real % 1/2hr","real % 2hr")
op_table2[which(op_table2==NaN)]<-"No orders"
kable(op_table2)
