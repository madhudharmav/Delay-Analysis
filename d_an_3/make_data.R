library(dplyr)
library(reshape2)
library(data.table)
library(knitr)
setwd("C:/Users/Madhu/Documents/delay analysis")
path <- "C:/Users/Madhu/Documents/delay analysis"
df_pivot_raw <- read.csv(paste(path,"Idletimepivot.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
df_class_raw<-read.csv(paste(path,"Geolocation.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
df_real_raw<-read.csv(paste(path,"widget.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)

dates <- rep(as.Date("07/10/16", "%m/%d/%y"), 2)

dates[1] <- dates[2] - 30
df_pivot <- transmute(df_pivot_raw,datei=as.Date(Days.in.forecast__completedAt__date,format="%m/%d/%y"),
                      arrived=paste(Days.in.forecast__completedAt__date,completedat_hours),
                      cluster=fleet__name,
                      schld_arrive=paste(Days.in.forecast__completedAt__date,destination_to_hours),
                      schld_dep=paste(Days.in.forecast__completedAt__date,destination_from_hours),
                      Reference = as.character(destination__externalId),time=forecast__travelingTime)
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


df_week2<-df_week[df_week$real_arr_delay>0,]

eachday<-unique(df_week$datei)
op_table_final<-NULL
for(i5 in eachday){
df_day<-df_week[datei==i5]
df_day2<-df_week2[datei==i5]
i2<-0
timeslots<-levels(df_all$timeslot)
no_delay3<-vector(mode="integer",length=length(timeslots))
no_delay4c<-vector(mode="integer",length=length(timeslots))
no_delay4d<-vector(mode="integer",length=length(timeslots))
spd_time<-vector(mode="integer",length=length(timeslots))
for(i3 in timeslots){
  i2<-i2+1
  no_delay3[i2]<-as.integer(df_day[cluster == "Soho" & timeslot == i3, .N])
  spd_time[i2]<-df_day[cluster == "Soho" & timeslot == i3, sum(time)]
  no_delay4c[i2]<-as.integer(df_day2[cluster == "Soho" & timeslot == i3 & timewin ==30, .N])
  no_delay4d[i2]<-as.integer(df_day2[cluster == "Soho" & timeslot == i3 & timewin ==120, .N])  
}

op_table2<-cbind(timeslots,no_delay3,no_delay4c ,no_delay4d,spd_time)
op_table2<-data.table(op_table2)
shift_start<-c("06:00","10:00","14:00","17:00")
shift_end<-c("10:00","13:00","17:00","22:30")
ag_op_table2<-NULL

for(i2 in 1:length(shift_start)){
  
  ag_op_table2<-rbind(ag_op_table2,op_table2[timeslots>shift_start[i2]& timeslots<shift_end[i2],.(shift_start[i2],sum(as.numeric(no_delay3)),sum(as.numeric(no_delay4c)),sum(as.numeric(no_delay4d)),sum(as.numeric(spd_time)))])
  }
op_table_final<-rbind(ag_op_table2,op_table_final)
}
colnames(op_table_final)<-c("timeslot","Total Orders","real No.Delays 1/2hr","real No.Delays 2hr","spd_time")
save(op_table_final,file="delay_aggregates_timeshifts.dat")


