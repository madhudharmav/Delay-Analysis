library(shiny)
shinyServer(function(input, output,session) {
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })
 pre_pro<-reactive({

    library(dplyr)
    library(reshape2)
    library(data.table)
    #setwd("C:/Users/Madhu/Documents/delay analysis")
    #path <- "C:/Users/Madhu/Documents/delay analysis"
    
    #df_pivot_raw <- read.csv(paste(path,"Idletimepivot.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
    #df_class_raw<-read.csv(paste(path,"Geolocation.csv",sep="/") ,sep=",", stringsAsFactors = FALSE)
    

    
      validate(
        need(input$file1 != "", "Please select data sets"),
        need(input$file2 != "", "Please select data sets")
      )
      
    infile1  <- input$file1
    infile2  <- input$file2

    
    df_pivot_raw <- read.csv(infile1$datapath ,sep=",", stringsAsFactors = FALSE)
    df_class_raw<-  read.csv(infile2$datapath ,sep=",", stringsAsFactors = FALSE)
    
    
    dates <- rep(as.Date("07/03/16", "%m/%d/%y"), 2)
    dates[2]<-input$enddate
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
    
    df_pivot$Reference<-substr(df_pivot$Reference,1,10)
    
    df_class<-transmute(df_class_raw,Reference=reference,class_name=chosenServiceClass__reference)
    
    df_all<-inner_join( x=df_pivot , y = df_class,  by = c("Reference"))
    df_all<-cbind(df_all,timeslot=strftime(df_all$schld_dep, format="%H:%M"),
                  arr_delay=difftime(df_all$arrived,df_all$schld_arrive,units="mins"),
                  timewin=difftime(df_all$schld_arrive,df_all$schld_dep,units="mins"),
                  colsplit(string=df_all$class_name, pattern="-", names=c("n", "class","city") ))
    
    df_all<-subset( df_all, select = -c(class_name,arrived,n,schld_arrive))
    df_all$arr_delay[df_all$arr_delay < 6] <- 0
    df_all<-data.table(df_all)
    selectweek<-(df_all$datei<=dates[2])&(df_all$datei>dates[1])
    df_week<-df_all[selectweek,]
    dat2plot<-list(df_week,levels(df_all$timeslot))
    
})
    
 
 
 output$table1 <- renderTable({
   dat2plot<-pre_pro()
   
   df_week<-dat2plot[[1]]
   df_week1<-df_week[df_week$arr_delay>0,]
   
   i2<-0
   no_delay1<-vector(mode="integer",length=4)
   no_delay2<-vector(mode="integer",length=4)
   no_delay2a<-vector(mode="integer",length=4)
   
   classes<-c("PLUS" ,   "LITE"  ,  "CLASSIC" , "EXPRESS")
   for(i1 in classes){
     i2<-i2+1
     no_delay1[i2]<-as.integer(df_week[cluster == input$e2 & class == i1, .N])
     no_delay2[i2]<-as.integer(df_week1[cluster ==input$e2 & class == i1 & timewin ==30, .N])
     no_delay2a[i2]<-as.integer(df_week1[cluster == input$e2 & class == i1 & timewin ==120, .N])
     
   }
   
   op_table1<-cbind(classes,no_delay1,no_delay2,no_delay2a,round(no_delay2/no_delay1*100,digits=2),round(no_delay2a/no_delay1*100,digits=2))
   colnames(op_table1)<-c("class","Total Orders","No.of Delays for 1/2hr","No.of Delays for2hr","% for 1/2hr","% for 2hr")
   
   return(op_table1)
 })
 
 
 output$table2 <- renderTable({
   dat2plot<-pre_pro()
   df_week<-dat2plot[[1]]
   df_week1<-df_week[df_week$arr_delay>0,]
   
   i2<-0
   timeslots<-dat2plot[[2]]
   print(timeslots)
   print(class(timeslots))
   no_delay3<-vector(mode="integer",length=length(timeslots))
   no_delay4<-vector(mode="integer",length=length(timeslots))
   no_delay4a<-vector(mode="integer",length=length(timeslots))
   for(i3 in timeslots){
     i2<-i2+1
     no_delay3[i2]<-as.integer(df_week[cluster == input$e2 & timeslot == i3, .N])
     no_delay4[i2]<-as.integer(df_week1[cluster == input$e2 & timeslot == i3 & timewin ==30, .N])
     no_delay4a[i2]<-as.integer(df_week1[cluster == input$e2 & timeslot == i3 & timewin ==120, .N])
     
   }
   op_table2<-cbind(timeslots,no_delay3,no_delay4,no_delay4a,round(no_delay4/no_delay3*100,digits=2),round(no_delay4a/no_delay3*100,digits=2))
   colnames(op_table2)<-c("timeslot","Total Orders","No.of Delays for 1/2hr","No.of Delays for 2hr","% for 1/2hr","% for 2hr")
   
   return(op_table2)
 })
 
 
})  
 