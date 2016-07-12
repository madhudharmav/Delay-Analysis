
#install.packages("rsconnect")
#library(rsconnect)

#host on shiny.io
#rsconnect::deployApp("C:/Users/Madhu/Documents/delay analysis/d_an")


#or run my laptop take care the app is running at the moment on R
#runApp(appDir="C:/Users/Madhu/Documents/delay analysis/d_an",host="192.168.100.232",port=5050)


shinyUI(fluidPage(
  titlePanel("Delay Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file1", "operations/Idletime dashboard/Idletimepivot.csv",accept=c('text/csv')), 
      fileInput("file2", "operations/orders dashboard/Geolocation.csv",accept=c('text/csv')), 
      fileInput("file3", "operations/testproject dashboard/widget.csv",accept=c('text/csv')),
      

      selectizeInput('e2', 'region', choices =c("Nord Westen",  "West","Zentral","Central","Camden","City" ,"Cooperations", "North East","Soho" ,"South", "Southeast","Victoria","West (new)")
      ),
         
  
  
    dateInput('enddate',
              label = 'Date input: yyyy-mm-dd',
              value = Sys.Date()
              
    ) ),
  

  mainPanel(tableOutput("table1"),
            tableOutput("table2"))
  )
  ))
    
  