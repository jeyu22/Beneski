library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(tidyverse)
library(rhandsontable)
library(RColorBrewer)
library(randomForest)
library(caret)

# TO DO: Exploratory: day of the week... , fix reactive rhandson table ,
attendance <- readRDS("~/git/beneski/attendance.Rds")
gtrends <- readRDS("~/git/beneski/gtrends.Rds")
rfFit <- readRDS("~/git/beneski/rfFit.Rds")

ui <- 
  dashboardPage(
    dashboardHeader(title = "Beneski Museum"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Historical", icon = icon("th"), tabName = "historical"
        ),
        menuItem("Future", icon = icon("dashboard"), tabName = "future"),
        menuItem("Data",tabName='data',icon=icon("database")),
        menuItem("Acknowledgements",tabName="acknowledgements",icon=icon("user"))
      )
      
      
    ),
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "home",
                h2("Welcome!"),
                
                
                fluidRow(
                  box(solidHeader = TRUE,status="primary","This shiny app shows trends and insights for",tags$a(href="https://www.amherst.edu/museums/naturalhistory", "Beneski Museum's"),
                      "Visitor Attendance over the past 9 years", br(),br(),
                      "Click on the historical tab to view attendance records from 2011 by month/year/daily"
                      ,br(),br(),
                      "Click on the future tab to see visitor attendance predictions based on weather patterns and basic machine learning",
                      width=12
                  )
                ),
                
                fluidRow(
                  valueBoxOutput("total"),
                  valueBoxOutput("daysopened"),
                  valueBoxOutput("admissions")
                ),
                
                fluidRow(
                  box(solidHeader=TRUE, status="primary",width=12,title='General Trends',
                      plotlyOutput("plot1"), br(),br(),
                      "Visitor attendance summarized by year (does not include admissions visitors). From 2011 to 2019 there was 
                      55% increase in general visitor attendance."
                  )
                  
                ),
                
                
                fluidRow(
                  box(solidHeader=TRUE, status="primary",width=12,title='More Insights',
                      plotlyOutput("avgvisitor",width="80%",height="300px"), br(),br(), "The average visitor count per day increased from 53 in 2011
                      to 71 visitors in 2019. ",br(),br(),
                      plotOutput("bymonth15",width="80%",height="300px"), br(),
                      "Bar graph shows distribution of general visitors, and overlayed line represents admissions visitors.",
                      "Trends in general visitors and admissions are very similar except for a drop in admissions in May, 
                      just after college decisions are due.","Overall the museum experiences a drop in visitors during the winter months and
                      right before the academic year starts in September"
                    
                  )
                  
                ),
                
                fluidRow(
                  box(solidHeader=TRUE, status="primary",width=12,title='Google Trends',
                     plotlyOutput("gtrend"),br(), "The orange line represents the popularity of Beneski Museum as a Google",
                     "search term over time, with values ranging from 0 -100. The gray line represents the attendance numbers.",
                    "We can compare the two patterns and identify if there are any moments where there is a spike in search",
                     "popularity followed by a surge in attendance to see if Google Trends may be able to forcast attendance. ",
                    "For example, around October 28th 2018, popularity for Beneski Museum as a search term reached the max",
                    "value of 100. Following that we see a spike in attendance on November 10th at 232 visitors. We can see ",
                    "similar occurences in both July 2019 and September 2019. Of course, attendance at the museum remains strong",
                    " even when the search popularity is at zero, so online queries cannot explain everything. "
  
                  )),
                
                
                
                fluidRow(
                  box(solidHeader=TRUE, status="primary",width=12,title='Over Time',
                      "To view animation, please press the play button on the lower right hand corner of the box",br(),
                      "Each point represents the visitor count of a single day. Hover over any point to see which day it was.",
                      br(),
                     plotlyOutput(outputId = "animated"),uiOutput(outputId = "slider")
                  )
                  
                  
                )
                
                
                
        ),
        
        tabItem(tabName = "historical",
                h2("View Past Attendance"),
                
                fluidRow(
                  box(solidHeader = TRUE, 'Select a date range to view general visitor attendance (this does not include groups or admissions tours)',br(),"Hover over bars to see info.",br(),
                      br(),"Note that Admissions Tour Attendnace is only available 2015 onwards" ,br(),br(),br(),
                      dateRangeInput('daterange', 'Range', start = "2011-01-01", end = "2019-12-31", min = NULL,
                                     max = NULL, format = "yyyy-mm-dd", startview = "month",
                                     weekstart = 0, language = "en", separator = " to ", width = "200px",
                                     autoclose = TRUE),br(),br(),
                      radioButtons('options','View data:',c("Yearly"='yearly',"Monthly"='monthly',"Daily"='daily'),selected="yearly")
                      
                  ,br(),checkboxInput('tour','Add Admissions Tour Attendnace?',value=TRUE),width=500)
                  
                ),
                
                
                fluidRow(
                  
                  box(solidHeader = TRUE,plotlyOutput('plot2'),width=500
                  )
                  

                  
                )
                
        ),
        
        tabItem(tabName = "future",
                fluidRow(tabBox(
                  id = "tabset", height = "500px" , width = 500,
                  tabPanel("Predicting","Using the attendance data we have for the past 9 years, we can build a model using machine learning",
                           " to predict the number of visitors based on day of the week, month, day, and temperature.",br(),br(),
                "Enter a day below to see the prediction, and an estimate for the temperature.",br(),br(),
                "Make sure to press the button to refresh each time! " , br(), br(),
                           dateInput("predict",label="Date",value="2020-03-01"),
                           numericInput("temp",label="Temperature in Farenheit",value="60"),
                           actionButton("button",label="Predict!"),br(),br(),
                           textOutput("attendancetext") ),
                  tabPanel("How it Works",
                           "In order to predict future attendance we can use machine learning. Technology is 
                           able to identify patterns in data that are not obvious to humans, and is also able to",
                           " continously learn and update the models they create with little human intervention.",
                           "For museum attendance we can look at the year, month, day, temperature, and day of week",
                           " for the past 9 years and build a model using those variables. The specific algorthim of",
                           " machine learning used here is called", 
                           tags$a(href="https://en.wikipedia.org/wiki/Random_forest", "Random Forest."),
                           "This prediction will never be 100% accurate, as there are many other variables that affect",
                           " attendance that are not accounted for. This prediction model has a R-squared value of 40%,
                           meaning about 40% of the variation in attendance can be accounted for by the variables mentioned above.",
                           "It isn't perfect, but more data that we collect can help improve our predictions.")
                        
                  
                ))
        ),
        
        tabItem(tabName="data",h2("Data"),
                fluidRow(tabBox(
      
                 
                  id = "tabset1", height = "550px", width=500,
                  tabPanel("View Data",dateRangeInput('daterange1', 'Range', start = "2019-05-01", end = "2019-12-31", min = NULL,
                                                      max = NULL, format = "yyyy-mm-dd", startview = "month",
                                                      weekstart = 0, language = "en", separator = " to ", width = NULL,
                                                      autoclose = TRUE),br(),
                           radioButtons('variable','View data:',c("Yearly Summary"='yearly',"Monthly Summary"='monthly',"Daily"='daily'),selected='monthly'),
                           br(),
                           rHandsontableOutput(outputId = 'datatable'),br(),
                           downloadButton("download",'Download!'))
                  #tabPanel("Download Data", "Click to download data based on the table shown in the view tab",
                          # br(),br()),
                  
                 # tabPanel("Insert Data", "Update Attendance Records here", 
                          # br(),
                         #  dateInput('updatedate',"Choose Date",start="2020-01-01",min=NULL,max=NULL,format="yyyy-mm-dd", autoclose = TRUE),
                         #  br(),
                         #  numericInput('updatevisitor','Visitor Number',value=0,min=0),br(),
                        #   numericInput('updateadmission','Admissions Visitors',value=0,min=0),br(),textInput('notes',"Notes"),
                        #   br(),
                        #   actionButton("update",'Update!'))
                  
                ))
                
                
                ),
        
        tabItem(tabName = "acknowledgements",
                fluidRow(box("App created by", tags$a(href="https://github.com/jeyu22", "Jessica Yu'22"),
                             "with the help of Kenny Chen'22.", br(), br(),
                             "Special thanks to Fred Venne, Beneski Museum Educator, for the data."))
                )
      )
      
      
      
      
      
    )
  )


server <- function(input, output, session) {
  
  
  attendance_val <- reactiveVal(read_rds("~/git/beneski/attendance.Rds"))
  attendance2_val <- reactiveVal(read_rds("~/git/beneski/attendance.Rds"))
  
  
  output$total <- renderValueBox({
    
    totalval <-attendance_val() %>%
      summarize(t = sum(Number,Admission,na.rm=TRUE) )
  
    
    valueBox(
      value = formatC(totalval$t, format = "d"),
      subtitle = "Total Visitors Since 2011",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  
  output$daysopened <- renderValueBox({
    
    dayso <-attendance_val() %>%
     filter(!is.na(Number) & Number>0)%>%
      summarize(t = n())
    
    
    valueBox(
      value = formatC(dayso$t, format = "d"),
      subtitle = "Days Opened Since 2011",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$admissions <- renderValueBox({
    
    aso <-attendance_val() %>%
      summarize(t = sum(Admission,na.rm=TRUE))
    
    
    valueBox(
      value = formatC(aso$t, format = "d"),
      subtitle = "Admissions Visitors",
      icon = icon("book-reader"),
      color = "purple"
    )
  })
  output$plot1 <-renderPlotly ({
    
 x<-   attendance_val()%>%
      group_by(year)%>%
      summarize(Number=sum(Number,na.rm=TRUE))%>%
   ggplot(aes(x=year,y=Number))+geom_bar(stat='identity',fill='dark grey')+
      scale_x_discrete(limits=c("2011", "2019"))+
      geom_text(aes(label=year),vjust=0)+
      theme_minimal()+
      xlab('Year')+
      ylab('Visitors')
 
 ggplotly(x)
  })
  
  
  observeEvent(input$year, {
    
    output$animated <- renderPlotly({
      
      x<-  attendance_val()%>%
        filter(Date < input$year & !is.null(Number) & Number !=0)%>%
        ggplot(aes(x=Date,y=Number))+geom_point(alpha=.2)+scale_y_continuous(limits = c(0,500))
      
      ggplotly(x)
    }
    ) 
               })

  
  output$slider <- renderUI({
    sliderInput("year","Date",min = min(as.Date(attendance_val()$Date)), max = max(as.Date(attendance_val()$Date)), 
                value = min(as.Date(attendance_val()$Date)),step = 30, animate = T,timeFormat = "%F")
  })
  
  
  output$avgvisitor <- renderPlotly({
   
     avgvisitor<-attendance_val() %>%
      group_by(year)%>%
      summarize(n=mean(Number,na.rm = TRUE))
    
    ggplot(data=avgvisitor,aes(x=as.character(year),y=n,group=1))+geom_line(stat="identity")+
      xlab("Year")+ylab("Average Daily Attendance")
    
  })
  
  output$bymonth15 <- renderPlot({
    
    bymonth15 <- attendance_val() %>%
      filter(year>2014)%>%
      group_by(month)%>%
      summarize(Visitors=sum(Number,na.rm = TRUE), Admissions = sum(Admission,na.rm = TRUE))
    
    getPalette <- colorRampPalette(brewer.pal(12, "YlGnBu"))
    
    bymonth15$month <- factor(bymonth15$month,levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
    
    p<-ggplot(data=bymonth15,aes(x=month,y=Visitors,fill=month))+geom_bar(stat="identity")+ 
      scale_fill_manual(values = getPalette(12))+geom_line(data=bymonth15,aes(x=month,y=Admissions),group=1)+xlab("Month")+
      ylab("Visitors")+ggtitle("General Visitor Trends by Month vs Admissions")+scale_x_discrete()
    
    p
    
  })
  
  
  output$gtrend <- renderPlotly({
    
    set.seed(2020)
    
    ssample <- attendance_val()%>%
      filter(year > "2017")
    
    av <- sample_n(ssample,200)
    
    gtrends <- gtrends %>%
      mutate(Number=as.numeric(Number),
             Number=Number*1)
    
    ggplot()+geom_line(aes(x=Date,y=as.numeric(Number)),data=gtrends,color="orange")+
      geom_line(aes(x=Date,y=Number),data=av,alpha=.2)+ylab(" ")
    
  })
  
  output$plot2 <-renderPlotly({
  
    
    if(input$options == 'yearly'){
      
      if(input$tour){
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
          group_by(year)%>%
          summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))%>% 
       plot_ly( x = ~year, y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          add_trace(y = ~Admission, name = 'Admission Tours') %>%
          layout(yaxis = list(title = 'Count'), barmode = 'dodge')
      }else{
        
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
          group_by(year)%>%
          summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))%>%
        plot_ly(x = ~year, y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          layout(yaxis = list(title = 'Count'))
      }
      
    }else if(input$options == 'monthly'){
      

      
      if(input$tour){
        
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
          group_by(year,month)%>%
          summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))%>%
        plot_ly( x = ~paste(year,month), y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          add_trace(y = ~Admission, name = 'Admission Tours') %>%
          layout(yaxis = list(title = 'Count'), barmode = 'dodge')
        
        
      }else{
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
          group_by(year,month)%>%
          summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))%>%
        plot_ly( x = ~paste(year,month), y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          layout(yaxis = list(title = 'Count'))
      }
      
    }else if(input$options == 'daily'){
      
      if(input$tour){
        
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
        plot_ly( x = ~Date, y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          add_trace(y = ~Admission, name = 'Admission Tours') %>%
          layout(yaxis = list(title = 'Count'), barmode = 'dodge')
      }else{
        attendance2_val()%>%
          filter(Date >= as.Date(input$daterange[1]) & Date <= as.Date(input$daterange[2]))%>%
        plot_ly(x = ~Date, y = ~Number, type = 'bar', name = 'Regular Visitors') %>%
          layout(yaxis = list(title = 'Count'))
      }
      
      
    }
    
    
    
  })


  
 v <- reactiveValues()
 
 observeEvent(input$button,{
  
   date <- input$predict
   year <- factor(year(date))
   month <-  factor(month(date))
   day <-factor(day(date))
   temp <- input$temp
   dow <- wday(date,label=TRUE)
   
   df <- data.frame("year"=year,"month"=month,
                    "day"=day,"temperature"=temp,"dow"=dow)
   
   df$year <- factor(df$year,levels=c("2011","2012","2013","2014","2015","2016","2017",
                                      "2018","2019","2020"))
   df$month <- factor(df$month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
   df$day <- factor(df$day, levels = c("1","2","3","4","5","6","7","8","9","10","11","12",
                                       "13","14","15","16","17","18","19","20","21","22",
                                       "23","24","25","26","27","28","29","30","31"))
   df$dow <- factor(df$dow, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),ordered=TRUE)
   
   prediction <- predict(rfFit,df)
   
   v$p <- ceiling(prediction[1])
   
 })
 
 
 output$attendancetext <- renderText({
   paste("The predicted visitor attendance for", input$predict, "is... ", v$p)

 })

 
 
attendance3 <- reactive(


        if(input$variable == 'daily'){
          attendance3 <- attendance_val() %>%
            filter(Date >= as.Date(input$daterange1[1]) & Date <= as.Date(input$daterange1[2]))%>%
            mutate(Date=as.character(Date))
          
          attendance3$f <- subset(attendance3, select= c("Date","Number","Notes","Admission"))
          
        } else if(input$variable == 'monthly'){
          attendance3 <- attendance_val() %>%
            filter(Date >= as.Date(input$daterange1[1]) & Date <= as.Date(input$daterange1[2]))%>%
            mutate(Yearmonth= paste(year,month,sep='-'))%>%
            group_by(Yearmonth)%>%
            summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))
          
          attendance3$f <- attendance3
          
        }else if(input$variable == "yearly"){
          attendance3 <-attendance_val() %>%
            group_by(year)%>%
            summarize(Number=sum(Number,na.rm=TRUE),Admission=sum(Admission,na.rm=TRUE))
          
          attendance3$f <- attendance3
        }
        
)
      output$datatable <- renderRHandsontable(
        
      rhandsontable(data=attendance3())%>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      
      )
  
  
  # trying to update the attendance dataset with newly added input from rhandsontable
  
  observeEvent(input$downloadsave,savedata())
    #finalDF <- hot_to_r(input$datatable)
    #write.csv(finalDF,file="test2.csv")
   # new <- read_csv("test.csv")
    
   # new <- new %>%
   #   mutate(Date = as.Date(Date,"%Y-%m-%d",tz="UTC"),
    #         Number = as.integer(Number),
    #         Admission = as.integer(Admission),
    #         Day = as.character(wday(Date,label=TRUE)),
    #         year=as.double(year(Date)),
    #         month = as.double(month(Date)),
    #         day = as.integer(day(Date))
    #  )
    #attendance_val() <- rbind(attendance_val(),new)
    
  #saveRDS(attendance_val(),"attendance.Rds")
 
  
  savedata <- function(){
    finalDF <- hot_to_r(input$datatable)
    write.csv(finalDF,file="test2.csv",row.names = FALSE)
  }


  
  output$download <- downloadHandler(
    filename = function() {
      paste("Beneski.csv")
    },
    content = function(file) {
      write.csv(attendance3(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)