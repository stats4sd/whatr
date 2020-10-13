#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(reshape2)
library(tm)
library(wordcloud2)
#setwd("C:/Users/sdumb/Dropbox (SSD)/ssd-shiny-server/WhatR")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
  tabsetPanel(tabPanel("Input", conditionalPanel(condition="input.submit==0",
                   textInput(inputId = "text1",label = "What is R?"), 
                   actionButton(inputId = "submit",label="Submit")  
  ),
  
  conditionalPanel(condition="input.submit>0",
                   uiOutput("Message")
  )),
  tabPanel("Results",actionButton(inputId = "refresh",label="Refresh"),tableOutput("Table")),
           tabPanel("Graph",actionButton(inputId = "refresh2",label="Refresh"),wordcloud2Output("Plot1"))
  )
  
  

 
)

# Define server logic required to draw a histogram
server <- function(input,output,session) {
observeEvent(input$submit,{
isolate(write.csv(data.frame(Text=input$text1),paste("tmp/R",Sys.time(),sample(1:100,1),".csv",sep=""),
                  row.names = FALSE))
})
    observeEvent(input$submit|input$refresh|input$refresh2,{
      l1<-paste("tmp/",list.files("tmp"),sep="")
      Text<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE))
      

    output$Message<-renderText({ "Thank you for your submission" })
    
  t1<-removePunctuation(tolower(unlist(str_split(stripWhitespace(Text)," "))))
    t1<-t1[!t1%in%stopwords("english")]
    t2<-data.frame(table(t1))
    
   output$Table<-renderTable({ 
     t1<-data.frame("R"=Text)
     colnames(t1)<-"R is:"
     t1
     })
   
    output$Plot1<-renderWordcloud2({ 
      wordcloud2(t2,size=0.4)})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

