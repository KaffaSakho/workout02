#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Returns on 3 Types of Investment Modalities"),
  
  #Slider and Select Inputs 
  fluidRow(
    column(4, 
           sliderInput(inputId = "amount", 
                       label = "Initial Amount", 
                       value = 1000, min = 0, max = 100000, step = 500, pre = "$"),
           sliderInput(inputId = "contrib", 
                       label = "Annual Contribution", 
                       value = 2000, min = 0, max = 50000, step = 500, pre = "$")
  ),
  
    column(4, 
         sliderInput(inputId = "r", 
                     label = "Return Rate (in %)", 
                     value = 5, min = 0, max = 20, step = 0.1),
         sliderInput(inputId = "g", 
                     label = "Growth Rate (in %)", 
                     value = 2, min = 0, max = 20, step = 0.1)
  ),
  
  column(4, 
         sliderInput(inputId = "Year", 
                     label = "Years", 
                     value = 20, min = 0, max = 50, step = 1),
         selectInput(inputId = "facet", 
                     label = "Facet?", 
                     choices = c("No"= "No", "Yes" = "Yes")) #still need to set default facet to no
  )
),

hr(),
p(strong("Timelines")),
  plotOutput(outputId = "tgraph"),

br(),
p(strong("Balances")),
verbatimTextOutput("view")
#dataTableOutput("view")
)
   

# Define server logic required to draw plots
library(dplyr)
library(reshape2)
library(ggplot2)
library(shiny)

server <- function(input, output) {
  
  #Creating the Graph 
  my_table <- reactive({
     
     #Creating the Functions to be used 
     future_value <- function(amount=1000, rate=0.05, years=20) {
       future <- amount*((1+rate)^years)
       return(future)
     }
     annuity <- function(contrib=2000, rate=0.05, years) {
       annuity <- contrib*((1+rate)^years -1)/rate
       return(annuity)
     }
     growing_annuity <- function(contrib=2000, rate=0.05, growth=0.02, years) {
       growing <- contrib * (((1+rate)^years) - ((1+growth)^years))/(rate-growth)
       return(growing)
     }
     
     #Creating the Columns of the Data Frame 
     no_contrib <- c() 
     fixed_contrib <- c() 
     growing_contrib <- c()  
     
     for (y in 0:input$Year) {
       no_contrib <- c(no_contrib, future_value(input$amount,input$r/100,y))
       fixed_contrib <- c(fixed_contrib, future_value(input$amount,input$r/100,y) + annuity(input$contrib,input$r/100,y))
       growing_contrib <- c(growing_contrib,future_value(input$amount,input$r/100,y) + growing_annuity(input$contrib,input$r/100,input$g/100,y))
       
     }
     
     no_contrib <- round(no_contrib, digits=2)
     fixed_contrib <- round(fixed_contrib,digits=2)
     growing_contrib <- round(growing_contrib,digits=2)
     
     #Creating the Data Frame Modalities
     year <- 0:input$Year
     modalities <- data.frame(year, no_contrib, fixed_contrib, growing_contrib, row.names = NULL)
     modalities
     
  })
     #Creating the plot
     
     output$tgraph <- renderPlot({
       modalities1 <- melt(my_table(), id.vars= "year")
       colnames(modalities1) <- c("year","modality","balance")
     #Non-facetted plot 
     if (input$facet == "No") { 
      
       
       ggplot(data=modalities1) + 
         
         #Lines colors and sizes
         geom_line(aes(x = year, y = balance, color = modality)) + 
         geom_point(aes(x = year, y = balance, color=modality)) + 
         
         #Axis Scales & Labels
         
         #Background
         theme_gray() +
         
         #Title 
         labs(title = "Three Modes of Investing") + 
         theme(plot.title = element_text(size = 12, face = "bold", family = "Times New Roman"), axis.title =  element_text(size = 11, family = "Times New Roman"))
     } else {
       
       ggplot(data=modalities1, aes(x=year, y=balance)) +
         
       #Lines colors and sizes
         geom_line(aes(color = modality)) + 
         geom_point(aes(color = modality)) + 
         geom_area(aes(fill=modality,alpha=1), show.legend=FALSE) +

         #Facet
        facet_grid(. ~ modality) + 

         #Background
          theme_bw() +
         
         #Title
        labs(title = "Three Modes of Investing") +
         theme(plot.title = element_text(size = 12, face = "bold", family = "Times New Roman"), axis.title =  element_text(size = 11, family = "Times New Roman"))
         
  
   }
})
   
  output$view <- 
    renderPrint({ my_table()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

