library(readr)
library(shiny) #Loading Shiny Package

ui <- fluidPage(
  titlePanel("Churn_out_rate Prediction"),   #Labeling the title
  sidebarLayout(  #These are all the widgets ex sidebars etc
    sidebarPanel(   
      numericInput("num","SALARY HIKE",1)),  #User will be inputting these 4, 1 is the default value which will show in dashboard
    mainPanel(
      tableOutput("distplot")
    )
  )
)



server <- function(input, output){
  output$distplot <- renderTable({
    
    emp_data <- read_csv("emp_data.csv")
    colnames(emp_data) <- c("SH","COR")
    
    model_q3poly <- lm(COR ~ SH + I(SH * SH),data = emp_data)
    
    nw=data.frame(SH=input$num)
    nw
    w=predict( model_q3poly,nw)
    w
  })
}


shinyApp(ui=ui, server=server)

