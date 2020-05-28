library(readr)
library(shiny) #Loading Shiny Package

ui <- fluidPage(
  titlePanel("Weight Gained Prediction"),   #Labeling the title
  sidebarLayout(  #These are all the widgets ex sidebars etc
    sidebarPanel(   
      numericInput("num","Calories consumed",1)),  #User will be inputting these 4, 1 is the default value which will show in dashboard
    mainPanel(
      tableOutput("distplot")
    )
  )
)



server <- function(input, output){
  output$distplot <- renderTable({
    
    calories_consumed <- read_csv("calories_consumed.csv")
    colnames(calories_consumed) <- c("gained","calories")
    
    model_q1 <- lm(gained ~ calories, data = calories_consumed)
    
    nw=data.frame(calories=input$num)
    nw
    w=predict(model_q1,nw)
    w
  })
}


shinyApp(ui=ui, server=server)

