library(readr)
library(shiny) #Loading Shiny Package

ui <- fluidPage(
  titlePanel("Salary_hike Prediction"),   #Labeling the title
  sidebarLayout(  #These are all the widgets ex sidebars etc
    sidebarPanel(   
      sliderInput("bins",
                   "YEAR OF EXPERIANCE",
                   min = 1 ,
                   max = 11,
                   step = 0.2,
                   value  = 2)),  #User will be inputting these 4, 1 is the default value which will show in dashboard
    mainPanel(
      tableOutput("distplot")
    )
  )
)



server <- function(input, output){
  output$distplot <- renderTable({
    
    Salary_Data <- read_csv("Salary_Data.csv")
    colnames(Salary_Data) <- c("YE","SH")
    
    model_q4 <- lm(SH ~ YE,data = Salary_Data) 
    
    nw=data.frame(YE=input$bins)
    nw
    w=predict(model_q4,interval = "confidence",nw)
    w
  })
}


shinyApp(ui=ui, server=server)

