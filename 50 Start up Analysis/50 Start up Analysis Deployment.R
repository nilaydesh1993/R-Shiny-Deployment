library(shiny) #Loading Shiny Package
library(readr)
ui <- fluidPage(
    titlePanel("Profit Prediction of startups"),   #Labeling the title
    sidebarLayout(  #These are all the widgets ex sidebars etc
        sidebarPanel(   
            numericInput("num","R&D",1),
            numericInput("num1","Marketing",1),
            numericInput("num2","Admin",1)),  #User will be inputting these 4, 1 is the default value which will show in dashboard
        mainPanel(
            tableOutput("distplot")
        )
    )
)



server <- function(input, output){
    output$distplot <- renderTable({
        
        startups <- read_csv("50_Startups.csv")
        colnames(startups) <- c("RandD","admin","marketing","state","profit")
        
        model2 <-lm(profit ~ RandD + marketing, data = startups[-c(50),])
        
        nw=data.frame(RandD=input$num, marketing=input$num1, admin=input$num2)
        nw
        w=predict(model2,nw)
        w
    })
}


shinyApp(ui=ui, server=server)

