library(shiny) #Loading Shiny Package
library(readr)
ui <- fluidPage(
    titlePanel("Toyota Used Car Price Predicter"),   #Labeling the title
    sidebarLayout(  #These are all the widgets ex sidebars etc
        sidebarPanel(   
            numericInput("num","Age_08_04",1),
            numericInput("num1","KM",1),
            numericInput("num2","HP",1),
            numericInput("num3","cc",1),
            numericInput("num4","Doors",1),
            numericInput("num5","Gears",1),
            numericInput("num6","Quarterly_Tax",1),
            numericInput("num7","Weight",1)),  #User will be inputting these , 1 is the default value which will show in dashboard
        mainPanel(
            tableOutput("distplot")
        )
    )
)



server <- function(input, output){
    output$distplot <- renderTable({
        
        toyota <- read_csv("ToyotaCorolla.csv")
        
        model2 <-lm(Price ~ Age_08_04 + KM + HP + cc + Doors + Gears + Quarterly_Tax + Weight , toyota)
        
        nw=data.frame(Age_08_04=input$num, KM=input$num1, HP=input$num2, cc=input$num3, Doors=input$num4, Gears=input$num5, Quarterly_Tax=input$num6, Weight=input$num7) 
        nw
        w=predict(model2,nw)
        w
    })
}


shinyApp(ui=ui, server=server)

