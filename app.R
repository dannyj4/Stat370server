library(shiny)
library(readr)
NHL_Stats <- read_csv("~/Stat370server/NHL_Stats.csv")
View(NHL_Skaters)

library(readr)
NHL_Stats_Goalies <- read_csv("~/Stat370server/NHL_Stats_Goalies.csv")
View(NHL_Stats_Goalies)


ui <- fluidPage(
  titlePanel('MoneyPuck Data Sets'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Skaters", "Goalies")),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
)

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "Skaters" = NHL_Stats,
           "Goalies" = NHL_Stats_Goalies)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
}

shinyApp(ui = ui, server = server)