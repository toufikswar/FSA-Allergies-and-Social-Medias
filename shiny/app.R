#load(image_analysis)
#load("output_files/Data_set_1/Waterfall.RData")
library(shiny)
library(leaflet)

list_selected <- data.frame(row.names = fourteen.allergen.names)
list_selected$value <- TRUE

ui <- fluidPage(
  
      selectInput("fourteen", "14 Allergens", fourteen.allergen.names, selected = c("celery","peanuts"), multiple = TRUE,
                  selectize = TRUE, width = NULL, size = NULL),
  
      
      sliderInput(inputId = "number",
                  label = "choose a number",
                  value = 25, min = 1, max = 100),
      
      
      mainPanel(
        leafletOutput("map", height = 1000)
      )
      
  
)



server <- function(input, output){

  rv <- reactiveValues( data = labelled.df.geo)
  
  
  observe(
    x <- rv$data[1:input$number,input$fourteen]
  )
  
  output$map <- renderLeaflet({
    
    
    x <- rv$data[1:input$number,]
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(lat = x$lat, lng = x$long)
    m
    
  })
  
}

shinyApp(ui = ui, server = server)