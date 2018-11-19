# load the preprocessed and labelled data
load(paste("../",image_analysis,sep=""))

library(shiny)
library(leaflet)
library(dplyr)

###### Functions to implement user selection

#==================================================================
do_inclusive_list_selection <- function(mylist,the_data)
{

  # select entries by summing up the list of columns in mylist
  # entries selection must have sum_of_columns > 0

  selection <- rep(TRUE,nrow(the_data))

  if(length(mylist) > 0) {
    if(length(mylist) == 1) {
      selection <- selection & (the_data[,mylist[1]] > 0)
    }
    else {
      selection <- selection & (rowSums(the_data[,mylist]) > 0)
    }
  }

  return(selection)

}
#==================================================================
do_exclusive_list_selection <- function(mylist,the_data)
{

  # select entries by multiplying the list of columns in mylist
  # entries selection must have prod_of_columns > 0

  selection <- rep(TRUE,nrow(the_data))

  if(length(mylist) > 0) {
    for(i in 1:length(mylist)) {
      selection <- selection & (the_data[,mylist[i]] > 0)
    }
  }

  return(selection)

}
#==================================================================
do_colvalues_selection <- function(mylist,variable)
{

  # selection entries
  if(length(mylist) == 0) {
    return(rep(FALSE,length(variable)))
  }
  else {
    return(variable %in% mylist)
  }

}
#==================================================================
do_range_selection <- function(myrange,variable)
{

  # select entries from a range in a variable

  return((variable >= myrange[1]) & (variable <= myrange[2]))

}
#==================================================================

# the data
the_data <- labelled.df.geo

# controid for map coordinates
long_m <- 0.5*sum(range(labelled.df.geo$long))
lat_m  <- 0.5*sum(range(labelled.df.geo$lat))

source.names            <- as.character(unique(labelled.df.geo$source))
sentiment.names         <- c("negative","neutral","positive")
allergy_enquiries.name  <- c("allergy_enquiries")
food_labelling.name     <- c("food_labelling")
reporting_reaction.name <- as.character(unique(labelled.df.geo$reactions_report))
stream1issue.names      <- c("allergy_enquiries","food_labelling","mild_reaction","severe_reaction")

time_range_moth      <- range(labelled.df.geo$Month)
time_range_moth_init <- c(time_range_moth[1],time_range_moth[1] + 1)
if(time_range_moth_init[2] > time_range_moth[2]) time_range_moth_init[2] <- time_range_moth[2]

ui <- fluidPage(
  # Creation of a page with a Tab
  navbarPage("FSA Dashboard",
    # First tab of the page
     tabPanel("Map",
          sidebarPanel(
            # 14 allergens listing
            selectInput("fourteen", "14 Allergens", fourteen.allergen.names, selected = vector(),
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # other allergens listing
            selectInput("other", "Other Allergens", other.allergen.names, selected = vector(),
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Sources listing
            selectInput("source", "Sources", source.names, selected = source.names,
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Sentiment listing
            selectInput("sentiment", "Sentimen", sentiment.names, selected = sentiment.names,
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Allergy enquiries
            selectInput(allergy_enquiries.name, "Allergy enquiries", allergy_enquiries.name, selected = vector(),
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Food labelling
            selectInput("food_labelling", "Food labelling", food_labelling.name, selected = vector(),
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Reporting reactions
            selectInput("reporting_reaction", "Reporting reactions", reporting_reaction.name, selected = reporting_reaction.name,
                        multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

            # Normalization
            radioButtons("norm", "Normalization", choices = list("Raw","N. Businesses","Population"), selected = "Raw"),

            # Time range slider
            sliderInput("timerange", "Time interval",
                        min   = time_range_moth[1],
                        max   = time_range_moth[2],
                        value = time_range_moth_init)
          ),
          mainPanel(
            leafletOutput("map", height = 1000)
          )
        ),
       # Second tab to display raw data
       tabPanel("Data",
                dataTableOutput("raw_data")
       )
    )
)



server <- function(input, output) {

  # Create the map
  output$map <- renderLeaflet({
      leaflet()   %>%
      addTiles()  %>%
      setView(lng = long_m, lat = lat_m, zoom = 6)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(the_data[FALSE,])

    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east,  bounds$west)

    subset(the_data,
           lat  >= latRng[1] & lat  <= latRng[2] &
           long >= lngRng[1] & long <= lngRng[2])
  })

  # This creates an output where we display the raw data that is on the map
  output$raw_data <- renderDataTable(dataInBounds())

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    selection <- rep(TRUE,nrow(the_data))

    # # in bounds
    # bounds <- input$map_bounds
    # latRng <- range(bounds$north, bounds$south)
    # lngRng <- range(bounds$east,  bounds$west)
    # selection <- selection & (the_data$lat  >= latRng[1] & the_data$lat  <= latRng[2]) & (the_data$long >= lngRng[1] & the_data$long <= lngRng[2])

    # Allergens
    # 14 allergens selection
    selection <- selection & do_inclusive_list_selection(input$fourteen,the_data)
    # other allergens selection
    selection <- selection & do_inclusive_list_selection(input$other,the_data)

    # Stream 1 issues
    # Allergy enquiries
    selection <- selection & do_inclusive_list_selection(input$allergy_enquiries,the_data)
    # Food labelling
    selection <- selection & do_inclusive_list_selection(input$food_labelling,the_data)
    # Reporting reactions
    selection <- selection & do_colvalues_selection(input$reporting_reaction,the_data$reactions_report)

    # source selection
    selection <- selection & do_colvalues_selection(input$source,the_data$source)

    # sentiment selection
    selection <- selection & do_colvalues_selection(input$sentiment,the_data$sentiment_class)

    # time selection
    selection <- selection & do_range_selection(input$timerange,the_data$Month)

    summary_data <- the_data[selection,] %>%
      group_by(long, lat, District) %>%
      summarise(count = n())

    summary_data$Norm <- rep(1,nrow(summary_data))
    summary_data$District <- as.character(summary_data$District)

    mytitle <- "raw mentions"

    if(input$norm == "N. Businesses") {
      mytitle <- paste("mentions per ",norm_factor_businesses," Establishments",sep="")

      restaurants_per_local_authority.df$District <- as.character(restaurants_per_local_authority.df$District)
      summary_data      <- left_join(summary_data,restaurants_per_local_authority.df[,c("District","TotalEstablishments")], by="District")
      summary_data$Norm <- summary_data$TotalEstablishments/norm_factor_businesses
    } else if(input$norm == "Population") {
      mytitle <- paste("mentions per ",norm_factor_population," people",sep="")

      population_per_local_authority.df$District <- as.character(population_per_local_authority.df$District)
      summary_data      <- left_join(summary_data,population_per_local_authority.df[,c("District","all_ages")],by="District")
      summary_data$Norm <- summary_data$all_ages/norm_factor_population
    }

    radius    <- summary_data$count/summary_data$Norm
    colorData <- radius
    pal       <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # radius    <- (radius / max(radius))*30000
    # radius    <- 10000
    radius    <- 2500

    leafletProxy("map", data = summary_data) %>%
      clearShapes() %>%
      addCircles(~long, ~lat, radius=radius,stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      # addCircles(~long, ~lat, radius=radius,layerId=~zipcode,stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=mytitle,layerId="colorLegend")

    # leafletProxy("map", data = the_data[selection,]) %>%
    #   clearMarkers() %>%
    #   addMarkers(lat = ~latitude,lng = ~longitude,
    #                popup = ~as.character(original_content))

  })

}

shinyApp(ui = ui, server = server)
