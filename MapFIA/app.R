library(raster)
library(rgdal)
library(ggplot2)
library(googledrive)
library(shiny)
db <- read.csv("data/summary_table_all.csv")
usa <- readOGR("data/shapefiles", "states")
usa <- subset(usa, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. usa shapefile
states <- sort(as.character(unique(usa$STATE_NAME)))
# NOTE: Need to add regions based on this map (http://www.pathwaystoscience.org/IBPImages/maps/smallusa.gif)
states <- c(states, "Northeast", "Mid-Atlantic", "Midwest", "Southeast", 
            "Southwest", "Mountain West", "Pacific West")
# List of which states need to go with which region
# Northeast: Maine, Vermont, New Hampshire, New York, Rhode Island, Massachusetts, Connecticut
# Mid-atlantic: Pennsylvania, New Jersey, Delaware, Maryland, Virginia, West Virginia
# Midwest: Ohio, Indiana, Kentucky, Michigan, Illinois, Wisconsin, Iowa, Missouri, Minnesota
# North Dakota, South Dakota, Nebraska, Kansas
# Southeast: Florida, Tennessee, North Carolina, South Carolina, Georgia, Alabama, Mississippi,
# Louisiana, Arkansas
# Southwest: Texas, Oklahoma, Arizona, New Mexico
# Mountain West: Colorado, Wyoming, Montana, Idaho, Nevada, Utah
# Pacific West: Oregon, Washington, California

# NOTE: Maybe need to add something that this is only for the contiguous US?
ui <- fluidPage(

  # Application title
  titlePanel("Mapping Tree Distributions"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("scientific.name",
                  "Scientific Name of Organism",
                  choices = db$scientific_name,
                  multiple = TRUE,
                  selected = NULL),

      selectizeInput("common.name",
                  "Common Name of Organism",
                  choices = db$common,
                  multiple = TRUE,
                  selected = NULL),

      selectInput("map",
                  "Geographic Area to Plot",
                  choices = states,
                  multiple = TRUE,
                  selectize = TRUE),
      
      uiOutput("conditional.map.options"),  
      
      numericInput("pixels",
                   "Number of Pixels",
                   min = 1,
                   max = 5000000,
                   value = 30000,
                   step = 10000),

      helpText("Plots with more pixels will take longer to generate"),
      
      actionButton("go", "Generate Map")
      
    ),

    mainPanel(
      #plotOutput(Distribution map goes here)
      #plotOutput(Proportional bar charts here (conditional display based on inputs?))
      #
    )

    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$scientific.name, {
    updateSelectizeInput(session,
                         "common.name",
                         selected = db$common[db$scientific_name %in% input$scientific.name])
  })

  observeEvent(input$common.name, {
    updateSelectizeInput(session,
                         "scientific.name",
                         selected = db$scientific_name[db$common %in% input$common.name])
  })
  
  observeEvent(input$go, {
    # script for accessing files and any necessary cleaning
    output$map <- renderPlot({
      # Plot script goes here
    })
  })
  
  observeEvent(input$map, {
    if (input$map == "Northeast") {
      updateSelectizeInput(session,
                           "map",
                           selected = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York",                             "Rhode Island", "Vermont"))
    } else if (input$map == "Mid-Atlantic") {
      updateSelectizeInput(session,
                           "map",
                           selected = c("Delaware", "Maryland", "New Jersey", "Pennsylvania", "Virginia",
                           "West Virginia"))
    } else if (input$map == "Southeast") {
      
    } else if (input$map == "Midwest") {
      updateSelectizeInput(session,
                           "map",
                           selected = c("Illiois", "Indiana", "Iowa", "Kansas", "Kentucky", "Michigan",
                                        "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio",
                                        "South Dakota", "Wisconsin"))
      
      Ohio, Indiana, Kentucky, Michigan, Illinois, Wisconsin, Iowa, Missouri, Minnesota
      # North Dakota, South Dakota, Nebraska, Kansas
    } else if (input$map == "Southwest") {
      
    } else if (input$map == "Mountain West") {
      
    } else (input$map == "Pacific West") {
      
    }
  })
  
  output$conditional.map.options <- renderUI({
    if (length(input$scientific.name) > 1 |
        length(input$common.name) > 1) {
      checkboxGroupInput(
        "dist.options",
        label = "Distribution Mapping Options",
        choices = list(
          "Map showing the combined distribution of selected species" =
            "overlay",
          "Map of only the co-occurence of selected species" =
            "cooccurence"
        ),
        selected = "overlay"
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

