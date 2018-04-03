# Load libraries
library(raster)
library(rgdal)
library(ggplot2)
library(googledrive)
library(shiny)
library(data.table)
library(scales)
# TO DO:
# Write scientific and common names to tot.ba dt
# Put in more options to customize resulting graphs

# Global data stuff
db <- read.csv("data/summary_table_all.csv")
tot.ba.file.path <- list.files(path = "./data/", pattern = "states_tot_ba.csv", full.names = T) # get full file names
tot.ba.list <- lapply(tot.ba.file.path, read.csv)

list.id <- tot.ba.file.path %>% # extract trial numbers with this pipeline
  basename() %>%
  strsplit(split = "st")
list.id <- unlist(list.id)[c(T, F)]
names(tot.ba.list) <- list.id
tot.ba <- rbindlist(tot.ba.list)
names(tot.ba) <- c("X", "spp_code", "state", "tot_ba")
usa <- readOGR("data/shapefiles", "states")
usa <- subset(usa, STATE_NAME != "Hawaii" & STATE_NAME != "Alaska") # contig. usa shapefile
states <- sort(as.character(unique(usa$STATE_NAME)))
states <- c(states, "Northeast", "Mid-Atlantic", "Midwest", "Southeast", 
            "Southwest", "Mountain West", "Pacific West")

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

      selectInput("shapefiles",
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
      plotOutput("barchart")
      #
    )

    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Reactive code chunk updating common name field if species selected by scientific name
  observeEvent(input$scientific.name, {
    updateSelectizeInput(session,
                         "common.name",
                         selected = db$common[db$scientific_name %in% input$scientific.name])
  })
  # Reactive code chunk updating scientific name field if species selected by common name
  observeEvent(input$common.name, {
    updateSelectizeInput(session,
                         "scientific.name",
                         selected = db$scientific_name[db$common %in% input$common.name])
  })
  
  
  
  # For selecting states by region
  # Regions based on this map (http://www.pathwaystoscience.org/IBPImages/maps/smallusa.gif)
  observeEvent(input$shapefiles, {
    if ("Northeast" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Northeast"], "Connecticut", 
                                        "Maine", "Massachusetts", "New Hampshire", "New York",
                                        "Rhode Island", "Vermont"))
      
    } else if ("Mid-Atlantic" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Mid-Atlantic"], "Delaware",
                                        "Maryland", "New Jersey", "Pennsylvania", "Virginia",
                           "West Virginia"))
      
    } else if ("Southeast" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Southeast"], "Alabama", "Arkansas", 
                                        "Florida", "Georgia", "Louisiana", "Mississippi",
                                        "North Carolina", "South Carolina", "Tennessee"))
      
    } else if ("Midwest" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Midwest"], "Illiois", "Indiana", 
                                        "Iowa", "Kansas", "Kentucky", "Michigan",
                                        "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio",
                                        "South Dakota", "Wisconsin"))
      
    } else if ("Southwest" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Southwest"], "Arizona", 
                                        "New Mexico", "Oklahoma", "Texas"))
    
    } else if ("Mountain West" %in% input$shapefiles) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Mountain West"], "Colorado", 
                                        "Idaho", "Montana", "Nevada", "Utah", "Wyoming"))
     
    } else if("Pacific West" %in% input$shapefiles ) {
      updateSelectizeInput(session,
                           "map",
                           selected = c(input$shapefiles[!input$shapefiles %in% "Pacific West"], "California", 
                                        "Oregon", "Washington"))
      
    }
  })
  
  # Conditional part of side panel - show if selected species is > 1
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
  
  # Reactive to "go" button, plot selection
  observeEvent(input$go, {
    # script for accessing files and any necessary cleaning
    output$map <- renderPlot({
      # Plot script goes here
    })
    output$barchart <- renderPlot({
      # Get ID for entered spp
      id <- db$spp_code[db$scientific_name %in% input$scientific.name]
      # Get scientific & common names for select spp
      sci.name <- db$scientific_name[db$spp_code %in% id]
      com.name <- db$common[db$spp_code %in% id]
      # Get region for entered states
      regions <- input$shapefiles
      # Get basal area for spp & state combos
      subset.tot.ba <- tot.ba[spp_code %in% id & state %in% regions]
      # Get total basal area for selected region
      sum.tot.ba <- sub.tot.ba[, .(tot.ba = sum(tot.ba)), by = spp_code][, state := "Total"]
      # Recombine datatable
      subset.tot.ba <-rbindlist(
          list(subset.tot.ba, sum.tot.ba),
          use.names = T,
          fill = T,
          idcol = F
        )
      # Percent proportion bar chart
      ggplot(subset.tot.ba, aes(x = state, y = tot_ba, fill = spp_code)) +
        geom_bar(position = "fill", stat = "identity") +
        scale_y_continuous(labels = percent_format()) +
        labs(x = "States", y = "Percentage of Total Basal Area in State")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

