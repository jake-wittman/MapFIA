# Load libraries
library(raster)
library(rgdal)
library(ggplot2)
library(googledrive)
library(shiny)
library(data.table)
library(scales)
# TO DO:
# Put in more options to customize resulting graphs
# Make raster map aesthetics reactive (not the map itself)

# Global data 
db <- fread("data/summary_table_all.csv") # db with name info
# Basal area data
tot.ba <- rbindlist(tot.ba.list) 
tot.ba <- fread("data/basal_area_summary.csv")
names(tot.ba) <- c("spp_code", "state", "tot_ba") 
# State shapefiles
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
  
  # Reactive to "go.bp" button, plots selection
  bc <- eventReactive(input$go, {
    # Get ID for select spp
    id <- db$spp_code[db$scientific_name %in% input$scientific.name]
    # Get scientific & common names for select spp
    sci.name <- db$scientific_name[db$spp_code %in% id]
    com.name <- db$common[db$spp_code %in% id]
    # Get region for entered states
    regions <- input$shapefiles
    # Get basal area for spp & state combos
    subset.tot.ba <- tot.ba[spp_code %in% id & state %in% regions]
    # Get total basal area for selected region
    sum.tot.ba <- subset.tot.ba[, .(tot_ba = sum(tot_ba)), by = spp_code][, state := "Total"]
    # Recombine datatable
    subset.tot.ba <- rbindlist(
      list(subset.tot.ba, sum.tot.ba),
      use.names = T,
      fill = T,
      idcol = F)
    
    # Percent proportion bar chart
    ggplot(subset.tot.ba, aes(x = state, y = tot_ba, fill = spp_code)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(labels = percent_format()) +
      theme_bw() + 
      labs(x = "States", y = "Percentage of Total Basal Area in State") +
      scale_fill_discrete(labels = sci.name,
                          name = "Species") 
  })
  
  output$barchart <- renderPlot({
    bc()
  })

  
  # Reactive to map button
  map <- eventReactive(input$go, {
    id <- db$spp_code[db$scientific_name %in% input$scientific.name]
    # Get scientific & common names for select spp
    sci.name <- db$scientific_name[db$spp_code %in% id]
    com.name <- db$common[db$spp_code %in% id]
    # Get region for entered states
    regions <- input$shapefiles
    
    if (is.na(regions)) { # If no selection made, plot whole US
      plot <- gplot(x = SPECIES, maxpixels = 2000) +
        geom_raster(aes(x = x, y = y, fill = value)) +
        geom_polygon(data = contig.usa, aes(x = long, y = lat, group = group),
                     fill = NA, color = "black")
     # This part here v needs to be reactive to changing plot aesthetics
      # but should not remake plot
       plot +
        scale_fill_gradientn(colors = c("white", terrain.colors(2)),name = "Basal Area") + 
        theme_map()
      
    } else { # plot just selected state shapefiles
      
    }
  })
  
  output$map <- renderPlot({
    map()
  })
}
  


# Run the application
shinyApp(ui = ui, server = server)

